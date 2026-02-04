;; I rewrote this literally over a dozen times
;; I will rewrite it one more later, but
;; at the current time this *does* 
;; work for AMD GPUs (and with little)
;; effort it can be cleaned up to work 
;; all GPUs. Calling it a day though.
(define-module (gunit packages ai)
  #:use-module (guix)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages perl)
  #:use-module (guix-science-nonfree packages cuda)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system cmake)
  #:use-module (gunit packages rocm-base)
  #:use-module (guix build-system trivial)
  #:use-module (gunit packages rocm-tools)
  #:use-module (gunit packages rocm-libs)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gl)
  #:use-module (gunit packages rocm-hip)
  #:use-module (gunit packages go-common))

(define-public rocm-union
  (package
    (name "rocm-union")
    (version "6.2.2")
    (source
     #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build union))
      #:builder
      #~(begin
          (use-modules (guix build union))
          (union-build #$output
                       (list #$hipamd
                             #$hipblas
                             #$rocblas
                             #$rocm-smi
                             #$rocm-device-libs-6
                             #$rocr-runtime
                             #$rocm-comgr
                             #$rocm-toolchain)))))
    (inputs (list hipamd
                  hipblas
                  rocblas
                  rocm-smi
                  rocm-device-libs-6
                  rocr-runtime
                  rocm-comgr
                  rocm-toolchain))
    (home-page "https://rocm.docs.amd.com/")
    (synopsis "Union of ROCm libraries for Ollama")
    (description
     "A merged directory of ROCm libraries to simplify LD_LIBRARY_PATH.")
    (license license:expat)))

(define-public vulkan-union
  (package
    (name "vulkan-union")
    (version "1.4.321")
    (source
     #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build union))
      #:builder
      #~(begin
          (use-modules (guix build union))
          (union-build #$output
                       (list #$vulkan-loader
                             #$vulkan-headers
                             #$vulkan-tools
                             #$glslang
                             #$mesa
                             #$shaderc)))))
    (inputs (list vulkan-headers
                  vulkan-loader
                  vulkan-tools
                  shaderc
                  glslang
                  mesa))
    (home-page "https://www.vulkan.org/")
    (synopsis "Union of Vulkan libraries for Ollama")
    (description
     "A merged directory of Vulkan libraries to simplify LD_LIBRARY_PATH.")
    (license license:expat)))

(define-public ollama-libs
  (package
    (name "ollama-libs")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ollama/ollama")
             (commit (string-append "v" version))))
       (file-name (git-file-name "ollama" version))
       (modules '((guix build utils)))
       (sha256
        (base32 "149l7s99n7f3ys6hrxrcf4pq85cmpgxyj5d1vc720sr6cdajxwc5"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f
      #:configure-flags
      #~(let ((rocm (assoc-ref %build-inputs "rocm-union")))
          (list (string-append "-DCMAKE_CXX_COMPILER=" rocm "/bin/hipcc")
                (string-append "-DCMAKE_C_COMPILER=" rocm "/bin/hipcc")
                (string-append "-DCMAKE_HIP_COMPILER=" rocm "/bin/hipcc")
                (string-append "-DCMAKE_CXX_FLAGS=" "--rocm-device-lib-path="
                               rocm "/amdgcn/bitcode "
                               "-fpermissive -D_GNU_SOURCE")
                (string-append "-DCMAKE_PREFIX_PATH=" rocm ";"
                               #$vulkan-union)
                (string-append "-DCMAKE_INSTALL_RPATH="
                               rocm
                               ";"
                               #$vulkan-tools
                               ";"
                               "$ORIGIN")
                (string-append "-DVULKAN_SDK="
                               #$vulkan-union)
                "-DAMDGPU_TARGETS=gfx1030"
                ;;  "-DCMAKE_HIP_ARCHITECTURES=gfx1030;gfx1100"
                "-DCMAKE_INSTALL_LIBDIR=lib/ollama"
                "-DCMAKE_INSTALL_BINDIR=lib/ollama"
                "-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON"
                "-DCMAKE_SKIP_BUILD_RPATH=ON"
                "-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON"
                "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CGO_ENABLED" "1")
              (setenv "CGO_CFLAGS"
                      (string-append "-I"
                                     #$(package-source ollama-libs)
                                     "/ml/backend/ggml/ggml/include"))
              (setenv "CGO_LDFLAGS"
                      (string-append "-L"
                                     (assoc-ref inputs "ollama-libs")
                                     "/lib/ollama"))

              (invoke "go"
                      "build"
                      "-tags"
                      "rocm vulkan"
                      "-ldflags"
                      "-X github.com/ollama/ollama/gpu.PayloadLibDir="
                      "github.com/ollama/ollama"
                      "-o"
                      "ollama")))
          ;;          (delete 'validate-runpath)
          (delete 'strip))))
    (native-inputs (list cmake pkg-config perl bash))
    (inputs (list rocm-union vulkan-union glslang))
    (home-page "https://ollama.com")
    (synopsis "Ollama C++ backend")
    (description "C++ backend libraries for Ollama.")
    (license license:expat)))

(define-public ollama
  (package
    (name "ollama")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ollama/ollama")
             (commit (string-append "v" version))))
       (file-name (git-file-name "ollama" version))
       (modules '((guix build utils)))
       (sha256
        (base32 "0bg2rqs9n9lgpv87fk22ragspgg4cy5cd29nr8vgr8zd8whdlsys"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:go go-1.24
      #:allow-go-reference? #t
      #:unpack-path "github.com/ollama/ollama"
      #:import-path "github.com/ollama/ollama"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'cmake
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (begin
                (chdir "src/github.com/ollama/ollama")

                (system (string-join `("cmake -B build"

                                       "-DCMAKE_SKIP_BUILD_RPATH=ON"
                                       "-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON"
                                       "-DAMDGPU_TARGETS=gfx1030"
                                       ,(string-append "-DCMAKE_CXX_COMPILER="
                                         #$rocm-union "/bin/hipcc")
                                       ,(string-append "-DCMAKE_C_COMPILER="
                                                       #$rocm-union
                                                       "/bin/hipcc")
                                       ,(string-append "-DCMAKE_HIP_COMPILER="
                                         #$rocm-union "/bin/hipcc")
                                       ,(string-append "-DROCM_PATH="
                                                       #$rocm-union)
                                       ,(string-append "-DHIP_PATH="
                                                       #$rocm-union)
                                       ,(string-append "-DVulkan_LIBRARY="
                                                       #$vulkan-union
                                                       "/lib/libvulkan.so")
                                       ,(string-append "-DVulkan_INCLUDE_DIR="
                                         #$vulkan-union "/include")

                                       ,(string-append "-DCMAKE_CXX_FLAGS=\""
                                         "--rocm-device-lib-path="
                                         #$rocm-union "/amdgcn/bitcode "
                                         "-fpermissive -D_GNU_SOURCE\"")
                                       ,(string-append
                                         "-DCMAKE_PREFIX_PATH=\""
                                         #$rocm-union ";"
                                         #$vulkan-union "\"")) " "))
                (system (string-append "cmake --build build -j"
                                       (number->string (parallel-job-count))))
                (let ((out-lib (string-append #$output "/lib")))
                  (mkdir-p out-lib)
                  (copy-recursively "build/lib" out-lib))
                (let ((out-lib (string-append ".")))
                  (copy-recursively "build/lib/ollama" out-lib))
                (symlink (string-append #$rocm-union "/lib")
                         (string-append "build/lib/ollama/rocm")))))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CGO_ENABLED" "1")

              (let ((root (getcwd)))
                (setenv "CGO_CFLAGS"
                        (string-append "-I" root
                                       "/ml/backend/ggml/ggml/include"))
                (setenv "CGO_LDFLAGS"
                        (string-append "-L" root "/ml/backend/ggml/ggml/src")))
              (invoke "go"
                      "build"
                      "-tags"
                      "rocm vulkan"
                      "-ldflags"
                      "-X=github.com/ollama/ollama/version.Version=0.15.2"
                      "-o"
                      "ollama"
                      ".")
              (install-file "ollama" "../../../..")
              (chdir "../../../..")))

          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib-ollama (string-append out "/lib/ollama"))
                     (src-path "src/github.com/ollama/ollama"))
                
                (mkdir-p bin)
                (mkdir-p lib-ollama)

                (install-file (string-append src-path "/ollama") bin)

                (for-each (lambda (f)
                            (install-file f lib-ollama))
                          (find-files (string-append src-path
                                                     "/build/lib/ollama")
                                      "\\.so$"))

                (symlink (string-append (assoc-ref inputs "rocm-union") "/lib")
                         (string-append lib-ollama "/rocm")))))
          (delete 'check)
          (delete 'strip)
          (delete 'validate-runpath))))

    (native-inputs (list pkg-config patchelf cmake))
    (inputs `(("rocm-union" ,rocm-union)
              ("vulkan-loader" ,vulkan-loader)
              ("libdrm" ,libdrm)
              ("gcc:lib" ,gcc)
              ("go-std-1.23" ,go-std-1.23)
              ("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
              ("go-github-com-pdevine-tensor" ,go-github-com-pdevine-tensor)
              ("go-golang-org-x-text" ,go-golang-org-x-text)
              ("go-golang-org-x-term" ,go-golang-org-x-term)
              ("go-golang-org-x-sys" ,go-golang-org-x-sys)
              ("go-golang-org-x-exp" ,go-golang-org-x-exp)
              ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
              ("go-github-com-gin-contrib-cors" ,go-github-com-gin-contrib-cors)
              ("go-golang-org-x-tools" ,go-golang-org-x-tools)
              ("go-golang-org-x-image" ,go-golang-org-x-image)
              ("go-gorgonia-org-tensor" ,go-gorgonia-org-tensor)
              ("go-github-com-nlpodyssey-gopickle" ,go-github-com-nlpodyssey-gopickle)
              ("go-github-com-mattn-go-runewidth" ,go-github-com-mattn-go-runewidth)
              ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
              ("go-github-com-emirpasic-gods-v2" ,go-github-com-emirpasic-gods-v2)
              ("go-github-com-dlclark-regexp2-v1.11.5" ,go-github-com-dlclark-regexp2-v1.11.5)
              ("go-github-com-d4l3k-go-bfloat16" ,go-github-com-d4l3k-go-bfloat16)
              ("go-github-com-agnivade-levenshtein" ,go-github-com-agnivade-levenshtein)
              ("go-golang-org-x-sync" ,go-golang-org-x-sync)
              ("go-github-com-x448-float16" ,go-github-com-x448-float16)
              ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
              ("go-github-com-spf13-cobra" ,go-github-com-spf13-cobra)
              ("go-github-com-olekukonko-tablewriter" ,go-github-com-olekukonko-tablewriter)
              ("go-github-com-wk8-go-ordered-map-v2" ,go-github-com-wk8-go-ordered-map-v2)
              ("go-github-com-google-uuid" ,go-github-com-google-uuid)
              ("go-github-com-gin-gonic-gin" ,go-github-com-gin-gonic-gin)
              ("go-github-com-containerd-console" ,go-github-com-containerd-console)))
    (home-page "https://ollama.com")
    (synopsis "Get up and running with large language models.")
    (description "Run LLMs locally.")
    (license license:expat)))

ollama
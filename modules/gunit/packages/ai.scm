;; Currently non-functional in a practical sense
;; Will detect the GPU but models fail to execute.
;; Upstream progress on ROCm 7.1's math libraries
;; ROCBLAS and HIPBLAS are in merge progress
;; actively, so we should wait for that and try
;; again with the 7.1 build which may address 
;; some stability issues anyway. Doesn't hurt
;; to have eyes belonging to smarter individuals
;; than myself looking at it.
;; https://codeberg.org/guix/guix/pulls/5787
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

;;(define-public cuda-union
;;  )


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
    (version "0.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ollama/ollama")
             (commit (string-append "v" version))))
       (file-name (git-file-name "ollama" version))
       (modules '((guix build utils)))
       (sha256
        (base32 "1d1x83056jnjahm2nw7m2i9mkalai5xn6h1rrkq9y615770wd052"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f
      #:build-flags
      #~(list (string-append
               "-ldflags=-X github.com/ollama/ollama/server.RunnerDir="
               #$output "/lib/ollama"))
      #:configure-flags
      #~(let ((rocm (assoc-ref %build-inputs "rocm-union")))
          (list "-DCMAKE_SKIP_BUILD_RPATH=ON"
                "-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON"
                "-DAMDGPU_TARGETS=gfx1030"
                "-DCMAKE_INSTALL_LIBDIR=lib/ollama" ;; Can be removed for >15
                ;;  "-DCMAKE_INSTALL_BINDIR=lib/ollama" ;; Can be removed for >15
                (string-append "-DCMAKE_CXX_COMPILER="
                               #$rocm-union "/bin/hipcc")
                (string-append "-DCMAKE_C_COMPILER="
                               #$rocm-union "/bin/hipcc")
                (string-append "-DCMAKE_HIP_COMPILER="
                               #$rocm-union "/bin/hipcc")
                (string-append "-DROCM_PATH="
                               #$rocm-union)
                (string-append "-DHIP_PATH="
                               #$rocm-union)
                (string-append "-DVulkan_LIBRARY="
                               #$vulkan-union "/lib/libvulkan.so")
                (string-append "-DVulkan_INCLUDE_DIR="
                               #$vulkan-union "/include")
                (string-append "-DVULKAN_SDK="
                               #$vulkan-union)
                (string-append "-DOLLAMA_RUNNER_DIR=" "rocm") ;; Likely needs to be dynamically set
                (string-append "-DCMAKE_CXX_FLAGS=" "--rocm-device-lib-path="
                               #$rocm-union "/amdgcn/bitcode "
                               "-fpermissive -D_GNU_SOURCE")
                (string-append "-DCMAKE_INSTALL_RPATH="
                               #$rocm-union
                               "/lib"
                               ";"
                               #$vulkan-union
                               "/lib"
                               ";"
                               #$output
                               "/lib/ollama")
                (string-append "-DCMAKE_PREFIX_PATH="
                               #$rocm-union ";"
                               #$vulkan-union)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (delete 'validate-runpath)
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
    (version "0.12.3")
    (source
     (package-source ollama-libs))
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
          (add-after 'install 'copy
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((lib-ollama (string-append (assoc-ref outputs "out")
                                                "/lib/ollama")))
                (mkdir-p lib-ollama)
                (copy-recursively (string-append #$ollama-libs "/lib/ollama")
                                  lib-ollama
                                  #:copy-file symlink))))
          (add-after 'install 'wrap
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin/ollama"))
                     (libs (string-append #$ollama-libs "/lib/ollama")))
                (wrap-program bin
                  `("OLLAMA_LIBRARY_PATH" ":" prefix
                    (,libs))
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append #$rocm-union "/lib")))))))
          (delete 'check)
          (delete 'strip)
          (delete 'validate-runpath))))

    (native-inputs (list pkg-config patchelf cmake))
    (inputs `(("ollama-libs" ,ollama-libs)
              ("rocm-union" ,rocm-union)
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
              ("go-github-com-pkg-browser" ,go-github-com-pkg-browser)
              ("go-github-com-gin-contrib-cors" ,go-github-com-gin-contrib-cors)
              ("go-golang-org-x-tools" ,go-golang-org-x-tools)
              ("go-golang-org-x-image" ,go-golang-org-x-image)
              ("go-gorgonia-org-tensor" ,go-gorgonia-org-tensor)
              ("go-github-com-nlpodyssey-gopickle" ,go-github-com-nlpodyssey-gopickle)
              ("go-github-com-mattn-go-runewidth" ,go-github-com-mattn-go-runewidth)
              ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
              ("go-github-com-mattn-go-sqlite3" ,go-github-com-mattn-go-sqlite3)
              ("go-golang-org-x-sys" ,go-golang-org-x-sys)
              ("go-github-com-tkrajina-typescriptify-golang-structs" ,go-github-com-tkrajina-typescriptify-golang-structs)
              (" go-github-com-thetitanrain-w32" ,go-github-com-thetitanrain-w32)
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
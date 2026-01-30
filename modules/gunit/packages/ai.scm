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
  #:use-module (guix build-system cmake)
  #:use-module (gunit packages rocm-base)
  #:use-module (gunit packages rocm-tools)
  #:use-module (gunit packages rocm-libs)
  #:use-module (gnu packages llvm)
  #:use-module (gunit packages rocm-hip)
  #:use-module (gunit packages go-common))

(define-public ollama-bin
  (package
    (name "ollama-bin")
    (version "0.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ollama/ollama")
             (commit (string-append "v" version))))
       (file-name (git-file-name "ollama" version))
       (modules '((guix build utils)))
       (snippet #~(begin
                    (for-each (lambda (file)
                                (substitute* file
                                  (("github.com/pdevine/tensor")
                                   "gorgonia.org/tensor")))
                              (append (find-files "convert" "\\.go$")
                                      (list "go.mod" "go.sum")))))
       (sha256
        (base32 "0vy8mwfxfxszayi7c5jbmz0wisvp77q1pgv8v0lfmz2bfw4jzykx"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f
      #:configure-flags
      #~(list (string-append "-DCMAKE_CXX_COMPILER="
                             #$hipamd "/bin/hipcc")
              (string-append "-DCMAKE_C_COMPILER="
                             #$hipamd "/bin/hipcc")
              (string-append "-DCMAKE_PREFIX_PATH="
                             #$hipblas ";"
                             #$rocblas)
              "-DGGML_HIPBLAS=ON"
              "-DGGML_HIP=ON"
              "-DCLR_BUILD_HIP=ON"
              "-DCLR_BUILD_OCL=OFF"
              "-DHIP_ENABLE_ROCPROFILER_REGISTER=OFF"
              "-D__HIP_ENABLE_PCH=OFF"
              "-DHIP_PLATFORM=amd")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (delete 'validate-runpath))))
    (native-inputs (list cmake pkg-config hipcc perl bash))
    (propagated-inputs (list perl))
    (inputs (list hipamd rocblas hipblas))
    (home-page "https://ollama.com")
    (synopsis "Ollama C++ backend")
    (description "C++ backend libraries for Ollama.")
    (license license:expat)))

(define-public ollama
  (package
    (name "ollama")
    (version "0.13.2")
    (source
     (package-source ollama-bin))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.24
      #:unpack-path "github.com/ollama/ollama"
      #:import-path "github.com/ollama/ollama"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (delete 'validate-runpath)
          (add-after 'install 'install-backend
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (backend (assoc-ref inputs "ollama-bin"))
                     (runners-dst (string-append out "/lib/ollama")))
                (mkdir-p runners-dst)

                (copy-recursively (string-append backend "/lib")
                                  (string-append runners-dst))

                (wrap-program (string-append out "/bin/ollama")
                  `("LD_LIBRARY_PATH" prefix
                    (,(string-append backend "/lib") ,(string-append (assoc-ref
                                                                      inputs
                                                                      "hipamd")
                                                                     "/lib")
                     ,(string-append (assoc-ref inputs "hipblas") "/lib")
                     ,(string-append (assoc-ref inputs "rocblas") "/lib")
                     ,(string-append (assoc-ref inputs "rocblas")
                                     "/lib/rocblas")
                     ,(string-append (assoc-ref inputs "rocm-toolchain")
                                     "/lib")))
                  `("HSA_OVERRIDE_GFX_VERSION" =
                    ("10.3.0")))))))))

    (native-inputs (list pkg-config))
    (inputs (list ollama-bin
                  hipamd
                  hipblas
                  rocblas
                  rocm-toolchain

                  go-std-1.23
                  go-google-golang-org-protobuf
                  go-golang-org-x-text
                  go-golang-org-x-term
                  go-golang-org-x-sys
                  go-golang-org-x-exp
                  go-golang-org-x-crypto
                  go-github-com-gin-contrib-cors
                  go-golang-org-x-tools
                  go-golang-org-x-image
                  go-gorgonia-org-tensor
                  go-github-com-nlpodyssey-gopickle
                  go-github-com-mattn-go-runewidth
                  go-github-com-google-go-cmp
                  go-github-com-emirpasic-gods-v2
                  go-github-com-dlclark-regexp2-v1.11.5
                  go-github-com-d4l3k-go-bfloat16
                  go-github-com-agnivade-levenshtein
                  go-golang-org-x-sync
                  go-github-com-x448-float16
                  go-github-com-stretchr-testify
                  go-github-com-spf13-cobra
                  go-github-com-olekukonko-tablewriter
                  go-github-com-google-uuid
                  go-github-com-gin-gonic-gin
                  go-github-com-containerd-console))
    (home-page "https://ollama.com")
    (synopsis "Get up and running with large language models.")
    (description "Run LLMs locally.")
    (license license:expat)))


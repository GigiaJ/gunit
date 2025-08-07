(define-module (gunit packages code-server)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages elf)
  #:use-module (selected-guix-works packages fonts)
  #:use-module (guix build-system copy)
  #:use-module (guix build copy-build-system))


(define-public code-server
    (package
    (name "code-server")
    (version "4.102.2")
    (source (origin
    (method url-fetch)
    (uri (string-append "https://github.com/coder/code-server/releases/download/v" version "/code-server-" version "-linux-amd64.tar.gz"))
        (sha256
        (base32 "1s33d8821hcpkv10643dc58m177nrsy4238fyx2qbkh3smx98p25"))))
    (build-system copy-build-system)

    (inputs
        (list 
        gcc-toolchain
        glibc
        ))
        (arguments
        (list
        #:tests? #f             ; no check target
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'install 'patch-interpreter
                  (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (interpreter (string-append (assoc-ref inputs "glibc")
                                                       "/lib/ld-linux-x86-64.so.2"))
                           (binary (string-append out "/lib/node")))
                      (invoke "patchelf" "--set-interpreter" interpreter binary))))
                (add-after 'patch-interpreter 'wrap 
                    (lambda _
                    (wrap-program (string-append (assoc-ref %outputs "out") "/lib/node")
                        `("LD_LIBRARY_PATH" ":" prefix (,(string-append #$(this-package-input "gcc-toolchain") "/lib"))))))
                (delete 'validate-runpath))))
    (native-inputs
        (list git curl patchelf))
    (synopsis "Code server used for accessing VS Code in the browser.")
    (home-page "https://coder.com/")
    (description "Free open source code server. Use a VS Code in any browser wherever you are.")
    (license license:expat)))

code-server
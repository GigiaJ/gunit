(define-module (gchannel packages code-server)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system copy)
  #:use-module (guix build copy-build-system)
)

(define-public code-server
    (package
    (name "code-server")
    (version "4.98.0")
    (source (origin
    (method url-fetch)
    (uri (string-append "https://github.com/coder/code-server/releases/download/v" version "/code-server-" version "-linux-amd64.tar.gz"))
  
        (sha256
        (base32 "0ajiy9aav1w7pf7bdvyqbd54v9kfl9qbc1446p7i9nhv5wd1h7p1"))))
    (build-system copy-build-system)

    (inputs
        (list 
        gcc-toolchain))
        (arguments
        (list
        #:tests? #f             ; no check target
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'install 'wrap
                (lambda _ 
                (wrap-program (string-append (assoc-ref %outputs "out") "/lib/node")
                `("LD_LIBRARY_PATH" ":" prefix (
                    ,(string-append #$(this-package-input "gcc-toolchain") "/lib")
                )))))
                (delete 'validate-runpath)
            )))
    (native-inputs
        (list git))
    (synopsis "Soup")
    (home-page "https://coder.com/")
    (description "Free open source code server")
    (license license:agpl3)))

    code-server
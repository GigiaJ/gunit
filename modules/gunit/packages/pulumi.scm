(define-module (gunit packages pulumi)
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
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (guix build copy-build-system))


(define-public pulumi
    (package
    (name "pulumi")
    (version "3.190.0")
    (source (origin
    (method url-fetch)
    (uri (string-append "https://get.pulumi.com/releases/sdk/pulumi-v" version "-linux-x64.tar.gz"))
        (sha256
        (base32 "0l3vhn847dm9lvd21pcl254ad030ybann4fkrvl37x10znhabkd8"))))
    (build-system copy-build-system)

    (inputs
        (list 
        gcc-toolchain
        glibc
        ))
        (arguments
        (list
        #:tests? #f             ; no check target

                      #:install-plan #~'(("." "bin"))
            #:phases
            #~(modify-phases %standard-phases
;;                (add-after 'install 'patch-interpreter
;;                  (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
;;                    (let* ((out (assoc-ref outputs "out"))
;;                           (interpreter (string-append (assoc-ref inputs "glibc")
;;                                                     "/lib/ld-linux-x86-64.so.2"))
;;                            (binaries (list 
;;                         (string-append out "/bin/pulumi")
;;                         (string-append out "/bin/pulumi-analyzer-policy")
;;                         (string-append out "/bin/pulumi-analyzer-policy-python")
;;                         (string-append out "/bin/pulumi-language-dotnet")
;;                         (string-append out "/bin/pulumi-language-go")
;;                         (string-append out "/bin/pulumi-language-java")
;;                         (string-append out "/bin/pulumi-language-nodejs")
;;                         (string-append out "/bin/pulumi-language-python")
;;                         (string-append out "/bin/pulumi-language-python-exec")
;;                         (string-append out "/bin/pulumi-resource-pulumi-nodejs")
;;                         (string-append out "/bin/pulumi-resource-pulumi-python")
;;                         (string-append out "/bin/pulumi-watch"))))
;;                     (for-each (lambda (f) ((invoke "patchelf" "--set-interpreter" interpreter f))) binaries))))

                ;;(add-after 'patch-interpreter 'wrap 
                  ;;  (lambda _
                    ;;(wrap-program (string-append (assoc-ref %outputs "out") "/lib/node")
                      ;;  `("LD_LIBRARY_PATH" ":" prefix (,(string-append #$(this-package-input "gcc-toolchain") "/bin"))))))
                (delete 'validate-runpath))))
    (native-inputs
        (list git curl patchelf))
    (synopsis "Infrastructure as code in whatever language you choose.")
    (home-page "https://www.pulumi.com/")
    (description "Iinfrastructure code using programming languages you know and love. Write statements to define infrastructure using your IDE with autocomplete, type checking, and documentation.")
    (license license:asl2.0)))

pulumi
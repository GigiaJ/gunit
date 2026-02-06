(define-module (gunit packages azure)
  #:use-module (guix)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix git-download)
  #:use-module (gnu packages elf)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gcc)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define-public azure-cli
  (package
    (name "azure-cli")
    (version "2.83.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://packages.microsoft.com/repos/"
                           name
                           "/pool/main/a/"
                           name
                           "/"
                           name
                           "_"
                           version
                           "-1~"
                           "bookworm_amd64.deb"))
       (file-name (string-append name "-" version "_bookworm_amd64.deb"))
       (sha256
        (base32 "1xb1h2bmzab4a73wdp5c1lwpzawsnhsbb80xsrg147ip6ia4bnv6"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "ar" "x" source)
              (invoke "tar" "xf" "data.tar.xz")))
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (py (string-append out "/opt/az/bin/python3"))
                     (lib-path (string-join (map (lambda (name)
                                                   (string-append (assoc-ref
                                                                   inputs name)
                                                                  "/lib"))
                                                 '("zlib" "openssl" "libffi"
                                                   "util-linux" "gcc-lib"))
                                            ":")))
                (copy-recursively "opt"
                                  (string-append out "/opt"))
                (copy-recursively "usr"
                                  (string-append out "/usr"))
                (invoke "patchelf" "--set-interpreter"
                        (string-append (assoc-ref inputs "glibc")
                                       "/lib/ld-linux-x86-64.so.2") py)
                (mkdir-p bin)
                (with-output-to-file (string-append bin "/az")
                  (lambda ()
                    (format #t
                            ;; I think my formatter is too opinionated
                            (string-append "#!/bin/sh\n"
                             "export LD_LIBRARY_PATH=~a:$LD_LIBRARY_PATH" "\n"
                             "exec ~a -W ignore -m azure.cli \"$@\"\n")
                            lib-path py)))
                (chmod (string-append bin "/az") #o755)))))))
    (inputs `(("glibc" ,(@@ (gnu packages base) glibc-for-fhs))
              ("zlib" ,zlib)
              ("openssl" ,openssl)
              ("libffi" ,libffi)
              ("util-linux" ,util-linux)
              ("gcc-lib" ,gcc "lib")))
    (native-inputs (list binutils tar patchelf))
    (synopsis "Azure CLI")
    (description "Condensed FHS-compatible Azure CLI for Guix.")
    (home-page "https://github.com/Azure/azure-cli")
    (license license:asl2.0)))

azure-cli
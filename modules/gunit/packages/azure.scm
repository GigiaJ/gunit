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

(define-public az
  (package
    (name "azure-cli")
    (version "2.82.0-1")
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
                           "~"
                           "bookworm_amd64.deb"))
       (file-name (string-append name "-" version "_bookworm_amd64.deb"))
       (sha256
        (base32 "045mx5j2hhrbh54brg6w2z9v59x3yzapq474k2iz2f3virfkhchz"))))
    (build-system gnu-build-system)
    (arguments
     
     '(#:tests? #f
       #:validate-runpath? #f
       #:phases

       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "ar" "x" source)
             (invoke "tar" "xf" "data.tar.xz")))
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (copy-recursively "opt"
                                 (string-append out "/opt"))
               (copy-recursively "usr"
                                 (string-append out "/usr"))
               (mkdir-p bin))))
         (add-after 'install 'write-az-wrapper
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (py (string-append out "/opt/az/bin/python3")))
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/az")
                 (lambda (port)
                   (format port "#!/bin/sh\nexec ~a -m azure.cli \"$@\"\n" py)))
               (chmod (string-append bin "/az") #o755))))

         (add-after 'install 'patch-python
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (glibc (assoc-ref inputs "glibc"))
                    (python (string-append out "/opt/az/bin/python3")))
               (invoke "patchelf" "--set-interpreter"
                       (string-append glibc "/lib/ld-linux-x86-64.so.2")
                       python))))

         )))
    (inputs (list glibc))
    (native-inputs (list binutils tar patchelf))
    (synopsis "Azure CLI")
    (description
     "Azure CLI is a CLI tool for interacting with the Azure cloud platform.")
    (home-page "https://github.com/Azure/azure-cli")
    (license license:asl2.0)))

(define azure-cli-libs
  `(("coreutils" ,coreutils)
    ("zlib" ,zlib)
    ("openssl" ,openssl)
    ("libffi" ,libffi)
    ("util-linux" ,util-linux)
    ("gcc" ,gcc "lib")))

(define azure-cli-ld.so.conf
  (packages->ld.so.conf (list (fhs-union `(,@azure-cli-libs ,@fhs-min-libs)
                                         #:name "fhs-union-64"))))

(define azure-cli-ld.so.cache
  (ld.so.conf->ld.so.cache azure-cli-ld.so.conf))

(define-public azure-cli-container
  (nonguix-container (name "az")
                     (wrap-package az)
                     (run "/bin/az")
                     (ld.so.conf azure-cli-ld.so.conf)
                     (ld.so.cache azure-cli-ld.so.cache)
                     ;; TODO: This should almost certainly be 
                     ;; shared with the actual user home
                     ;; so it creates files in expected locations   
                     (union64 (fhs-union `(,@azure-cli-libs ,@fhs-min-libs)
                                         #:name "fhs-union-64"))
                     (description (package-description az))))

(define-public azure-cli
  (nonguix-container->package azure-cli-container))


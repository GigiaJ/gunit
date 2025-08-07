(define-module (gchannel packages mac-compat-kernel)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nongnu packages linux))

(define-public linux-t2-patches
  (package
    (name "linux-t2-patches")
    (version "6.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/t2linux/linux-t2-patches.git")
                    (commit "54606b8797c539da9782dcbb16456fdf92d74f12")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1ig7373xl5vqfpblh0sh4z8vgks4hq761j58lyns5cw2n648xl61"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Disable the 'check' phase
       #:phases
       (modify-phases %standard-phases
         (replace 'configure (lambda _ #t))
         (replace 'build (lambda _ #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p out)
               (copy-recursively "." out))
             #t)))))

    (synopsis "Patch set for running Linux on T2-based Macs")
    (description "This package provides a set of patches from the t2linux project
to enable support for the Apple T2 security chip and related hardware.")
    (home-page "https://github.com/t2linux/linux-t2-patches")
    (license license:gpl2+)))

(define (mbp-t2-extra-options)
  '(("CONFIG_SPI_APPLE"        . y)
    ("CONFIG_APPLE_BCE"        . y)
    ("CONFIG_APPLE_DCP"        . y)
    ("CONFIG_PINCTRL_APPLE_GPIO" . y)))

(define-public linux-kernel-mbp-t2
  (package
    (inherit linux-6.12)
    (name "linux-kernel-mbp-t2")
    (synopsis "Linux kernel for Macs with the T2 chip")
    (description "This package provides a Linux kernel with patches applied to support hardware found in Apple T2-based MacBooks.")
    (license license:gpl2+)

    (native-inputs
     (append (package-native-inputs linux-6.12)
             `(("t2-patches" ,linux-t2-patches))))

    (arguments
     (substitute-keyword-arguments (package-arguments linux-6.12)
       ((#:native-inputs native-inputs)
        #~(cons* (list "t2-patches" ,linux-t2-patches)
                 #$native-inputs))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'apply-t2-patches
  (lambda* (#:key inputs #:allow-other-keys)
    (use-modules (guix build utils))
    (let ((patch-dir (assoc-ref inputs "t2-patches")))
      (let ((patch-files (find-files patch-dir "\\.patch$")))
        (format #t "Applying ~a T2 patches...~%" (length patch-files))
        (for-each
         (lambda (patch)
           (invoke "patch" "-p1" "-i" patch))
         (sort patch-files string<))
        #t))))))
       ((#:kernel-config config)
        #~(kernel-config-union #$config
           (kernel-config #:configs (mbp-t2-extra-options))))))))
           
linux-kernel-mbp-t2
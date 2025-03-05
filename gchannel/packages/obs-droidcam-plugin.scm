(define-module (gchannel packages obs-droidcam)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages video)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages image)
  #:use-module (guix build-system gnu)
)

(define-public obs-droidcam
    (package
    (name "obs-droidcam")
    (version "2.3.4")
    (source (origin
    (method git-fetch)
    (uri (git-reference
    (url "https://github.com/dev47apps/droidcam-obs-plugin")
  (commit version)
  (recursive? #t)))
  
        (sha256
        (base32 "0q4nbfd7xly44psmv2hb3bimcx5mp20vw8z10dqsvrsasy2hnqr9"))))
    (build-system gnu-build-system)

    (inputs
        (list 
        obs libusbmuxd libjpeg-turbo libimobiledevice ffmpeg))
        (arguments
        (list
        #:tests? #f             ; no check target
        #:make-flags
                #~(list
                    (string-append "LIBUSBMUXD=" "libusbmuxd-2.0") ;;(assoc-ref %outputs "out")
                    (string-append "LIBIMOBILEDEV=" "libimobiledevice-1.0")
                    (string-append "LIBOBS_INCLUDES=" #$(this-package-input "obs") "/include/obs")
                    (string-append "FFMPEG_INCLUDES=" #$(this-package-input "ffmpeg") "/include")
                    (string-append "ALLOWSTATIC=" "no")
                )
            #:phases
            #~(modify-phases %standard-phases
            (delete 'configure)
            (add-before 'build 'cats
            (lambda _
            (mkdir-p "build")))
            (replace 'install
            (lambda _ 
            (display (assoc-ref %outputs "out"))
            (invoke "ls" "-a" "./build")
            (mkdir-p (string-append (assoc-ref %outputs "out")  "/lib/obs-plugins"))
            (mkdir-p (string-append (assoc-ref %outputs "out")  "/share/obs/obs-plugins"))
            (invoke "cp" "./build/droidcam-obs.so" (string-append (assoc-ref %outputs "out") "/lib/obs-plugins/droidcam-obs.so"))
            (invoke "cp" "-r" "./data/locale" (string-append (assoc-ref %outputs "out") "/share/obs/obs-plugins/droidcam-obs"))
            (display "Do nothing.") #t)
            )
            )         
                ))
      
    (native-inputs
        (list git pkg-config))
    (synopsis "Soup")
    (home-page "https://dev47apps.com/obs/")
    (description "plugin for droidcam obs")
    (license license:agpl3)))

obs-droidcam
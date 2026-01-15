(define-module (gunit packages obs-plugins)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages video)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages image)
  #:use-module (guix build-system gnu))

(define-public obs-droidcam
  (package
    (name "obs-droidcam")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dev47apps/droidcam-obs-plugin")
                    (commit version)
                    (recursive? #t)))
              (sha256
               (base32 "1kkiijl25safjw7l7qp4yddba9mdhig3a0af5nx3zq2hbszvy4c7"))))
    (build-system gnu-build-system)

    (inputs
     (list obs
           simde
           libusbmuxd
           libjpeg-turbo
           libimobiledevice
           ffmpeg))
    (arguments
     (list
      #:tests? #f ; no check target
      #:make-flags
      #~(list
         (string-append "LIBUSBMUXD=" "libusbmuxd-2.0")
         (string-append "LIBIMOBILEDEV=" "libimobiledevice-1.0")
         (string-append "LIBOBS_INCLUDES=" #$(this-package-input "obs") "/include/obs")
         (string-append "FFMPEG_INCLUDES=" #$(this-package-input "ffmpeg") "/include")
         (string-append "ALLOWSTATIC=" "no"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'fix-ffmpeg-compatibility
            (lambda _
              (substitute* "src/ffmpeg_decode.cc"
                (("FF_PROFILE_AAC_LOW") "AV_PROFILE_AAC_LOW"))))

          (add-before 'build 'configure-directory
            (lambda _
              (mkdir-p "build")))
          (replace 'install
            (lambda _ 
              (let* ((out (assoc-ref %outputs "out"))
                     (lib (string-append out "/lib/obs-plugins"))
                     (data (string-append out "/share/obs/obs-plugins/droidcam-obs")))
                (mkdir-p lib)
                (mkdir-p data)
                (install-file "./build/droidcam-obs.so" (string-append (assoc-ref %outputs "out") "/lib/obs-plugins"))
                (copy-recursively "./data/locale" (string-append data "/locale"))))))))
      
    (native-inputs
     (list git pkg-config))
    (synopsis "Droidcam OBS plugin")
    (home-page "https://dev47apps.com/obs/")
    (description "A plugin for OBS to enable droidcam which allows you to use your phone as a webcam (and even a mic). Supports iOS and Android.")
    (license license:gpl2)))
  
obs-droidcam
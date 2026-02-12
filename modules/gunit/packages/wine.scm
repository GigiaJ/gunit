(define-module (gunit packages wine)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages base)
  #:use-module (gnu packages qt)
  #:use-module (guix gexp)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages linux))

(define-public q4wine
  (package
    (name "q4wine")
    (version "v1.4.2") 
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brezerk/q4wine")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ckzgarp8208avhxjzjhqvfpd0z0v96x74q5ybxjcrhf7c8gxf76"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list (string-append "-DLIBS_ENTRY_PATH=" #$output "/lib")
              (string-append "-DMANPAGE_ENTRY_PATH=" #$output "/share/man")
              (string-append "-DDESKTOP_ENTRY_PATH=" #$output "/share/applications")
              (string-append "-DPIXMAPS_ENTRY_PATH=" #$output "/share/pixmaps"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-executable
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin/q4wine")))
                (wrap-program bin
                  `("PATH" ":" prefix
                    ,(map (lambda (pkg)
                            (string-append (assoc-ref inputs pkg) "/bin"))
                          '("wine" "wget" "icoutils" "util-linux" "which"))))))))))
    (inputs 
     (list qtbase
           qtsvg
           qttools
           sqlite
           wine
           wget
           icoutils
           util-linux
           which)) ; Added 'which' as it's often used for detection
    (native-inputs (list pkg-config cmake))
    (home-page "https://q4wine.brezblock.org.ua/")
    (synopsis "Qt GUI for managing Wine prefixes and applications")
    (description
     "Q4Wine is a Qt-based graphical user interface for Wine. It helps 
manage Wine prefixes and installed Windows applications.")
    (license license:gpl3)))

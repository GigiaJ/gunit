(define-module (gchannel packages synergy)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build utils) ; Added: Provides substitute* used in phases
  #:use-module ((guix licenses) #:select (gpl2 expat))
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages avahi)
  #:use-module (gchannel packages libportal)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages freedesktop) #:select (libei))
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26))

(define-public synergy
  (package
    (name "synergy")
    (version "1.18.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/symless/synergy-core")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1d3v2zp3vf2wb5ddxagl8s554h7rpgjwnwjkc0lpx0rp6g6hm3z9"))))
    (build-system cmake-build-system)
    (arguments
    (list
     #:tests? #f ; No 'make check' or 'ctest' target observed

     ;; Add CMAKE_MODULE_PATH to point to the 'ext' directory in the source tree
     ;; where custom Find*.cmake modules like Findpugixml.cmake might reside.
     #:configure-flags
     #~(list (string-append "-D " "CMAKE_INSTALL_PREFIX=" (assoc-ref %outputs "out"))
        "-DBUILD_TESTS=OFF")

     #:phases
     #~(modify-phases %standard-phases ; Ensure phases are within the G-expression
       (add-after 'unpack 'patch-cmake-install-prefix
       (lambda* (#:key #:allow-other-keys)
       (system* "sed" "-i" "s|set(CMAKE_INSTALL_PREFIX /usr)| |g" "cmake/Packaging.cmake")
         #t)) ;
         (add-after 'install 'wrap-synergy-executable
         (lambda* (#:key inputs outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out"))
         (openssl (assoc-ref inputs "openssl"))
         (xkeyboard-config (assoc-ref inputs "xkeyboard-config")))
         (wrap-program (string-append out "/bin/deskflow")
         `("PATH" ":" prefix (,(string-append openssl "/bin"))))
          (wrap-program (string-append out "/bin/deskflow-client")
          `("PATH" ":" prefix (,(string-append openssl "/bin"))))
           (wrap-program (string-append out "/bin/deskflow-server")
           `("PATH" ":" prefix (,(string-append openssl "/bin"))))
            (wrap-program (string-append out "/bin/deskflow-legacy")
            `("PATH" ":" prefix (,(string-append openssl "/bin")))   
        )
   
   
   
   
    )
           #t))
         (add-after 'wrap-synergy-executable 'patch-desktop
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
           (substitute* (string-append out "/share/applications/deskflow.desktop")
           (("/usr") out))
           (system* "mv" (string-append out "/share/applications/deskflow.desktop") (string-append out "/share/applications/synergy.desktop"))
         #t)))
         
)))
    (native-inputs
     (list qttools pkg-config))           ; for Qt5LinguistTools
    (inputs
     `(("avahi" ,avahi)
       ("python"    ,python-wrapper)
       ("openssl"   ,openssl)
       ("curl"      ,curl)
       ("cli11"      ,cli11)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("libei"     ,libei)
       ("libnotify" ,libnotify)
       ("libportal" ,libportal)
       ("libxi"     ,libxi)
       ("libx11"    ,libx11)
       ("libxcb"    ,libxcb)
       ("libxkbcommon" ,libxkbcommon)
       ("libxkbfile" ,libxkbfile)
       ("libxtst"   ,libxtst)
       ("pugixml"   ,pugixml)
       ("tomlplusplus"    ,tomlplusplus)
       ("qtbase"    ,qtbase)
       ("xkeyboard-config" ,xkeyboard-config)
    ))
    (home-page "https://symless.com/synergy")
    (synopsis "Mouse and keyboard sharing utility")
    (description
     "Synergy brings your computers together in one cohesive experience; it's
software for sharing one mouse and keyboard between multiple computers on your
desk.")
    (license gpl2)))

synergy
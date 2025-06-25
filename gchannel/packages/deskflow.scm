(define-module (gchannel packages synergy)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:select (gpl2))
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public deskflow
  (package
    (name "deskflow")
    (version "1.22.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/deskflow/deskflow")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1ahyjvm29gnqxmqra68gxbnpqzq9384c09z43jyvkzk9l15h4l99"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; No 'make check' or 'ctest' target observed
       #:configure-flags
       '("-DBUILD_TESTS=OFF")

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source-files
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xkeyboard-config (assoc-ref inputs "xkeyboard-config")))
               (substitute* "src/lib/deskflow/unix/AppUtilUnix.cpp"
                 (("/usr/share/X11/xkb/rules/evdev.xml")
                  (string-append xkeyboard-config "/share/X11/xkb/rules/evdev.xml")))
               (substitute* "deploy/linux/deploy.cmake"
                 (("message\\(FATAL_ERROR \"Unable to read file /etc/os-release\"\\)")
                  "message(STATUS \"Guix build: Bypassing /etc/os-release check.\")")))
             #t))

         (add-after 'install 'wrap-deskflow-executables
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (openssl (assoc-ref inputs "openssl")))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/bin/" prog)
                    `("PATH" ":" prefix (,(string-append openssl "/bin")))))
                '("deskflow" "deskflow-client" "deskflow-server")))
             #t))

         (add-after 'wrap-deskflow-executables 'patch-desktop-file
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
           (substitute* (string-append out "/share/applications/org.deskflow.deskflow.desktop")
           (("/usr") out))
           ;; TODO: but actually make this more idiomatic... bit hacky
           (system* "mv" (string-append out "/share/applications/org.deskflow.deskflow.desktop") (string-append out "/share/applications/deskflow.desktop"))
         #t)))
            
            
            )))
      (native-inputs
     (list qttools pkg-config))
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
       ("libxinerama" ,libxinerama)
       ("libxrandr"   ,libxrandr)
       ("libxkbcommon" ,libxkbcommon)
       ("libxkbfile" ,libxkbfile)
       ("libxtst"   ,libxtst)
       ("pugixml"   ,pugixml)
       ("tomlplusplus"    ,tomlplusplus)
       ("qtbase"    ,qtbase)
       ("qtsvg"     ,qtsvg)
       ("qtwayland" ,qtwayland)
       ("xkeyboard-config" ,xkeyboard-config)))
    (home-page "https://deskflow.org/")
    (synopsis "Mouse and keyboard sharing utility")
    (description
     "Share a single keyboard and mouse between multiple computers.")
    (license gpl2)))



deskflow

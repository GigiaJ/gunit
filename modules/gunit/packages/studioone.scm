(define-module (studio-one)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages image)
  #:use-module (guix build utils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages vulkan)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix licenses)
  #:use-module (gnu packages xdisorg))

(define libjpeg-turbo-8
  (package
    (inherit libjpeg-turbo)
    (name "libjpeg-turbo-8")
    (arguments
     (substitute-keyword-arguments (package-arguments libjpeg-turbo)
       ((#:configure-flags flags
         #~'())
        #~(append #$flags
                  '("-DWITH_JPEG8=ON")))))))

(define icu4c-74
  (package
    (inherit icu4c)
    (name "icu4c")
    (version "74.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/unicode-org/icu/releases/download/release-"
             (string-map (lambda (c)
                           (if (char=? c #\.) #\- c)) version) "/icu4c-"
             (string-map (lambda (c)
                           (if (char=? c #\.) #\_ c)) version) "-src.tgz"))
       (sha256
        (base32 "1l5qa85nv8pvpzwsa06l1p3cwgs6jz39096b9l7fcwhrd1h8xkl6"))))))

(define-public studio-one
  (package
    (name "studio-one")
    (version "6.0.0-beta")
    (source
     (local-file "packages/studioone6.deb"
                 #:recursive? #t))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:validate-runpath? #f
       #:modules ((guix build gnu-build-system)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "ar" "x" source)
                      (invoke "tar" "xf" "data.tar.gz")))

                  (delete 'configure)
                  (delete 'build)

                  (replace 'install
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (target (string-append out "/lib/studio-one"))
                             (bin (string-append out "/bin"))
                             (s1-exec (string-append target "/Studio One"))
                             (ld-linux (search-input-file inputs
                                        "/lib/ld-linux-x86-64.so.2"))
                             (input-libs (map (lambda (input)
                                                (string-append (cdr input)
                                                               "/lib")) inputs))
                             (rpath (string-join (cons* "\\$ORIGIN" target
                                                        input-libs) ":")))
                        
                        (mkdir-p target)
                        (copy-recursively "opt/PreSonus/Studio One 6" target)

                        (invoke "patchelf" "--set-interpreter" ld-linux
                                s1-exec)
                        (invoke "patchelf" "--set-rpath" rpath s1-exec)

                        (for-each (lambda (file)
                                    (invoke "patchelf" "--set-rpath" rpath
                                            file))
                                  (find-files target "\\.so$"))

                        (mkdir-p bin)
                        (let ((wrapper (string-append bin "/studio-one")))
                          (with-output-to-file wrapper
                            (lambda ()
                              (format #t "#!/bin/sh\nexec \"~a\" \"$@\"\n"
                                      s1-exec)))
                          (chmod wrapper #o755))
                        #t)))

                  (add-after 'install 'add-desktop-file
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (apps (string-append out "/share/applications"))
                             (pixmaps (string-append out "/share/pixmaps"))
                             (icon-src (string-append out
                                        "/lib/studio-one/shared/resources/studioone.png")))
                        
                        (mkdir-p pixmaps)
                        (if (file-exists? icon-src)
                            (copy-file icon-src
                                       (string-append pixmaps
                                                      "/studio-one.png"))
                            (display
                             "Warning: Studio One icon not found in expected location.
"))

                        (mkdir-p apps)
                        (with-output-to-file (string-append apps
                                              "/studio-one.desktop")
                          (lambda ()
                            (format #t
                             "[Desktop Entry]
Name=Studio One 6
GenericName=Digital Audio Workstation
Comment=Professional DAW for Linux
Exec=~a/bin/studio-one
Icon=studio-one
Terminal=false
Type=Application
Categories=AudioVideo;Audio;AudioVideoEditing;
Keywords=daw;music;audio;recording;
StartupNotify=true
"
                             out)))
                        #t))))))

    (native-inputs (list binutils tar xz elfutils patchelf))
    (inputs `(("alsa-lib" ,alsa-lib)
              ("jack" ,jack-2)
              ("pipewire" ,pipewire)
              ("vulkan-loader" ,vulkan-loader)
              ("wayland" ,wayland)
              ("libxkbcommon" ,libxkbcommon)
              ("freetype" ,freetype)
              ("fontconfig" ,fontconfig)
              ("cairo" ,cairo)
              ("glib" ,glib)
              ("gtkmm" ,gtkmm)
              ("knotifications" ,knotifications)
              ("kwallet" ,kwallet)
              ("kservice" ,kservice)
              ("glibc" ,glibc)
              ("avahi" ,avahi)
              ("icu4c" ,icu4c-74)
              ("libjpeg" ,libjpeg-turbo-8)
              ("dbus" ,dbus)
              ("openssl" ,openssl-3.0)
              ("gcc-lib" ,gcc "lib")
              ("libunistring" ,libunistring)
              ("openssl" ,openssl)
              ("libx11" ,libx11)
              ("mesa" ,mesa)))
    (synopsis "Studio One 6 (Linux Beta)")
    (description "Digital Audio Workstation. Requires Wayland and Vulkan.")
    (home-page "https://presonus.com")
    (license #f)))

studio-one
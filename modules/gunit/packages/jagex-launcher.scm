(define-module (gunit packages jagex-launcher)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define jagex-launcher-client
  (package
    (name "jagex-launcher")
    (version "beta")
    (source
     (origin
       (method url-fetch)
       (uri "https://rs-launcher-updates.runescape.com/production/linux/x64/latest/jagex-launcher-beta-linux-x86_64.AppImage")
       (sha256
        (base32 "0anjrjrdq0inl24bglihbrmgq5q9kann6bqq3n7hxpws846j5ydm"
                ))))
    (build-system gnu-build-system)
    (native-inputs
     (list patchelf squashfs-tools))
    (inputs
     (list glibc))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (copy-file source "jagex.AppImage")
              #t))
          (delete 'configure)
          (delete 'build)

          (replace 'install
                   (lambda* (#:key outputs inputs #:allow-other-keys)
                            (let ((out (assoc-ref outputs "out")))
                              (invoke "chmod" "+x" "jagex.AppImage")
                              (invoke "./jagex.AppImage" "--appimage-extract")
                              (copy-recursively "squashfs-root" (string-append out "/opt/jagex-launcher"))
                              #t))))))
    (synopsis "Official Jagex Launcher")
    (description "Official Jagex Launcher for Linux")
    (home-page "https://www.jagex.com/")
    (license (license:nonfree "https://www.jagex.com/en-GB/terms"))))


(define jagex-launcher-libs
  `(("alsa-lib" ,alsa-lib)
    ("at-spi2-core" ,at-spi2-core)
    ("bash" ,bash)
    ("cairo" ,cairo)
    ("coreutils" ,coreutils)
    ("cups" ,cups)
    ("dbus" ,dbus)
    ("dbus-glib" ,dbus-glib)
    ("eudev" ,eudev)
    ("expat" ,expat)
    ("fontconfig" ,fontconfig)
    ("freetype" ,freetype)
    ("gdk-pixbuf" ,gdk-pixbuf)
    ("gcc:lib" ,gcc-14 "lib")
    ("glib" ,glib)
    ("glibc" ,glibc)
    ("gtk+" ,gtk+)
    ("libdrm" ,libdrm)
    ("libx11" ,libx11)
    ("libxcb" ,libxcb)
    ("libxcomposite" ,libxcomposite)
    ("libxdamage" ,libxdamage)
    ("libxext" ,libxext)
    ("libxfixes" ,libxfixes)
    ("libxkbcommon" ,libxkbcommon)
    ("libxrandr" ,libxrandr)
    ("libxshmfence" ,libxshmfence)
    ("mesa" ,mesa)
    ("nspr" ,nspr)
    ("nss" ,nss)
    ("nss-certs" ,nss-certs)
    ("pango" ,pango)
    ("wayland" ,wayland)
    ("zlib" ,zlib)))

(define jagex-launcher-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union `(,@jagex-launcher-libs ,@fhs-min-libs)
                    #:name "fhs-union-64"))))

(define jagex-launcher-ld.so.cache
  (ld.so.conf->ld.so.cache jagex-launcher-ld.so.conf))

(define-public jagex-launcher-container
  (nonguix-container
   (name "jagex-launcher")
   (wrap-package jagex-launcher-client)
   (run "/opt/jagex-launcher/jagex-launcher")
   (ld.so.conf jagex-launcher-ld.so.conf)
   (ld.so.cache jagex-launcher-ld.so.cache)
   (union64
    (fhs-union `(,@jagex-launcher-libs ,@fhs-min-libs)
               #:name "fhs-union-64"))
   (link-files '("opt"))
   (description "Jagex Launcher running inside an FHS container.")))

(define-public jagex-launcher
  (nonguix-container->package jagex-launcher-container))

jagex-launcher

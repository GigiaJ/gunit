(define-module (gunit packages helium)
  #:use-module ((guix licenses) #:prefix license:)
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
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define helium-client
  (package
    (name "helium-client")
    (version "0.18.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/imputnet/helium-linux/releases/download/"
                           version "/helium-" version "-x86_64_linux.tar.xz"))
       (sha256
        (base32 "0p0miclv5qvczs35awdf3xas1x93bhhyrh25gpyy7ywap5bdg4s4"
                ))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:strip-binaries? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (helium-dir (string-append out "/opt/helium")))
                (copy-recursively "." helium-dir)
                #t))))))
    (synopsis "Privacy-first, open-source Chromium-based web browser")
    (description "Helium blocks ads, trackers, and fingerprinting by default with no background bloat.")
    (home-page "https://helium.computer/")
    (license license:bsd-3)))

(define helium-libs
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
    ("gcc:lib" ,gcc-14 "lib")
    ("gdk-pixbuf" ,gdk-pixbuf)
    ("glib" ,glib)
    ("glibc" ,glibc)
    ("gtk+" ,gtk+)
    ("libdrm" ,libdrm)
    ("libx11" ,libx11)
    ("libxcb" ,libxcb)
    ("libxcomposite" ,libxcomposite)
    ("libxcursor" ,libxcursor)
    ("libxdamage" ,libxdamage)
    ("libxext" ,libxext)
    ("libxfixes" ,libxfixes)
    ("libxi" ,libxi)
    ("libxkbcommon" ,libxkbcommon)
    ("libxrandr" ,libxrandr)
    ("libxrender" ,libxrender)
    ("libxshmfence" ,libxshmfence)
    ("mesa" ,mesa)
    ("nspr" ,nspr)
    ("nss" ,nss)
    ("nss-certs" ,nss-certs)
    ("pango" ,pango)
    ("pulseaudio" ,pulseaudio)
    ("wayland" ,wayland)
    ("xdg-utils" ,xdg-utils)
    ("zlib" ,zlib)))

(define helium-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union `(,@helium-libs ,@fhs-min-libs)
                    #:name "fhs-union-64"))))

(define helium-ld.so.cache
  (ld.so.conf->ld.so.cache helium-ld.so.conf))

(define-public helium-container
  (nonguix-container
   (name "helium")
   (wrap-package helium-client)
   (run "/opt/helium/helium")
   (ld.so.conf helium-ld.so.conf)
   (ld.so.cache helium-ld.so.cache)
   (union64
    (fhs-union `(,@helium-libs ,@fhs-min-libs)
               #:name "fhs-union-64"))
   (link-files '("opt"))
   (description "Helium Browser running inside an FHS container.")))

(define-public helium
  (nonguix-container->package helium-container))

helium

(define-module (gunit packages paradise)
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
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define paradise-client
  (package
    (name "paradise-client")
    (version "0.39.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Paradise-Chat/Paradise/releases/download/v"
                           version "/Paradise-" version ".AppImage"))
       (sha256
        (base32 "1ifmcxflk03qj8qr5a0bwj5afqx6fx327xz550s8jn5kfxl8f1q8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list squashfs-tools))
    (inputs
     (list bash-minimal
           fontconfig
           font-adobe-source-code-pro
           font-dejavu
           font-google-noto
           font-google-noto-emoji
           font-google-noto-sans-cjk
           font-google-roboto
           font-liberation
           gsettings-desktop-schemas
           gtk+))
    (arguments
     (list
      #:tests? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 binary-ports)
                  (rnrs bytevectors))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (let* ((port (open-file source "rb"))
                     (header (get-bytevector-n port 64)))
                (close-port port)
                (let* ((e-shoff (bytevector-u64-ref header 40 (endianness little)))
                       (e-shentsize (bytevector-u16-ref header 58 (endianness little)))
                       (e-shnum (bytevector-u16-ref header 60 (endianness little)))
                       (offset (number->string (+ e-shoff (* e-shentsize e-shnum)))))
                  (invoke "unsquashfs" "-q" "-o" offset "-d" "squashfs-root" source)
                  (chdir "squashfs-root")
                  (for-each make-file-writable
                            (find-files "." #:directories? #t))
                  (when (file-exists? "usr/lib")
                    (delete-file-recursively "usr/lib"))))))
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (paradise-dir (string-append out "/opt/paradise")))
                (copy-recursively "." paradise-dir))))
          (add-after 'patch-shebangs 'wrap-environment
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((apprun (string-append (assoc-ref outputs "out")
                                           "/opt/paradise/AppRun")))
                (wrap-program apprun
                  `("ELECTRON_OZONE_PLATFORM_HINT" ":" = ("auto"))
                  `("FONTCONFIG_PATH" ":" = (,(string-append #$fontconfig "/etc/fonts")))
                  `("XDG_DATA_DIRS" ":" prefix
                    (,(string-append #$font-adobe-source-code-pro "/share")
                     ,(string-append #$font-dejavu "/share")
                     ,(string-append #$font-google-noto "/share")
                     ,(string-append #$font-google-noto-emoji "/share")
                     ,(string-append #$font-google-noto-sans-cjk "/share")
                     ,(string-append #$font-google-roboto "/share")
                     ,(string-append #$font-liberation "/share")
                     ,(string-append #$gsettings-desktop-schemas "/share")
                     ,(string-append #$gtk+ "/share")
                     "/usr/share"
                     "/share")))))))))
    (synopsis "Lightweight cross-platform chat client")
    (description "Paradise is a lightweight, cross-platform chat application.")
    (home-page "https://github.com/Paradise-Chat/Paradise")
    (license license:agpl3)))

(define paradise-libs
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
    ("font-adobe-source-code-pro" ,font-adobe-source-code-pro)
    ("font-dejavu" ,font-dejavu)
    ("font-google-noto" ,font-google-noto)
    ("font-google-noto-emoji" ,font-google-noto-emoji)
    ("font-google-noto-sans-cjk" ,font-google-noto-sans-cjk)
    ("font-google-roboto" ,font-google-roboto)
    ("font-liberation" ,font-liberation)
    ("fontconfig" ,fontconfig)
    ("freetype" ,freetype)
    ("gcc:lib" ,gcc-14 "lib")
    ("gdk-pixbuf" ,gdk-pixbuf)
    ("glib" ,glib)
    ("glibc" ,glibc)
    ("gtk+" ,gtk+)
    ("libdrm" ,libdrm)
    ("libglvnd" ,libglvnd)
    ("libnotify" ,libnotify)
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
    ("libxscrnsaver" ,libxscrnsaver)
    ("libxshmfence" ,libxshmfence)
    ("libxtst" ,libxtst)
    ("mesa" ,mesa)
    ("nspr" ,nspr)
    ("nss" ,nss)
    ("nss-certs" ,nss-certs)
    ("pango" ,pango)
    ("pipewire" ,pipewire)
    ("pulseaudio" ,pulseaudio)
    ("sed" ,sed)
    ("vulkan-loader" ,vulkan-loader)
    ("wayland" ,wayland)
    ("xdg-utils" ,xdg-utils)
    ("zlib" ,zlib)))

(define paradise-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union `(,@paradise-libs ,@fhs-min-libs)
                    #:name "fhs-union-64"))))

(define paradise-ld.so.cache
  (ld.so.conf->ld.so.cache paradise-ld.so.conf))

(define-public paradise-container
  (nonguix-container
   (name "paradise")
   (wrap-package paradise-client)
   (run "/opt/paradise/AppRun")
   (ld.so.conf paradise-ld.so.conf)
   (ld.so.cache paradise-ld.so.cache)
   (union64
    (fhs-union `(,@paradise-libs ,@fhs-min-libs)
               #:name "fhs-union-64"))
   (link-files '("opt"))
   (description "Paradise running inside an FHS container.")))

(define-public paradise
  (nonguix-container->package paradise-container))

paradise


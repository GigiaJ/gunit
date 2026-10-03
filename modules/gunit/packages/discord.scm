(define-module (gunit packages discord)
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

(define discord-client
  (package
    (name "discord-client")
    (version "1.0.160")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.discordapp.net/apps/linux/"
                           version "/discord-" version ".tar.gz"))
       (sha256
        (base32 "1m7lbj41wm7068wa71cn7py80si4mswwrmy04w0ysfhl4d68fg1w"))))
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
                     (discord-dir (string-append out "/opt/Discord")))
                (copy-recursively "." discord-dir)
                #t))))))
    (synopsis "All-in-one voice and text chat")
    (description "Discord is a voice, video and text communication service.")
    (home-page "https://discord.com/")
    (license (license:nonfree "https://discord.com/terms"))))

(define discord-libs
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
    ("libappindicator" ,libappindicator)
    ("libdrm" ,libdrm)
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
    ("pulseaudio" ,pulseaudio)
    ("wayland" ,wayland)
    ("zlib" ,zlib)))

(define discord-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union `(,@discord-libs ,@fhs-min-libs)
                    #:name "fhs-union-64"))))

(define discord-ld.so.cache
  (ld.so.conf->ld.so.cache discord-ld.so.conf))

(define-public discord-container
  (nonguix-container
   (name "discord")
   (wrap-package discord-client)
   (run "/opt/Discord/discord")
   (ld.so.conf discord-ld.so.conf)
   (ld.so.cache discord-ld.so.cache)
   (union64
    (fhs-union `(,@discord-libs ,@fhs-min-libs)
               #:name "fhs-union-64"))
   (link-files '("opt"))
   (description "Discord running inside an FHS container.")))

(define-public discord
  (nonguix-container->package discord-container))

discord

;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024, 2025 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2024 antlers <antlers@illucid.net>
;;; Copyright © 2026 Jaggar Boone <jaggar@chickensalad.quest>


(define-module (gunit packages productivity)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix multiarch-container)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module ((guix licenses) #:prefix free-license:))

(define anki-client
  (package
    (name "anki-client")
    (version "26.09.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ankitects/anki/releases/download/"
                       version "/anki-" version "-linux-"
                       (match (or (%current-target-system) (%current-system))
                         ("aarch64-linux" "aarch64")
                         (_ "x86_64"))
                       ".tar.zst"))
       (file-name (string-append name "-" version ".tar.zst"))
       (sha256
        (base32
         (match (or (%current-target-system) (%current-system))
           ("aarch64-linux"
            "0nz7cy61w30rbz15n0j0gmkhjbdqj858iq7vj1ypbpi4qbc8hv4m")
           (_
            "16kfq3pl9nxqifz417b72mvdyyxq8h7alcpnbvf7bz7vzkgnnjhz"))))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; ~195 MiB prebuilt bundle.
      #:substitutable? #f
      #:validate-runpath? #f
      #:strip-binaries? #f
      #:patch-shebangs? #f
      #:install-plan
      #~'(("anki" "share/anki/")
          ("app" "share/anki/")
          ("app_packages" "share/anki/")
          ("python" "share/anki/")
          ("LICENSE" "share/doc/anki/")
          ("README.md" "share/doc/anki/")
          ("anki.xpm" "share/pixmaps/")
          ("anki.png" "share/pixmaps/")
          ("anki.desktop" "share/applications/")
          ("anki.xml" "share/mime/packages/")
          ("anki.1" "share/man/man1/"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Keep "Exec=anki %f" relative so the .desktop entry linked by the
          ;; outer nonguix-container launches the containerized binary rather
          ;; than the unwrapped store binary.
          (delete 'patch-dot-desktop-files)
          (add-after 'install 'install-entrypoint
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin"))
                     (exe (string-append bin "/anki"))
                     (target (string-append #$output "/share/anki/anki"))
                     (sh (search-input-file inputs "/bin/sh")))
                (mkdir-p bin)
                (call-with-output-file exe
                  (lambda (port)
                    (format port "#!~a
if [ -z \"$SSL_CERT_FILE\" ] || [ ! -e \"$SSL_CERT_FILE\" ]; then
  export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
fi
if [ -z \"$SSL_CERT_DIR\" ] || [ ! -d \"$SSL_CERT_DIR\" ]; then
  export SSL_CERT_DIR=/etc/ssl/certs
fi
exec \"~a\" \"$@\"
" sh target)))
                (chmod exe #o755)))))))
    (inputs (list bash-minimal))
    (synopsis "Spaced repetition flashcard program")
    (supported-systems '("x86_64-linux"))
    (description "Anki is a flashcard program that helps you spend more time
on challenging material, and less on what you already know.")
    (home-page "https://apps.ankiweb.net/")
    (license free-license:agpl3+)))

(define-public anki
  (nonguix-container->package
   (nonguix-container
     (name "anki")
     (wrap-package anki-client)
     (run "/bin/anki")
     (preserved-env '("^ANKI_"
                      "^QT_"
                      "^QTWEBENGINE_"))
     (link-files '("share/applications"
                   "share/doc"
                   "share/man"
                   "share/mime"
                   "share/pixmaps"))
     (union64
      (fhs-union
       (append fhs-min-libs
               `(("alsa-lib" ,alsa-lib)
                 ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")
                 ("at-spi2-core" ,at-spi2-core)
                 ("brotli" ,brotli)
                 ("bzip2" ,bzip2)
                 ("cairo" ,cairo)
                 ("coreutils" ,coreutils)
                 ("cups-minimal" ,cups-minimal)
                 ("dbus" ,dbus)
                 ("eudev" ,eudev)
                 ("expat" ,expat)
                 ("flatpak-xdg-utils" ,flatpak-xdg-utils)
                 ("font-google-noto" ,font-google-noto)
                 ("font-google-noto-emoji" ,font-google-noto-emoji)
                 ("font-google-noto-sans-cjk" ,font-google-noto-sans-cjk)
                 ("fontconfig" ,fontconfig)
                 ("freetype" ,freetype)
                 ("gcc:lib" ,gcc-14 "lib")
                 ("gdk-pixbuf" ,gdk-pixbuf)
                 ("glib" ,glib)
                 ("gtk+" ,gtk+)
                 ("harfbuzz" ,harfbuzz)
                 ("lame" ,lame)
                 ("libdrm" ,libdrm)
                 ("libx11" ,libx11)
                 ("libxcb" ,libxcb)
                 ("libxcomposite" ,libxcomposite)
                 ("libxdamage" ,libxdamage)
                 ("libxext" ,libxext)
                 ("libxfixes" ,libxfixes)
                 ("libxi" ,libxi)
                 ("libxinerama" ,libxinerama)
                 ("libxkbcommon" ,libxkbcommon)
                 ("libxkbfile" ,libxkbfile)
                 ("libxrandr" ,libxrandr)
                 ("libxrender" ,libxrender)
                 ("libxshmfence" ,libxshmfence)
                 ("libxtst" ,libxtst)
                 ("mesa" ,mesa)
                 ("mit-krb5" ,mit-krb5)
                 ("mpv" ,mpv)
                 ("nspr" ,nspr)
                 ("nss" ,nss)
                 ("nss-certs" ,nss-certs)
                 ("pango" ,pango)
                 ("pulseaudio" ,pulseaudio)
                 ("wayland" ,wayland)
                 ("xcb-util" ,xcb-util)
                 ("xcb-util-cursor" ,xcb-util-cursor)
                 ("xcb-util-image" ,xcb-util-image)
                 ("xcb-util-keysyms" ,xcb-util-keysyms)
                 ("xcb-util-renderutil" ,xcb-util-renderutil)
                 ("xcb-util-wm" ,xcb-util-wm)
                 ("zlib" ,zlib)
                 ("zstd:lib" ,zstd "lib")))
       #:system "x86_64-linux")))))



anki

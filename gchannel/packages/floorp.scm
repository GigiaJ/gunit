(define-module (gchannel packages floorp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (guix build-system copy)
  #:use-module (guix build copy-build-system)

  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages vulkan)
)

(define-public floorp
    (package
    (name "floorp")
    (version "11.24.1")
    (source (origin
    (method url-fetch)
    (uri (string-append "https://github.com/Floorp-Projects/Floorp/releases/download/v" "11.24.0" "/floorp-" "11.24.0" ".linux-x86_64.tar.bz2"))
        (sha256
        (base32 "1abmanvim06nc3dhiwi9j63714dyfqqwlnj3984ld21ydz9kaffv"))))
    (build-system copy-build-system)

    (inputs
        (list 
        alsa-lib
        gcc-toolchain
        bash-minimal
        eudev
        libnotify
        libpng-apng
        libva
        mesa
        pipewire
        pulseaudio
        glibc
        gtk+
        libdrm
        llvm-for-mesa
        expat
        zlib
        zstd
        spirv-tools
        libxcb
        libxshmfence
        elfutils
        libx11
        wayland
        libxext
        libxxf86vm
        ffmpeg
        libvpx
        libwebp
        xz
        dav1d
        libaom
        lame
        opus
        rav1e
        speex
        svt-av1
        libtheora
        libogg
        twolame
        libvorbis
        libx264
        x265
        xvid
        soxr
        libvdpau
        sdl2
        openal
        libcdio-paranoia
        libcdio
        libcaca
        libass
        vidstab
        fontconfig
        freetype
        bzip2
        libbluray
        gnutls
        pciutils
    ))
        (arguments
        (list
            #:install-plan #~'(("." "lib/floorp"))
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'install 'create
                    (lambda _
                        (mkdir-p (string-append  #$output "/bin"))
                        ;;(mkdir-p (string-append  #$output "/share/icons/hicolor"))
                    )
                )
                (add-after 'create 'install-icons
                (lambda _
                    (let ((icons (string-append #$output "/share/icons/hicolor"))
                        (share (string-append #$output "/lib/floorp/browser/chrome/icons")))
                        (for-each (lambda (icon)
                                    (let* ((icon-name (basename icon))
                                        (icon-size (string-drop-right (string-drop icon-name 7) 4))
                                        (target (string-append icons "/" icon-size "x" icon-size "/apps/" "floorp" ".png")))
                                    (mkdir-p (dirname target))
                                    (rename-file icon target)))
                                (find-files share "default.*\\.png")))
                )
                )
                (add-after 'install-icons 'install-share
                    (lambda _
                        (display "cat")
                        (let* ((exec-path (string-append #$output "/bin/floorp %u"))
                        (icon-path (string-append #$output "/share/icons/hicolor/128x128/apps/floorp.png")))
                   (define desktop-entry
                     `((Version . "1.0")
                       (Name . "Floorp")
                       (GenericName . "Web Browser")
                       (Comment . "Your web, the way you like it")
                       (Exec . ,exec-path) ;; Precomputed value
                       (Icon . ,icon-path) ;; Precomputed value
                       (Terminal . false)
                       (Type . "Application")
                       (StartupWMClass . "Floorp")
                       (MimeType . "text/html;text/xml;application/xhtml+xml;text/mml;x-scheme-handler/http;x-scheme-handler/https;")
                       (Startup-Notify . true)
                       (X-MultipleArgs . false)
                       (X-Desktop-File-Install-Version . "0.16")
                       (Categories . "Network;WebBrowser;")
                       (Encoding . "UTF-8")))
                 
                   (define (write-desktop-entry file-name entry)
                        (call-with-output-file file-name
                            (lambda (port)
                            (format port "[Desktop Entry]~%")
                            (for-each
                            (lambda (field)
                                (format port "~a=~a~%" (car field) (cdr field)))
                            entry))))

                        (mkdir-p (string-append #$output "/share/applications"))
                        (write-desktop-entry (string-append #$output "/share/applications/" "floorp.desktop") desktop-entry))
                    )
                )
                (add-after 'create 'wrap
                (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (lib (string-append out "/lib/floorp"))
                       (libs (map
                              (lambda (lib-name)
                                (string-append (assoc-ref inputs
                                                          lib-name)
                                               "/lib"))
                              '(
                                "alsa-lib"
                                "libpng-apng"
                                "libva"
                                "mesa"
                                "pipewire"
                                "pulseaudio"
                                "glibc"
                                "gcc-toolchain"
                                "libdrm"
                                "llvm-for-mesa"
                                "expat"
                                "zlib"
                                "zstd"
                                "spirv-tools"
                                "libxcb"
                                "libxshmfence"
                                "elfutils"
                                "libx11"
                                "wayland"
                                "libxext"
                                "libxxf86vm"
                                "ffmpeg"
                                "libvpx"
                                "libwebp"
                                "xz"
                                "dav1d"
                                "libaom"
                                "lame"
                                "opus"
                                "rav1e"
                                "speex"
                                "svt-av1"
                                "libtheora"
                                "libogg"
                                "twolame"
                                "libvorbis"
                                "libx264"
                                "x265"
                                "xvid"
                                "soxr"
                                "libvdpau"
                                "sdl2"
                                "openal"
                                "libcdio-paranoia"
                                "libcdio"
                                "libcaca"
                                "libass"
                                "vidstab"
                                "fontconfig-minimal"
                                "freetype"
                                "bzip2"
                                "libbluray"
                                "gnutls"
                                "gtk+"
                                "pciutils"
                            )))
                              (gtk-share (string-append (assoc-ref inputs
                              "gtk+")
                   "/share")))
                   (display libs)
                  (wrap-program (car (find-files lib "^glxtest$"))
                    `("LD_LIBRARY_PATH" prefix ,libs))
                (wrap-program (car (find-files lib "^floorp$"))
                    `("LD_LIBRARY_PATH" prefix
                      (,@libs))
                    `("XDG_DATA_DIRS" prefix
                    (,gtk-share))
                    `("MOZ_LEGACY_PROFILES" =
                      ("1"))
                    `("MOZ_ALLOW_DOWNGRADE" =
                      ("1")))
                )
                    
            
            (invoke "mv" (string-append #$output "/lib/floorp/floorp") (string-append #$output "/bin/floorp"))
            ))
                (delete 'validate-runpath)
            )))
    (native-inputs
        (list git))
    (synopsis "Soup")
    (home-page "https://coder.com/")
    (description "Free open source code server")
    (license license:agpl3)))

floorp
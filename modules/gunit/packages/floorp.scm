(define-module (gunit packages floorp)
  #:use-module ((nonguix licenses)
                #:prefix license:)
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
  #:use-module (gnu packages vulkan))

(define-public floorp
  (package
    (name "floorp")
    (version "12.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Floorp-Projects/Floorp/releases/download/v"
             version "/floorp-linux-x86_64.tar.xz"))
       (sha256
        (base32 "04697gss404bzf8kcj8ihz00bhwgl9qal2ci5iv6l2mp2rjfq540"))))
    (build-system copy-build-system)
    (inputs (list alsa-lib
                  bash-minimal
                  eudev
                  libnotify
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
                  elfutils
                  wayland
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
                  libcaca
                  libass
                  fontconfig
                  freetype
                  bzip2
                  libbluray
                  gnutls
                  pciutils
                  gcc-toolchain
                  pango
                  cairo
                  gdk-pixbuf
                  atk
                  cups
                  libcanberra
                  dbus-glib
                  dbus
                  libx11
                  libxcb
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxrandr
                  libxrender
                  libxtst
                  libxxf86vm))
    (arguments
     (list
      #:install-plan
      #~'(("." "lib/floorp"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'validate-runpath)
          (add-after 'install 'patch-interpreters
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/floorp"))
                     (interpreter (string-append (assoc-ref inputs "glibc")
                                                 "/lib/ld-linux-x86-64.so.2")))
                (for-each (lambda (binary)
                            (invoke "patchelf" "--set-interpreter" interpreter
                                    binary))
                          (find-files lib
                           "^(floorp|glxtest|vaapitest|plugin-container|minidump-analyzer|updater|pingsender)$")))))
          (add-after 'patch-interpreters 'wrap-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib/floorp"))
                     ;; FIX: Map EVERY input to the LD_LIBRARY_PATH
                     (libs (map (lambda (input)
                                  (string-append (assoc-ref inputs input)
                                                 "/lib"))
                                '("alsa-lib" "libva"
                                  "mesa"
                                  "pipewire"
                                  "pulseaudio"
                                  "glibc"
                                  "gtk+"
                                  "libdrm"
                                  "pciutils"
                                  "ffmpeg"
                                  "wayland"
                                  "gcc-toolchain"
                                  "eudev"
                                  "glib"
                                  "dbus"
                                  "dbus-glib"
                                  "pango"
                                  "cairo"
                                  "gdk-pixbuf"
                                  "atk"
                                  "cups"
                                  "libcanberra"
                                  "libx11"
                                  "libxcb"
                                  "libxcomposite"
                                  "libxcursor"
                                  "libxdamage"
                                  "libxext"
                                  "libxfixes"
                                  "libxi"
                                  "libxrandr"
                                  "libxrender"
                                  "libxtst"
                                  "libxxf86vm")))
                     (mesa-dri (string-append (assoc-ref inputs "mesa")
                                              "/lib/dri")))
                (mkdir-p bin)
                (wrap-program (string-append lib "/floorp")
                  `("LD_LIBRARY_PATH" prefix
                    ,libs)
                  `("LIBVA_DRIVERS_PATH" =
                    (,mesa-dri))
                  `("MOZ_ENABLE_WAYLAND" =
                    ("1"))
                  `("MOZ_DISABLE_RDD_SANDBOX" =
                    ("1")))
                (symlink (string-append lib "/floorp")
                         (string-append bin "/floorp"))))))))
    (native-inputs (list git patchelf))
    (synopsis "A highly customizable Firefox-based (Gecko) browser")
    (home-page "https://floorp.app/")
    (description
     "Floorp is the first Firefox-based browser to enable UI customization.")
    (license (license:nonfree
              "https://github.com/Floorp-Projects/Floorp-private-components/blob/main/LICENSE"))))

floorp


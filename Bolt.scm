(define-module (gnu packages radio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix base16)
)

(define github-source
        (origin
            (method url-fetch)
            (uri (string-append "https://bolt.adamcake.com/" "#tag=" "0.10.0"))
            (sha256
              (base16-string->bytevector "72c8c43dcb61f778a807eb262b2c2ebcb2e1705756de5a9003484af0663aa924"))))

(define cef-source
    (origin
    (method url-fetch)
    (uri "https://adamcake.com/cef/cef-114.0.5735.134-linux-x86_64-minimal-ungoogled.tar.gz")
    (hash "c6618af7c0a787318c655253eced33baed5cc1b1665c938d2d7dc38a8972876f")))

(define-public bolt-launcher
    (package
    (name "bolt-launcher")
    (version "0.10.0")
    (source github-source)
    (build-system cmake-build-system)
    (arguments
        (list
            #:configure-flags
                #~(list 
                    "-S" "Bolt" "-B" "build"
                    "-G" "Unix Makefiles"
                    "CMAKE_BUILD_TYPE=Release"
                    (string-append %source "/CEF_ROOT=cef_binary_114.2.11+g87c8807+chromium-114.0.5735.134_linux64_minimal")
                    "CMAKE_INSTALL_PREFIX=output"
                    "BOLT_BINDIR=usr/bin"
                    "BOLT_LIBDIR=usr/lib"
                    "BOLT_META_NAME=bolt-launcher"
                    "BOLT_SKIP_LIBRARIES=1")
            #:phases
            (modify-phases %standard-phases
                (add-before 'unpack
                    (format "test")
                    ;;(github-source)
                    ;;(cef-source)
                )
                (add-after 'unpack 'custom-unpack
                    (lambda* (#:key source #:allow-other-keys)
                    (format "test")
                    ;; (system (cmd (string-append "git -C " %source "/Bolt submodule update --init --recursive")))
                    ;;(system (cmd (string-append "git -C " %source "/Bolt apply " %source "/fmt.patch")))
                    ;;(invoke "patch" "-p1" "-d" (string-append %source "/cef_binary_114.2.11+g87c8807+chromium-114.0.5735.134_linux64_minimal")
                    ;;"-i" (string-append %source "/cef-no-fortify.patch"))
                    ;; Configure is handled by Guile as we have fed the flags needed for it.
                    ;; Build is handled by Guile as the command is simple
                    ;; Install is handled by Guile as the command is simple
            )
    ))))
    (inputs
        (list "alsa-lib" "at-spi2-core" "cairo" "dbus" "expat" "fmt" "gcc-libs" "gdk-pixbuf2"
        "glib2" "glibc" "gtk3" "hicolor-icon-theme" "libarchive" "libdrm" "libx11" "libxcb"
        "libxcomposite" "libxdamage" "libxext" "libxfixes" "libxkbcommon" "libxrandr" "mesa"
        "nspr" "nss" "pango"))
    (native-inputs
        (list "cmake"))
    (synopsis "Soup")
    (home-page "https://bolt.adamcake.com/")
    (description "Free open-source third-party implementation of the Jagex Launcher")
    (license license:agpl3)))

    bolt-launcher

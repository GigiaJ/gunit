(define-module (gchannel packages xdg-desktop-portal-kde)

  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public kwayland
  (package
    (name "kwayland")
    (version "6.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/kwayland" "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1acdgylx3j90h0mimv5ybwhby9zf3b1bs8h4ydrn18xx61by38v7"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config
           ;; for wayland-scanner
           wayland))
    (inputs
     (list libxkbcommon
           plasma-wayland-protocols
           qtwayland
           wayland
           wayland-protocols))
    (arguments
     (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/plasma/kwayland")
    (synopsis "Qt-style API to interact with the wayland client and server")
    (description "As the names suggest they implement a Client respectively a
Server API for the Wayland protocol.  The API is Qt-styled removing the needs to
interact with a for a Qt developer uncomfortable low-level C-API.  For example
the callback mechanism from the Wayland API is replaced by signals, data types
are adjusted to be what a Qt developer expects - two arguments of int are
represented by a QPoint or a QSize.")
    (license license:lgpl2.1+)))

kwayland
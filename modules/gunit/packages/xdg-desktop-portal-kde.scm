(define-module (gchannel packages xdg-desktop-portal-kde)

  #:use-module (guix bzr-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gchannel packages extra-cmake-modules)
  #:use-module (gchannel packages kwayland)
  #:use-module (gchannel packages xdg-desktop-portal)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module ((gnu packages freedesktop) #:hide (xdg-desktop-portal))
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)                ;intltool
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages image)
  #:use-module ((gnu packages kde-frameworks) #:hide (extra-cmake-modules kwayland))
  #:use-module (gnu packages kde)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public xdg-desktop-portal-kde-alt
  (package
    (name "xdg-desktop-portal-kde")
    (version "6.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" "6.1.5" "/"
                                  name "-" "6.1.5" ".tar.xz"))
              (sha256
               (base32
                "0y39bbgh2fvl4ryx6xzm06gs67i7gs8bxxfi94r0ip7x0q63rl89"))))
    (build-system qt-build-system)
    (arguments (list
                #:tests? #f ;; colorschemetest test fail, because require dbus.
                #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules pkg-config
                         ;; require by test.
                         python-minimal
                         python-pygobject))
    (inputs (list cups
                  kcoreaddons
                  kconfig
                  ki18n
                  kdeclarative
                  kio
                  kirigami
                  knotifications
                  libplasma
                  plasma-wayland-protocols
                  kstatusnotifieritem
                  kwayland
                  kwidgetsaddons
                  kwindowsystem
                  kiconthemes
                  qtdeclarative
                  qtwayland
                  wayland
                  kglobalaccel
                  kguiaddons
                  libxkbcommon
                  wayland-protocols))
    (propagated-inputs
     (list xdg-desktop-portal))
    (synopsis "Backend implementation for xdg-desktop-portal using Qt/KF5")
    (description "This package provides a backend implementation
for xdg-desktop-portal that is using Qt/KF5.")
    (home-page "https://invent.kde.org/plasma/xdg-desktop-portal-kde")
    (license license:lgpl2.0+)))


xdg-desktop-portal-kde-alt
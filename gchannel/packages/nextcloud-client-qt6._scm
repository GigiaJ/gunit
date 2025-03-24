(define-module (gchannel packages nextcloud-client)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gchannel packages libp11)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages tls))

(define-public nextcloud-client
  (package
    (name "nextcloud-client")
    (version "3.15.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/nextcloud/desktop")
         (commit (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1qyzyfwr32w4pamjhl0ssiv444nbqnqxi3fn4bs7agg4217pv6bv"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        '(begin
           ;; Not available in Guix.
           (let* ((keep '("QProgressIndicator" "qtokenizer" "kirigami")))
             (with-directory-excursion "src/3rdparty"
               (for-each delete-file-recursively
                         (lset-difference string=?
                                          (scandir ".")
                                          (cons* "." ".." keep)))))
           (with-directory-excursion "src/gui"
             (substitute* "CMakeLists.txt"
               ;; Remove references of deleted 3rdparties.
               (("[ \t]*\\.\\./3rdparty/qtlockedfile/?.*\\.(cpp|h)")
                "")
               (("[ \t]*\\.\\./3rdparty/qtsingleapplication/?.*\\.(cpp|h)")
                "")
               (("[ \t]*\\.\\./3rdparty/kmessagewidget/?.*\\.(cpp|h)")
                "")
               (("[ \t]*list\\(APPEND 3rdparty_SRC \\.\\./3rdparty/?.*\\)")
                "")
               (("\\$\\{CMAKE_SOURCE_DIR\\}/src/3rdparty/qtlockedfile")
                "")
               (("\\$\\{CMAKE_SOURCE_DIR\\}/src/3rdparty/qtsingleapplication")
                "")
               (("\\$\\{CMAKE_SOURCE_DIR\\}/src/3rdparty/kmessagewidget")
                ;; For this, we rely on build inputs, so let's just replace
                ;; them by an autoconf-style variable.
                "@kwidgetsaddons@")
               ;; Expand libraries, that used to be statically linked, but
               ;; no longer are post-vendoring.
               (("KF6::Archive")
                (string-append "KF6::Archive "
                               "QtSolutions_LockedFile "
                               "QtSolutions_SingleApplication "
                               "KF6WidgetsAddons")))
             ;; Fix compatibility with QtSingleApplication from QtSolutions.
             (substitute* '("application.h" "application.cpp")
               (("SharedTools::QtSingleApplication")
                "QtSingleApplication")
               (("slotParseMessage\\(const QString &(msg)?.*\\)")
                "slotParseMessage(const QString &msg)")))
           #t))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags
       (list
        "-DUNIT_TESTING=ON" "-DBUILD_UPDATER=OFF")
       #:imported-modules
       ((guix build glib-or-gtk-build-system)
        ,@%qt-build-system-modules)
       #:modules
       (((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
        (guix build qt-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cmake
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Patch install directory for dbus service files.
             (substitute* "shell_integration/libcloudproviders/CMakeLists.txt"
               (("pkg_get_variable\\(_install_dir dbus-1 .*\\)")
                (string-append "set(_install_dir \"${CMAKE_INSTALL_PREFIX}"
                               "/share/dbus-1/services\")")))
             (substitute* "shell_integration/dolphin/CMakeLists.txt"
               ;; Make sure, that Qt modules are installed under $prefix.
               (("ON CACHE") "OFF CACHE"))
             (substitute* "src/gui/CMakeLists.txt"
               (("@kwidgetsaddons@")
                (search-input-directory inputs
                                        "/include/KF6/KWidgetsAddons/")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "QT_QPA_PLATFORM" "offscreen")
               (invoke "ctest" "-E" "SyncXAttrTest"))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             #t))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
          (delete 'check)
          )))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("glib:bin" ,glib "bin")
       ("librsvg" ,(librsvg-for-system))
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("qttools" ,qttools)
       ("ruby" ,ruby)))
    (inputs
     (list appstream
           dbus
           desktop-file-utils
           glib
           karchive
           kconfig
           kcoreaddons
           kguiaddons
          kio-5
           kjs
           kwidgetsaddons
           libcloudproviders
           libzip
           libxkbcommon
           vulkan-headers
           pcre
           pkg-config
           openssl
           qt5compat
           qtbase
           qtdeclarative
           qtgraphicaleffects
           mesa
           which
           qtkeychain-qt6
           ;;qtquickcontrols2
           qtsolutions
           git
           qtsvg
           qtwebchannel
           libp11
           qtwebsockets
           sqlite
           python-sphinx
           qtwayland
           wayland
           xdg-utils
           zlib
          zstd
          xcb-util-cursor
          libxext
          libxcb
          libsm
          libice
          libb2
          md4c
          at-spi2-core
        ))
    (propagated-inputs
     (list qtwebengine))
    (synopsis "Desktop sync client for Nextcloud")
    (description "Nextcloud-Desktop is a tool to synchronize files from
Nextcloud Server with your computer.")
    (home-page "https://nextcloud.com")
    (license (list license:expat     ; QProgressIndicator
                   license:lgpl2.1+  ; qtokenizer
                   license:gpl2+))))

nextcloud-client
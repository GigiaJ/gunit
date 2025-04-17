(define-module (gchannel packages libportal)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
)

(define-public libportal
  (package
    (name "libportal")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flatpak/libportal")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rbqkmvvfig98ig8gsf93waiizrminj7gywxbza15hzx3an3hwh9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Ddocs=false")          ; requires unpackaged gi-docgen
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-qt-environment-variables
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Required for tests
              (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list pkg-config
           docbook-xsl
           docbook-xml
           `(,glib "bin")
           gobject-introspection
           libxml2
           vala))
    (inputs
     (list gtk
           gtk+
           qtbase-5
           qtx11extras))
    (propagated-inputs
     (list glib))
    (home-page "https://github.com/flatpak/libportal")
    (synopsis "Flatpak portal library")
    (description
     "libportal provides GIO-style async APIs for most Flatpak portals.")
    (license license:lgpl2.1+)))

libportal
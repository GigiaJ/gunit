(define-module (gunit packages emacs)
  #:use-module ((guix licenses)
                #:prefix license:)
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
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gawk)
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
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages libedit)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
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
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages emacs)
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix base16)
  #:use-module (gunit packages chromium)
  #:use-module (nongnu packages editors)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define-public emacs-application-framework
  (package
    (name "emacs-application-framework")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-eaf/emacs-application-framework")
             (commit "bb00de50f946f4f185aaeecabc0271f68c1ffb6e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "019fy88aa82y6did7vvzrqd34qgvzn5rx30i12nz39vpc5gij7f6"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:include
      #~(cons* "eaf.py"
               "core"
               "app"
               "extension"
               "reinput"
               %default-include)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "eaf.el"
                (("\"python3\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/python3") "\"")))
              (substitute* "eaf.el"
                (("\\(eaf--check-dependencies\\)")
                 "t"))))

          (add-after 'patch-paths 'add-core-to-load-path
            (lambda _
              (setenv "EMACSLOADPATH"
                      (string-append (getcwd) "/core:"
                                     (getenv "EMACSLOADPATH")))))

          (add-after 'add-core-to-load-path 'build-reinput
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcc (search-input-file inputs "/bin/gcc")))
                (invoke gcc
                        "reinput/main.c"
                        "-o"
                        "reinput/reinput"
                        "-I"
                        (string-append (assoc-ref inputs "libinput")
                                       "/include")
                        "-I"
                        (string-append (assoc-ref inputs "libevdev")
                                       "/include/libevdev-1.0")
                        "-I"
                        (string-append (assoc-ref inputs "eudev") "/include")
                        "-linput"
                        "-levdev"
                        "-ludev")))))))
    (propagated-inputs (list python
                             python-pyqt-6
                             python-pyqtwebengine-6
                             libinput
                             python-epc
                             python-sexpdata
                             libevdev
                             eudev
                             emacs-minimal
                             emacs-epc
                             gcc
                             pkg-config))
    (home-page "https://github.com/emacs-eaf/emacs-application-framework")
    (synopsis "Graphical application framework for Emacs")
    (description "EAF allows Emacs to run full-featured GUI applications.")
    (license license:gpl3+)))

(define-public emacs-eaf-browser
  (let ((darkreader (origin
                      (method url-fetch)
                      (uri
                       "https://registry.npmjs.org/darkreader/-/darkreader-4.9.58.tgz")
                      (sha256 (base32
                               "1b1qaynsslkcsxxcqm28h62g6g2zn9v2zynywnyy69d4c7hd1mr5"))))
        (readability (origin
                       (method url-fetch)
                       (uri
                        "https://registry.npmjs.org/@mozilla/readability/-/readability-0.6.0.tgz")
                       (sha256 (base32
                                "06frg9i7ajd4w0m5yn0x5lkd535msmkdrsjxj91valdd1p2kqg3d")))))
    (package
      (name "emacs-eaf-browser")
      (version "2026.02.07")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-eaf/eaf-browser.git")
               (commit "master")))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0svypmag4k2nmpb1lrd96ngnljs94yxxij027n0j2vbms1rfphqf"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:include
        #~(cons* "buffer.py" %default-include)
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'install-js-deps
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((node-dir "node_modules"))
                  (mkdir-p (string-append node-dir "/darkreader"))
                  (mkdir-p (string-append node-dir "/@mozilla/readability"))
                  (invoke "tar"
                          "-xf"
                          #+darkreader
                          "-C"
                          (string-append node-dir "/darkreader")
                          "--strip-components=1")
                  (invoke "tar"
                          "-xf"
                          #+readability
                          "-C"
                          (string-append node-dir "/@mozilla/readability")
                          "--strip-components=1"))))
            (add-after 'install 'install-node-modules
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (site-lisp (string-append out
                                   "/share/emacs/site-lisp/eaf-browser-"
                                   #$version)))
                  (copy-recursively "node_modules"
                                    (string-append site-lisp "/node_modules"))))))))
      (inputs (list aria2))
      (propagated-inputs (list emacs-application-framework
                               python-pyqt-6
                               python-pyqtwebengine-6
                               python-pyqt6-sip
                               python-sip
                               python-pysocks))
      (home-page "https://github.com/emacs-eaf/eaf-browser")
      (synopsis "EAF web browser application")
      (description "A modern browser for Emacs using PyQtWebEngine.")
      (license license:gpl3+))))

emacs-eaf-browser



(define-module (gchannel packages hyprland)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gchannel packages hyprland-protocols-input-capture)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.49.1")
        (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/3l0w/Hyprland.git")
             (commit "821497bc2379b8bef091c455bbfbfeec19f5ae4b")))
       (sha256
        (base32 "0njqyl6vsqlb8dv4wdn5h34dk67yqzc99gvwa13j252cv3n0bpya"))))
    ;; The project's native build system is Meson.
    (build-system meson-build-system)

    (arguments
     (list
      #:tests? #f
      ;; Use Meson's build-in options to configure the project.
      #:configure-flags
      #~'("-Dhyprpm=disabled")
      #:phases
 #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/xwayland/Server.cpp"
                     (("Xwayland( \\{\\})" _ suffix)
                      (string-append
                       (search-input-file inputs "bin/Xwayland")
                       suffix)))
                   (substitute* (find-files "src" "\\.cpp$")
                     (("/usr/local(/bin/Hyprland)" _ path)
                      (string-append #$output path))
                     (("/usr") #$output)
                     (("\\<(addr2line|cat|lspci|nm)\\>" cmd)
                      (search-input-file
                       inputs (string-append "bin/" cmd))))
                   (substitute* '("src/Compositor.cpp"
                                  "src/xwayland/XWayland.cpp"
                                  "src/managers/VersionKeeperManager.cpp")
                     (("!NFsUtils::executableExistsInPath.*\".") "false")
                     (("hyprland-update-screen" cmd)
                      (search-input-file inputs (in-vicinity "bin" cmd)))))))))

    (native-inputs
     (list gcc-14
           hyprwayland-scanner
           (module-ref (resolve-interface
                  '(gnu packages commencement))
                 'ld-wrapper)
           pkg-config))
    (inputs
     (list aquamarine
           binutils
           cairo
           hyprcursor
           hyprgraphics
           hyprland-protocols-input-capture
           hyprland-qtutils
           hyprlang
           hyprutils
           libinput-minimal
           libxcursor
           libxkbcommon
           mesa
           pango
           pciutils
           re2-next
           udis86
           wayland
           wayland-protocols
           linux-libre-headers-6.14
           xcb-util-errors
           xcb-util-wm
           xorg-server-xwayland))

    (home-page "https://hyprland.org/")
    (synopsis "Dynamic tiling Wayland compositor")
    (description
     "Hyprland is a dynamic tiling Wayland compositor that doesn't sacrifice on
its looks.")
    (license license:bsd-3)))

hyprland
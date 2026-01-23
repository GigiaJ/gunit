(define-module (gunit packages bolt-launcher)
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
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
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
  #:use-module (gnu packages libedit)
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
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix base16)
  #:use-module (nongnu packages chromium)
  #:use-module (nongnu packages editors)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils)
)

(define bolt-launcher-client
    (package
    (name "bolt-launcher")
    (version "0.11.0")
    (source (origin
    (method git-fetch)
    (uri (git-reference
    (url "https://github.com/Adamcake/Bolt")
  (commit "1720eb0758221d9cd79abddf613d7335ed05e032")
  (recursive? #t)))
  
        (sha256
        (base32 "075dbjsy4nkbsnd1w57l85gpqk9sg2q75gp07xykl5g3j0qgwhcx"))))
    (build-system cmake-build-system)

    (inputs
        (list 
        chromium-embedded-framework eudev libarchive glib glibc gtk xdg-utils libxshmfence hicolor-icon-theme nss fmt spng mesa vulkan-loader wayland))
        (arguments
        (list
        #:tests? #f 
        #:configure-flags
                #~(list
                    ;; Can probably clean this up
                    (string-append "-D " "CMAKE_INSTALL_PREFIX=" (assoc-ref %outputs "out"))
                    ;;(string-append "-D " "BOLT_DEV_SHOW_DEVTOOLS=" "1")
                    (string-append "-DBOLT_BINDIR=" "./bin")
                    (string-append "-DBOLT_SHAREDIR=" "./share")
                    (string-append "-DBOLT_LIBDIR=" "./lib")
                    (string-append "-DCEF_DIR=" (assoc-ref %build-inputs "chromium-embedded-framework"))
                    (string-append "-DCEF_ROOT=" (assoc-ref %build-inputs "chromium-embedded-framework"))
                    (string-append "-DBOLT_CEF_RESOURCEDIR_OVERRIDE=" (assoc-ref %build-inputs "chromium-embedded-framework") "/share/cef")
                    (string-append "-DBOLT_LIBCEF_DIRECTORY=" (assoc-ref %build-inputs "chromium-embedded-framework") "/lib")
                    (string-append "-DBOLT_CEF_INCLUDEPATH=" (assoc-ref %build-inputs "chromium-embedded-framework"))
                    (string-append "-DBOLT_CEF_DLLWRAPPER=" (assoc-ref %build-inputs "chromium-embedded-framework") "/lib/libcef_dll_wrapper.a")
                    "-D BOLT_META_NAME=bolt-launcher"
                    "-D BOLT_SKIP_LIBRARIES=1")
            #:phases
            #~(modify-phases %standard-phases
            (add-after 'install 'link-cef
            (lambda _
                (map (lambda (entry)
                    (let* ((source (car entry)) (file (cdr entry)))
                    (symlink (string-append (assoc-ref %build-inputs source) file)
                            (string-append (assoc-ref %outputs "out") "/opt/bolt-launcher/" (basename file)))))
                    
                    
(append
  ;; CEF files
  (map (lambda (file)
         (cons "chromium-embedded-framework" file))
       '("/lib/libcef.so"
         "/share/cef/icudtl.dat"
         "/share/cef/v8_context_snapshot.bin"))

  ;; Mesa GL/EGL/GLES/Vulkan
  (map (lambda (file)
         (cons "mesa" file))
       '("/lib/libGL.so.1"
         "/lib/libEGL.so.1"
         "/lib/libGLESv2.so.2"
         "/lib/libvulkan.so.1"))
)

                        
                    )

                (wrap-program (string-append (assoc-ref %outputs "out") "/opt/bolt-launcher/bolt")
                `("LD_PRELOAD" ":" prefix (
                    ,(string-append #$(this-package-input "mesa") "/lib/libGL.so.1")
                    ,(string-append #$(this-package-input "mesa") "/lib/libEGL.so.1")
                    ,(string-append #$(this-package-input "mesa") "/lib/libGLESv2.so.2")
                    ,(string-append #$(this-package-input "mesa") "/lib/libvulkan.so.1")))
                `("LD_LIBRARY_PATH" ":" prefix (
                    ,(string-append #$(this-package-input "mesa") "/lib")
                    ,(string-append #$(this-package-input "eudev") "/lib")
                    ,(string-append #$(this-package-input "nss") "/lib/nss")))
                `("XDG_DATA_DIRS" ":" prefix (
                    ,(string-append #$(this-package-input "gtk") "/share")))
                `("PATH" ":" prefix (
                    ,(string-append #$(this-package-input "xdg-utils") "/bin"))))
                
                    (invoke "mv" (string-append (assoc-ref %outputs "out") "/opt/bolt-launcher/bolt") (string-append (assoc-ref %outputs "out") "/bin/bolt"))
             #t)))))
    (native-inputs (list cmake git wayland))
    (synopsis "An alternative third-party open-source launcher for RuneScape or Old School RuneScape.")
    (home-page "https://bolt.adamcake.com/")
    (description "Free open-source third-party implementation of the Jagex Launcher.")
    (license license:agpl3)))

(define bolt-launcher-libs
    `(("at-spi2-core" ,at-spi2-core)      ; Required (often) for bolt-launcherVR interface.
      ("bash" ,bash)                      ; Required for bolt-launcher startup.
      ("cairo", cairo)
      ("coreutils" ,coreutils)
      ("diffutils" ,diffutils)
      ("dbus-glib" ,dbus-glib)            ; Required for bolt-launcher browser.
      ("elfutils" ,elfutils)              ; Required for capturing library dependencies in pv.
      ("eudev" ,eudev)                    ; Required for bolt-launcherwebhelper/heavy runtime.
      ("expat" ,expat)                    ; Needed for RS3
      ("fontconfig" ,fontconfig)          ; Required for bolt-launcher client.
      ("file" ,file)                      ; Used for bolt-launcher installation.
      ("find" ,findutils)                 ; Required at least for some logging.
      ("fmt" ,fmt)                 ; Needed for RS3
      ("font-google-noto" ,font-google-noto) ; Not required but to match following fonts.
      ;; These next three fonts are to cover emoji and Chinese/Japanese/Korean
      ;; and related scripts.
      ("font-google-noto-emoji" ,font-google-noto-emoji)
      ("font-google-noto-sans-cjk" ,font-google-noto-sans-cjk)
      ("font-google-noto-serif-cjk" ,font-google-noto-serif-cjk)
      ("freetype" ,freetype)              ; Required for bolt-launcher login.
      ("bzip2" ,bzip2)        ; CRITICAL: Cache decompression
      ("curl" ,curl)          ; CRITICAL: Asset streaming
      ("libxtst" ,libxtst)    ; CRITICAL: Input handling
      ("libxscrnsaver" ,libxscrnsaver) ; Recommended: Idle detection
      ("gawk" ,gawk)
      ("gdk-pixbuf" ,gdk-pixbuf)          ; Required for bolt-launcher tray icon.
      ;; Required for bolt-launcher startup; use newer version for better compatibility
      ;; with some games like Dwarf Fortress.
      ("gcc:lib" ,gcc-14 "lib")
      ("glib" ,glib)
      ("glibc" ,glibc)
      ("grep" ,grep)
      ("gtk+" ,gtk+)
      ("gtk" ,gtk+-2)
      ("libbsd" ,libbsd)
      ("libcap" ,libcap)                  ; Required for bolt-launcherVR, but needs pkexec too.
      ("libdrm" ,libdrm)                  ; Needed for RS3
      ("libglvnd" ,libglvnd)
      ("libusb" ,libusb)                  ; Required for bolt-launcherVR.
      ("libsm" ,libsm)
      ("libxcb" ,libxcb)                  ; Needed for RS3
      ("libxcomposite" ,libxcomposite)    ; Needed for RS3
      ("libxext" ,libxext)    ; Needed for RS3
      ("libxkbcommon" ,libxkbcommon)    ; Needed for RS3
      ("libva" ,libva)                    ; Required for hardware video encoding/decoding.
      ("libvdpau" ,libvdpau)              ; Required for hardware video encoding/decoding.
      ("libvdpau-va-gl" ,libvdpau-va-gl)  ; Additional VDPAU support.
      ("libx11" ,libx11)
      ("libxdamage" ,libxdamage)          ; Needed for RS3
      ("libxfixes" ,libxfixes)            ; Needed for RS3
      ("libxxf86vm" ,libxxf86vm)
      ("zstd:lib" ,zstd "lib")
      ("libnsl" ,libnsl)
      ("libpng" ,libpng)
      ("icu4c" ,icu4c)
      ("llvm" ,llvm-for-mesa)             ; Required for mesa.
      ("lsof" ,lsof)                      ; Required for some friend's list actions.
      ("mesa" ,mesa)                      ; Required for bolt-launcher startup.
      ("nspr" ,nspr)                      ; Required for RS3
      ("nss-certs" ,nss-certs)            ; Required for bolt-launcher login.
      ("nss" ,nss)                        ; Needed for RS3
      ("pango" ,pango)
      ("pciutils" ,pciutils)              ; Tries to run lspci at bolt-launcher startup.
      ("procps" ,procps)
      ("openssl" ,openssl-1.1)
      ("sed" ,sed)
      ("sdl2" ,sdl2)
      ("tar" ,tar)
      ("usbutils" ,usbutils)              ; Required for bolt-launcherVR.
      ("util-linux" ,util-linux)          ; Required for bolt-launcher login.
("vulkan-loader" ,vulkan-loader)   ; <--- ADD THIS
      ("libxshmfence" ,libxshmfence)     ; <--- ADD THIS
      ("cups" ,cups)
      ("wayland" ,wayland)                ; Required for mesa vulkan (e.g. libvulkan_radeon).
      ("libxcursor" ,libxcursor)            ; Often needed for custom game cursors.
      ("libxrandr" ,libxrandr)              ; Essential for changing resolutions/fullscreen.
      ("libxi" ,libxi)                      ; Input extension (gaming mice/tablets).
      ("xdg-user-dirs" ,xdg-user-dirs)    ; Suppress warning of missing xdg-user-dir.
      ("flatpak-xdg-utils" ,flatpak-xdg-utils)
      ("xz" ,xz)
      ("zenity" ,zenity)
      ("zlib" ,zlib)
      ("alsa-lib" ,alsa-lib)              ; Required for audio in most games.
      ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio") ; Required for audio in most games.
      ("font-dejavu" ,font-dejavu)
      ("font-liberation" ,font-liberation)
      ("imgui" ,imgui-1.86)               ; Required for MangoHud.
      ("mangohud" ,mangohud)
      ("openal" ,openal)                  ; Prevents corrupt audio in Crypt of the Necrodancer.
      ("pulseaudio" ,pulseaudio)          ; Prevents corrupt audio in Sven Coop.
      ("python" ,python)                  ; Required for KillingFloor2 and Wreckfest.
      ("spdlog" ,spdlog)
    ))                ; Required for progress dialogs.

(define bolt-launcher-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union `(,@bolt-launcher-libs
                      ,@fhs-min-libs)
                    #:name "fhs-union-64"))))

(define bolt-launcher-ld.so.cache
  (ld.so.conf->ld.so.cache bolt-launcher-ld.so.conf))

(define-public bolt-launcher-container
  (nonguix-container
   (name "bolt-launcher")
   (wrap-package bolt-launcher-client)
   (run "/bin/bolt")
   (ld.so.conf bolt-launcher-ld.so.conf)
   (ld.so.cache bolt-launcher-ld.so.cache)
   
   (union64
    (fhs-union `(,@bolt-launcher-libs
                 ,@fhs-min-libs)
               #:name "fhs-union-64"))
   (link-files '("share/applications/bolt-launcher.desktop"))
   (description (package-description bolt-launcher-client))))

(define-public bolt-launcher (nonguix-container->package bolt-launcher-container))

bolt-launcher
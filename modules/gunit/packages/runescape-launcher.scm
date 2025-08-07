(define-module (gunit packages runescape-launcher)
#:use-module ((guix licenses) #:prefix license:)
#:use-module ((nonguix licenses) #:prefix license:)
#:use-module (guix git-download)
#:use-module (guix packages)
#:use-module (guix download)
#:use-module (guix gexp)
#:use-module (guix build-system gnu)
#:use-module (guix build-system python)
#:use-module (guix build-system copy)
#:use-module (gnu packages audio)
#:use-module (gnu packages base)
#:use-module (gnu packages bash)
#:use-module (gnu packages certs)
#:use-module (gnu packages compression)
#:use-module (gnu packages curl)
#:use-module (gnu packages debian)
#:use-module (gnu packages elf)
#:use-module (gnu packages file)
#:use-module (gnu packages fonts)
#:use-module (gnu packages fontutils)
#:use-module (gnu packages freedesktop)
#:use-module (gnu packages gawk)
#:use-module (gnu packages gcc)
#:use-module (gnu packages gl)
#:use-module (gnu packages glib)
#:use-module (gnu packages gnome)
#:use-module (gnu packages graphics)
#:use-module (gnu packages gtk)
#:use-module (gnu packages libbsd)
#:use-module (gnu packages libusb)
#:use-module (gnu packages linux)
#:use-module (gnu packages llvm)
#:use-module (gnu packages logging)
#:use-module (gnu packages lsof)
#:use-module (gnu packages networking)
#:use-module (gnu packages nss)
#:use-module (nongnu packages nvidia)
#:use-module (gnu packages pciutils)
#:use-module (gnu packages pulseaudio)
#:use-module (gnu packages pretty-print)
#:use-module (gnu packages python)
#:use-module (gnu packages python-web)
#:use-module (gnu packages python-xyz)
#:use-module (gnu packages toolkits)
#:use-module (gnu packages tls)
#:use-module (gnu packages sdl)
#:use-module (gnu packages video)
#:use-module (gnu packages xdisorg)
#:use-module (gnu packages xorg)
#:use-module (gnu packages xml)
#:use-module (nonguix build-system chromium-binary)
#:use-module (nonguix multiarch-container)
#:use-module (nonguix utils))


(define runescape-launcher
    (package
    (name "runescape-launcher")
    (version "2.2.11")
    (source       
    (origin
    (method url-fetch)
    (uri (string-append "https://content.runescape.com/downloads/ubuntu/pool/non-free/r/" name "/" name "_" version "_amd64.deb"))
  
        (sha256
        (base32 "1b4lspdipm0irb3x9ia8g6d1n16qc4y7j8lprm7z3llmkhhdhqlg"))))
    (build-system copy-build-system)

    (inputs
        (list 
        cairo gcc glib glibc gtk libcap libglvnd libsm libx11 libxxf86vm openssl pango sdl2 zlib))
        (arguments
        (list
        #:tests? #f  ; no check target
        #:phases
        #~(modify-phases %standard-phases
        (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "dpkg-deb" "-x" source ".")
             #t))
        (delete 'configure)
        (delete `build)
        (add-before 'install 'patch-elf-interpreter
           (lambda _
             (display (invoke "ls" "-a" "./usr/share/games/runescape-launcher"))
             ;;(invoke "setcap" "cap_net_raw+ep" "./usr/share/games/runescape-launcher/runescape")
             (substitute* "./usr/bin/runescape-launcher"
             (("unset XMODIFIERS") (string-append "$envVarsWithXmodifiers"))
             (("/usr/share/games/runescape-launcher/runescape")
              (string-append (assoc-ref %outputs "out") "/usr/share/games/runescape-launcher/runescape")))
              (display "catssss")
            
              (let ((glibc (assoc-ref %build-inputs "glibc"))
                   (elf-file "./usr/share/games/runescape-launcher/runescape"))
              (invoke "patchelf" "--set-interpreter" (string-append glibc "/lib/ld-linux-x86-64.so.2") elf-file))
               #t))
        ;; (delete 'install)
        )
        )
    )
      
    (native-inputs
        (list unzip dpkg patchelf iputils))
    (synopsis "A client for RuneScape")
    (home-page "https://www.runescape.com/")
    (description "RuneScape Game Client (NXT)")
    (license
    (license:nonfree "https://www.jagex.com/en-GB/terms")
    ))) 

    (define steam-client-libs
    `(("at-spi2-core" ,at-spi2-core)      ; Required (often) for SteamVR interface.
      ("bash" ,bash)                      ; Required for steam startup.
      ("cairo", cairo)
      ("coreutils" ,coreutils)
      ("diffutils" ,diffutils)
      ("dbus-glib" ,dbus-glib)            ; Required for steam browser.
      ("elfutils" ,elfutils)              ; Required for capturing library dependencies in pv.
      ("eudev" ,eudev)                    ; Required for steamwebhelper/heavy runtime.
      ("expat" ,expat)                    ; Needed for RS3
      ("fontconfig" ,fontconfig)          ; Required for steam client.
      ("file" ,file)                      ; Used for steam installation.
      ("find" ,findutils)                 ; Required at least for some logging.
      ("fmt" ,fmt)                 ; Needed for RS3
      ("font-google-noto" ,font-google-noto) ; Not required but to match following fonts.
      ;; These next three fonts are to cover emoji and Chinese/Japanese/Korean
      ;; and related scripts.
      ("font-google-noto-emoji" ,font-google-noto-emoji)
      ("font-google-noto-sans-cjk" ,font-google-noto-sans-cjk)
      ("font-google-noto-serif-cjk" ,font-google-noto-serif-cjk)
      ("freetype" ,freetype)              ; Required for steam login.
      ("gawk" ,gawk)
      ("gdk-pixbuf" ,gdk-pixbuf)          ; Required for steam tray icon.
      ;; Required for steam startup; use newer version for better compatibility
      ;; with some games like Dwarf Fortress.
      ("gcc:lib" ,gcc-14 "lib")
      ("glib" ,glib)
      ("glibc" ,glibc)
      ("grep" ,grep)
      ("gtk+" ,gtk+)
      ("gtk" ,gtk+-2)
      ("libbsd" ,libbsd)
      ("libcap" ,libcap)                  ; Required for SteamVR, but needs pkexec too.
      ("libdrm" ,libdrm)                  ; Needed for RS3
      ("libglvnd" ,libglvnd)
      ("libusb" ,libusb)                  ; Required for SteamVR.
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
      ("llvm" ,llvm-for-mesa)             ; Required for mesa.
      ("lsof" ,lsof)                      ; Required for some friend's list actions.
      ("mesa" ,mesa)                      ; Required for steam startup.
      ("nspr" ,nspr)                      ; Required for RS3
      ("nss-certs" ,nss-certs)            ; Required for steam login.
      ("nss" ,nss)                        ; Needed for RS3
      ("pango" ,pango)
      ("pciutils" ,pciutils)              ; Tries to run lspci at steam startup.
      ("procps" ,procps)
      ("openssl" ,openssl-1.1)
      ("sed" ,sed)
      ("sdl2" ,sdl2)
      ("tar" ,tar)
      ("usbutils" ,usbutils)              ; Required for SteamVR.
      ("util-linux" ,util-linux)          ; Required for steam login.
      ("wayland" ,wayland)                ; Required for mesa vulkan (e.g. libvulkan_radeon).
      ("xdg-user-dirs" ,xdg-user-dirs)    ; Suppress warning of missing xdg-user-dir.
      ("flatpak-xdg-utils" ,flatpak-xdg-utils)
      ("xz" ,xz)
      ("zenity" ,zenity)
      ("zlib" ,zlib)
    ))                ; Required for progress dialogs.
  
  (define steam-gameruntime-libs
    `(("alsa-lib" ,alsa-lib)              ; Required for audio in most games.
      ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio") ; Required for audio in most games.
      ("font-dejavu" ,font-dejavu)
      ("font-liberation" ,font-liberation)
      ("imgui" ,imgui-1.86)               ; Required for MangoHud.
      ("mangohud" ,mangohud)
      ("openal" ,openal)                  ; Prevents corrupt audio in Crypt of the Necrodancer.
      ("pulseaudio" ,pulseaudio)          ; Prevents corrupt audio in Sven Coop.
      ("python" ,python)                  ; Required for KillingFloor2 and Wreckfest.
      ("spdlog" ,spdlog)))                ; Required for MangoHud.
  
  (define steam-container-libs
    (append steam-client-libs
            steam-gameruntime-libs
            fhs-min-libs))
  
  (define steam-nvidia-container-libs
    (modify-inputs steam-container-libs
      (replace "mesa" nvda)))

(define steam-ld.so.conf
(packages->ld.so.conf
 (list (fhs-union steam-container-libs
                  #:name "fhs-union-64")
       (fhs-union steam-container-libs
                  #:name "fhs-union-32"
                  #:system "i686-linux"))))

(define steam-ld.so.cache
(ld.so.conf->ld.so.cache steam-ld.so.conf))

(define steam-nvidia-ld.so.conf
(packages->ld.so.conf
 (list (fhs-union steam-nvidia-container-libs
                  #:name "fhs-union-64")
       (fhs-union steam-nvidia-container-libs
                  #:name "fhs-union-32"
                  #:system "i686-linux"))))

(define steam-nvidia-ld.so.cache
(ld.so.conf->ld.so.cache steam-nvidia-ld.so.conf))


(define-public runescape-container
    (nonguix-container
    (name "runescape")
    (wrap-package runescape-launcher)
    (run "/usr/bin/runescape-launcher")
    (ld.so.conf steam-ld.so.conf)
    (ld.so.cache steam-ld.so.cache)
    (union64
    (fhs-union steam-container-libs
                #:name "fhs-union-64"))
    (union32
    (fhs-union steam-container-libs
                #:name "fhs-union-32"
                #:system "i686-linux"))
    (link-files '("usr"))
    (description "Runescape.")))
    
(define-public runescape (nonguix-container->package runescape-container))
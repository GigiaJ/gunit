;; This is an abomination that needs to be cleaned up. It DOES work in supplying Millennium.
;; I believe we should actually make a change upstream and change the steam-client
;; to be publicly visible or inheritable in some manner... as it would make
;; mixing in easier
(define-module (gunit packages millennium)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (nonguix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
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
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages video)
  #:use-module (guix transformations)
  #:use-module (guix utils)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define steam-client
  (package
    (name "steam-client")
    (version "1.0.0.84")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.steampowered.com/steam/archive/precise/steam_"
                           version ".tar.gz"))
       (sha256
        (base32
         "0i3v0zz36x7v81qslvfbiby57hk96hn15w4xxal1lgvrb0npdyii"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:validate-runpath? #f ; Looks for bin/steam which doesn't exist.
       #:make-flags
       (list "PREFIX=" (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; Patch Makefile so it creates links to the store rather than /lib.
         (add-after 'unpack 'patch-makefile
           (lambda _
             (substitute* "Makefile"
               (("-fns ")
                "-fns $(DESTDIR)"))))
         (add-after 'unpack 'patch-startscript
           (lambda _
             (substitute* "bin_steam.sh"
               (("/usr") (assoc-ref %outputs "out")))))
         (add-after 'patch-dot-desktop-files 'patch-desktop-file
           (lambda _
             (let ((path (string-append (assoc-ref %outputs "out")
                                        "/share/applications/")))
               (substitute* (string-append path "steam.desktop")
                 (("Exec=.*/steam") "Exec=steam"))
               (copy-file (string-append path "steam.desktop")
                          (string-append path "steam-asound32.desktop"))
               (substitute* (string-append path "steam-asound32.desktop")
                 (("Exec=steam %U") "Exec=steam %U -- --asound32")
                 (("Name=Steam") "Name=Steam (32-bit ALSA)")))))
         (delete 'patch-dot-desktop-files)
         ;; Steamdeps installs missing packages, which doesn't work with Guix.
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out")))
               (delete-file (string-append out "/lib/steam/bin_steamdeps.py"))
               (delete-file (string-append out "/bin/steamdeps"))))))))
    (home-page "https://store.steampowered.com")
    (synopsis "Digital distribution platform for managing and playing games")
    (description "Steam is a digital software distribution platform created by Valve.")
    (license (license:nonfree "file:///share/doc/steam/steam_subscriber_agreement.txt"))
    (supported-systems '("x86_64-linux"))))

(define steam-client-libs
  `(("at-spi2-core" ,at-spi2-core)      ; Required (often) for SteamVR interface.
    ("bash" ,bash)                      ; Required for steam startup.
    ("coreutils" ,coreutils)
    ("diffutils" ,diffutils)
    ("dbus-glib" ,dbus-glib)            ; Required for steam browser.
    ("elfutils" ,elfutils)              ; Required for capturing library dependencies in pv.
    ("eudev" ,eudev)                    ; Required for steamwebhelper/heavy runtime.
    ("fontconfig" ,fontconfig)          ; Required for steam client.
    ("file" ,file)                      ; Used for steam installation.
    ("find" ,findutils)                 ; Required at least for some logging.
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
    ("grep" ,grep)
    ("libbsd" ,libbsd)
    ("libcap" ,libcap)                  ; Required for SteamVR, but needs pkexec too.
    ("libusb" ,libusb)                  ; Required for SteamVR.
    ("libva" ,libva)                    ; Required for hardware video encoding/decoding.
    ("libvdpau" ,libvdpau)              ; Required for hardware video encoding/decoding.
    ("libvdpau-va-gl" ,libvdpau-va-gl)  ; Additional VDPAU support.
    ("llvm" ,llvm-for-mesa)             ; Required for mesa.
    ("lsof" ,lsof)                      ; Required for some friend's list actions.
    ("mesa" ,mesa)                      ; Required for steam startup.
    ("nss-certs" ,nss-certs)            ; Required for steam login.
    ("pciutils" ,pciutils)              ; Tries to run lspci at steam startup.
    ("procps" ,procps)
    ("sed" ,sed)
    ("tar" ,tar)
    ("usbutils" ,usbutils)              ; Required for SteamVR.
    ("util-linux" ,util-linux)          ; Required for steam login.
    ("wayland" ,wayland)                ; Required for mesa vulkan (e.g. libvulkan_radeon).
    ("xdg-user-dirs" ,xdg-user-dirs)    ; Suppress warning of missing xdg-user-dir.
    ("flatpak-xdg-utils" ,flatpak-xdg-utils)
    ("xz" ,xz)
    ("zenity" ,zenity)))                ; Required for progress dialogs.

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



(define-public millennium-bin
  (package
    (name "millennium-bin")
    (version "2.34.0")
    (source 
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/SteamClientHomebrew/Millennium/releases/download/v" 
                           version "/millennium-v" version "-linux-x86_64.tar.gz"))
       (sha256 (base32 "1rsfvnsryff2r0ww7i1min134i6l273qsd886vv3llm7j76d51bh"))))
    
    (build-system copy-build-system)
    
    (arguments
     (list

#:install-plan
      #~'(("opt" "share")
            ("usr/lib/millennium" "share")             
          ("usr/share/millennium/assets" "share/millennium")) 
      #:phases
      #~(modify-phases %standard-phases   
(replace 'unpack
  (lambda* (#:key source #:allow-other-keys)
    (invoke "tar" "-xzvf" source)
    #t))

(add-after 'install 'patch-elf
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/share/millennium"))
                     (glibc (assoc-ref inputs "glibc"))
                     (gcc (assoc-ref inputs "gcc:lib"))
                     (openssl (assoc-ref inputs "openssl"))
                     (rpath (string-join 
                             (list (string-append glibc "/lib")
                                   (string-append gcc "/lib")
                                   (string-append openssl "/lib"))
                             ":")))
                (for-each 
                 (lambda (file)
                   (when (and (file-exists? file) (elf-file? file))
                     (invoke "patchelf" "--set-rpath" rpath file)
                     (invoke "patchelf" "--set-interpreter" 
                             (string-append glibc "/lib/ld-linux-x86-64.so.2") 
                             file)))
                 (find-files lib "\\.so$")))))
            
            )))
    
    (native-inputs (list patchelf coreutils))
    (inputs 
     `(("glibc" ,glibc)
       ("gcc:lib" ,gcc "lib")
       ("openssl" ,openssl)))
    (synopsis "Debug")
    (description "Debug")
    (home-page "")
    (license license:gpl3)))

(define-public openssl32
  (package
    (name "openssl32")
    (version "3.0.18-1")
    (source (origin
              (method url-fetch)
              (uri (string-append 
                    "http://ftp.us.debian.org/debian/pool/main/o/openssl/"
                    "libssl3_" version "~deb12u1_i386.deb"))
                    (file-name (string-append "libssl3-" version "-i386.deb"))
              (sha256
        (base32
         "1ynrq56777pn9l4plqv05dzp1sapar0zna9kfa4c0z2qv0pfnb80"))))
    (build-system gnu-build-system)
    (arguments
    
     '(
     #:tests? #f
     #:validate-runpath? #f

     #:phases
       
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "ar" "x" source)
             (invoke "tar" "xf" "data.tar.xz")))
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "usr" 
                              (string-append (assoc-ref outputs "out") )))))))
    (native-inputs (list binutils tar))
    (synopsis "32-bit OpenSSL")
    (description "i686 OpenSSL libraries")
    (home-page "https://www.openssl.org")
    (license license:asl2.0)))

(define steam-millennium
  (package
    (inherit steam-client)
    (name "steam-millennium")
    (source #f)
    (build-system copy-build-system)
    (native-inputs (list coreutils))
    (inputs 
     (list bash steam-client millennium-bin python openssl32 coreutils))
    (arguments
     (list
      #:install-plan
      #~'(("steam/bin" "bin")
        ;;("steam/sbin" "sbin")
        ("steam/share" "share")
    ;;    ("steam/etc" "etc")
    )

      #:phases
      #~(modify-phases %standard-phases
          (delete `patch-dot-desktop-files)
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((millennium-src (assoc-ref inputs "millennium-bin"))  
                    (steam-src (assoc-ref inputs "steam-client")))
                (mkdir-p "steam")
                (copy-recursively steam-src "steam" #:follow-symlinks? #t  #:keep-mtime? #t)
                (copy-recursively millennium-src "steam" #:follow-symlinks? #t ))))

          (add-after 'install 'inject-millennium
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
(bin (string-append out "/bin/steam"))
                     (hidden-dir (string-append out "/lib/steam-original"))
                     (hidden-bin (string-append hidden-dir "/steam"))
(python (assoc-ref inputs "python"))
(crypto32 (assoc-ref inputs "openssl32"))
(openssl32 (assoc-ref inputs "openssl32"))
                     (bash (assoc-ref inputs "bash")))


                (mkdir-p hidden-dir)
                (rename-file bin hidden-bin)

                (call-with-output-file bin
                  (lambda (port)
                    (format port "#!~a/bin/bash

export USER_HOME=\"$HOME\"
export MILLENNIUM_ROOT=\"~a/share\"
export MILLENNIUM_RUNTIME_PATH=\"$MILLENNIUM_ROOT/libmillennium_x86.so\"
export OPENSSL_CONF=\"/dev/null\"
export STEAM_RUNTIME_LOGGER=\"0\"

# This must use our system Python as the bundled one is read-only by this point
# And pip behaves weird
MILLENNIUM_VENV=\"$USER_HOME/.local/share/millennium/.venv\"
if [ ! -d \"$MILLENNIUM_VENV\" ]; then
  echo \"[Millennium] Bootstrapping venv...\"
  ~a/bin/python3 -m venv \"$MILLENNIUM_VENV\"
  \"$MILLENNIUM_VENV/bin/python3\" -m pip install requests websockets charset-normalizer
fi

STEAM_LIB_PATH=\"$USER_HOME/.local/share/Steam/ubuntu12_32\"
mkdir -p \"$STEAM_LIB_PATH\"


export PYTHONHOME=\"$MILLENNIUM_ROOT/python-i686-3.11.8\"
export PYTHONPATH=\"$PYTHONHOME/lib/python3.11:$PYTHONHOME/lib/python3.11/site-packages\"

rm -f \"$STEAM_LIB_PATH/libXtst.so.6\"
rm -f \"$STEAM_LIB_PATH/libpython-3.11.8.so\"
rm -f \"$STEAM_LIB_PATH/libssl.so.3\"
rm -f \"$STEAM_LIB_PATH/libcrypto.so.3\"

ln -s \"$MILLENNIUM_ROOT/libmillennium_bootstrap_86x.so\" \"$STEAM_LIB_PATH/libXtst.so.6\"
ln -s \"$PYTHONHOME/lib/libpython-3.11.8.so\" \"$STEAM_LIB_PATH/libpython-3.11.8.so\"
ln -s \"$MILLENNIUM_ROOT/python-i686-3.11.8/lib/libpython-3.11.8.so\" \"$STEAM_LIB_PATH/libpython-3.11.8.so\"
ln -s \"~a/lib/i386-linux-gnu/libssl.so.3\"    \"$STEAM_LIB_PATH/libssl.so.3\"
ln -s \"~a/lib/i386-linux-gnu/libcrypto.so.3\" \"$STEAM_LIB_PATH/libcrypto.so.3\"

exec \"~a\" \"$@\"
"
                            (assoc-ref inputs "bash")
                            out
                            python
                            openssl32
                            crypto32
                            hidden-bin)))
                (chmod bin #o755)))))))))



(define (steam-container-for driver)
  (nonguix-container
   (name "steam-m")
   (wrap-package steam-millennium)
   (run "/bin/steam")
   (packages
    (modify-inputs steam-container-libs
      (replace "mesa" driver)))
   (preserved-env %nvidia-environment-variable-regexps)
   (link-files '("share"))
(exposed
 (list
  #~#$(file-append steam-millennium
                   "/share/python-i686-3.11.8=/opt/python-i686-3.11.8")
                  #~#$(file-append steam-millennium
                   "/share/millennium/pipx=/usr/share/millennium/assets/pipx")))
   (description "Steam is a digital software distribution platform created by
Valve.  This package provides a script for launching Steam in a Guix container
which will use the directory @file{$HOME/.local/share/guix-sandbox-home} where
all games will be installed.")))


(define-deprecated/public-alias steam-container (steam-container-for mesa))
(define-deprecated/public-alias steam-nvidia-container (steam-container-for nvda))

(define steam-for
  (compose nonguix-container->package steam-container-for))

(define-public steam-m (steam-for mesa))
(define-public steam-m-nvidia
  (package-with-alias "steam-m-nvidia" (steam-for nvda)))


  steam-m


(define-module (gunit packages glamoroustoolkit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (nonguix multiarch-container)
  #:use-module (nongnu packages nvidia))

(define gtoolkit-version "1.1.674")

(define glamoroustoolkit-client
  (package
    (name "glamoroustoolkit-client")
    (version gtoolkit-version)
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/feenkcom/gtoolkit/releases/download/v"
         version
         "/GlamorousToolkit-Linux-"
         (match (or (%current-target-system) (%current-system))
           ("aarch64-linux" "aarch64")
           (_ "x86_64"))
         "-v"
         version
         ".zip"))
       (sha256
        (base32
         (match (or (%current-target-system) (%current-system))
           ("aarch64-linux"
            "1rgi76si4rk4d8wmssr8s57ljjdyjfnc87mhrkbfvx2gcwr2rcc1")
           (_
            "0wpyd3rjsgmvazhc41cldv15pf49aqydf2nvhs863xdvqi0ahkq9"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (inputs
     (list bash coreutils))
    (arguments
     (list
      ;; ~243 MiB prebuilt VM + Pharo image bundle.
      #:substitutable? #f
      #:tests? #f
      #:validate-runpath? #f
      #:strip-binaries? #f
      #:patch-shebangs? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (mkdir "source")
              (chdir "source")
              (invoke "unzip" "-q" source)))
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          ;; Keep Exec=glamoroustoolkit relative so the linked .desktop file
          ;; invokes the outer nonguix-container wrapper.
          (delete 'patch-dot-desktop-files)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (dist (string-append out "/share/glamoroustoolkit"))
                     (lib (string-append dist "/lib"))
                     (bin (string-append out "/bin"))
                     (apps (string-append out "/share/applications"))
                     (bash-bin (string-append #$(this-package-input "bash")
                                              "/bin/bash"))
                     (coreutils-bin (string-append
                                     #$(this-package-input "coreutils")
                                     "/bin")))
                (mkdir-p dist)
                (copy-recursively "." dist)

                ;; Ensure bundled shared libraries are executable and expose
                ;; expected versioned SONAME symlinks (e.g. libssl.so needs
                ;; libcrypto.so.1.1).
                (for-each (lambda (file)
                            (chmod file #o755))
                          (find-files lib "\\.so$"))
                (for-each
                 (lambda (pair)
                   (let ((target (car pair))
                         (link (string-append lib "/" (cdr pair))))
                     (when (and (file-exists? (string-append lib "/" target))
                                (not (file-exists? link)))
                       (symlink target link))))
                 '(("libcrypto.so" . "libcrypto.so.1.1")
                   ("libssl.so" . "libssl.so.1.1")
                   ("libcairo.so" . "libcairo.so.2")
                   ("libfreetype.so" . "libfreetype.so.6")
                   ("libgit2.so" . "libgit2.so.1.6")
                   ("libSDL2.so" . "libSDL2-2.0.so.0")))

                (mkdir-p bin)
                (let ((make-launcher
                       (lambda (name)
                         (let ((launcher (string-append bin "/" name)))
                           (call-with-output-file launcher
                             (lambda (port)
                               (format port "#!~a
set -eu
export PATH=\"~a${PATH:+:$PATH}\"
DIST=\"~a\"
GT_ROOT=\"${XDG_DATA_HOME:-$HOME/.local/share}/glamoroustoolkit\"
GT_DIR=\"$GT_ROOT/~a\"
STAMP=\"$GT_DIR/.guix-installed\"

if [ ! -e \"$STAMP\" ]; then
  TMP=\"$GT_ROOT/.~a.tmp.$$\"
  rm -rf \"$TMP\"
  mkdir -p \"$TMP\"
  cp -f \"$DIST/GlamorousToolkit.image\" \"$TMP/GlamorousToolkit.image\"
  cp -f \"$DIST/GlamorousToolkit.changes\" \"$TMP/GlamorousToolkit.changes\"
  chmod u+w \"$TMP/GlamorousToolkit.image\" \"$TMP/GlamorousToolkit.changes\"
  rm -rf \"$GT_DIR\"
  mv \"$TMP\" \"$GT_DIR\"
  touch \"$STAMP\"
fi

for src in \"$DIST\"/*.sources \"$DIST/gt-extra\"; do
  if [ -e \"$src\" ]; then
    ln -sfn \"$src\" \"$GT_DIR/$(basename \"$src\")\"
  fi
done

if [ -z \"${SSL_CERT_FILE:-}\" ] || [ ! -e \"${SSL_CERT_FILE:-}\" ]; then
  export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
fi
if [ -z \"${SSL_CERT_DIR:-}\" ] || [ ! -d \"${SSL_CERT_DIR:-}\" ]; then
  export SSL_CERT_DIR=/etc/ssl/certs
fi
export LD_LIBRARY_PATH=\"$DIST/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"

cd \"$GT_DIR\"
exec \"$DIST/bin/~a\" \"$@\"
"
                                       bash-bin
                                       coreutils-bin
                                       dist
                                       #$gtoolkit-version
                                       #$gtoolkit-version
                                       name)))
                           (chmod launcher #o755)))))
                  (make-launcher "GlamorousToolkit")
                  (make-launcher "GlamorousToolkit-cli"))

                (mkdir-p apps)
                (make-desktop-entry-file
                 (string-append apps "/glamoroustoolkit.desktop")
                 #:name "Glamorous Toolkit"
                 #:type "Application"
                 #:exec "glamoroustoolkit"
                 #:categories '("Development" "IDE")
                 #:terminal #f
                 #:startup-notify #t
                 #:startup-w-m-class "GlamorousToolkit"
                 #:comment
                 '(("en" "Moldable development environment")
                   (#f "Moldable development environment")))))))))
    (supported-systems '("x86_64-linux"))
    (synopsis "Moldable development environment")
    (description
     "Glamorous Toolkit is the moldable development environment based on Pharo
Smalltalk.")
    (home-page "https://gtoolkit.com")
    (license license:expat)))

(define glamoroustoolkit-libs
  `(("alsa-lib" ,alsa-lib)
    ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")
    ("at-spi2-core" ,at-spi2-core)
    ("bash" ,bash)
    ("cairo" ,cairo)
    ("coreutils" ,coreutils)
    ("curl" ,curl)
    ("dbus" ,dbus)
    ("eudev" ,eudev)
    ("expat" ,expat)
    ("flatpak-xdg-utils" ,flatpak-xdg-utils)
    ("font-google-noto" ,font-google-noto)
    ("font-google-noto-emoji" ,font-google-noto-emoji)
    ("font-google-noto-sans-cjk" ,font-google-noto-sans-cjk)
    ("fontconfig" ,fontconfig)
    ("freetype" ,freetype)
    ("gcc:lib" ,gcc-14 "lib")
    ("gdk-pixbuf" ,gdk-pixbuf)
    ("git" ,git)
    ("glib" ,glib)
    ("gtk+" ,gtk+)
    ("harfbuzz" ,harfbuzz)
    ("libgit2" ,libgit2)
    ("libsoup" ,libsoup)
    ("libx11" ,libx11)
    ("libxcb" ,libxcb)
    ("libxcursor" ,libxcursor)
    ("libxext" ,libxext)
    ("libxi" ,libxi)
    ("libxinerama" ,libxinerama)
    ("libxkbcommon" ,libxkbcommon)
    ("libxrandr" ,libxrandr)
    ("libxrender" ,libxrender)
    ("libxt" ,libxt)
    ("libxxf86vm" ,libxxf86vm)
    ("llvm" ,llvm-for-mesa)
    ("mesa" ,mesa)
    ("mesa-utils" ,mesa-utils)
    ("nss" ,nss)
    ("nss-certs" ,nss-certs)
    ("openssl" ,openssl)
    ("pango" ,pango)
    ("pulseaudio" ,pulseaudio)
    ("util-linux" ,util-linux)
    ("vulkan-loader" ,vulkan-loader)
    ("wayland" ,wayland)
    ("webkitgtk-for-gtk3" ,webkitgtk-for-gtk3)
    ("zenity" ,zenity)
    ("zlib" ,zlib)))

(define (glamoroustoolkit-container-for driver)
  (let* ((libs
          (modify-inputs glamoroustoolkit-libs
            (replace "mesa" driver)))
         (union
          (fhs-union
           (append libs fhs-min-libs)
           #:name "glamoroustoolkit-fhs-union-64"
           #:system "x86_64-linux"))
         (ld-conf
          (packages->ld.so.conf
           (list union)))
         (ld-cache
          (ld.so.conf->ld.so.cache ld-conf)))
    (nonguix-container
      (name "glamoroustoolkit")
      (wrap-package glamoroustoolkit-client)
      (run "/bin/GlamorousToolkit")
      (preserved-env '("^GT_"
                       "^RUST_"
                       "^WEBKIT_"
                       "^WINIT_"))
      (ld.so.conf ld-conf)
      (ld.so.cache ld-cache)
      (union64 union)
      (link-files '("share/applications"))
      (description
       (package-description glamoroustoolkit-client)))))

(define-public glamoroustoolkit
  (nonguix-container->package
   (glamoroustoolkit-container-for mesa)))

(define-public glamoroustoolkit-nvda
  (package
    (inherit
     (nonguix-container->package
      (glamoroustoolkit-container-for nvda)))
    (name "glamoroustoolkit-nvda")))

glamoroustoolkit

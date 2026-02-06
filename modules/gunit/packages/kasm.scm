(define-module (gunit packages kasm)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages autotools)
  #:use-module (guix gexp)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages base)
  #:use-module (guix build-system node)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages pciutils))

(define perl-hash-merge-simple
  (package
    (name "perl-hash-merge-simple")
    (version "0.052")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/H/HA/HAARG/Hash-Merge-Simple-" version
             ".tar.gz"))
       (sha256
        (base32 "02ww88nbrpj6v6q4marvdrq7j7chpwak7jnhbc66xa8lb24j0zy2"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-clone))
    (home-page "https://metacpan.org/release/Hash-Merge-Simple")
    (synopsis "Recursively merge two or more hashes, simply")
    (description
     "Hash::Merge::Simple recursively merges two or more hashes and
returns the result.  It is designed to be a simpler alternative to Hash::Merge.")
    (license license:perl-license)))

;; Only packages the base and not the node portion
;; Will revisit this at a later point to package the
;; node portion
(define kasmvnc
  (let ((xorg-ver "1.20.14"))
    (package
      (name "kasmvnc")
      (version "1.3.2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kasmtech/KasmVNC.git")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1yph4f0gbiqgww0wdnv732kz3xy8qzydp5v03mf0chcy7sgldp2l"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f
         #:out-of-source? #f
         #:configure-flags (list "-DBUILD_TESTING=OFF" "-DENABLE_PAM=ON"
                                 "-DENABLE_GNUTLS=ON"
                                 (string-append "-DCMAKE_INSTALL_PREFIX="
                                                (assoc-ref %outputs "out")))
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'prepare-xserver
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((xorg-src (assoc-ref inputs "xorg-server-source")))
                          (mkdir-p "unix/xserver")
                          (invoke "tar"
                                  "-C"
                                  "unix/xserver"
                                  "-xf"
                                  xorg-src
                                  "--strip-components=1")

                          (with-directory-excursion "unix/xserver"
                            (invoke "patch" "-Np1" "-i" "../xserver120.patch")

                            (substitute* "hw/xfree86/common/xf86Opt.h"
                              (("Bool bool;")
                               "Bool kasm_bool;")) #t))))

                    (add-after 'prepare-xserver 'patch-cmake-and-perl
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "CMakeLists.txt"
                          (("-Wl,-Bstatic -lturbojpeg -Wl,-Bdynamic")
                           "-lturbojpeg"))
                        (substitute* "CMakeLists.txt"
                          (("add_subdirectory\\(tests\\)")
                           "# add_subdirectory(tests)"))
                        (substitute* "unix/vncserver"
                          (("/usr/bin/xkbcomp")
                           (string-append (assoc-ref inputs "xkbcomp")
                                          "/bin/xkbcomp"))
                          (("/usr/share/X11/xkb")
                           (string-append (assoc-ref inputs "xkeyboard-config")
                                          "/share/X11/xkb"))) #t))

                    (add-after 'build 'build-xserver
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let* ((sh (search-input-file inputs "/bin/sh"))
                               (xkb-path (assoc-ref inputs "xkeyboard-config"))
                               (xkb-bin (assoc-ref inputs "xkbcomp")))
                          (with-directory-excursion "unix/xserver"
                            (invoke "autoreconf" "-vfi")
                            (for-each patch-shebang
                                      (find-files "."
                                                  (lambda (f s)
                                                    (executable-file? f))))
                            (setenv "SHELL" sh)
                            (setenv "CONFIG_SHELL" sh)
                            (setenv "CFLAGS"
                             "-O2 -g -std=gnu99 -Wno-error=array-bounds")
                            (setenv "LIBS" "-lturbojpeg -ljpeg -lgnutls -lz")

                            (invoke sh
                                    "./configure"
                                    (string-append "CONFIG_SHELL=" sh)
                                    (string-append "--prefix="
                                                   (assoc-ref %outputs "out"))
                                    "--disable-xorg"
                                    "--disable-xnest"
                                    "--disable-xvfb"
                                    "--disable-dmx"
                                    "--enable-glx"
                                    "--with-sha1=libcrypto"
                                    (string-append "--with-xkb-path=" xkb-path
                                                   "/share/X11/xkb")
                                    (string-append "--with-xkb-bin-directory="
                                     xkb-bin "/bin"))

                            (invoke "make" "-j"
                                    (number->string (parallel-job-count)))) #t)))

                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin"))
                               (share (string-append out "/share/kasmvnc"))
                               (builder-startup (string-append out
                                                 "/builder/startup")))
                          (invoke "make" "install")
                          (if (file-exists? "unix/xserver/hw/vnc/Xvnc")
                              (install-file "unix/xserver/hw/vnc/Xvnc" bin)
                              (install-file "unix/xserver/hw/vnc/Xkasmvnc" bin))
                          (unless (file-exists? (string-append bin "/Xkasmvnc"))
                            (rename-file (string-append bin "/Xvnc")
                                         (string-append bin "/Xkasmvnc")))
                          (symlink "Xkasmvnc"
                                   (string-append bin "/Xvnc"))

                          (mkdir-p builder-startup)
                          (copy-recursively "builder/startup" builder-startup)
                          (mkdir-p share)
                          (install-file "unix/kasmvnc_defaults.yaml" share)
                          (if (file-exists? "unix/kasmvnc.yaml")
                              (install-file "unix/kasmvnc.yaml" share)
                              (with-directory-excursion share
                                (symlink "kasmvnc_defaults.yaml"
                                         "kasmvnc.yaml")))
                          (install-file "unix/kasmvnc_defaults.yaml" share)
                          (invoke "openssl"
                           "req"
                           "-x509"
                           "-nodes"
                           "-days"
                           "3650"
                           "-newkey"
                           "rsa:2048"
                           "-keyout"
                           (string-append share "/kasmvnc.key")
                           "-out"
                           (string-append share "/kasmvnc.pem")
                           "-subj"
                           "/C=US/ST=CA/L=somewhere/O=user/CN=localhost")
                          (let ((www-src (if (directory-exists? "www") "www"
                                             "unix/www")))
                            (copy-recursively www-src
                                              (string-append share "/www")))
                          #t)))

                    (add-after 'install 'wrap-vncserver
                      (lambda* (#:key outputs inputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (share (string-append out "/share/kasmvnc"))
                               (perl-lib (string-append out
                                          "/lib/perl5/site_perl"))
                               (perl-paths (map (lambda (input)
                                                  (string-append (cdr input)
                                                   "/lib/perl5/site_perl"))
                                                (filter (lambda (input)
                                                          (string-prefix?
                                                           "perl-"
                                                           (car input)))
                                                        inputs))))
                          (mkdir-p perl-lib)
                          (copy-recursively "unix/KasmVNC"
                                            (string-append perl-lib "/KasmVNC"))

                          (substitute* (find-files out "\\.(pm|pl|sh)$")
                            (("/usr/share/kasmvnc")
                             share)
                            (("/etc/kasmvnc")
                             share))

                          (substitute* (string-append out "/bin/vncserver")
                            (("/usr/share/kasmvnc")
                             share)
                            (("/etc/kasmvnc")
                             share))
                          (let ((defaults-file (string-append share
                                                "/kasmvnc_defaults.yaml")))
                            (substitute* defaults-file
                              (("/etc/ssl/certs/ssl-cert-snakeoil\\.pem")
                               (string-append share "/kasmvnc.pem"))
                              (("/etc/ssl/private/ssl-cert-snakeoil\\.key")
                               (string-append share "/kasmvnc.key"))
                              (("/usr/share/kasmvnc/www")
                               (string-append share "/www"))))
                          (let ((startup-dir (string-append out
                                              "/builder/startup")))
                            (when (directory-exists? startup-dir)
                              (for-each patch-shebang
                                        (find-files startup-dir ".*"))))

                          (wrap-program (string-append out "/bin/vncserver")
                            `("PERL5LIB" ":" prefix
                              (,(string-append out "/lib/perl5/site_perl") ,@perl-paths))
                            `("PATH" ":" prefix
                              (,(string-append out "/bin") ,(string-append (assoc-ref
                                                                            inputs
                                                                            "xkbcomp")
                                                             "/bin")
                               ,(string-append (assoc-ref inputs "openssl")
                                               "/bin"))))
                          #t)))

                    )))
      (native-inputs `(("pkg-config" ,pkg-config)
                       ("cmake" ,cmake-minimal)
                       ("autoconf" ,autoconf)
                       ("automake" ,automake)
                       ("libtool" ,libtool)
                       ("util-macros" ,util-macros)
                       ("font-util" ,font-util)
                       ("ninja" ,ninja)
                       ("nasm" ,nasm)
                       ("git" ,git-minimal)
                       ("perl" ,perl)
                       ("ruby" ,ruby)
                       ("xorg-server-source" ,(origin
                                                (method url-fetch)
                                                (uri (string-append
                                                      "https://www.x.org/pub/individual/xserver/xorg-server-"
                                                      xorg-ver ".tar.gz"))
                                                (sha256 (base32
                                                         "0rn079gmkdym229wklamfbdfw3mx8nb7ajm5fc7vzy0g534rkcal"))))))
      (inputs (list gnutls
                    libpng
                    libtiff
                    which
                    coreutils
                    grep
                    sed
                    giflib
                    ffmpeg
                    openssl
                    libxrandr
                    libxcursor
                    libjpeg-turbo
                    libva
                    libxtst
                    linux-pam
                    libxext
                    libxcrypt
                    pixman
                    xorgproto
                    libx11
                    libxshmfence
                    libxkbfile
                    xkbcomp
                    xkeyboard-config
                    libxfont2
                    libdrm
                    libpciaccess
                    libepoxy
                    perl-switch
                    perl-list-moreutils
                    perl-yaml
                    perl-hash-merge
                    perl-hash-merge-simple
                    perl-try-tiny
                    perl-yaml-tiny
                    perl-datetime
                    perl-datetime-timezone
                    mesa
                    xtrans
                    zlib
                    libwebp
                    libxkbcommon
                    libxau))
      (home-page "https://github.com/kasmtech/KasmVNC")
      (synopsis "High-performance VNC server with modern web features")
      (description
       "KasmVNC is a modern VNC server-client solution that renders directly 
to the browser via WebRTC or WebSockets.")
      (license license:gpl2+))))


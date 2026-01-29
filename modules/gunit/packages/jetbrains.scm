;; Credits to small-guix for the base to work from
(define-module (gunit packages jetbrains)
  #:use-module ((nonguix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages node)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define (make-jetbrains-product product
                                product-short
                                name
                                version
                                uri
                                hash
                                w-m-class
                                home-page
                                synopsis
                                description
                                supported-systems
                                license)
  (package
    (name name)
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri uri)
       (sha256
        (base32 hash))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("." #$(string-append "share/" name)))
      #:patchelf-plan
      #~'() ;; fsnotifier is static, skip patching
      #:phases
      #~(let ((lo-name (string-downcase #$product-short))
              (hi-name (string-upcase #$product-short))
              (bin (string-append #$output "/bin"))
              (lib (string-append #$output "/lib"))
              (plugins (string-append #$output "/plugins"))
              (info (string-append #$output "/product-info.json"))
              (libexec (string-append #$output "/libexec/"
                                      #$name))
              (icon-path (string-append #$output "/share/pixmaps"))
              (share/product (string-append #$output "/share/"
                                            #$name)))
          (modify-phases %standard-phases
            (add-after 'unpack 'remove-bundled-jvm
              (lambda _
                (delete-file-recursively "jbr")))
            (add-after 'install 'symlink-structure
              (lambda _
                (let* ((lib-target (string-append share/product "/lib"))
                       (bin-target (string-append share/product "/bin"))
                       (plugins-target (string-append share/product "/plugins"))
                       (info-target (string-append share/product
                                                   "/product-info.json")))
                  (symlink bin-target bin)
                  (symlink lib-target lib)
                  (when (file-exists? plugins-target)
                    (symlink plugins-target plugins))
                  (when (file-exists? info-target)
                    (symlink info-target info))
                  (mkdir-p libexec)
                  (when (file-exists? (string-append bin-target "/fsnotifier"))
                    (rename-file (string-append bin-target "/fsnotifier")
                                 (string-append libexec "/fsnotifier"))))))
            (add-after 'symlink-structure 'install-icon
              (lambda _
                (let* ((icon.png (string-append share/product "/bin/" lo-name
                                                ".png"))
                       (icon.svg (string-append share/product "/bin/" lo-name
                                                ".svg")))
                  (mkdir-p icon-path)
                  (when (file-exists? icon.png)
                    (symlink icon.png
                             (string-append icon-path "/"
                                            #$name ".png")))
                  (when (file-exists? icon.svg)
                    (symlink icon.svg
                             (string-append icon-path "/"
                                            #$name ".svg"))))))
            (add-after 'install-icon 'install-desktop-file
              (lambda _
                (make-desktop-entry-file (string-append #$output
                                          "/share/applications/"
                                          #$name ".desktop")
                                         #:name #$product
                                         #:comment #$synopsis
                                         #:exec #$name
                                         #:icon #$name
                                         #:type "Application"
                                         #:keywords `("development" ,lo-name
                                                      "ide")
                                         #:categories '("TextEditor"
                                                        "Development" "IDE")
                                         #:startup-notify #t
                                         #:startup-w-m-class #$w-m-class)))
            (add-after 'install-desktop-file 'install-wrapper
              (lambda _
                (wrap-program (string-append bin "/" lo-name ".sh")
                  `("_JAVA_AWT_WM_NONREPARENTING" =
                    ("1"))
                  `("JAVA_HOME" =
                    ,(list #$openjdk:jdk))
                  `("ANDROID_JAVA_HOME" =
                    ,(list #$openjdk:jdk))
                  `("JDK_HOME" =
                    ,(list #$openjdk:jdk))
                  `("JETBRAINSCLIENT_JDK" =
                    ,(list #$openjdk:jdk))
                  `(,(string-append hi-name "_JDK") =
                    ,(list #$openjdk:jdk))
                  `("IDE_PROPERTIES_PROPERTY" =
                    ("-Didea.platform.prefix=IntelliJIdea"))
                  `("FONTCONFIG_PATH" ":" prefix
                    (,(string-join (list (string-append #$(this-package-input
                                                           "fontconfig-minimal")
                                                        "/etc/fonts")) ":")))
                  `("PATH" ":" prefix
                    (,(string-joins (list (string-append #$(this-package-input
                                                            "coreutils")
                                                         "/bin")
                                          (string-append #$(this-package-input
                                                            "git") "/bin")
                                          (string-append #$(this-package-input
                                                            "grep") "/bin")
                                          ;; this-package-input seems incapable 
                                          ;; of handling multi-output packages
                                          (string-append #$openjdk:jdk "/bin")
                                          (string-append #$(this-package-input
                                                            "python") "/bin")
                                          (string-append #$(this-package-input
                                                            "node") "/bin") ; Node
                                          (string-append #$(this-package-input
                                                            "which") "/bin")
                                          bin
                                          libexec) ":")))
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-join (list (string-append #$(this-package-input
                                                           "e2fsprogs") "/lib")
                                         (string-append #$(this-package-input
                                                           "gcc:lib") "/lib")
                                         (string-append #$(this-package-input
                                                           "libnotify") "/lib")
                                         (string-append #$(this-package-input
                                                           "libsecret") "/lib")
                                         (string-append #$(this-package-input
                                                           "nss") "/lib/nss")
                                         lib) ":"))))))))))
    (inputs `(("coreutils" ,coreutils)
              ("e2fsprogs" ,e2fsprogs)
              ("fontconfig-minimal" ,fontconfig)
              ("gcc:lib" ,(make-libstdc++ gcc))
              ("git" ,git)
              ("grep" ,grep)
              ("libnotify" ,libnotify)
              ("libsecret" ,libsecret)
              ("openjdk:jdk" ,openjdk "jdk")
              ("nss" ,nss)
              ("python" ,python)
              ("node" ,node)
              ("which" ,which)))
    (home-page home-page)
    (synopsis synopsis)
    (description description)
    (supported-systems supported-systems)
    (license license)))

(define-public idea-ultimate-unwrapped
  (make-jetbrains-product "IntelliJ IDEA"
   "IDEA"
   "idea-ultimate-unwrapped"
   "2025.3.1.1-2"
   "https://download.jetbrains.com/idea/ideaIU-2025.3.1.1.tar.gz"
   "0ywrjq8yiw23bmbgk8r3sh4ggl70b22n4jxncsw3jgqzjqi4n1is"
   "jetbrains-idea"
   "https://www.jetbrains.com/idea/"
   "IntelliJ IDEA - the Leading Java and Kotlin IDE"
   "IDE for Java SE, Groovy & Scala development."
   '("x86_64-linux")
   (license:nonfree
    "https://www.jetbrains.com/legal/docs/toolbox/user_community/")))

(define idea-ultimate-libs
  `(("alsa-lib" ,alsa-lib)
    ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")
    ("at-spi2-core" ,at-spi2-core)
    ("bash" ,bash-minimal)
    ("zsh" ,zsh)
    ("cairo" ,cairo)
    ("coreutils" ,coreutils)
    ("dbus" ,dbus)
    ("dbus-glib" ,dbus-glib)
    ("diffutils" ,diffutils)
    ("e2fsprogs" ,e2fsprogs)
    ("eudev" ,eudev)
    ("file" ,file)
    ("find" ,findutils)
    ("flatpak-xdg-utils" ,flatpak-xdg-utils)
    ("font-dejavu" ,font-dejavu)
    ("font-liberation" ,font-liberation)
    ("fontconfig" ,fontconfig)
    ("freetype" ,freetype)
    ("gawk" ,gawk)
    ("gcc:lib" ,(make-libstdc++ gcc))
    ("git" ,git-minimal)
    ("glib" ,glib)
    ("grep" ,grep)
    ("gtk+" ,gtk+)
    ("gnupg" ,gnupg)
    ("libdrm" ,libdrm)
    ("libgccjit" ,libgccjit)
    ("libnotify" ,libnotify)
    ("libsecret" ,libsecret)
    ("libx11" ,libx11)
    ("libxcb" ,libxcb)
    ("libxcomposite" ,libxcomposite)
    ("libxcursor" ,libxcursor)
    ("libxdamage" ,libxdamage)
    ("libxext" ,libxext)
    ("libxfixes" ,libxfixes)
    ("libxi" ,libxi)
    ("libxkbcommon" ,libxkbcommon)
    ("libxkbfile" ,libxkbfile)
    ("libxrandr" ,libxrandr)
    ("libxrender" ,libxrender)
    ("libxshmfence" ,libxshmfence)
    ("libxtst" ,libxtst)
    ("llvm" ,llvm-12)
    ("mesa" ,mesa)
    ("nss-certs" ,nss-certs)
    ("openssh" ,openssh)
    ("pango" ,pango)
    ("pciutils" ,pciutils)
    ("procps" ,procps)
    ("pulseaudio" ,pulseaudio)
    ("qemu-minimal" ,qemu-minimal)
    ("sed" ,sed)
    ("tar" ,tar)
    ("util-linux" ,util-linux "lib")
    ("wayland" ,wayland)
    ("which" ,which)
    ("xz" ,xz)
    ("zlib" ,zlib)))

(define idea-ultimate-ld.so.conf
  (packages->ld.so.conf (list (fhs-union `(,@idea-ultimate-libs ,@fhs-min-libs)
                                         #:name "fhs-union-64"))))

(define idea-ultimate-ld.so.cache
  (ld.so.conf->ld.so.cache idea-ultimate-ld.so.conf))

(define-public idea-ultimate-container
  (nonguix-container (name "idea-ultimate")
                     (wrap-package idea-ultimate-unwrapped)
                     (run "/bin/idea.sh")
                     (ld.so.conf idea-ultimate-ld.so.conf)
                     (ld.so.cache idea-ultimate-ld.so.cache)
                     (shared '("/run/user/1000/gnupg"))
                     (preserved-env '("SSH_AUTH_SOCK"))
                     (union64 (fhs-union `(,@idea-ultimate-libs ,@fhs-min-libs)
                                         #:name "fhs-union-64"))
                     (link-files '("share/applications/idea-ultimate.desktop"))
                     (description (package-description idea-ultimate-unwrapped))))

(define-public idea-ultimate
  (nonguix-container->package idea-ultimate-container))

idea-ultimate
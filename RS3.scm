(define-module (runescape-launcher)
#:use-module ((guix licenses) #:prefix license:)
#:use-module (guix gexp)
#:use-module (guix packages)
#:use-module (guix download)
#:use-module (guix build-system copy)
#:use-module (gnu packages gtk)
#:use-module (gnu packages gcc)
#:use-module (guix build-system glib-or-gtk)
#:use-module (gnu packages glib)
#:use-module (gnu packages debian)
#:use-module (gnu packages tls)
#:use-module (gnu packages base)
#:use-module (gnu packages linux)
#:use-module (gnu packages gl)
#:use-module (gnu packages xorg)
#:use-module (gnu packages sdl)
#:use-module (gnu packages compression)
#:use-module (gnu packages elf)
#:use-module (gnu packages networking)
)
(define-public runescape-launcher
    (package
    (name "runescape-launcher")
    (version "2.2.11")
    (source       
    (origin
    (method url-fetch)
    (uri (string-append "https://content.runescape.com/downloads/ubuntu/pool/non-free/r/" name "/" name "_" version "_amd64.deb"))
  
        (sha256
        (base32 "00krw818s15bc38v9qv4ggk32704czjlw9c7vggg3vxxdhxvvscq"))))
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
             (invoke "setcap" "cap_net_raw+ep" "./usr/share/games/runescape-launcher/runescape")
             ;;(let ((glibc (assoc-ref %build-inputs "glibc"))
               ;;    (elf-file "./usr/share/games/runescape-launcher/runescape"))
               ;;(invoke "patchelf" "--set-interpreter" (string-append glibc "/lib/ld-linux-x86-64.so.2") elf-file))
               #t))
        ;; (delete 'install)
        )
        )
    )
      
    (native-inputs
        (list unzip dpkg patchelf iputils))
    (synopsis "Soup")
    (home-page "https://www.runescape.com/")
    (description "RuneScape Game Client (NXT)")
    (license license:agpl3))) ;; Update license as this is a placeholder

runescape-launcher

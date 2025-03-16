(define-module (gchannel packages code-server)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system copy)
  #:use-module (guix build copy-build-system)
)

(define-public floorp
    (package
    (name "floorp")
    (version "11.24.0")
    (source (origin
    (method url-fetch)
    (uri (string-append "https://github.com/Floorp-Projects/Floorp/releases/download/v" version "/floorp-" version ".linux-x86_64.tar.bz2"))
        (sha256
        (base32 "1abmanvim06nc3dhiwi9j63714dyfqqwlnj3984ld21ydz9kaffv"))))
    (build-system copy-build-system)

    (inputs
        (list 
        gcc-toolchain gtk+ alsa-lib libx11
    ))
        (arguments
        (list
            #:install-plan #~'(("." "lib/floorp"))
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'install 'create
                    (lambda _
                        (mkdir-p (string-append  #$output "/bin"))
                        ;;(mkdir-p (string-append  #$output "/share/icons/hicolor"))
                    )
                )
                (add-after 'create 'install-icons
                (lambda _
                    (let ((icons (string-append #$output "/share/icons/hicolor"))
                        (share (string-append #$output "/lib/floorp/browser/chrome/icons")))
                        (for-each (lambda (icon)
                                    (let* ((icon-name (basename icon))
                                        (icon-size (string-drop-right (string-drop icon-name 7) 4))
                                        (target (string-append icons "/" icon-size "x" icon-size "/apps/" "floorp" ".png")))
                                    (mkdir-p (dirname target))
                                    (rename-file icon target)))
                                (find-files share "default.*\\.png")))
                )
                )
                (add-after 'install-icons 'install-share
                    (lambda _
                        (display "cat")
                    )
                )
                (add-after 'create 'wrap
                (lambda _ 
                (system* "pwd")
                (system* "ls" "-a")
                (wrap-program (string-append #$output "/lib/floorp/floorp")
                `("LD_LIBRARY_PATH" ":" prefix (
                    ,(string-append #$(this-package-input "gcc-toolchain") "/lib")
                    ,(string-append #$(this-package-input "gtk+") "/lib")
                    ,(string-append #$(this-package-input "alsa-lib") "/lib")
                    ,(string-append #$(this-package-input "libx11") "/lib")
                ))
                `("XDG_DATA_DIRS" ":" prefix (
                    ,(string-append #$(this-package-input "gtk+") "/share")
                ))
            )
            (invoke "mv" (string-append #$output "/lib/floorp/floorp") (string-append #$output "/bin/floorp"))
            ))
                (delete 'validate-runpath)
            )))
    (native-inputs
        (list git))
    (synopsis "Soup")
    (home-page "https://coder.com/")
    (description "Free open source code server")
    (license license:agpl3)))

floorp
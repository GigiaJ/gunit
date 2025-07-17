(define-module (gchannel packages code-server)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages elf)
  #:use-module (selected-guix-works packages fonts)
  #:use-module (guix build-system copy)
  #:use-module (guix build copy-build-system))


(define-public code-server
    (package
    (name "code-server")
    (version "4.101.1")
    (source (origin
    (method url-fetch)
    (uri (string-append "https://github.com/coder/code-server/releases/download/v" version "/code-server-" version "-linux-amd64.tar.gz"))
  
        (sha256
        (base32 "00vmv64gzj14yks3lizh722nnhr2wy9d6js0bs4m2rnaawgqh551"))))
    (build-system copy-build-system)

    (inputs
        (list 
        gcc-toolchain
        woff-tools
        font-nerd-fonts-jetbrains-mono
        glibc
        ))
        (arguments
        (list
        #:tests? #f             ; no check target
            #:phases
            #~(modify-phases %standard-phases
                            (add-after 'install 'patch-interpreter
                  (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (interpreter (string-append (assoc-ref inputs "glibc")
                                                       "/lib/ld-linux-x86-64.so.2"))
                           (binary (string-append out "/lib/node")))
                      (invoke "patchelf" "--set-interpreter" interpreter binary))))
                (add-after 'patch-interpreter 'wrap 
                (lambda _
                (wrap-program (string-append (assoc-ref %outputs "out") "/lib/node")
                `("LD_LIBRARY_PATH" ":" prefix (
                    ,(string-append #$(this-package-input "gcc-toolchain") "/lib")
                )))))
                (add-after 'wrap 'enable-font-magic
                    (lambda _
                    (let ((font-package "font-nerd-fonts-jetbrains-mono")
                        (font-path "/share/fonts/truetype/")
                        (font-prefix "JetBrainsMonoNerdFontMono-")  
                )
                    (use-modules (ice-9 ftw)
                    (srfi srfi-1)) ; For list processing
       (define (collect-files-with-prefix directory prefix)
        (filter (lambda (str) (string-prefix? prefix str)) (cddr (scandir directory))))
        (map (lambda (entry)
        (let* ((source (car entry)) (file (cdr entry)))
            (symlink (string-append  (assoc-ref %build-inputs source) font-path file)
                (string-append (assoc-ref %outputs "out") "/src/browser/pages/" (basename file)))
            (system* "sfnt2woff" (string-append (assoc-ref %outputs "out") "/src/browser/pages/" (basename file))) 
            ))
        (append
        (map (lambda (file) (cons font-package file))
        (collect-files-with-prefix (string-append (assoc-ref %build-inputs font-package) font-path) font-prefix)                  
        )))
            (let ((font-files (string-join
            (map 
                (lambda (x) 
                    (string-append "url('_static/src/browser/pages/" (substring x 0 (- (string-length x) 4)) ".woff') format('woff')" ))
                (collect-files-with-prefix (string-append (assoc-ref %build-inputs font-package) font-path) font-prefix))
                ","
            )))
            (system* "sed" "-i" (string-append "s|</head>|<style> @font-face {font-family: 'Personal';font-style: normal;src:" font-files ";}\\</style></head>|g") (string-append #$output "/lib/vscode/out/vs/code/browser/workbench/workbench.html"))
        )
))
                )
                (delete 'validate-runpath)
            )))
    (native-inputs
        (list git curl))
    (synopsis "Soup")
    (home-page "https://coder.com/")
    (description "Free open source code server")
    (license license:agpl3)))

    code-server


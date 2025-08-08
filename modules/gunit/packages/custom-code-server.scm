(define-module (gunit packages custom-code-server)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages)
  #:use-module (guix build-system copy)
  #:use-module (guix build copy-build-system)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (gunit packages code-server)
  #:use-module (selected-guix-works packages fonts)
  #:use-module (gnu packages fontutils))

(define-public code-server-with-fonts
  (package/inherit code-server
    (name "code-server-with-fonts")
    
    (inputs
(append (package-inputs code-server)
            ;; (list font-nerd-fonts-jetbrains-mono)
            ))
    
    (native-inputs
     (append (package-native-inputs code-server)
            ;; (list woff-tools)
            ))
    
    (arguments
     (substitute-keyword-arguments (package-arguments code-server)
       ((#:phases phases)
        #~(modify-phases #$phases
(add-after 'wrap 'enable-font-magic
                    (lambda _
                    (let ((font-package "font-nerd-fonts-jetbrains-mono")
                        (font-path "/share/fonts/truetype/")
                        (font-prefix "JetBrainsMonoNerdFontMono-"))
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
        ))))
              )))))
)

code-server-with-fonts
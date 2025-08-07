(define-module (gunit packages custom-code-server)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu) ; For modify-phases
  #:use-module (ice-9 ftw)             ; For scandir
  #:use-module (srfi srfi-1)           ; For list processing
  
  #:use-module (gunit packages code-server)
  #:use-module (selected-guix-works packages fonts)
  #:use-module (gnu packages fontutils))

(define-public code-server-with-fonts
  (package/inherit code-server
    (name "code-server-with-fonts")
    
    ;; Add the font package to the regular inputs
    (inputs (append (package-inputs code-server)
                    (list font-nerd-fonts-jetbrains-mono)))
    
    ;; Add the woff tools to the native inputs (for building)
    (native-inputs (append (package-native-inputs code-server)
                           (list woff-tools)))
    
    (arguments
      #~(modify-phases #$@(package-arguments code-server)
          (add-after 'wrap 'enable-font-magic
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((font-pkg-name "font-nerd-fonts-jetbrains-mono")
                     (font-package (assoc-ref inputs font-pkg-name))
                     (font-path (string-append font-package "/share/fonts/truetype/"))
                     (font-prefix "JetBrainsMonoNerdFontMono-")
                     (out (assoc-ref outputs "out"))
                     (pages-dir (string-append out "/lib/vscode/out/vs/code/browser/pages/"))
                     (workbench-html (string-append out "/lib/vscode/out/vs/code/browser/workbench/workbench.html")))

                ;; Helper to find all the font files in the font package
                (define (collect-font-files)
                  (map (lambda (file)
                         (cons font-pkg-name file))
                       (filter (lambda (str) (string-prefix? font-prefix str))
                               (scandir-files font-path))))
                
                (let ((font-files-to-process (collect-font-files)))
                  ;; Create symlinks and convert fonts to woff format
                  (for-each
                    (lambda (font-entry)
                      (let* ((source-file-name (cdr font-entry))
                             (source-path (string-append font-path source-file-name))
                             (dest-path (string-append pages-dir source-file-name)))
                        (symlink source-path dest-path)
                        (invoke "sfnt2woff" dest-path)))
                    font-files-to-process)
                  
                  ;; Generate the CSS @font-face rule
                  (let ((font-face-urls
                         (string-join
                          (map (lambda (font-entry)
                                 (let ((woff-file (string-append (basename (cdr font-entry) ".ttf") ".woff")))
                                   (string-append "url('_static/src/browser/pages/" woff-file "') format('woff')")))
                               font-files-to-process)
                          ", ")))
                    
                    ;; Inject the CSS into the workbench HTML file
                    (substitute* workbench-html
                      (("</head>")
                       (string-append "<style> @font-face {font-family: 'Personal'; font-style: normal; src: "
                                      font-face-urls
                                      ";} </style></head>"))))))))))))
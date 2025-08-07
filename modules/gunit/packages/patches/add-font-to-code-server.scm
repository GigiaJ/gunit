;;        woff-tools
;;        font-nerd-fonts-jetbrains-mono

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
(define-module (gunit packages code-server)
  #:use-module ((guix licenses)
                #:prefix license:)
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
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build copy-build-system)
  #:export (make-code-server-with-font))

(define-public code-server
  (package
    (name "code-server")
    (version "4.102.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/coder/code-server/releases/download/v"
             version "/code-server-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32 "1s33d8821hcpkv10643dc58m177nrsy4238fyx2qbkh3smx98p25"))))
    (build-system copy-build-system)
    (inputs (list gcc-toolchain glibc))
    (arguments
     (list
      #:tests? #f ;; test
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
              (wrap-program (string-append #$output "/lib/node")
                (list "LD_LIBRARY_PATH" ":"
                      'prefix
                      (list (string-append #$(this-package-input
                                              "gcc-toolchain") "/lib"))))))
          (delete 'validate-runpath))))
    (native-inputs (list git curl patchelf))
    (synopsis "Code server used for accessing VS Code in the browser.")
    (home-page "https://coder.com/")
    (description
     "Free open source code server. Use a VS Code in any browser wherever you are.")
    (license license:expat)))

;; Example usage:
;; (make-code-server-with-font code-server font-nerd-fonts-jetbrains-mono
;;                  "JetBrainsMonoNerdFontMono-")
;;
(define-public (make-code-server-with-font base-package font-package
                                           font-prefix)
  "Create a variant of BASE-PACKAGE with FONT-PACKAGE injected into src/browser/pages."
  (package
    (inherit base-package)
    (name (string-append (package-name base-package) "-with-"
                         (package-name font-package)))

    (inputs (modify-inputs (package-inputs base-package)
              (prepend font-package)))

    (native-inputs (modify-inputs (package-native-inputs base-package)
                     (prepend woff-tools)))

    (arguments
     (substitute-keyword-arguments (package-arguments base-package)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'wrap 'enable-font-magic
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (use-modules (ice-9 ftw)
                             (srfi srfi-1)
                             (ice-9 string-fun)
                             (guix build utils))

                (let* ((out (assoc-ref outputs "out"))
                       (font-dir (assoc-ref inputs
                                            #$(package-name font-package)))
                       (font-path "/share/fonts/truetype/")
                       (full-font-path (string-append font-dir font-path))
                       (prefix #$font-prefix)

                       (pages-dir (string-append out "/src/browser/pages/"))

                       (workbench-html (string-append out
                                        "/lib/vscode/out/vs/code/browser/workbench/workbench.html")))
                  
                  (define (collect-files directory)
                    (filter (lambda (file)
                              (string-prefix? prefix file))
                            (scandir directory)))

                  (define (get-font-metadata filename)
                    (let ((lower (string-downcase filename)))
                      (cons (cond
                              ((string-contains lower "thin")
                               "100")
                              ((string-contains lower "extralight")
                               "200")
                              ((string-contains lower "light")
                               "300")
                              ((string-contains lower "medium")
                               "500")
                              ((string-contains lower "semibold")
                               "600")
                              ((string-contains lower "extrabold")
                               "800")
                              ((string-contains lower "bold")
                               "700")
                              ((string-contains lower "black")
                               "900")
                              (else "400"))
                            (if (string-contains lower "italic") "italic"
                                "normal"))))

                  (let* ((files (collect-files full-font-path))
                         (css-rules (map (lambda (file)
                                           (let* ((src (string-append
                                                        full-font-path file))
                                                  (dest (string-append
                                                         pages-dir file))
                                                  (woff-name (string-append (substring
                                                                             file
                                                                             0
                                                                             (-
                                                                              (string-length
                                                                               file)
                                                                              4))
                                                              ".woff"))
                                                  (meta (get-font-metadata
                                                         file))
                                                  (weight (car meta))
                                                  (style (cdr meta)))

                                             (symlink src dest)
                                             (invoke "sfnt2woff" dest)
                                             (delete-file dest)

                                             (format #f
                                              "@font-face { font-family: 'Personal'; font-weight: ~a; font-style: ~a; src: url('{{BASE}}/_static/src/browser/pages/~a') format('woff'); }"
                                              weight style woff-name))) files)))
                    (substitute* workbench-html
                      (("</head>")
                       (string-append "<style>"
                                      (string-join css-rules "\n")
                                      "</style></head>")))))))))))))



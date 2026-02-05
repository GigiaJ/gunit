(define-module (guix scripts toys)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix i18n)
  #:export (guix-toys))

(define *base-url* "https://toys.whereis.social")

(define (fetch-body url)
  "Fetches URL and returns body as a UTF-8 string, safely handling types."
  (let ((headers '((User-Agent . "Guix-Toy-Search/1.0"))))
    (let-values (((response body) (http-get (string->uri url) #:headers headers)))
      (if (= (response-code response) 200)
          (cond
           ((bytevector? body) (utf8->string body))
           ((string? body)     body)
           (else (error "Unknown body type" body)))
          (error "HTTP Error" (response-code response))))))

(define (clean-entities s)
  "Decodes HTML entities to make the snippet valid Scheme."
  (let* ((s (regexp-substitute/global #f "&quot;" s 'pre "\"" 'post))
         (s (regexp-substitute/global #f "&apos;" s 'pre "'" 'post))
         (s (regexp-substitute/global #f "&lt;"   s 'pre "<" 'post))
         (s (regexp-substitute/global #f "&gt;"   s 'pre ">" 'post))
         (s (regexp-substitute/global #f "&amp;"  s 'pre "&" 'post)))
    s))

(define (extract-substring str start-marker end-marker start-index)
  "Finds text between START-MARKER and END-MARKER starting from START-INDEX."
  (let ((s-pos (string-contains str start-marker start-index)))
    (if s-pos
        (let* ((content-start (+ s-pos (string-length start-marker)))
               (e-pos (string-contains str end-marker content-start)))
          (if e-pos
              (values (substring str content-start e-pos) e-pos)
              (values #f #f)))
        (values #f #f))))

(define (search-channels query page)
  (format #t "📡 Channels matching '\x1b[1m~a\x1b[0m' (Page ~a)\n\n" query page)
  (let* ((url (format #f "~a/channels?search=~a&page=~a" *base-url* (uri-encode query) page))
         (body (fetch-body url))
         (found-any #f))
    
    (let loop ((index 0))
      (let ((item-start (string-contains body "<div class=\"item\">" index)))
        (if item-start
            (let-values (((name next-idx) (extract-substring body "<strong>" "</strong>" item-start)))
              (if name
                  (let-values (((snippet end-idx) (extract-substring body "<pre class=\"pre\">" "</pre>" next-idx)))
                    (if snippet
                        (begin
                          (set! found-any #t)
                          (format #t "─── \x1b[1;34m~a\x1b[0m ───\n" name)
                          (display (clean-entities snippet))
                          (newline) (newline)
                          (loop end-idx))
                        (loop next-idx)))
                  (loop (+ item-start 18))))
            (unless found-any
              (format #t "No channels found.\n")))))))

(define (print-item name version channel desc)
  (format #t "  \x1b[1;32m~a\x1b[0m ~a\n" name (if version (string-append "@ " version) ""))
  (format #t "    Channel: ~a\n" channel)
  (format #t "    Desc:    ~a\n\n" (if (string? desc) desc "No description")))

(define (search-generic type-label endpoint query page)
  (format #t "🔍 ~a matching '\x1b[1m~a\x1b[0m' (Page ~a)\n\n" type-label query page)
  (let* ((url (format #f "~a/api/~a?search=~a&page=~a&limit=20" 
                      *base-url* endpoint (uri-encode query) page))
         (body (fetch-body url))
         (json (json-string->scm body))
         (results (cond ((vector? json) (vector->list json))
                        ((list? json) (or (assoc-ref json "data") '()))
                        (else '()))))
    (if (null? results)
        (format #t "No ~a found.\n" (string-downcase type-label))
        (for-each 
         (lambda (item)
           (let ((desc (or (assoc-ref item "synopsis") (assoc-ref item "description"))))
             (print-item (assoc-ref item "name") 
                         (assoc-ref item "version") 
                         (assoc-ref item "channel") desc)))
         results))))

;;; ----------------------------------------------------------------------------
;;; Help Display
;;; ----------------------------------------------------------------------------

(define (show-help)
  (display (G_ "Usage: guix toys [OPTIONS]
Search the toys.whereis.social GNU Guix webring for packages, services, and channels.

Options:
  -q, --query=QUERY      The search term (required)
  -t, --type=TYPE        Search type (default: package)
  -p, --page=NUM         Page number (default: 1)
  -h, --help             Display this help and exit

Types:
  package   Search for packages (shows synopsis)
  service   Search for system services (shows description)
  channel   Search for channels (outputs subscription configuration)
  symbol    Search for public symbols

Examples:
  guix toys -q rust
  guix toys -t channel -q rde
  guix toys -t service -q desktop
  guix toys -q \"json\" -p 2

Report bugs to the script maintainer.\n")))

(define-command (guix-toys . args)
  (category extension)
  (synopsis "search packages, services, and channels on toys.whereis.social")

  (define options-spec
    '((type   (single-char #\t) (value #t))
      (query  (single-char #\q) (value #t))
      (page   (single-char #\p) (value #t))
      (help   (single-char #\h) (value #f))))

  (let* ((options (catch 'misc-error 
                    (lambda () (getopt-long (cons "guix toys" args) options-spec))
                    (lambda _ (leave (G_ "invalid arguments: try 'guix toys --help'~%")))))
         (help    (option-ref options 'help #f))
         (type    (option-ref options 'type "package"))
         (query   (option-ref options 'query #f))
         (page    (option-ref options 'page "1")))

    (cond
     (help (show-help))
     ((not query)
      (leave (G_ "missing query: use -q \"term\" or try --help~%")))
     (else
      (match type
        ("package" (search-generic "Packages" "packages" query page))
        ("service" (search-generic "Services" "services" query page))
        ("channel" (search-channels query page))
        ("symbol"  (search-generic "Symbols"  "symbols"  query page))
        (_ (leave (G_ "unknown type: ~a~%") type)))))))

;;; Allow direct execution for testing
(let ((args (command-line)))
  (when (string-suffix? "toys.scm" (car args))
    (apply guix-toys (cdr args))))
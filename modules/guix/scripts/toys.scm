(define-module (guix scripts toys)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:export (guix-toys))

(define *base-url*
  "https://toys.whereis.social")

(define (fetch-body url)
  (let ((headers '((User-Agent . "Guix-Toy-Search/1.0"))))
    (let-values (((response body)
                  (http-get (string->uri url)
                            #:headers headers)))
                (if (= (response-code response) 200)
                    (utf8->string body)
                    (error "HTTP Error"
                           (response-code response))))))

(define (print-item name version channel desc)
  (format #t "  \x1b[1;32m~a\x1b[0m ~a\n" name
          (if version
              (string-append "@ " version) ""))
  (format #t "    Channel: ~a\n" channel)
  (format #t "    Desc:    ~a\n\n"
          (if (string? desc) desc "No description")))

(define (search-generic type-label endpoint query)
  (format #t "🔍 Searching ~a for '~a'...\n" type-label query)
  (let* ((url (format #f "~a/api/~a?search=~a&page=1&limit=20" *base-url*
                      endpoint
                      (uri-encode query)))
         (body (fetch-body url))
         (json (json-string->scm body))
         (results (cond
                    ((vector? json)
                     (vector->list json))
                    ((list? json)
                     (or (assoc-ref json "data")
                         '()))
                    (else '()))))
    (if (null? results)
        (format #t "No ~a found.\n"
                (string-downcase type-label))
        (for-each (lambda (item)
                    (let ((desc (or (assoc-ref item "synopsis")
                                    (assoc-ref item "description"))))
                      (print-item (assoc-ref item "name")
                                  (assoc-ref item "version")
                                  (assoc-ref item "channel") desc))) results))))

(define (search-channels query)
  (format #t "📡 Searching Channels for '~a'...\n" query)
  (let* ((url (format #f "~a/api/channels?search=~a" *base-url*
                      (uri-encode query)))
         (json (json-string->scm (fetch-body url)))
         (results (if (vector? json)
                      (vector->list json)
                      '())))
    (if (null? results)
        (format #t "No channels found.\n")
        (for-each (lambda (c)
                    (format #t "  \x1b[1;34m~a\x1b[0m (~a packages)\n"
                            (assoc-ref c "name")
                            (or (assoc-ref c "package_count") 0))
                    (format #t "    URL: ~a\n\n"
                            (assoc-ref c "url"))) results))))

(define-command (guix-toys . args)
                (category extension)
                (synopsis
                 "search packages, services, and channels on toys.whereis.social")

                (define options-spec
                  '((type (single-char #\t)
                          (value #t))
                    (query (single-char #\q)
                           (value #t))
                    (help (single-char #\h)
                          (value #f))))

                (let* ((options (catch 'misc-error
                                       (lambda ()
                                         (getopt-long (cons "guix toys" args)
                                                      options-spec))
                                       (lambda _
                                         (leave (G_
                                                 "invalid arguments: try 'guix toys --help'~%")))))
                       (help (option-ref options
                                         'help #f))
                       (type (option-ref options
                                         'type "package"))
                       (query (option-ref options
                                          'query #f)))
                  
                  (cond
                    (help (display
                           "Usage: guix toys -q <query> [-t package|channel|symbol|service]
"))
                    ((not query)
                     (leave (G_ "missing query: use -q \"term\"~%")))
                    (else (match type
                            ("package" (search-generic "Packages" "packages"
                                                       query))
                            ("service" (search-generic "Services" "services"
                                                       query))
                            ("channel" (search-channels query))
                            ("symbol" (search-generic "Symbols" "symbols"
                                                      query))
                            (_ (leave (G_ "unknown type: ~a~%") type)))))))
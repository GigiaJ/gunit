(define (ollama-home-service config)
  (list (shepherd-service (documentation "Run the Ollama LLM backend.")
                          (provision '(ollama))
                          (modules '((shepherd support)))

                          (start #~(make-forkexec-constructor (list #$(file-append
                                                                       (specification->package
                                                                        "ollama")
                                                                       "/bin/ollama")
                                                                    "serve")
                                                              #:log-file (string-append
                                                                          %user-log-dir
                                                                          "/ollama.log")))

                          (stop #~(make-kill-destructor))
                          (respawn? #t))))

(define home-ollama-service-type
  (service-type (name 'ollama)
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   ollama-home-service)))
                (default-value #t)
                (description "A user service to run the Ollama LLM server.")))
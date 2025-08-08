(define-module (services code-server)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu services)
  #:use-module (gnu services home)
  #:use-module (shepherd support)
  #:export (home-code-server-service-type))

(define (code-server-service config)
  ;; This procedure defines a Shepherd service to run and manage code-server,
  ;; which provides a web-based instance of VS Code. The service ensures
  ;; code-server starts automatically and restarts if it crashes.
  (list
    (shepherd-service
      (documentation "Run the code-server backend.")
      (provision '(code-server)) ;; Ensure the code-server package is available.
      (modules '((shepherd support)))

      ;; Start the service by directly executing the binary from its package path.
      ;; All output is redirected to a dedicated log file in the user's home directory.
      (start #~(make-forkexec-constructor
                (list #$(file-append (specification->package "code-server") "/bin/code-server"))
                #:log-file (string-append %user-log-dir "/code-server.log")))

      ;; To stop the service, simply send a kill signal to the process.
      (stop #~(make-kill-destructor))

      ;; Automatically respawn the service if it terminates unexpectedly.
      (respawn? #t))))

(define code-server-service-type
  (service-type
   (name 'code-server)
   ;; This links our custom 'code-server-service' implementation to the main user Shepherd service.
   (extensions (list (service-extension home-shepherd-service-type code-server-service)))
   ;; The #t default value means the service will be active unless explicitly disabled.
   (default-value #t)
   (description "A user service to run a VS Code instance in the browser.")))
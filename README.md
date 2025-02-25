Clean up your package builds with garbage collection occasionally, otherwise you might see old builds and weird build issues.
(add-before 'configure 'set-env-vars         
    (lambda _
        (setenv "BOLT_CEF_DLLWRAPPER" (string-append (assoc-ref %build-inputs "chromium-embedded-framework") "/lib/libcef_dll_wrapper.a"))
        ;;(putenv "CATS=NOTDOGS")
        (display (environ))
        (display "Test")
        #t))
        ;; (replace 'configure (lambda _ (display (environ)) #t))
        )


Back-up info
https://guix.gnu.org/manual/en/html_node/Replicating-Guix.html

port Conky over

Discord needs 
--no-sandbox --no-gpu --in-process-gpu ./EXECUTABLE
https://peter.sh/experiments/chromium-command-line-switches/
Probably unstable and we should address it

https://guix.gnu.org/manual/en/html_node/package-Reference.html#package-Reference




Update licenses for packages



Bolt needs to be wrapped so it has access to LD_LIBRARY_PATH
(wrap-program)
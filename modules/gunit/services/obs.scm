(define-module (gunit services obs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages)
  #:export (home-obs-virtual-mic-service-type))





(define (obs-virtual-mic-service config)
  ;; This procedure defines a Shepherd service to set up a virtual audio bridge.
  ;; It creates a Null Sink for OBS to output to and a Virtual Source for 
  ;; applications like Discord or Edge to use as a microphone.
  (list (shepherd-service (documentation
                           "Run a PipeWire loopback for OBS Virtual Mic.")
                          (provision '(obs-virtual-mic))
                          (requirement '(dbus))

                          ;; We use a lambda here to handle the process spawning and the 
                          ;; immediate volume adjustment required for PipeWire nodes.
                          (start #~(lambda _
                                     (let ((pid (fork+exec-command (list #$(file-append
                                                                            pipewire
                                                                            "/bin/pw-loopback")
                                                                    "--name=OBS-Bridge"
                                                                    ;; Define the Sink (where OBS sends audio)
                                                                    "--capture-props=node.name=OBS-Audio-Sink 
                                                       node.description='OBS Audio Destination' 
                                                       media.class=Audio/Sink"
                                                                    ;; Define the Source (what Discord sees)
                                                                    "--playback-props=node.name=OBS-Virtual-Mic 
                                                        node.description='OBS Virtual Mic' 
                                                        media.class=Audio/Source 
                                                        audio.position=[FL FR]"))))
                                       ;; Give the graph a moment to register the new nodes.
                                       (sleep 1)
                                       ;; Force the volumes to 100% (1.0) so the mic isn't silent.
                                       (system* #$(file-append pipewire
                                                               "/bin/wpctl")
                                                "set-volume" "OBS-Audio-Sink"
                                                "1.0")
                                       (system* #$(file-append pipewire
                                                               "/bin/wpctl")
                                                "set-volume" "OBS-Virtual-Mic"
                                                "1.0")
                                       pid)))

                          ;; Stop the service by killing the pw-loopback process.
                          (stop #~(make-kill-destructor))

                          ;; Ensure the virtual mic restarts if the PipeWire daemon resets.
                          (respawn? #t))))

(define home-obs-virtual-mic-service-type
  (service-type (name 'obs-virtual-mic)
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   obs-virtual-mic-service)))
                ;; The #t default value means the service will be active unless explicitly disabled.
                (default-value #t)
                (description "A user service to set up a virtual mic for OBS.")))


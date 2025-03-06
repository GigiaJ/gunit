(define-module (gchannel packages pia-vpn)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages image)
  #:use-module (guix build-system copy)
  #:use-module (ice-9 string-fun)
)

(define build-number "08339")

(define-public pia-vpn
    (package
    (name "pia-vpn")
    (version "3.6.1")
    (source (origin
    (method url-fetch)
    (uri (string-append "https://installers.privateinternetaccess.com/download/pia-linux-" version "-" "08339" ".run"))
        (sha256
        (base32 "04x65h8zmb8k51iid66gigffdbdgh4iv8bzah9s5xg3zgcycc53g"))))
    (build-system copy-build-system)

    (inputs
        (list 
        mesa libx11 gtk sdl2 gcc-toolchain glib libxkbcommon libnl libnsl iptables))
    (arguments
    (list
        #:phases
        #~(modify-phases %standard-phases
        (add-after 'unpack 'unbundled (lambda _ (system* "sh" (string-append "./pia-linux-" #$version "-" "08339" ".run") "--noexec" "--nox11" "--keep")))
        (add-after 'unbundled 'patch-install-script (lambda _ (system* "sed" "-i" "s/\\/bin\\/cp/cp/g" "./pia-linux-3.6.1-08339/install.sh")))
        ;; This doesn't work, but is the general idea. Once this is done we can organize the files in the directory and then build out the service for the daemon
        ;; (add-after 'patch-install-script 'patch-qt-conf (lambda _ (system* "sed" "-i" "s/\\/opt\\/piavpn/" (string-replace (assoc-ref %outputs "out")  "/" "\\/") "/g" "./pia-linux-3.6.1-08339/piafiles/bin/qt.conf")))
        (replace 'install (lambda _ (system* "sh" "install.sh" "--skip-service")))
    )))
    
    (native-inputs
        (list git pkg-config))
    (synopsis "Soup")
    (home-page "https://www.privateinternetaccess.com/")
    (description "Private Internet Access VPN")
    (license license:agpl3)))

pia-vpn
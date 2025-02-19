;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages edge)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix licenses)
  #:use-module (ice-9 string-fun))

(define-public (make-microsoft-edge repo version hash)
  (let* ((name (string-append "microsoft-edge-" repo))
         (appname (if (string=? repo "stable")
                      "edge"
                      (string-replace-substring name "microsoft-" ""))))
    (package
     (name name)
     (version version)
     (source (origin
               (method url-fetch)
               (uri
                (string-append
                 "https://packages.microsoft.com/repos/edge/pool/main/m/"
                 name "/" name "_" version "-1_amd64.deb"))
               (sha256
                (base32 hash))))
     (build-system chromium-binary-build-system)
     (arguments
      (list
        ;; almost 300MB, faster to download and build from Google servers
        #:substitutable? #f
        #:wrapper-plan
         #~(let ((path (string-append "opt/microsoft/" "ms" #$appname "/")))
             (map (lambda (file)
                    (string-append path file))
                  '("msedge"
                    "msedge-sandbox"
                    "msedge_crashpad_handler"
                    "libEGL.so"
                    "libGLESv2.so"
                    ;;"libaugloop_client.so"
                    "liblearning_tools.so"
                    ;;"libmicrosoft-apis.so"
                    "libmip_core.so"
                    "libmip_protection_sdk.so"
                    "liboneauth.so"
                    "liboneds.so"
                    "libqt5_shim.so"
                    "libqt6_shim.so"
                    ;; "libsmartscreenn.so"
                    "libtelclient.so"
                    "libvk_swiftshader.so"
                    "libvulkan.so.1"
                    "libwns_push_client.so"
                    "WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so")))
        #:install-plan
         #~'(("opt/" "/share")
             ("usr/share/" "/share"))
        #:phases
         #~(modify-phases %standard-phases
             (add-before 'install 'patch-assets
               ;; Many thanks to
               ;; https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/networking/browsers/microsoft-edge/default.nix
               (lambda _
                 (let* ((bin (string-append #$output "/bin"))
                        (share (string-append #$output "/share"))
                        (opt "./opt")
                        (usr/share "./usr/share")
                        (old-exe (string-append "/opt/microsoft/" #$appname "/microsoft-" #$appname))
                        (exe (string-append bin "/microsoft-" #$appname)))
                   ;; This allows us to override CHROME_WRAPPER later.
                   (substitute* (string-append opt "/microsoft/" #$appname "/microsoft-" #$appname)
                     (("CHROME_WRAPPER") "WRAPPER"))
                   (substitute* (string-append usr/share "/applications/microsoft-" #$appname ".desktop")
                     (("^Exec=.*") (string-append "Exec=" exe "\n")))
                   (substitute* (string-append usr/share "/gnome-control-center/default-apps/microsoft-" #$appname ".xml")
                     ((old-exe) exe))
                   (substitute* (string-append usr/share "/menu/microsoft-" #$appname ".menu")
                     (("/opt") share)
                     ((old-exe) exe)))))
        #!(add-after 'install 'install-icons
                     (lambda _
                       (define (format-icon-size name)
                         (car
                           (string-split
                            (string-drop-right (string-drop name 13) 4)
                            #\_)))
                       (let ((icons (string-append #$output "/usr/share/icons/hicolor"))
                             (share (string-append #$output "/usr/share/microsoft/" #$appname)))
                         (for-each (lambda (icon)
                                     (let* ((icon-name (basename icon))
                                            (icon-size (format-icon-size icon-name))
                                            (target (string-append icons "/" icon-size "x" icon-size "/apps/microsoft-" #$appname ".png")))
                                       (mkdir-p (dirname target))
                                       (rename-file icon target)))
                                   (find-files share "product_logo_.*\\.png")))))
                  (add-before 'install-wrapper 'install-exe
                   (lambda _
                     (let* ((bin (string-append #$output "/usr/bin"))
                            (exe (string-append bin "/microsoft-" #$appname "-stable"))
                            (share (string-append #$output "/usr/share"))
                            (edge-target (string-append share "/microsoft/" #$appname "/microsoft-" #$appname)))
                       (mkdir-p bin)
                       (symlink edge-target exe)
                       (wrap-program exe
                         '("CHROME_WRAPPER" = (#$appname))))))
                         !#
                (delete 'patch-assets)
                (delete 'install-wrapper))))
     (inputs
      (list bzip2
            curl
            flac
            font-liberation
            gdk-pixbuf
            gtk
            harfbuzz
            libexif
            libglvnd
            libpng
            libva
            libxscrnsaver
            opus
            pciutils
            pipewire
            qtbase-5
            qtbase
            snappy
            util-linux
            xdg-utils
            wget))
     (synopsis  "Freeware web browser")
     (supported-systems '("x86_64-linux"))
     (description "Google Chrome is a cross-platform web browser developed by Google.")
     (home-page "https://www.microsoft.com/edge/")
     (license (nonfree "https://www.microsoft.com/intl/en/edge/terms/")))))

(define-public microsoft-edge-stable
  (make-microsoft-edge "stable" "133.0.3065.69" "05vx3lvwk0nyj8yyfq0cnc15mcpsic4npqm1b553jdx0m2d05cr2"))

(define-public microsoft-edge-beta
  (make-microsoft-edge "beta" "132.0.6834.32" "0ncjyg3vxsjspb0i1nsflcry9sfsffnqiwa2y7cgh46xamdq6f2c"))

(define-public microsoft-edge-unstable
  (make-microsoft-edge "unstable" "133.0.6847.2" "1z15maqs1g8lhi7dm9hn4r272nl0g967a11igmw2sj14zl3n9j6g"))

microsoft-edge-stable

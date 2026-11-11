;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>


(define-module (gunit packages edge)
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
         (appname (if (string=? repo "stable") "edge"
                      (string-replace-substring name "microsoft-" "")))
         (msname (string-append "ms" appname)))
    (package
      (name name)
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://packages.microsoft.com/repos/edge/pool/main/m/"
               name
               "/"
               name
               "_"
               version
               "-1_amd64.deb"))
         (sha256
          (base32 hash))))
      (build-system chromium-binary-build-system)
      (arguments
       (list
        #:substitutable? #f
        #:wrapper-plan
        #~(let ((path (string-append "opt/microsoft/" #$msname "/")))
            (map (lambda (file)
                   (string-append path file))
                 '("msedge"
                   "msedge-sandbox"
                   "msedge_crashpad_handler"
                   "liblearning_tools.so"
                   "liboneauth.so"
                   "liboneds.so"
                   "libqt5_shim.so"
                   "libqt6_shim.so"
                   "libtelclient.so"
                   "libvk_swiftshader.so"
                   "libvulkan.so.1"
                   "WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so")))
        #:install-plan
        #~'(("opt/" "/share")
            ("usr/share/" "/share"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'patch-assets
              (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (opt "./opt")
                       (usr/share "./usr/share")
                       (old-exe (string-append
                                 "/opt/microsoft/" #$msname "/microsoft-"
                                 #$appname))
                       (exe (string-append bin "/microsoft-"
                                           #$appname)))
                  (substitute* (string-append opt
                                              "/microsoft/" #$msname "/microsoft-"
                                              #$appname)
                    (("CHROME_WRAPPER")
                     "WRAPPER"))
                  (for-each
                   (lambda (desktop)
                     (substitute* desktop
                       (("^Exec=/usr/bin/microsoft-edge[-a-z]*")
                        (string-append "Exec=" exe))))
                   (find-files (string-append usr/share "/applications")
                               "\\.desktop$"))
                  (substitute* (string-append usr/share
                                              "/gnome-control-center/default-apps/microsoft-"
                                              #$appname ".xml")
                    ((old-exe)
                     exe)))))
            (add-after 'install 'install-icons
              (lambda _
                (define (format-icon-size name)
                  (car (string-split (string-drop-right (string-drop name 13)
                                                        4) #\_)))
                (let ((icons (string-append #$output "/share/icons/hicolor"))
                      (share (string-append #$output "/share/microsoft/" #$msname)))
                  (for-each (lambda (icon)
                              (let* ((icon-name (basename icon))
                                     (icon-size (format-icon-size icon-name))
                                     (target (string-append icons
                                                            "/"
                                                            icon-size
                                                            "x"
                                                            icon-size
                                                            "/apps/microsoft-"
                                                            #$appname
                                                            ".png")))
                                (mkdir-p (dirname target))
                                (rename-file icon target)))
                            (find-files share "product_logo_.*\\.png")))))
            (add-before 'install-wrapper 'install-exe
              (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (exe (string-append bin "/microsoft-" #$appname))
                       (share (string-append #$output "/share"))
                       (edge-target (string-append share
                                                   "/microsoft/" #$msname
                                                   "/microsoft-" #$appname)))
                  (mkdir-p bin)
                  (symlink edge-target exe)
                  (wrap-program exe
                    '("CHROME_WRAPPER" =
                      (#$appname)))))))))
      (inputs (list bzip2
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
      (synopsis "Freeware web browser")
      (supported-systems '("x86_64-linux"))
      (description
       "Microsoft Edge is a cross-platform web browser developed by Microsoft using Chromium.")
      (home-page "https://www.microsoft.com/edge/")
      (license (nonfree "https://www.microsoft.com/intl/en/edge/terms/")))))

(define-public microsoft-edge-stable
  (make-microsoft-edge "stable" "157.0.4341.46"
                       "06mna7ipxsykxabypcd29d92h6pwciccy46cb7cnc0vd1r88a9dj"))

(define-public microsoft-edge-beta
  (make-microsoft-edge "beta" "158.0.4370.7"
                       "0qq0j16gr2q3mmwi64wxgr0b6i5acrwhpqswjr13qh2qr0algdxl"))
microsoft-edge-stable

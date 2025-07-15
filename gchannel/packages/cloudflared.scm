;; SPDX-FileCopyrightText: 2022-2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
(define-module (gchannel packages cloudflared)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages linux))

  
;; Thank you https://github.com/rakino/rosenthal/blob/trunk/modules/rosenthal/packages/networking.scm
;; I didn't want to have to package this right now, so truly thank you.
(define-public cloudflared
  (package
    (name "cloudflared")
    (version "2025.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cloudflare/cloudflared")
                    (commit version)))
              (file-name (git-file-name name version))
              ;; TODO: Unbundle vendored dependencies.
              ;; (modules '((guix build utils)))
              ;; (snippet '(delete-file-recursively "vendor"))
              (sha256
               (base32
                "0by8pqp2zfqwvqff0pjvrllskcyb2xfzn9ck8d38nhc5vicr428q"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.24
           #:install-source? #f
           #:import-path "github.com/cloudflare/cloudflared/cmd/cloudflared"
           #:unpack-path "github.com/cloudflare/cloudflared"
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    " -X main.Version=" #$(package-version this-package)
                    " -X github.com/cloudflare/cloudflared/cmd/cloudflared/updater.BuiltForPackageManager=Guix"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'disable-cgo
                 (lambda _
                   (setenv "CGO_ENABLED" "0")))
               (add-after 'install 'install-documentation
                 (lambda _
                   (let ((src "src/github.com/cloudflare/cloudflared/cloudflared_man_template")
                         (dst (string-append #$output "/share/man/man1/cloudflared.1")))
                     (substitute* src
                       (("\\$\\{VERSION\\}") #$(package-version this-package)))
                     (mkdir-p (dirname dst))
                     (copy-file src dst)))))))
    (home-page "https://developers.cloudflare.com/cloudflare-one/connections/connect-apps/")
    (synopsis "Cloudflare Tunnel client")
    (description
     "This package provides the command-line client for Cloudflare Tunnel, a
tunneling daemon that proxies traffic from the Cloudflare network to your
origins.  This daemon sits between Cloudflare network and your origin (e.g. a
webserver).  Cloudflare attracts client requests and sends them to you via
this daemon, without requiring you to poke holes on your firewall --- your
origin can remain as closed as possible.")
    (license license:asl2.0)))

cloudflared
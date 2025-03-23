
(define-module (gchannel packages libp11)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public libp11
  (package
    (name "libp11")
    (version "0.4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/OpenSC/libp11/releases/download/libp11-"
                    version "/libp11-" version ".tar.gz"))
              (sha256
               (base32
                "11khasljs7ylk5ygb9n94lnmhqv0w80c3lmna4ny28xny77xjpfj"))))
    (build-system gnu-build-system)
    (arguments
    (list #:phases
          #~(modify-phases %standard-phases
              ;;(delete 'configure)
            )
          #:tests? #f ; there are no tests for the runtime library
          #:make-flags
          #~(list
                 (string-append "DESTDIR=" #$output)
                )))
    (inputs
     (list openssl-1.1))
    (native-inputs
     (list libxslt docbook-xsl pkg-config))
    (home-page "https://github.com/OpenSC/libp11/wiki")
    (synopsis "Tools and libraries related to smart cards")
    (description
     "The PKCS#11 API is an abstract API to perform operations on cryptographic
objects such as private keys, without requiring access to the objects themselves.
That is, it provides a logical separation of the keys from the operations.
The PKCS #11 API is mainly used to access objects in smart cards and Hardware
or Software Security Modules (HSMs). That is because in these modules the
cryptographic keys are isolated in hardware or software and are not made
available to the applications using them.")
    (license license:lgpl2.1+)))

libp11
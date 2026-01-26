;;; Copyright © 2023 Advanced Micro Devices, Inc.
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gunit packages python-cppheaderparser)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix licenses)

  #:use-module (gnu packages python-xyz))

(define-public python-cppheaderparser
  (package
    (name "python-cppheaderparser")
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "CppHeaderParser" version))
       (sha256
        (base32 "0hncwd9y5ayk8wa6bqhp551mcamcvh84h89ba3labc4mdm0k0arq"))))
    (build-system python-build-system)
    (arguments
        (list 
            #:tests? #f))
    (propagated-inputs (list python-ply))
    (home-page "http://senexcanis.com/open-source/cppheaderparser/")
    (synopsis
     "Parse C++ header files and generate a data structure representing the class")
    (description
     "Parse C++ header files and generate a data structure representing the class")
    (license bsd-3)))
python-cppheaderparser
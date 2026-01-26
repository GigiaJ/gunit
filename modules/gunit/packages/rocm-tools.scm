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

(define-module (gunit packages rocm-tools)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module (guix licenses)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages serialization)

  #:use-module (amd packages rocm-origin)
  #:use-module (gunit packages rocm-base))

; rocminfo
(define (make-rocminfo rocr-runtime)
  (package
    (name "rocminfo")
    (version (package-version rocr-runtime))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     `(#:build-type "Release"
       #:tests? #f))
    (inputs (list rocr-runtime python))
    (propagated-inputs (list grep kmod))
    (synopsis "ROCm application for reporting system info")
    (description
     "List @acronym{HSA,Heterogeneous System Architecture} Agents available to ROCm and show their properties.")
    (home-page "https://github.com/RadeonOpenCompute/rocminfo")
    (license ncsa)))

(define-public rocminfo
  (make-rocminfo rocr-runtime))

; rocm-bandwidth-test
(define (make-rocm-bandwidth rocr-runtime)
  (package
    (name "rocm-bandwidth-test")
    (version (package-version rocr-runtime))
    (source
     (rocm-origin "rocm_bandwidth_test" version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags #~(list "-DCMAKE_CXX_FLAGS=-Wno-error=cpp" ;ignores include reorg issue
                                )))
    (inputs (list rocr-runtime))
    (synopsis "Bandwith test for ROCm")
    (description
     "RocBandwidthTest is designed to capture the performance
characteristics of buffer copying and kernel read/write operations. The help
screen of the benchmark shows various options one can use in initiating
copy/read/writer operations.  In addition one can also query the topology of the
system in terms of memory pools and their agents.")
    (home-page "https://github.com/ROCm/rocm_bandwidth_test")
    (license ncsa)))

(define-public rocm-bandwidth
  (make-rocm-bandwidth rocr-runtime))

; rocm-smi
(define (make-rocm-smi version)
  (package
    (name "rocm-smi")
    (version version)
    (source
     (rocm-origin "rocm_smi_lib" version))
    (build-system cmake-build-system)
    (arguments
     `(#:build-type "Release"
       #:tests? #f)) ;No tests.
    (inputs (list python))
    (propagated-inputs (list grep coreutils))
    (synopsis "The ROCm System Management Interface (ROCm SMI) Library")
    (description
     "The ROCm System Management Interface Library, or ROCm SMI library, 
is part of the Radeon Open Compute ROCm software stack. It is a C library for Linux that 
provides a user space interface for applications to monitor and control GPU applications.")
    (home-page "https://github.com/RadeonOpenCompute/rocm_smi_lib.git")
    (license ncsa)))

(define-public rocm-smi
  (make-rocm-smi rocm-version-latest))

; tensile
(define (make-tensile version)
  (package
    (name "tensile")
    (version version)
    (source
     (rocm-origin name version))
    (build-system python-build-system)
    (native-inputs (list python-pandas))
    (propagated-inputs (list msgpack-3 python-msgpack python-pyyaml
                             python-joblib python-psutil))
    (synopsis "A GEMM kernel generator for AMD GPUs.")
    (description
     "Tensile is a tool for creating benchmark-driven backend libraries for GEMMs, GEMM-like problems
(such as batched GEMM), and general N-dimensional tensor contractions on a GPU. The Tensile library
is mainly used as backend library to rocBLAS. Tensile acts as the performance backbone for a wide
variety of 'compute' applications running on AMD GPUs.")
    (home-page "https://github.com/ROCmSoftwarePlatform/Tensile.git")
    (license expat)))

(define-public tensile
  (make-tensile rocm-version-latest))

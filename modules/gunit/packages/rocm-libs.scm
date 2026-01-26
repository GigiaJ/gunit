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

(define-module (gunit packages rocm-libs)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build utils)
  #:use-module (guix utils)

  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages debug)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages sqlite)

  #:use-module (amd packages rocm-origin)
  #:use-module (gunit packages rocm-base)
  #:use-module (gunit packages rocm-hip)
  #:use-module (gunit packages rocm-tools)
  #:use-module (gunit packages python-cppheaderparser)

  #:use-module (gnu packages mpi)

  #:use-module (gnu packages fabric-management))

; rocdbg-api
(define (make-rocdbg-api rocm-comgr hipamd)
  (package
    (name "rocdbg-api")
    (version (package-version hipamd))
    (source
     (rocm-origin "rocdbgapi" version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DPCI_IDS_PATH="
                                               #$(this-package-input "hwdata")))))
    (inputs (list libbacktrace hwdata rocm-comgr hipamd))
    (synopsis "ROCm debugger API.")
    (description
     "The AMD Debugger API is a library that provides all the support necessary for a debugger
and other tools to perform low level control of the execution and inspection of execution state
of AMD's commercially available GPU architectures.")
    (home-page "https://github.com/ROCm/ROCdbgapi")
    (license expat)))

(define-public rocdbg-api
  (make-rocdbg-api llvm-comgr hipamd))

; rocprim
(define (make-rocprim rocm-cmake hipamd)
  (package
    (name "rocprim")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f ;No tests.
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$(this-package-input "hipamd")
                                               "/bin/hipcc"))))
    (inputs (list hipamd))
    (native-inputs (list rocm-cmake python-wrapper))
    (synopsis
     "rocPRIM: a header-only library providing HIP parallel primitives")
    (description
     "The rocPRIM is a header-only library providing HIP parallel primitives 
for developing performant GPU-accelerated code on the AMD ROCm platform.")
    (home-page "https://github.com/ROCmSoftwarePlatform/rocPRIM.git")
    (license expat)))

(define-public rocprim
  (make-rocprim rocm-cmake hipamd))

; hipcub
(define-public (make-hipcub hipamd rocm-cmake rocprim)
  (package
    (name "hipcub")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                "-DAMDGPU_TARGETS=gfx90a,gfx1030")))
    (inputs (list hipamd rocprim))
    (native-inputs (list rocm-cmake))
    (synopsis "hipCUB is a HIP wrapper for rocPRIM.")
    (description
     "hipCUB is a thin wrapper library on top of rocPRIM or CUB. You can use it to port a
CUB project into HIP so you can use AMD hardware (and ROCm software).")
    (home-page "https://github.com/ROCm/hipCUB")
    (license bsd-3)))

(define-public hipcub
  (make-hipcub hipamd rocm-cmake rocprim))

; rccl
(define-public (make-rccl hipamd rocm-cmake rocm-smi hipify)
  (package
    (name "rccl")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DROCM_PATH="
                                               #$(this-package-input "hipamd"))
                                (string-append "-DCMAKE_CXX_COMPILER="
                                               #$(this-package-input "hipamd")
                                               "/bin/hipcc")
                                "-DAMDGPU_TARGETS=gfx90a,gfx1030")
      #:phases #~(modify-phases %standard-phases
                   (add-before 'configure 'parallel-jobs
                     (lambda _
                       (substitute* "CMakeLists.txt"
                         (("target_compile_options\\(rccl PRIVATE -parallel-jobs=.*\\)")
                          "target_compile_options(rccl PRIVATE -parallel-jobs=1)")
                         (("target_link_options\\(rccl PRIVATE -parallel-jobs=.*\\)")
                          "target_link_options(rccl PRIVATE -parallel-jobs=4)")))))))
    (inputs (list hipamd rocm-smi))
    (native-inputs (list rocm-cmake hipify))
    (properties '((max-silent-time . 14400)))
    (synopsis "ROCm Communication Collectives Library")
    (description
     "RCCL (pronounced \"Rickle\") is a stand-alone library of standard collective communication
routines for GPUs, implementing all-reduce, all-gather, reduce, broadcast, reduce-scatter, gather,
scatter, and all-to-all. There is also initial support for direct GPU-to-GPU send and receive operations.
It has been optimized to achieve high bandwidth on platforms using PCIe, xGMI as well as networking using
InfiniBand Verbs or TCP/IP sockets. RCCL supports an arbitrary number of GPUs installed in a single node
or multiple nodes, and can be used in either single- or multi-process (e.g., MPI) applications.")
    (home-page "https://github.com/ROCm/rccl")
    (license bsd-3)))

(define-public rccl
  (make-rccl hipamd rocm-cmake rocm-smi hipify))

; rocthrust
(define (make-rocthrust hipamd rocm-cmake rocprim)
  (package
    (name "rocthrust")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc"))))
    (inputs (list rocprim hipamd))
    (native-inputs (list rocm-cmake))
    (synopsis "rocThrust is a parallel algorithm library.")
    (description
     "rocThrust is a parallel algorithm library that has been ported to HIP and ROCm,
which uses the rocPRIM library. The HIP-ported library works on HIP and ROCm software.")
    (home-page "https://github.com/ROCm/rocThrust.git")
    (license asl2.0)))

(define-public rocthrust
  (make-rocthrust hipamd rocm-cmake rocprim))

; rocsparse
(define (make-rocsparse rocm-cmake hipamd rocm-device-libs rocr-runtime
                        rocprim)
  (package
    (name "rocsparse")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags #~(list "-DAMDGPU_TARGETS=gfx90a;gfx1030"
                                (string-append "-DCMAKE_CXX_COMPILER="
                                               #$(this-package-input "hipamd")
                                               "/bin/hipcc")
                                (string-append "-DCMAKE_Fortran_COMPILER="
                                               #$(this-package-native-input
                                                  "gfortran") "/bin/gfortran")
                                "-DBUILD_CLIENTS_SAMPLES:BOOL=OFF" ;build fails otherwise
                                )))
    (native-inputs (list git gfortran rocm-cmake python-wrapper))
    (inputs (list hipamd rocm-device-libs rocr-runtime rocprim))
    (synopsis "rocSPARSE provides an interface for sparse BLAS operations.")
    (description
     "rocSPARSE exposes a common interface that provides Basic Linear Algebra Subroutines (BLAS)
for sparse computation. It's implemented on top of AMD ROCm runtime and toolchains. rocSPARSE is created using
the HIP programming language and optimized for AMD's latest discrete GPUs.")
    (home-page "https://github.com/ROCm/rocSPARSE.git")
    (license expat)))

(define-public rocsparse
  (make-rocsparse rocm-cmake hipamd llvm-device-libs rocr-runtime rocprim))

; hipsparse
(define-public (make-hipsparse hipamd rocm-cmake rocsparse)
  (package
    (name "hipsparse")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                "-DBUILD_CLIENTS_SAMPLES:BOOL=OFF" ;build fails otherwise
                                "-DAMDGPU_TARGETS=gfx90a,gfx1030")))
    (inputs (list hipamd rocsparse))
    (native-inputs (list git gfortran rocm-cmake))
    (synopsis "A HIP interface to rocSPARSE.")
    (description
     "hipSPARSE is a SPARSE marshalling library with multiple supported backends.
It sits between your application and a 'worker' SPARSE library, where it marshals inputs to the backend
library and marshals results to your application. hipSPARSE exports an interface that doesn't require the
client to change, regardless of the chosen backend. Currently, hipSPARSE supports rocSPARSE and cuSPARSE
backends.")
    (home-page "https://github.com/ROCm/hipSPARSE.git")
    (license expat)))

(define-public hipsparse
  (make-hipsparse hipamd rocm-cmake rocsparse))

; libfabric built with rocm
(define (make-ofi-rocm rocr-runtime)
  (package/inherit libfabric
    (name "libfabric-rocm")
    (version (string-append (package-version libfabric) ".rocm"
                            (package-version rocr-runtime)))

    (arguments (substitute-keyword-arguments (package-arguments libfabric)
                 ((#:configure-flags flags)
                  #~(append (list (string-append "--with-rocr="
                                                 #$rocr-runtime))
                            #$flags))))
    (inputs (modify-inputs (package-inputs libfabric)
              (append rocr-runtime)))))

(define-public ofi-rocm
  (make-ofi-rocm rocr-runtime))

; ucx built with rocm
(define (make-ucx-rocm roct-thunk rocr-runtime hipamd)
  (package/inherit ucx
    (name "ucx-rocm")
    (version (string-append (package-version ucx) ".rocm"
                            (package-version hipamd)))
    (arguments (substitute-keyword-arguments (package-arguments ucx)
                 ((#:configure-flags flags)
                  #~(append (list "--without-cuda" "--without-knem"
                                  "--without-java"
                                  (string-append "--with-rocm="
                                                 #$(this-package-input
                                                    "rocr-runtime"))
                                  (string-append "--with-hip="
                                                 #$(this-package-input
                                                    "hipamd")))
                            #$flags))))
    (native-inputs (modify-inputs (package-native-inputs ucx)
                     (append roct-thunk)))
    (inputs (modify-inputs (package-inputs ucx)
              (append hipamd)
              (append rocr-runtime)))
    (properties `((tunable? . #t) ,@(package-properties ucx)))))

(define-public ucx-rocm
  (make-ucx-rocm roct-thunk rocr-runtime hipamd))

; openmpi built with ucx-rocm and libfabric-rocm
(define (make-openmpi-rocm ucx ofi hipamd)
  (package/inherit openmpi
    (name (string-append (package-name openmpi) "-rocm"))
    (version (string-append (package-version openmpi) ".rocm"
                            (package-version hipamd)))
    (arguments (substitute-keyword-arguments (package-arguments openmpi)
                 ((#:configure-flags flags)
                  #~(append (list (string-append "--with-rocm="
                                                 #$(this-package-input
                                                    "hipamd"))
                                  (string-append "--with-ofi="
                                                 #$(this-package-input
                                                    "libfabric")))
                            #$flags))
                 ((#:phases phases
                   '%standard-phases)
                  #~(modify-phases #$phases
                      ;; opensm is needed for InfiniBand support.
                      (add-after 'unpack 'find-opensm-headers
                        (lambda* (#:key inputs #:allow-other-keys)
                          (setenv "C_INCLUDE_PATH"
                                  (search-input-directory inputs
                                   "/include/infiniband"))
                          (setenv "CPLUS_INCLUDE_PATH"
                                  (search-input-directory inputs
                                   "/include/infiniband"))))))))
    (inputs (modify-inputs (package-inputs openmpi)
              (replace "ucx" ucx)
              (replace "libfabric" ofi)
              (append hipamd)))))

(define-public openmpi-rocm
  (make-openmpi-rocm ucx-rocm ofi-rocm hipamd))

; roctracer
(define (make-roctracer hipamd)
  (package
    (name "roctracer")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DROCM_PATH="
                                               #$hipamd)
                                #$(if (version>=? version "5.5.0") ""
                                      "-DCMAKE_CXX_COMPILER=g++"))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'update-filesystem
                     ;; only needed from 5.5 onwards
                     (lambda _
                       (substitute* (append (find-files "." ".cpp$")
                                            (find-files "." ".h$"))
                         (("std::experimental::filesystem")
                          "std::filesystem")
                         (("<experimental/filesystem>")
                          "<filesystem>")))))))
    (inputs (list numactl hipamd python python-cppheaderparser))
    (synopsis "A callback/activity library for performance tracing AMD GPUs.")
    (description
     "ROCm tracer provides an API to provide functionality for registering
the runtimes API callbacks and asynchronous activity records pool support.")
    (home-page "https://github.com/ROCm/roctracer")
    (license expat)))

(define-public roctracer
  (make-roctracer hipamd))

; rocblas
(define (make-rocblas tensile rocm-cmake hipamd)
  (package
    (name "rocblas")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f
      #:validate-runpath? #f
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                (string-append "-DTensile_CPU_THREADS="
                                               (number->string (parallel-job-count)))
                                "-DBUILD_WITH_PIP=OFF"
                                (if (string=? #$version "5.3.3")
                                 "-DCMAKE_TOOLCHAIN_FILE=toolchain-linux.cmake"
                                 "") "-DAMDGPU_TARGETS=gfx1030;gfx90a")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
                     ;; only needed for version<=5.4
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define cplus-include-path
                         ;; Delete glibc/include and gcc/include/c++ from CPLUS_INCLUDE_PATH
                         ;; to allow clang to include the cuda_wrappers first.
                         (delete (string-append (assoc-ref inputs "libc")
                                                "/include")
                                 (delete (string-append (assoc-ref inputs
                                                                   "gcc")
                                                        "/include/c++")
                                         (string-split (getenv
                                                        "CPLUS_INCLUDE_PATH")
                                                       #\:))))
                       (setenv "CPLUS_INCLUDE_PATH"
                               (string-join cplus-include-path ":")))))))
    (native-inputs (list python-wrapper tensile hipamd rocm-cmake))
    (properties '((max-silent-time . 14400)))
    (synopsis "Next generation BLAS implementation for ROCm platform.")
    (description
     "rocBLAS is the ROCm Basic Linear Algebra Subprograms (BLAS) library.
rocBLAS is implemented in the HIP programming language and optimized for AMD GPUs.")
    (home-page "https://github.com/ROCmSoftwarePlatform/rocBLAS.git")
    (license (list expat bsd-3))))

(define-public rocblas
  (make-rocblas tensile rocm-cmake hipamd))

; rocsolver
(define (make-rocsolver hipamd rocm-cmake rocblas rocprim)
  (package
    (name "rocsolver")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                "-DBUILD_WITH_SPARSE=OFF"
                                "-DAMDGPU_TARGETS=gfx1030;gfx90a")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define cplus-include-path
                         ;; Delete glibc/include and gcc/include/c++ from CPLUS_INCLUDE_PATH
                         ;; to allow clang to include the cuda_wrappers first.
                         (delete (string-append (assoc-ref inputs "libc")
                                                "/include")
                                 (delete (string-append (assoc-ref inputs
                                                                   "gcc")
                                                        "/include/c++")
                                         (string-split (getenv
                                                        "CPLUS_INCLUDE_PATH")
                                                       #\:))))
                       (setenv "CPLUS_INCLUDE_PATH"
                               (string-join cplus-include-path ":")))))))
    (inputs (list fmt-8 hipamd rocblas rocprim))
    (native-inputs (list python-wrapper rocm-cmake))
    (synopsis "rocSOLVER provides LAPACK operations for the ROCm platform.")
    (description
     "rocSOLVER is a work-in-progress implementation of a subset of LAPACK functionality
on the ROCm platform.")
    (home-page "https://github.com/ROCm/rocSOLVER.git")
    (license (list bsd-2 bsd-3))))

(define-public rocsolver
  (make-rocsolver hipamd rocm-cmake rocblas rocprim))

; hipblas
(define-public (make-hipblas hipamd rocm-cmake rocblas rocsolver)
  (package
    (name "hipblas")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                "-DAMDGPU_TARGETS=gfx90a,gfx1030")))
    (inputs (list gfortran hipamd rocblas rocsolver))
    (native-inputs (list rocm-cmake))
    (synopsis "hipBLAS is a HIP interface to rocBLAS.")
    (description
     "hipBLAS is a Basic Linear Algebra Subprograms (BLAS) marshalling library with multiple
supported backends. It sits between your application and a 'worker' BLAS library, where it marshals inputs to
the backend library and marshals results to your application. hipBLAS exports an interface that doesn't
require the client to change, regardless of the chosen backend. Currently, hipBLAS supports rocBLAS and
cuBLAS backends.")
    (home-page "https://github.com/ROCm/hipBLAS.git")
    (license (list bsd-3 expat))))

(define-public hipblas
  (make-hipblas hipamd rocm-cmake rocblas rocsolver))

; rocrand
(define-public (make-rocrand hipamd rocm-cmake)
  (package
    (name "rocrand")
    (version (package-version hipamd))
    (source
     (rocm-origin name version
                  #:recursive? #f))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                (string-append
                                 "-DCMAKE_SHARED_LINKER_FLAGS=-Wl,-rpath="
                                 #$output "/lib") "-DBUILD_HIPRAND:BOOL=OFF"
                                "-DAMDGPU_TARGETS=gfx90a,gfx1030")))
    (inputs (list hipamd))
    (native-inputs (list git rocm-cmake))
    (synopsis "RAND library for HIP programming language.")
    (description
     "The rocRAND project provides functions that generate pseudorandom and quasirandom numbers.
The rocRAND library is implemented in the HIP programming language and optimized for AMD's latest discrete GPUs.
It is designed to run on top of AMD's ROCm runtime, but it also works on CUDA-enabled GPUs.")
    (home-page "https://github.com/ROCm/rocRAND.git")
    (license expat)))

(define-public rocrand
  (make-rocrand hipamd rocm-cmake))

; hiprand
(define-public (make-hiprand hipamd rocm-cmake rocrand)
  (package
    (name "hiprand")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                "-DAMDGPU_TARGETS=gfx90a,gfx1030")))
    (inputs (list hipamd rocrand))
    (native-inputs (list git rocm-cmake))
    (synopsis "RAND library for HIP programming language.")
    (description
     "The rocRAND project provides functions that generate pseudorandom and quasirandom numbers.
The rocRAND library is implemented in the HIP programming language and optimized for AMD's latest discrete GPUs.
It is designed to run on top of AMD's ROCm runtime, but it also works on CUDA-enabled GPUs.")
    (home-page "https://github.com/ROCm/rocRAND.git")
    (license expat)))

(define-public hiprand
  (make-hiprand hipamd rocm-cmake rocrand))

; rocalution
(define-public (make-rocalution hipamd
                                rocm-cmake
                                rocsparse
                                rocblas
                                rocprim
                                rocrand)
  (package
    (name "rocalution")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                (string-append "-DROCM_PATH="
                                               #$hipamd)
                                "-DAMDGPU_TARGETS=gfx1030;gfx90a"
                                "-DSUPPORT_HIP=ON"
                                "-DBUILD_CLIENTS_SAMPLES=OFF"
                                "-DBUILD_CLIENTS_TESTS=OFF"
                                "-DBUILD_CLIENTS_BENCHMARKS=OFF")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define cplus-include-path
                         ;; Delete glibc/include and gcc/include/c++ from CPLUS_INCLUDE_PATH
                         ;; to allow clang to include the cuda_wrappers first.
                         (delete (string-append (assoc-ref inputs "libc")
                                                "/include")
                                 (delete (string-append (assoc-ref inputs
                                                                   "gcc")
                                                        "/include/c++")
                                         (string-split (getenv
                                                        "CPLUS_INCLUDE_PATH")
                                                       #\:))))
                       (setenv "CPLUS_INCLUDE_PATH"
                               (string-join cplus-include-path ":")))))))
    (inputs (list hipamd rocsparse rocblas rocprim rocrand))
    (native-inputs (list git rocm-cmake))
    (synopsis "rocALUTION is a sparse linear algebra library.")
    (description
     "rocALUTION is a sparse linear algebra library that can be used to explore fine-grained
parallelism on top of the ROCm platform runtime and toolchains. Based on C++ and HIP, rocALUTION provides a
portable, generic, and flexible design that allows seamless integration with other scientific software packages.")
    (home-page "https://github.com/ROCm/rocALUTION.git")
    (license expat)))

(define-public rocalution
  (make-rocalution hipamd
                   rocm-cmake
                   rocsparse
                   rocblas
                   rocprim
                   rocrand))

; rocfft
(define (make-rocfft hipamd rocm-cmake)
  (package
    (name "rocfft")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      ;; #:build-type "Release"
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                "-DSQLITE_USE_SYSTEM_PACKAGE:BOOL=ON"
                                "-DAMDGPU_TARGETS=gfx90a;gfx1030")))
    (inputs (list hipamd))
    (native-inputs (list rocm-cmake python-wrapper sqlite))
    (properties '((max-silent-time . 14400)))
    (synopsis "Fast Fourier transforms (FFTs) for ROCm.")
    (description
     "rocFFT is a software library for computing fast Fourier transforms (FFTs) written in
the HIP programming language. It's part of AMD's software ecosystem based on ROCm. The rocFFT
library can be used with AMD and NVIDIA GPUs.")
    (home-page "https://github.com/ROCm/rocFFT")
    (license expat)))

(define-public rocfft
  (make-rocfft hipamd rocm-cmake))

; hipfft
(define (make-hipfft hipamd rocm-cmake rocfft)
  (package
    (name "hipfft")
    (version (package-version hipamd))
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags #~(list (string-append "-DCMAKE_CXX_COMPILER="
                                               #$hipamd "/bin/hipcc")
                                (string-append "-DROCM_PATH="
                                               #$hipamd)
                                "-DAMDGPU_TARGETS=gfx90a;gfx1030")))
    (inputs (list hipamd rocfft))
    (native-inputs (list git rocm-cmake)) ;python-wrapper
    (synopsis "A HIP interface for rocFFT and cuFFT.")
    (description
     "hipFFT exports an interface that doesn't require the client to change, regardless of the
chosen backend. It sits between your application and the backend FFT library, where it marshals
inputs to the backend and marshals results back to your application.")
    (home-page "https://github.com/ROCm/hipFFT")
    (license expat)))

(define-public hipfft
  (make-hipfft hipamd rocm-cmake rocfft))

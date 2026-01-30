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


(define-module (gunit packages rocm-hip)
  #:use-module (amd packages rocm-origin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gunit packages python-cppheaderparser)
  #:use-module (gunit packages rocm-base)
  #:use-module (gunit packages rocm-tools))

(define (make-rocm-comgr rocm-device-libs llvm-rocm lld-rocm clang-rocm)
  (package
    (name "rocm-comgr")
    (version (package-version rocm-device-libs))
    (source
     (rocm-origin (if (version>=? version "6.1.1") "llvm-project"
                      "rocm-compilersupport") version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (setenv "HIP_DEVICE_LIB_PATH"
                      (string-append #$(this-package-input "rocm-device-libs")
                                     "/amdgcn/bitcode"))
              (chdir #$(if (version>=? version "6.1.1") "amd/comgr"
                           "lib/comgr"))))
          (add-before 'configure 'fix-path
            (lambda _
              (substitute* "src/comgr-env.cpp"
                (("getDetector\\(\\)->getLLVMPath\\(\\)")
                 (string-append "\""
                                #$clang-rocm "\""))))))))
    (inputs (list rocm-device-libs))
    (native-inputs (list llvm-rocm lld-rocm clang-rocm))
    (synopsis "The ROCm Code Object Manager")
    (description
     "The Comgr library provides APIs for compiling and inspecting AMDGPU code objects.")
    (home-page "https://github.com/RadeonOpenCompute/ROCm-CompilerSupport")
    (license ncsa)))

(define-public llvm-comgr
  (make-rocm-comgr llvm-device-libs llvm-rocm lld-rocm clang-rocm))

; hip headers
(define (make-hip version)
  (hidden-package (package
                    (name "hip")
                    (version version)
                    (source
                     (rocm-origin name version))
                    (build-system copy-build-system)
                    (arguments
                     (list
                      #:install-plan
                      #~`(("." "/"))))
                    (synopsis
                     "The Heterogeneous Interface for Portability (HIP) framework")
                    (description
                     "The Heterogeneous Interface for Portability (HIP) framework is a
C++ Runtime API and Kernel Language that allows developers to create portable applications
for AMD and NVIDIA GPUs from single source code.")
                    (home-page "https://github.com/ROCm-Developer-Tools/HIP")
                    (license expat))))

(define-public hip
  (make-hip rocm-version-latest))

(define (make-hipcc rocminfo rocm-toolchain)
  (hidden-package (package
                    (name "hipcc")
                    (version (package-version rocm-toolchain))
                    (source
                     (rocm-origin (if (version>=? version "6.1.1")
                                      "llvm-project" name) version))
                    (build-system cmake-build-system)
                    (arguments
                     (list
                      #:build-type "Release"
                      #:tests? #f
                      #:phases
                      #~(modify-phases %standard-phases
                          (add-after 'unpack 'chdir
                            (lambda _
                              (chdir #$(if (version>=? version "6.1.1")
                                           "amd/hipcc" "."))))
                          (add-after 'install 'patch-perl
                            (lambda* (#:key inputs outputs #:allow-other-keys)
                              (let* ((out (assoc-ref outputs "out"))
                                     (rinfo (assoc-ref inputs "rocminfo"))
                                     (perl (assoc-ref inputs "perl"))
                                     (rocm (assoc-ref inputs "rocm-toolchain"))
                                     (device-libs (assoc-ref inputs
                                                   "rocm-device-libs"))
                                     (files '("hipcc.pl" "hipconfig.pl"
                                              "hipvars.pm" "hipcc" "hipconfig")))
                                (for-each (lambda (f)
                                            (substitute* (string-append out
                                                          "/bin/" f)
                                              (("\\$HIP_PATH *= *\\$ENV\\{'HIP_PATH'\\} *// *dirname\\(Cwd::abs_path\\(\"\\$0/\\.\\./\"\\)\\);")
                                               (string-append "$HIP_PATH = \""
                                                              out "\";")))
                                            (substitute* (string-append out
                                                          "/bin/hipcc.pl")
                                              (("\\$ROCMINFO_PATH[[:space:]]*=[[:space:]]*\\$hipvars::ROCMINFO_PATH;")
                                               (string-append
                                                "$ROCMINFO_PATH = \"" rinfo
                                                "\";")))
                                            (substitute* (string-append out
                                                          "/bin/hipcc.pl")
                                              (("\\DEVICE_LIB_PATH[[:space:]]*=[[:space:]]*\\$hipvars::DEVICE_LIB_PATH;")
                                               (string-append
                                                "DEVICE_LIB_PATH = \""
                                                device-libs "\";")))

                                            (substitute* (string-append out
                                                          "/bin/" f)
                                              (("^#!/usr/bin/env perl$")
                                               (string-append "#!" perl
                                                              "/bin/perl\n")))
                                            (substitute* (string-append out
                                                          "/bin/" f)
                                              (("/opt/rocm")
                                               (string-append out)))) files))))
                          (add-after 'patch-perl 'wrap-hipcc
                            (lambda* (#:key outputs inputs #:allow-other-keys)
                              (let* ((out (assoc-ref outputs "out"))
                                     (rt (assoc-ref inputs "rocm-toolchain"))
                                     (rinfo (assoc-ref inputs "rocminfo")))
                                (wrap-program (string-append out "/bin/hipcc")
                                  `("ROCMINFO_PATH" =
                                    (,(string-append rinfo)))
                                  `("ROCM_PATH" =
                                    (,(string-append rt)))
                                  `("HIP_CLANG_PATH" =
                                    (,(string-append rt "/bin")))
                                  `("DEVICE_LIB_PATH" =
                                    (,(string-append rt "/amdgcn/bitcode")))
                                  `("PATH" ":" prefix
                                    (,(string-append rt "/bin") ,(string-append
                                                                  rinfo "/bin"))))))))))
                    (propagated-inputs (list rocminfo rocm-toolchain
                                             rocm-device-libs))
                    (native-inputs (list perl))
                    (synopsis "HIP compiler driver (hipcc)")
                    (description
                     "The HIP compiler driver (hipcc) is a compiler utility that will call
clang and pass the appropriate include and library options for the target compiler and HIP infrastructure.")
                    (home-page "https://github.com/ROCm/HIPCC")
                    (license expat))))

(define-public hipcc
  (make-hipcc rocminfo rocm-toolchain))

(define (make-clr-hipamd hip hipcc rocm-comgr)
  (package
    (name "hipamd")
    (version (package-version hip))
    (source
     (rocm-origin "clr" version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:tests? #f
      #:configure-flags
      #~(list (string-append "-DHIP_COMMON_DIR="
                             #$hip)
              (string-append "-DHIPCC_BIN_DIR="
                             #$hipcc "/bin")
              "-DCLR_BUILD_HIP=ON"
              "-DCLR_BUILD_OCL=OFF"
              "-DHIP_ENABLE_ROCPROFILER_REGISTER=OFF"
              "-D__HIP_ENABLE_PCH=OFF"
              "-DHIP_PLATFORM=amd")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'info-version'
            (lambda _
              (mkdir (string-append #$output "/.info"))
              (with-output-to-file (string-append #$output "/.info/version")
                (lambda ()
                  (display (string-append #$version "-0"))))))
          (add-after 'install 'overwrite-hipvars
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (with-output-to-file (string-append (assoc-ref outputs "out")
                                                  "/bin/hipvars.pm")
                (lambda ()
                  (display (string-append "package hipvars;\n"
                            "$isWindows = 0;\n"
                            "$doubleQuote = \"\\\"\";\n"
                            "$CUDA_PATH = \"\";\n"
                            "$HIP_PLATFORM = \"amd\";\n"
                            "$HIP_COMPILER = \"clang\";\n"
                            "$HIP_RUNTIME = \"rocclr\";\n"
                            "$HIP_CLANG_RUNTIME = \""
                            (assoc-ref inputs "rocm-toolchain")
                            "\";\n"
                            "$DEVICE_LIB_PATH = \""
                            (assoc-ref inputs "rocm-toolchain")
                            "/amdgcn/bitcode\";\n"
                            "$HIP_CLANG_PATH = \""
                            (assoc-ref inputs "rocm-toolchain")
                            "/bin\";\n"
                            "$HIP_PATH = \""
                            #$output
                            "\";\n"
                            "$HIP_VERSION= \""
                            #$version
                            "\";\n"
                            "$ROCMINFO_PATH = \""
                            (assoc-ref inputs "rocminfo")
                            "\";\n"
                            "$ROCR_RUNTIME_PATH = \""
                            (assoc-ref inputs "rocm-toolchain")
                            "\";\n"
                            "$HIP_INFO_PATH = \"$HIP_PATH/lib/.hipInfo\";
"
                            "$HIP_ROCCLR_HOME = $HIP_PATH;\n"
                            "$ROCM_PATH = \""
                            (assoc-ref inputs "rocm-toolchain")
                            "\";")))))))))
    (native-inputs (list mesa
                         libffi
                         git
                         perl
                         python-wrapper
                         python-cppheaderparser
                         hip
                         hipcc))
    (propagated-inputs (modify-inputs (package-propagated-inputs hipcc)
                         (append rocm-comgr)))
    (synopsis "AMD CLR - Compute Language Runtimes for HIP applications")
    (description
     "AMD Common Language Runtime contains source code for AMD's
compute languages runtimes: HIP and OpenCL. This package is built for HIP only.")
    (home-page "https://github.com/ROCm/clr")
    (license expat)))

(define-public hipamd
  (make-clr-hipamd hip hipcc llvm-comgr))

; rocm-cmake
(define (make-rocm-cmake version)
  (package
    (name "rocm-cmake")
    (version version)
    (source
     (rocm-origin name version))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;No tests.
    (synopsis
     "ROCm-CMake is a collection of CMake modules for common build and development
tasks within the ROCm project.")
    (description
     "ROCm-CMake is a collection of CMake modules for common build and development
tasks within the ROCm project. It is therefore a build dependency for many of the libraries that
comprise the ROCm platform. ROCm-CMake is not required for building libraries or programs that use ROCm;
it is required for building some of the libraries that are a part of ROCm.")
    (home-page "https://github.com/RadeonOpenCompute/rocm-cmake.git")
    (license expat)))

(define-public rocm-cmake
  (make-rocm-cmake rocm-version-latest))


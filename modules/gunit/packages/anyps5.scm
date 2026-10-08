(define-module (gunit packages anyps5)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan))

(define-public anyps5
  (package
    (name "anyps5")
    (version "0.1.0-dev")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/boykopovar/AnyPS5")
             (commit "7a178e2276137d4533d7096491a048c1225158b3")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1syazsdjwmpif00pw45zi1c46yq6vxzxq61sl7vjq94hnr3v8b8b"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~'("-DANYPS5_ENABLE_SPIRV_TOOLS=ON"
          "-DCMAKE_BUILD_TYPE=Release")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-ffmpeg-core
            (lambda _
              (with-output-to-file "3rdparty/ffmpeg-core/CMakeLists.txt"
                (lambda ()
                  (display "
find_package(PkgConfig REQUIRED)
pkg_check_modules(FFMPEG REQUIRED libavcodec libavformat libavutil libswresample)

add_library(ffmpeg INTERFACE)
target_include_directories(ffmpeg INTERFACE ${FFMPEG_INCLUDE_DIRS})
target_link_libraries(ffmpeg INTERFACE ${FFMPEG_LIBRARIES})

add_library(FFmpeg::FFmpeg ALIAS ffmpeg)
add_library(ffmpeg-core ALIAS ffmpeg)
")))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (binaries (find-files "." "^(AnyPS5|anyps5|Relinker|relinker)$")))
                (if (null? binaries)
                    (error "No executables found! The CMake target name must be something else.")
                    (begin
                      (mkdir-p bin)
                      (for-each (lambda (file)
                                  (install-file file bin))
                                binaries)))
                #t))))))
    (native-inputs
     (list pkg-config
           python))
    (inputs
     (list ffmpeg
           sdl2
           vulkan-loader
           vulkan-headers))
    (synopsis "Tool for automatic PS5 executables porting to Linux and Windows")
    (description
     "AnyPS5 converts PlayStation 5 executables to run natively on Linux and Windows.
It includes a relinker that converts the executable to the target system's native format
and provides implementations of system PRX libraries for dynamic linking, skipping
the CPU overhead of traditional emulation.")
    (home-page "https://github.com/boykopovar/AnyPS5")
    (license license:gpl2)))

anyps5

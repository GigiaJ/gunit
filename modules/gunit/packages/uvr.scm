(define-module (gunit packages uvr)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages mp3)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages video)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages nss)
  #:use-module (nongnu packages nvidia)
  #:use-module (gunit packages python-common)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression))

(define-public ultimatevocalremovergui
  (package
    (name "ultimatevocalremovergui")
    (version "5.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Anjok07/ultimatevocalremovergui.git")
             (commit "5517e0cf0d1acd16a1618eeedec596957523f9e1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pqb1y4b0w5id7kp5wnrmdcry4mcwqafbg3gji40np6cwkfrqfwc"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'patch-paths
            (lambda _
              (let ((files (find-files "." "\\.py$")))
                (substitute* files
                  (("Image.ANTIALIAS")
                   "Image.LANCZOS")))

              (substitute* "UVR.py"
                (("BASE_PATH = os.path.dirname\\(os.path.abspath\\(__file__\\)\\)")
                 "BASE_PATH = os.path.expanduser('~/.uvr')"))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (share (string-append out
                                           "/share/ultimatevocalremovergui"))
                     (executable (string-append bin "/ultimatevocalremovergui"))
                     (python (search-input-file inputs "/bin/python3")))
                
                (mkdir-p share)
                (copy-recursively "." share)

                (mkdir-p bin)
                (with-output-to-file executable
                  (lambda ()
                    (format #t
                     "#!/bin/sh
UVR_HOME=\"$HOME/.uvr\"
mkdir -p \"$UVR_HOME\"

for item in gui_data lib_v5; do
  ln -sfn \"~a/$item\" \"$UVR_HOME/\"
done

for model_type in VR_Models MDX_Net_Models Demucs_Models; do
  STORE_MODEL_DIR=\"~a/models/$model_type\"
  if [ -d \"$STORE_MODEL_DIR\" ]; then
    find \"$STORE_MODEL_DIR\" -type d | while read -r d; do
      suffix=${d#\"~a\"}
      mkdir -p \"$UVR_HOME$suffix\"
    done
    find \"$STORE_MODEL_DIR\" -type f | while read -r f; do
      suffix=${f#\"~a\"}
      ln -sfn \"$f\" \"$UVR_HOME$suffix\"
    done
  fi
done

cd \"~a\"
exec \"~a\" UVR.py \"$@\"
"
                     share
                     share
                     share
                     share
                     share
                     python)))

                (chmod executable #o755)

                (wrap-program executable
                  `("GUIX_PYTHONPATH" ":" prefix
                    (,(getenv "GUIX_PYTHONPATH") ,share))
                  `("GI_TYPELIB_PATH" ":" prefix
                    (,(getenv "GI_TYPELIB_PATH")))
                  `("PATH" ":" prefix
                    (,(string-append (assoc-ref inputs "xrandr") "/bin")))
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append (assoc-ref inputs "libx11") "/lib") ,(string-append
                                                                           (assoc-ref
                                                                            inputs
                                                                            "libxrandr")
                                                                           "/lib"))))
                #t)))
          (add-after 'install 'add-desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (apps (string-append out "/share/applications"))
                     (pixmaps (string-append out "/share/pixmaps"))
                     (icon-source (string-append out
                                   "/share/ultimatevocalremovergui/gui_data/img/GUI-Icon.png")))
                (mkdir-p pixmaps)
                (copy-file icon-source
                           (string-append pixmaps "/uvr.png"))
                (mkdir-p apps)
                (with-output-to-file (string-append apps "/uvr.desktop")
                  (lambda ()
                    (format #t "[Desktop Entry]
Name=Ultimate Vocal Remover
Comment=Deep Neural Networks for Vocal Removal
Exec=~a/bin/ultimatevocalremovergui
Icon=uvr
Terminal=false
Type=Application
Categories=AudioVideo;Audio;AudioVideoEditing;
"
                            out))) #t))))))
    (inputs `(("python:tk" ,python "tk")
              ("python-pip" ,python-pip)
              ("gcc-toolchain" ,gcc)
              ("coreutils" ,coreutils)
              ("python-requests" ,python-requests)
              ("python-pytorch" ,python-pytorch)
              ("python-audioread" ,python-audioread)
              ("python-librosa" ,python-librosa)
              ("python-pygobject" ,python-pygobject)
              ("python-natsort" ,python-natsort)
              ("python-psutil" ,python-psutil)
              ("python-pyglet" ,python-pyglet)
              ("python-pyperclip" ,python-pyperclip)
              ("python-pillow" ,python-pillow)
              ("python-omegaconf" ,python-omegaconf)
              ("onnxruntime:python" ,onnxruntime "python")
              ("python-ml-collections" ,python-ml-collections)
              ("python-pydub" ,python-pydub)
              ("python-pytorch-lightning" ,python-pytorch-lightning)
              ("python-screeninfo" ,python-screeninfo)
              ("python-kthread" ,python-kthread)
              ("python-julius" ,python-julius)
              ("python-wget" ,python-wget)
              ("python-matchering" ,python-matchering)
              ("python-onnx2pytorch" ,python-onnx2pytorch)
              ("python-playsound" ,python-playsound)
              ("python-diffq" ,python-diffq)
              ("xrandr" ,xrandr)
              ("libxrandr" ,libxrandr)
              ("libx11" ,libx11)
              ("nvda" ,nvda)
              ("ffmpeg" ,ffmpeg)
              ("glu" ,glu)
              ("freetype" ,freetype)
              ("wget" ,wget)
              ("wget2" ,wget2)
              ("alsa-lib" ,alsa-lib)
              ("libsndfile" ,libsndfile)
              ("nss" ,nss)
              ("nss-certs" ,nss-certs)
              ("kmod" ,kmod)
              ("nvda-utils" ,nvda-utils)
              ("grep" ,grep)))
    (home-page "https://github.com/Anjok07/ultimatevocalremovergui")
    (synopsis "GUI for a Vocal Remover that uses Deep Neural Networks. ")
    (description
     "UltimateVocalRemoverGui uses state-of-the-art source separation models to remove vocals from audio files.")
    (license #f)))

ultimatevocalremovergui
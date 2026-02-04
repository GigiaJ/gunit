(define-module (modules gunit packages python-common)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gcc)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pulseaudio)
  #:use-module (guix git-download)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages nss)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages compression)
  #:use-module (guix licenses))

(define-public python-types-psutil
  (package
    (name "python-types-psutil")
    (version "7.2.2.20260130")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "types_psutil" version))
       (sha256
        (base32 "0l09b72xmgcfjwb2pqm8sq9ls2k21i4fi0y3wffcyh98qmlspc0m"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python/typeshed")
    (synopsis "Typing stubs for psutil")
    (description "Typing stubs for psutil.")
    (license #f)))

(define-public python-mypy-extensions
  (package
    (name "python-mypy-extensions")
    (version "1.1.0")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/python/mypy_extensions")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12a3qs6rckxljlgw8ylkgcgpwllz96rw82lrgmhlzdgqcnqhbl0w"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/python/mypy_extensions")
    (synopsis
     "Type system extensions for programs checked with the mypy type checker.")
    (description
     "Type system extensions for programs checked with the mypy type checker.")
    (license #f)))

(define-public python-librt
  (package
    (name "python-librt")
    (version "0.7.8")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mypyc/librt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fd5zjdbqvrwiqxqamh81z1q9l5s53v030dpgqc71c8cjjay4lhn"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mypyc/librt")
    (synopsis "Mypyc runtime library")
    (description "Mypyc runtime library.")
    (license #f)))

(define-public python-coverage
  (package
    (name "python-coverage")
    (version "7.13.2")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coveragepy/coveragepy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vf1qa2cndkrr0l6yc84frfmabr96vkly9nsw0n0cgxf10n1k23m"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/coveragepy/coveragepy")
    (synopsis "Code coverage measurement for Python")
    (description "Code coverage measurement for Python.")
    (license #f)))

(define-public python-julius
  (package
    (name "python-julius")
    (version "0.2.7")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "julius" version))
       (sha256
        (base32 "025v9xal2xjf9qqysvjrr7i0fvxf9hknn6cmriph3mnp0r9my3rw"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pytorch))
    (native-inputs (list python-coverage
                         python-flake8
                         python-mypy
                         onnxruntime
                         python-pdoc3
                         python-resampy
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/adefossez/julius")
    (synopsis
     "Nice DSP sweets: resampling, FFT Convolutions. All with PyTorch, differentiable and with CUDA support.")
    (description
     "Nice DSP sweets: resampling, FFT Convolutions.  All with @code{PyTorch},
differentiable and with CUDA support.")
    (license #f)))

(define-public python-kthread
  (package
    (name "python-kthread")
    (version "0.2.3")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kthread" version))
       (sha256
        (base32 "1d7r0id35xnwgwn5hpxzjr18r43w0flkwg8kqi03147zlzk99qch"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/munshigroup/kthread")
    (synopsis "Killable threads in Python!")
    (description "Killable threads in Python!")
    (license #f)))

(define-public python-matchering
  (package
    (name "python-matchering")
    (version "2.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sergree/matchering")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12mx9j37jfja0vclp1340xvbli1yf96y2whjxbxxhda61p6qi17n"))))
    (arguments
     '(#:tests? #f))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-resampy python-scipy
                             python-soundfile python-statsmodels))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/sergree/matchering")
    (synopsis "Audio Matching and Mastering Python Library")
    (description "Audio Matching and Mastering Python Library.")
    (license #f)))

(define-public python-wget
  (package
    (name "python-wget")
    (version "3.2")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wget" version ".zip"))
       (sha256
        (base32 "0qb0y7ipby42m4m7h0ipazpdyc3bn9xi46lvifcwwl5albn31rim"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel unzip))
    (home-page "https://bitbucket.org/techtonik/python-wget/")
    (synopsis "pure python download utility")
    (description "pure python download utility.")
    (license bsd-3)))

(define-public python-screeninfo
  (package
    (name "python-screeninfo")
    (version "0.8.1")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rr-/screeninfo")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q0nvj2ghbyjfymsbsl6dr7ggpg177fjncnzbxj18yglyz0vhk2c"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/rr-/screeninfo")
    (synopsis "Fetch location and size of physical screens.")
    (description "Fetch location and size of physical screens.")
    (license #f)))

(define-public python-diffq
  (package
    (name "python-diffq")
    (version "0.2.4")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/facebookresearch/diffq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07l7ws23xw8phlcmz0zca62lwzndp5wb2h0a545m9gxm7vfsavvm"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cython python-numpy python-pytorch))
    (native-inputs (list python-coverage
                         python-cython
                         python-flake8
                         python-pdoc3
                         python-setuptools
                         python-torchvision
                         python-wheel))
    (home-page "https://github.com/facebookresearch/diffq")
    (synopsis "Differentiable quantization framework for PyTorch.")
    (description "Differentiable quantization framework for @code{PyTorch}.")
    (license #f)))

(define-public python-onnx2pytorch
  (package
    (name "python-onnx2pytorch")
    (version "0.5.3")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ToriML/onnx2pytorch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iy0379nbc6db4gfks8rryi27y350cfjb6kv55aaxkkbg909p4pk"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list onnx python-pytorch python-torchvision))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ToriML/onnx2pytorch")
    (synopsis "Library to transform onnx model to pytorch.")
    (description "Library to transform onnx model to pytorch.")
    (license #f)))

(define-public python-playsound
  (package
    (name "python-playsound")
    (version "1.3.0")
    (arguments
     '(#:tests? #f))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TaylorSMarks/playsound")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jbq641lmb0apq4fy6r2zyag8rdqgrz8c4wvydzrzmxrp6yx6wyd"))))
    (build-system python-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/TaylorSMarks/playsound")
    (synopsis
     "Pure Python, cross platform, single function module with no dependencies for playing sounds.")
    (description
     "Pure Python, cross platform, single function module with no dependencies for
playing sounds.")
    (license #f)))
;; channel.scm
(use-modules (guix git))

(channel
(name 'nonguix)
(url "https://gitlab.com/nonguix/nonguix")
(introduction
  (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29 12AF 6F51 20A0 22FB B2D5"))))

(channel
  (name 'gchannel)
  (url "https://github.com/GigiaJ/Guix-Personal-Packages.git")
  (branch "main"))

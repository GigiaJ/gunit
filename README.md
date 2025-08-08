My *personal* channel where I build packages and service I use for GNU/Guix.

Channel declaration:
```
(channel
    (name 'gunit)
    (url "https://github.com/GigiaJ/gunit.git")
    (branch "channel")
    (make-channel-introduction
        "efaf0735519c85579b04baa0fb07d727aa55b128"
    (openpgp-fingerprint
          "AE6F 7F0F 6E0D AFB9 6E84  8994 C3A7 E8D7 2261 8435")))
```


## Packages
- **Browsers**
  - Microsoft Edge
  - Vivaldi
  - Floorp
- **Game Launchers**
  - Bolt Launcher
    - Technically the runescape-launcher is still not functional, however, you can use a flatpak for the actual GAME binary and replace the binary bolt launcher references with a small C program that simply launches the flatpak (which I've included in this repo). I'll attempt to package runescape-launcher (which installs the game client itself) later when I get Ghidra packaged correctly.
  - Deskflow
    - Currently, many display managers lack proper xdg-desktop-portal support whether as a result of just not being updated on Guix (KDE) or in Hyprland's case the features aren't completed and aren't added. I currently have a custom package for pulling in the PR branches to make it functional.
    It does, however, work fine on Gnome.
- **Editors**
  - Code server
    - In addition to code server I have a custom package that extends this and adds custom fonts. Not sure on how to elegantly make it modular, but it both allows you to simply install it and provides easy direction for adding your own fonts. Code server is a bit more nuanced than VS Code as it runs through your browser as far as fonts go.


---
These are more experimental ones thrown together either for specific behavior or to update without waiting on upstream.
- **Non-functional/Not added**
  - Runescape Launcher
  - PIA (Private Internet Access)
  - Ghidra
  - Nextcloud client on QT6
  - Discord
- **Hyprland w/ Input Capture Portal**
    - xdg-desktop-portal-hyprland
    - xdg-desktop-portal
    - hyprland-protocols
    - hyprland
    - libp11
    - libportal

## Services
- Code server

---

Credits to [Look](https://codeberg.org/look/saayix) for such a clear repo layout.
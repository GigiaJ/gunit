My *personal* channel where I build packages and service I use for GNU/Guix.

Channel declaration:
```
(channel
    (name 'gunit)
    (url "https://codeberg.org/Gigia/gunit.git")
    (branch "channel")
    (make-channel-introduction
        "efaf0735519c85579b04baa0fb07d727aa55b128"
    (openpgp-fingerprint
          "AE6F 7F0F 6E0D AFB9 6E84  8994 C3A7 E8D7 2261 8435")))
```

These are packages that even if upstreamed at some point, I'll upkeep for personal use.

## Packages
- **Browsers**
  - [Microsoft Edge](https://www.microsoft.com/en-us/edge/?form=MA13FJ)
    - I mean. Some companies require it for work.
  - [Vivaldi](https://vivaldi.com/)
    - Chromium based highly customizable browser based out of Sweden.
  - [Floorp](https://floorp.app/)
    - A Gecko based (Firefox) highly customizable browser based out of Japan. Best described as the Gecko/Firefox version of Vivaldi.
- **Game Launchers**
  - [Bolt Launcher](https://codeberg.org/Adamcake/Bolt)
    - Runs the Runescape Client in an FHS container, so the container relies on bolt to be the parent process. Mostly not an issue, but you can't close Bolt.
  - [Millennium](https://steambrew.app/)
    - A mixin to Steam that provides extensibility such as plugins, themes, and general behavior. 
- **Editors**
  - [Code server](https://coder.com/docs/code-server)
    - In addition to code server I have a custom package that extends this and adds custom fonts. Not sure on how to elegantly make it modular, but it both allows you to simply install it and provides easy direction for adding your own fonts. Code server is a bit more nuanced than VS Code as it runs through your browser as far as fonts go.
    - [IntelliJ IDEA](https://www.jetbrains.com/idea/)
      - The most feature complete IDE for writing Java.
- **AI**
  - [Ollama](https://ollama.com/)
    - A CLI tool that handles loading models and such for frontend tools. Best describable as Docker for LLMs. Tools like Continue for VSCode support it.
  - [Ultimate Vocal Remover](https://github.com/Anjok07/ultimatevocalremovergui)
    - A tool to remove the vocals (or instrumentals) from a song using AI models (NVIDIA only sadly)
- **Reverse Engineering**
  - [Ghidra](https://github.com/NationalSecurityAgency/ghidra)
    - Between this and IDA Pro, this is really the only open-source option that fits into the Guix ecosystem. Runs in an FHS container as Ghidra expects to find system libraries in their FHS locations. Uses the prebuilt binary as Ghidra is written in Java and uses Gradle as the build tool. Since Gradle is exceptionally difficult to package for on Guix it is more reasonable to do it this way. Not ideal, but the alternative would take an unreasonable amount of time.
- **Cloud platform**
  - [Azure CLI](https://github.com/Azure/azure-cli)
    - Runs in an FHS container and just unwraps there. Azure CLI is happy to touch K8 configs and so it makes more sense to isolate it anyway.
- **Video/Recording**
  - [OBS Droidcam](https://obsproject.com/forum/resources/droidcam-obs-camera.1308/)
    - Plugin for using your phone as a microphone and camera for OBS.

---
- **Firmware or files required for functionality**
  - Studio One 6
  - T2 Firmware (for Macs)

These are quite opinionated on where those local files live, so feel free to change those too.

---
These are more experimental ones thrown together either for specific behavior or to update without waiting on upstream.
- **Non-functional/Not added**
  - PIA (Private Internet Access)
    - This one is actually a bit challenging due to how VPN software interacts with the operating system's networking. Would likely need to be a system service and system package.
  - Nextcloud client on QT6
    - Many QT6 packages simply aren't updated and I didn't feel like updating them at the time
  - Discord
    - This one isn't particularly hard. I think the Linux client is an inferior experience to using the browser though.

---

## Services
  #### Home
  - Code server
  - Ollama

---

## Extension commands
- toys
  - An extension command that can be used like `guix toys -q "package-name"` or `guix toys -t channel -q "nonguix"`. It is just a lookup and parser for the website itself, but still handy to avoid having to leave editor or terminal to see if a package exists.

These are a neat feature not documented in the Guix Manual that I could find. They work through the basis of creating a module with the path in your channel like in this one root/modules/guix/scripts/extension-command-script.scm

---

Credits to [Look](https://codeberg.org/look/saayix) for such a clear repo layout.
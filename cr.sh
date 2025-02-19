guix shell --emulate-fhs --container --network --preserve='^DISPLAY$' --preserve='^XAUTHORITY$' --expose=$XAUTHORITY \
--preserve='^XDG_|^WAYLAND_DISPLAY$' --preserve='^DISPLAY$' --expose=/etc/machine-id \
--preserve='^DBUS_' --expose=/var/run/dbus --expose=/dev/dri --expose=/sys/class/net \
--share=/dev/snd/seq --share=/dev/shm --expose=/sys/class/input --expose=/sys/devices \
--expose=/sys/dev --expose=/sys/bus/pci --share=$HOME \
--expose=/run/user/"$(id -u)"/pulse --preserve='XDG_RUNTIME_DIR' --share=$HOME/.config/pulse \
bash glibc@2.39 gcc-toolchain gdk-pixbuf gtk+@2.24.33 glib strace libcap zlib cairo libsm \
libx11 gtk pango iputils libxxf86vm libglvnd sdl2 libxinerama libx11 libxext libxrandr mesa libxcursor alsa-lib \
pulseaudio openssl@1.1 coreutils sudo git traceroute nss-certs mediatek-firmware iproute2 gdb file patchelf wayland gawk diffutils dbus-glib elfutils eudev \
pciutils libva llvm alsa-plugins:pulseaudio

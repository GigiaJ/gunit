guix shell --emulate-fhs --container --network --preserve='^DISPLAY$' --preserve='^XAUTHORITY$' --expose=$XAUTHORITY \
--preserve='^XDG_|^WAYLAND_DISPLAY$' --preserve='^DISPLAY$' --expose=/etc/machine-id \
--preserve='^DBUS_' --expose=/var/run/dbus --expose=/dev/dri --expose=/sys/class/net \
--share=/dev/snd/seq --share=/dev/shm --expose=/sys/class/input --expose=/sys/devices --expose=/sys/dev --expose=/sys/bus/pci \
--expose=/sys/dev --expose=/sys/bus/pci --share=$HOME \
--expose=/run/user/"$(id -u)"/pulse --preserve='XDG_RUNTIME_DIR' --share=$HOME/.config/pulse \
gcc-toolchain gdk-pixbuf gtk+@2.24.33 glib strace libcap zlib cairo libsm \
libx11 gtk pango iputils libxxf86vm libglvnd sdl2 libxinerama libxext libxrandr mesa libxcursor alsa-lib \
pulseaudio openssl@1.1 coreutils sudo git traceroute nss-certs mediatek-firmware iproute2 gdb file patchelf wayland \
gawk diffutils dbus-glib elfutils eudev pciutils libva llvm alsa-plugins:pulseaudio


sudo setcap cap_net_raw,cap_net_admin,cap_sys_admin,cap_sys_ptrace,cap_sys_module,cap_sys_boot,cap_sys_time,cap_sys_tty_config,cap_syslog,cap_audit_control,cap_audit_write,cap_mac_admin,cap_mac_override,cap_mknod,cap_net_bind_service,cap_net_broadcast,cap_net_raw,cap_setgid,cap_setuid,cap_setpcap,cap_sys_chroot,cap_sys_nice,cap_sys_pacct,cap_sys_resource,cap_sys_rawio,cap_sys_time,cap_sys_tty_config,cap_syslog,cap_wake_alarm+ep /home/jaggar/Jagex/launcher/rs2client

guix shell -C -F -N -D ungoogled-chromium --expose=/etc/machine-id --pure --preserve='^DBUS_' \
--expose=/var/run/dbus --expose=/dev/dri --expose=/sys/devices --expose=/sys/dev --preserve='^DISPLAY$' --share=$HOME \
gcc-toolchain libglvnd sdl2 nss-certs gtk+@2.24.33 \
openssl@1.1 libsm gdb strace jbr ddd coreutils iputils shadow sudo \
-- /gnu/store/8f03xbxkm2g02n4fna653k82j405amsz-runescape-launcher-2.2.11/usr/bin/runescape-launcher
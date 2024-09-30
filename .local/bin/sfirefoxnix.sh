#!/usr/bin/env sh
exec bwrap --unshare-all --ro-bind /usr /usr \
     --ro-bind /etc /etc \
     --ro-bind /bin /bin \
     --ro-bind /lib /lib \
     --ro-bind /lib64 /lib64 \
     --tmpfs /tmp \
     --symlink usr/lib /lib \
     --symlink usr/lib64 /lib64 \
     --dev /dev \
     --dev-bind /dev/shm /dev/shm \
     --dev-bind /dev/pts /dev/pts \
     --proc /proc \
     --dir /home/firefox-sandboxed/Downloads /home/firefox-sandboxed/Downloads \
     --unshare-ipc --unshare-pid --unshare-uts \
     --unshare-net \
     --unshare-user \
     --unshare-cgroup \
     --ro-bind /var/run/dbus /var/run/dbus \
     --unshare-ipc \
     --unshare-pid \
     --unshare-uts \
     --unshare-cgroup \
     --ro-bind /var/run/dbus /var/run/dbus \
     --chdir / \
     /usr/bin/firefox --no-remote

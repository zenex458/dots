{ pkgs, lib, ... }:
let
  firefoxSandbox = pkgs.bubblewrapSandbox {
    home = "/home/firefox";
    extraOptions = [
      "--ro-bind=/home/user/Downloads /home/firefox/Downloads"
      "--symlink=/run/user/1000/wayland-0"
      "--ro-bind=/run/user/1000/wayland-0 /run/user/1000/wayland-0"
      "--ro-bind=/run/user/1000/pipewire-0 /run/user/1000/pipewire-0"
    ];
  };
in
{
  environment.systemPackages = with pkgs; [
    (pkgs.firefox.override { preFixup = "bwrap --ro-bind /dev /dev --ro-bind /etc/fonts /etc/fonts --ro-bind /etc/machine-id /etc/machine-id --ro-bind /usr /usr --ro-bind /var /var --symlink /tmp /var/tmp --symlink /run /var/run --proc /proc --ro-bind /usr/lib/libgtk-3.so.0 /usr/lib/libgtk-3.so.0 --ro-bind /usr/share/themes /usr/share/themes"; postFixup = ""; }) 
  ];
  boot.extraKernelParams = ["systemd.unified_cgroup_hierarchy=1"];
  system.activationScripts.bubblewrap = {
    text = ''
      #!/bin/sh
      exec ${firefoxSandbox} "$@"
    '';
    requires = [ "bubblewrap" ];
  };
  systemd.user.services.firefox-sandbox = {
    description = "Firefox sandbox";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.ExecStart = "${pkgs.stdenv.shell} -c 'bwrap ${pkgs.stdenv.shell} -c ${firefoxSandbox}'";
    serviceConfig.Restart = "on-failure";
    serviceConfig.RestartSec = "2";
    wantedBy = [ "graphical-session.target" ];
  };
}


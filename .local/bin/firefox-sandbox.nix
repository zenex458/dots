{ pkgs, lib, config, ... }:

let
  downloadsDir = "/home/<username>/Downloads";  # Replace <username> with your actual username

in
{
  options.services.firefox-sandbox = {
    enable = mkEnableOption "Enable the Firefox sandbox";

    user = config.users.users.<username>;

    # Define the sandbox environment
    # Note: Adjust the options based on your requirements
    sandboxOptions = {
      bindMounts = [
        # Only allow access to the downloads directory
        { hostPath = downloadsDir; targetPath = "/home/firefox/Downloads"; }
      ];

      extraOptions = [
        "--unshare-net"  # Unshare network namespace
        "--unshare-ipc"  # Unshare IPC namespace
      ];
    };
  };

  config = lib.mkIf config.services.firefox-sandbox.enable {
    security.bubblewrap.sandboxWrapper = {
      firefox = {
        exec = pkgs.firefox-bin;
        user = "firefox";
        args = [
          "--new-instance"              # Run in a new instance
          "--no-remote"                 # Don't connect to an existing Firefox instance
          "--class=Firefox-sandboxed"   # Set a unique class name for the sandboxed Firefox window
        ];
      };
    };

    users.users.<username>.services = {
      firefox-sandbox = {
        enable = true;
        sandboxOptions = config.services.firefox-sandbox.sandboxOptions;
      };
    };
  };
}


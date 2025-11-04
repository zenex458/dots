{...}: {
  imports = [
    ./disko-config.nix
    ./hardware-configuration.nix
    ../base.nix
  ];
  networking.hostName = "tetanus";

  services = {
    usbguard = {
      enable = true;
      presentControllerPolicy = "apply-policy";
      implicitPolicyTarget = "block";
      rules = ''
        allow id 1d6b:0002 serial "0000:03:00.3" name "xHCI Host Controller" hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" parent-hash "PeOe/xaA1IiqkrGiI1Zaa9uUS35iLSvdBe78BridqEQ=" with-interface 09:00:00 with-connect-type ""
        allow id 1d6b:0003 serial "0000:03:00.3" name "xHCI Host Controller" hash "BJI8loU2ltTPxc6wSrxp3Rzpt/xpSwuuJ/+QTie7fnI=" parent-hash "PeOe/xaA1IiqkrGiI1Zaa9uUS35iLSvdBe78BridqEQ=" with-interface 09:00:00 with-connect-type ""
        allow id 0951:1666 serial "408D5C15CB92E911290E05C5" name "DataTraveler 3.0" hash "2EaYKuXgzTb9sCQUwJZvP9dxQb0D4AguKGMqigouD5M=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface 08:06:50 with-connect-type "hotplug"
        allow id 05e3:0610 serial "" name "USB2.1 Hub" hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface 09:00:00 with-connect-type "hotplug"
        allow id 05e3:0610 serial "" name "USB2.0 Hub" hash "To7KDzOAgi4jFrnLNIttvUKO428/MLM1/eWqsv969gw=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface { 09:00:01 09:00:02 } with-connect-type "hardwired"
        allow id 05e3:0626 serial "" name "USB3.1 Hub" hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" parent-hash "BJI8loU2ltTPxc6wSrxp3Rzpt/xpSwuuJ/+QTie7fnI=" with-interface 09:00:00 with-connect-type "hotplug"
        allow id 2972:0047 serial "" name "FiiO K3" hash "YNur060AyFfKrnIT2qfA+GDscg0vNchYtt0Lh3j2Zt4=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface { 01:01:20 01:02:20 01:02:20 01:02:20 01:02:20 fe:01:01 01:01:20 01:02:20 01:02:20 01:02:20 01:02:20 fe:01:01 } with-connect-type "unknown"
        allow id 046d:c08b serial "1285335A3232" name "G502 HERO Gaming Mouse" hash "ukNMlamAkPMh7baihqjodyq1X2cF75bqoMTP6vnHADw=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface { 03:01:02 03:00:00 } with-connect-type "unknown"
        allow id 0951:16dc serial "" name "HyperX Alloy FPS RGB" hash "H/mSsemErhu6jSIjbiA0PEz4NYHdH0Q5juMtNDc0eGA=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface { 03:01:01 03:01:01 03:00:00 } with-connect-type "unknown"
        allow id 5986:2137 serial "" name "Integrated Camera" hash "eg+SlU0pANNmAjsl8cDYiULjq9l+rGJ1kbvX/N+2r/Y=" parent-hash "To7KDzOAgi4jFrnLNIttvUKO428/MLM1/eWqsv969gw=" with-interface { 0e:01:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 } with-connect-type "hardwired"
        allow id 0bc2:2343 serial "NACAPZXW" name "Portable" hash "uNlV1Q6teT1NaR89ByP5xMO1US495x/RUOVCAStUXtA=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface { 08:06:50 08:06:62 } with-connect-type "unknown"
        allow id 04e8:61fb serial "S6YJNJ0X301164A" name "PSSD T7 Shield" hash "m0oln1SciQzP6k4TPn9ZFC03YkSkbwUSkNQCHS9PvTw=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface { 08:06:50 08:06:62 } with-connect-type "unknown"
        allow id 0bda:0177 serial "20121112761000000" name "USB2.0-CRW" hash "e2uNi2LwAzeDYUbSBzd8VS6LqfpmZj/vnXkKxI8Fa4c=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface 08:06:50 with-connect-type "hardwired"
        allow id 16c0:27db serial "moergo.com:GLV80-4A2A8176E2496089" name "Glove80 Left" hash "aD39zz93dNP61EPXjHtlX+fg7EcNlc5a41B5pO+JDCU=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface 03:01:01 with-connect-type "unknown"
        allow id 090c:1000 serial "0320619110005669" name "Flash Drive" hash "HN91eZPfVx5LVYm22GQRriZM/HbCPF1fmILuq423EwQ=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface 08:06:50 with-connect-type "unknown"
        allow id 090c:1000 serial "0378623070002866" name "Flash Drive" hash "26QA1cD/Y0OQ39RG37alHNi4YKqSkA6hl+wiT+3SVzk=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface 08:06:50 with-connect-type "unknown"
        allow id 0781:5575 serial "04020130092220073710" name "Cruzer Glide" hash "q2jgfmoO2UHR5ZeeiwhacPfiBGPwGs+GCzv9vYk5efA=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface 08:06:50 with-connect-type "unknown"
      '';
    };
  };
}

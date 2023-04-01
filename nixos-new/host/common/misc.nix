{pkgs, ...}:{
  services.openssh.enable = true;

  networking.firewall = {
    allowedTCCPPorts = [
      22000 # syncthing
    ];
    allowedUDPPorts = [
      21027 # syncthing
      22000 # syncthing
    ];
  };

  security.sudo.extraRules = [
    {
      groups = ["wheel"];
      commands = [
        {
          command = "${pkgs.nixos-rebuild}/bin/nixos-rebuild";
          options = ["NOPASSWD"];
        }
        {
          command = "${pkgs.systemd}/bin/systemctl";
          options = ["NOPASSWD"];
        }
      ];
    }
  ];

  services.udisks2.enable = true;

  security.pki.certificates = [(builtins.readFile ../../misc/root_ca.crt)];
}
  

    

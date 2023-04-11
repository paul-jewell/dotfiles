{...}: {
  services.journald.extraConfig = [
    "SystemMaxUse=500"
  ];
}


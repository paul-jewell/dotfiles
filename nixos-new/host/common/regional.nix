{lib, ...}: {
  time.timeZone = lib.mkDefault "Europe/London";
  i18n.defaultLocale = "en_GB.UTF-8";
  console.keyMap = "uk";
}

{ config, pkgs, ...}:
let
    polybar-bars = builtins.readFile ./config/polybar/bars.ini;
    polybar-colors = builtins.readFile ./config/polybar/colors.ini;
    polybar-modules = builtins.readFile ./config/polybar/modules.ini;
    polybar-user_modules = builtins.readFile ./config/polybar/user_modules.ini;
in
{
    services.polybar = {
        enable = true;
        config = ./config/polybar/config.ini;
        extraConfig = polybar-bars + polybar-colors + polybar-modules + polybar-user_modules;
        package = pkgs.polybarFull;
        script = "polybar main &";
    };
}





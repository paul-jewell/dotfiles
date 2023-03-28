{ user, ...}:

let 
  home           = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
  xdg_dataHome   = "${home}/.local/share";
  xdg_stateHome  = "${home}/.local/state"; in
{
  "{xdg_configHome}/polybar/bin/popup-calendar.sh" = {
    executable = true;
    text = ''
      #!/bin/sh

      DATE="$(/run/current-system/sw/bin/date +""%B %d, %Y")"
      case "$1" in
      --popup)
          /etc/profiles/per-user/${user}/bin/yad --calendar --fixed \
            --posx=1800 --posy=80 --no-buttons --borders=0 --title="yad-calendar" \
            --close-on-unfocus
          ;;
      *)
          echo "$DATE"
          ;;
      esac
      '';
  };

  "${xdg_configHome}/polybar/bin/check-nixos-updates.sh" = {
    executable = true;
    text = ''
      #!/bin/sh
      
      /run/current-system/sw/bin/git -C ~${xdg_dataHome} ~/.local/share/src/nixpkgs fetch upstream master
      UPDATES=$(/run/current-system/sw/bin/git -C ~/.local/share/src/nixpkgs rev-list origin/master..upstream/master --count 2>/dev/null);
      /run/current-system/sw/bin/echo " $UPDATES"; # extra space for presentation with icon
      /run/current-system/sw/bin/sleep 1800;
    '';
  };
  

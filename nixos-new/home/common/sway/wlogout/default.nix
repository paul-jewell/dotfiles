{
  config,
  pkgs,
  ...
}: {
  programs.wlogout = {
    enable = true;
    layout = let
      swaymsg = "${pkgs.sway}/bin/swaymsg";
      gtklock = "${pkgs.gtklock}/bin/gtklock -d";
      systemctl = "${pkgs.systemd}/bin/systemctl";
    in [
      {
        label = "shutdown";
        action = "${systemctl} poweroff";
        text = "Shutdown";
        keybind = "s";
      }
      {
        label = "hibernate";
        action = "${systemctl} hibernate";
        text = "Hibernate";
        keybind = "h";
      }
      {
        label = "suspend";
        action = "${systemctl} suspend";
        text = "Suspend";
        keybind = "u";
      }
      {
        label = "exit";
        action = "${swaymsg} exit";
        text = "Exit";
        keybind = "e";
      }
      {
        label = "reboot";
        action = "${systemctl} reboot";
        text = "Reboot";
        keybind = "r";
      }
      {
        label = "lock";
        action = gtklock;
        text = "Lock";
        keybind = "l";
      }
    ];
    style = with config.theme.colors; ''
      window {
        font-family: "Fira Code";
        font-size: 10pt;
        color: #${text};
      }

      button {
        background-repeat: no-repeat;
        background-position: center;
        background-size: 25%;
        border: none;
        background-color: #${base};
      }

      button:hover {
        background-color: #${surface0};
      }

      button:focus {
        background-color: #${blue};
        color: #${base};
      }

      button:active {
        background-color: #${text};
        color: #${base};
      }

      #lock {
        background-image: image(url("${./lock.png}"));
      }

      #exit {
        background-image: image(url("${./exit-to-app.png}"));
      }

      #suspend {
        background-image: image(url("${./power-sleep.png}"));
      }

      #hibernate {
        background-image: image(url("${./power-cycle.png}"));
      }

      #shutdown {
        background-image: image(url("${./power.png}"));
      }

      #reboot {
        background-image: image(url("${./restart.png}"));
      }
    '';
  };
}

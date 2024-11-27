if status is-interactive
    # Commands to run in interactive sessions can go here
    set -gx TERM xterm-256color
    set fish_greeting ""

    # theme
    set -g theme_color_scheme terminal-dark
    set -g fish_prompt_pwd_dir_length 1
    set -g theme_display_user yes
    set -g theme_hide_hostname no
    set -g theme_hostname always

    if type -q eza
        alias ll "eza -l -g --icons"
        alias lla "ll -a"
    end

    # Keybindings

    direnv hook fish | source
    starship init fish | source
end

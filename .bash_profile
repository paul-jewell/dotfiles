# Bash startup:
#   Will load and execute the first of the following initialisation files it finds:
#   - .bash_profile
#   - .bash_login
#   - .profile
# Our configuration is held in .profile. If that isn't found, we execute .bashrc.
# Note: .bashrc is also executed at the end of .profile.

if [[ -f ~/.profile ]]; then
    . ./.profile
elif [[ -f ~/.bashrc ]] ; then
	. ~/.bashrc
fi



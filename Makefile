export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):./$(XDG_CONFIG_HOME)/guix/current/share/guile/site/3.0

.PHONY: zeus-home
zeus-home:
	guix home reconfigure ./home/zeus/core.scm

.PHONY: zeus-system
zeus-system:
	sudo -E guix system reconfigure ./system/zeus.scm

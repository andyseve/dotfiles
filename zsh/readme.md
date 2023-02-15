# Fixing nice failed on windows:
	add unsetopt BG_NICE /usr/etc/zshenv

# Fixing zplug unknown error:
	install gawk
	https://github.com/zplug/zplug/issues/359

# Run compinit only once a day
	
# Emulate zsh shell
	Functions that depend on zsh shell options can break if the options are changed in an interactive shell. It is important to reset all the options to defaults at the start of these functions. This is done using command
	emulate -LR zsh
	https://unix.stackexchange.com/questions/372779/when-is-it-necessary-to-include-emulate-lr-zsh-in-a-function-invoked-by-a-zsh

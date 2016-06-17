# export zplug home
ZPLUG_HOME="$HOME/.zplug"
ZPLUG_GIT="b4b4r07/zplug"
ZPLUG_CLONE_DEPTH=1

# clone zplug
if [[ ! -d $ZPLUG_HOME ]]; then
	git clone --depth=1 "https://github.com/$ZPLUG_GIT.git" "$ZPLUG_HOME/repos/$ZPLUG_GIT";
fi
source $ZPLUG_HOME/repos/$ZPLUG_GIT/zplug

# zplug manage itself
zplug "$ZPLUG_GIT"

# oh-my-zsh plugins
zplug "lib/clipboard", from:oh-my-zsh
zplug "lib/history", from:oh-my-zsh
zplug "lib/grep", from:oh-my-zsh
zplug "plugins/vi-mode", from:oh-my-zsh
zplug "plugins/git-flow-avh", from:oh-my-zsh
zplug "plugins/git-extra", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "plugins/archlinux", from:oh-my-zsh
zplug "plugins/systemd", from:oh-my-zsh
zplug "plugins/cp", from:oh-my-zsh
zplug "plugins/wd", from:oh-my-zsh
zplug "plugins/node", from:oh-my-zsh
zplug "plugins/npm", from:oh-my-zsh
zplug "plugins/adb", from:oh-my-zsh
zplug "plugins/web-search", from:oh-my-zsh

# theme
zplug "themes/frisk", from:oh-my-zsh

# others
zplug "tarruda/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting", nice:10

# install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
	printf "Install? [y/N]: "
	if read -q; then
		echo; zplug install
	fi
fi

# then, source plugins and add commands to $PATH
zplug load --verbose

# zsh colors loading
autoload -U colors && colors
setopt promptsubst

# load env source
if [[ -r $HOME/.env ]]; then
	source $HOME/.env
fi

# an optional source
if [[ -r $HOME/.optional-env ]]; then
	source $HOME/.optional-env
fi

# set timeout key for zsh
export KEYTIMEOUT=1


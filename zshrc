# export zplug home
ZPLUG_HOME="$HOME/.zplug"
ZPLUG_GIT="b4b4r07/zplug"

# clone zplug
if [[ ! -d $ZPLUG_HOME ]]; then
	git clone "https://github.com/b4b4r07/zplug.git" "$ZPLUG_HOME/repos/$ZPLUG_GIT";
	ln -s "$ZPLUG_HOME/repos/$ZPLUG_GIT/zplug" "$ZPLUG_HOME/zplug"
fi

source $ZPLUG_HOME/zplug

# zplug manage itself
zplug "$ZPLUG_GIT"

# oh-my-zsh plugins
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/git-flow", from:oh-my-zsh
zplug "plugins/git-extra", from:oh-my-zsh
zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "plugins/vi-mode", from:oh-my-zsh
zplug "plugins/arch", from:oh-my-zsh
zplug "plugins/tmux", from:oh-my-zsh
zplug "plugins/systemd", from:oh-my-zsh
zplug "plugins/cp", from:oh-my-zsh
zplug "plugins/node", from:oh-my-zsh

# after executing compinit command and sourcing other plugins
zplug "zsh-users/zsh-syntax-highlighting", nice: 10

# install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# then, source plugins and add commands to $PATH
zplug load --verbose

# load powerline
if [[ -r /usr/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh ]]; then
	source /usr/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh
fi

# set timeout key for zsh
export KEYTIMEOUT=1

# export PATH
export PATH="$PATH:$HOME/opt/bin:$HOME/opt/mongodb/bin:$HOME/opt/elasticsearch/bin:$HOME/opt/node/bin"

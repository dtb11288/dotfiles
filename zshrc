# export zplug home
ZPLUG_HOME="$HOME/.zplug"
ZPLUG_GIT="zplug/zplug"
zstyle :zplug:tag depth 1

# clone zplug
if [[ ! -d $ZPLUG_HOME ]]; then
  git clone --depth=1 "https://github.com/$ZPLUG_GIT.git" "$ZPLUG_HOME/repos/$ZPLUG_GIT";
fi
source $ZPLUG_HOME/repos/$ZPLUG_GIT/init.zsh

# zplug manage itself
zplug "$ZPLUG_GIT"

# oh-my-zsh plugins
zplug "lib/history", from:oh-my-zsh
zplug "lib/completion", from:oh-my-zsh
zplug "lib/correction", from:oh-my-zsh
zplug "plugins/vi-mode", from:oh-my-zsh
zplug "plugins/git-flow-avh", from:oh-my-zsh
zplug "plugins/git-extras", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "plugins/archlinux", from:oh-my-zsh
zplug "plugins/systemd", from:oh-my-zsh
zplug "plugins/cp", from:oh-my-zsh
zplug "plugins/wd", from:oh-my-zsh
zplug "plugins/node", from:oh-my-zsh
zplug "plugins/npm", from:oh-my-zsh
zplug "plugins/adb", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh
zplug "plugins/docker-machine", from:oh-my-zsh
zplug "plugins/docker-compose", from:oh-my-zsh
zplug "plugins/web-search", from:oh-my-zsh
zplug "plugins/rust", from:oh-my-zsh

# theme
zplug "themes/gentoo", from:oh-my-zsh

# others
zplug "chisui/zsh-nix-shell"
zplug "spwhitt/nix-zsh-completions"
zplug "tarruda/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

# then, source plugins and add commands to $PATH
zplug load

# zsh colors loading
autoload -U colors && colors
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down
bindkey '^[[Z' reverse-menu-complete # shift-tab
[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search
setopt promptsubst

# nix-shell prefix
prompt_nix_shell_setup

# change title bar
function precmd() {
  print -Pn "\e]2;%~\a"
}

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

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec ssh-agent startx
fi

eval "$(direnv hook zsh)"

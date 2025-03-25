{ pkgs, ... }:
{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    initExtra = "PS1='[$?] \\w \\n\\$ '";
    bashrcExtra = ''
      PATH=~/.local/bin:$PATH

      if command -v fzf-share >/dev/null; then
        source "$(fzf-share)/key-bindings.bash"
        source "$(fzf-share)/completion.bash"
      fi
    '';
    shellAliases = {
      ed = "emacs --daemon";
      e = "emacsclient -c & disown";
	    night-shift-on = "hyprsunset --temperature 3000 & disown";
	    night-shift-off = "pgrep hyprsunset | xargs kill";
	    ll = "ls -lah";
	    ff = "fastfetch";
      cdp = "cd $(cli-project-switcher | fzf)";
      vanilla-emacs = "emacs -q --load ~/projects/personal/vanilla-emacs/init.el";
    };
  };
}

{ pkgs, ... }:
{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    initExtra = "PS1='[$?] \\w \\$ '";
    shellAliases = {
      ed = "emacs --daemon";
      e = "emacsclient -c & disown";
	    night-shift-on = "hyprsunset --temperature 3000 & disown";
	    night-shift-off = "pgrep hyprsunset | xargs kill";
	    ll = "ls -lah";
	    ff = "fastfetch";
    };
  };
}

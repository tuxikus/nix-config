{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set -g fish_greeting ""
      set -g fish_color_quote green
      set -g fish_color_param magenta

      direnv hook fish | source
    '';
    shellAliases = {
      ed = "emacs --daemon";
      ecn = "emacsclient -c -nw";
    };
    functions = {
      fish_prompt = {
        body = ''
          set -l last_status $status
          set -l stat
          set stat "[$last_status]"
          string join ' ' -- $stat (prompt_pwd --full-length-dirs 4)
          echo -e '$ '
        '';
      };
      fish_right_prompt = {
        body = "date '+%H:%M'";
      };
    };
  };
}

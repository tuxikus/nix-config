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

      export FZF_COMPLETION_TRIGGER='*'
      export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    '';
    shellAliases = {
      ll = "ls -lah";
    };
  };
}

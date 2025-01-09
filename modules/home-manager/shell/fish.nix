{ config, pkgs, ... }:
{
  home.packages = [
    pkgs.fishPlugins.fzf-fish
    pkgs.fishPlugins.puffer
  ];

  programs.fish = {
    enable = true;
    shellAliases = {
      night-shift-on = "hyprsunset --temperature 3000 & disown";
      night-shift-off = "pgrep hyprsunset | xargs kill";
    };
    
    interactiveShellInit = ''
      fish_default_key_bindings

      function fish_prompt
        printf '%s %s%s%s \n > ' "[$status]" (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
      end

      direnv hook fish | source
    '';
  };
}
 

{ config, pkgs, ... }:
{
  home.packages = [
    pkgs.fishPlugins.fzf-fish
    pkgs.fishPlugins.puffer
  ];

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      fish_default_key_bindings

      function fish_prompt
        printf '%s %s%s%s \n > ' "[$status]" (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
      end

      function fish_right_prompt
        printf '%s' 🐟
      end
    '';
  };
}
 
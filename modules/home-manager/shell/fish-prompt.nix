{
  home.file.".config/fish/functions/fish_prompt" = {
    text = ''
      function fish_prompt
        printf '%s %s%s%s \n > ' "[$status]" (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
      end

      function fish_right_prompt
        printf '%s' 🐟
      end
    '';
  };
}

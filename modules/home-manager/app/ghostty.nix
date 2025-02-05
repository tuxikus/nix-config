{
  home.file.".config/ghostty/config" = {
    text = ''
      theme = tuxikus-almost-mono
      font-family = "Iosevka Nerd Font"
      font-family-bold = "Iosevka Nerd Font"
      font-family-italic = "Iosevka Nerd Font"
      font-family-bold-italic = "Iosevka Nerd Font"
      font-style = "Light"
      font-style-bold = "Light"
      font-style-italic = "Light"
      font-style-bold-italic = "Light"
      font-size = 15
      command = fish
      window-padding-x = 10
      window-padding-y = 10
      #window-decoration = none
      macos-titlebar-style = hidden
    '';  
  };

  home.file.".config/ghostty/themes/tuxikus-almost-mono" = {
    text = ''
      # tuxikus-almost-mono
      palette = 0=#000000
      palette = 1=#000000
      # string
      palette = 2=#3c5e2b
      palette = 3=#000000
      palette = 4=#000000
      palette = 5=#000000
      palette = 6=#000000
      palette = 7=#000000
      palette = 8=#000000
      palette = 9=#000000
      palette = 10=#000000
      palette = 11=#000000
      palette = 12=#000000
      palette = 13=#000000
      palette = 14=#000000
      palette = 15=#000000
      background = #ffffff
      foreground = #000000
      cursor-color = #000000
      cursor-text = #ffffff
      selection-background = #fcad03
      selection-foreground = #000000
    '';
  };
}

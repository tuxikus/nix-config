{
  home.file.".config/ghostty/config" = {
    text = ''
      # basic config
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
      shell-integration = fish

      # keys
      
    '';  
  };

  home.file.".config/ghostty/themes/tuxikus-almost-mono" = {
    text = ''
      # tuxikus-almost-mono
      palette = 0=#000000
      # red
      palette = 1=#5e2b5d
      # string - green
      palette = 2=#3c5e2b
      # yellow
      palette = 3=#c4b236
      # blue
      palette = 4=#2b4a5e
      # magenta
      palette = 5=#6e304d
      # cyan
      palette = 6=#b5cccb
      # gray
      palette = 7=#c5b99f
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
      selection-background = #fda50f
      selection-foreground = #000000
    '';
  };
}

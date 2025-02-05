{
  home.file.".config/ghostty/config" = {
    text = ''
      theme = tuxikus-almost-mono
      font-family = "Iosevka Nerd Font"
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
      palette = 0=#51576d
      palette = 1=#e78284
      palette = 2=#a6d189
      palette = 3=#e5c890
      palette = 4=#8caaee
      palette = 5=#f4b8e4
      palette = 6=#81c8be
      palette = 7=#a5adce
      palette = 8=#626880
      palette = 9=#e67172
      palette = 10=#8ec772
      palette = 11=#d9ba73
      palette = 12=#7b9ef0
      palette = 13=#f2a4db
      palette = 14=#5abfb5
      palette = 15=#b5bfe2
      background = #ffffff
      foreground = #000000
      cursor-color = #000000
      cursor-text = #ffffff
      selection-background = #fcad03
      selection-foreground = #000000
    '';
  };
}

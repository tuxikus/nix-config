{
  programs.zellij = {
    enable = true;
    #enableBashIntegration = true;
  };

  home.file.".config/zellij/config.kdl".text = ''
    theme "catppuccin-latte"
    default_shell "bash"
  '';

}

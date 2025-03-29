{
  programs.zellij = {
    enable = true;
    enableBashIntegration = true;
  };

  home.file.".config/zellij/config.kdl".text = ''
    default_mode "locked"
    theme "catppuccin-latte"
    default_shell "bash"
  '';
  
}

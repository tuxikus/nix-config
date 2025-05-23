{
  home.file.".config/ghostty/config" = {
    text = ''
      shell-integration = bash
      
      font-family = "Iosevka Nerd Font"
      font-size = 15
      
      window-padding-x = 10
      window-padding-y = 10
      
      macos-titlebar-style = hidden
      
      confirm-close-surface = false
      
      theme = BlulocoLight
      
      # emacs bindings
      keybind = ctrl+v=scroll_page_down
      keybind = alt+v=scroll_page_up
      
      keybind = ctrl+q=toggle_tab_overview
      
      keybind = ctrl+x>3=new_split:right
      keybind = ctrl+x>2=new_split:down
    '';
  };
}

{
  home.file.".config/ghostty/config" = {
    text = ''
      window-padding-x = 10
      window-padding-y = 10
      macos-titlebar-style = hidden
      confirm-close-surface = false
      theme = BlulocoLight
      font-family = "Iosevka Nerd Font"
      font-family-bold = "Iosevka Nerd Font"
      font-family-italic = "Iosevka Nerd Font"
      font-family-bold-italic = "Iosevka Nerd Font"
      font-style = "Light"
      font-style-bold = "Light"
      font-style-italic = "Light"
      font-style-bold-italic = "Light"
      font-size = 15
      command = xonsh
      keybind = alt+w=copy_to_clipboard
      keybind = ctrl+y=paste_from_clipboard
      
      keybind = ctrl+v=scroll_page_down
      keybind = alt+v=scroll_page_up
      
      keybind = ctrl+t=new_tab
    '';
  };
}

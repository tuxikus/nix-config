{
  home.file.".config/hypr/hyprlock.conf" = {
    text = ''
      background {
          blur_size = 5
          blur_passes = 3
          noise = true
          contrast = 1.0
          brightness = 0.9
          vibrancy = 0.3
      }
      
      # Clock (optional)
      label {
          text = $TIME
          font_family = Iosevka Nerd Font
          font_size = 64
          color = rgba(255,255,255,0.8)
          position = 0, 300
          halign = center
          valign = center
      }
      
      # Password prompt
      input-field {
          size = 200, 40
          outline_thickness = 2
          dots_size = 0.2
          dots_spacing = 0.2
          dots_center = true
          outer_color = rgba(255,255,255,0.2)
          inner_color = rgba(255,255,255,0.05)
          font_family = Iosevka Nerd Font
          font_size = 18
          placeholder_text = Password...
          hide_input = false
          position = 0, 100
          halign = center
          valign = center
      }
    '';
  };
}

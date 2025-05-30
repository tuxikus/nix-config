let
  waybarConfigDirectory = ".config/waybar";
in
{
  home.file."${waybarConfigDirectory}/config".text = ''
    {
        "layer": "top", // Waybar at top layer
        "position": "top", // Waybar position (top|bottom|left|right)
        "modules-left": ["hyprland/workspaces"],
        "modules-center": ["custom/music"],
        "modules-right": ["", "clock", "tray"],
        "clock": {
          "timezone": "Europe/Berlin",
          "tooltip-format": "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>",
          "format-alt": "  {:%d/%m/%Y} ",
          "format": "  {:%H:%M} "
        },
        "pulseaudio": {
          // "scroll-step": 1, // %, can be a float
          "format": "{icon} {volume}%",
          "format-muted": "  ",
          "format-icons": {
              "default": ["", "", ""]
          },
        },
    }
  '';

  home.file."${waybarConfigDirectory}/style.css".text = ''
    * {
        font-size: 15px;
        font-family: "Iosevka Nerd Font";
        color: white;
    }
    
    window#waybar {
        background: black;
    }
    
    #workspaces button {
        background-color: black;
        color: white;
    }
    
    #workspaces button:hover {
        background-color: white;
        color: black;
    }
    
    #workspaces button.focused {
        background-color: black;
        color: white;
    }
  '';
}

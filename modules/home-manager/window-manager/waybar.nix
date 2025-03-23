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
      "modules-right": ["pulseaudio", "clock", "tray"],
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
      font-size: 20px;
      font-family: "Iosevka Nerd Font";
  }
  
  window#waybar {
      background: rgba(0,0,0,1.0);
  }
  
  #window {
      color: #c5c8c6;
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
      background-color: white;
      color: black;
  }
  
  #custom-notification {
      font-family: "Fira Code";
  }
  
  #clock,
  #pulseaudio,
  #workspaces {
      background: black;
      color: white;
  }
  '';
}

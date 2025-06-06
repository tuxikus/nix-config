{ config, pkgs, lib, ... }:
let
  hyprConfigDirectory = ".config/hypr";
in
{
  options = {
    terminal = lib.mkOption {
          type = lib.types.str;
    };
    appLauncher = lib.mkOption {
          type = lib.types.str;
    };
  };

  config = {
    home.file."${hyprConfigDirectory}/hyprland.conf" = {
          text = ''
            exec-once = waybar
            exec-once = hyprpaper
            exec-once = dunst
            exec-once = wl-paste --type text --watch cliphist store # Stores only text data
            #exec-once = wl-paste --type image --watch cliphist store # Stores only image data
            
            $terminal = ${config.terminal}
            $app_launcher = ${config.appLauncher}
            
            env = XCURSOR_SIZE,24
            env = QT_QPA_PLATFORMTHEME,qt5ct
            
            monitor = eDP-1, 1920x1080@60, 0x0, 1
            monitor = DP-3, 2560x1440@60, 1920x0, 1
            
            input {
                kb_layout = us
                kb_variant =
                kb_model =
                kb_options =
                kb_rules =
            
                follow_mouse = 1
            
                touchpad {
                    natural_scroll = no
                }
            
                sensitivity = 0
                accel_profile = flat
            }
            
            general {
                gaps_in = 10
                gaps_out = 10
                border_size = 3
                col.active_border = rgba(aa0000ff)
                col.inactive_border = rgba(aaaaaaff)
            
                layout = dwindle
            
                allow_tearing = false
            }
            
            decoration {
                rounding = 10
            
                blur {
                    enabled = true
                    size = 3
                    passes = 1
                }
            }
            
            animations {
                enabled = false
                bezier = myBezier, 0.05, 0.9, 0.1, 1.05
                animation = windows, 1, 7, myBezier
                animation = windowsOut, 1, 7, default, popin 80%
                animation = border, 1, 10, default
                animation = borderangle, 1, 8, default
                animation = fade, 1, 7, default
                animation = workspaces, 1, 6, default
            }
            
            dwindle {
                pseudotile = yes
                preserve_split = yes
            }
            
            misc {
                force_default_wallpaper = -1
            }
            
            $mainMod = SUPER
            
            bind = $mainMod, q, killactive
            
            bind = $mainMod CTRL, q, exec, hyprlock
            bind = $mainMod SHIFT, e, exit
            bind = $mainMod, m, fullscreen
            bind = $mainMod, d, exec, $app_launcher
            bind = $mainMod, o, movecurrentworkspacetomonitor, +1
            bind = $mainMod, s, exec, grim -g "$(slurp)"
            bind = $mainMod SHIFT, s, exec, grim
            bind = $mainMod, h, exec, cliphist list | fuzzel --dmenu | cliphist decode | wl-copy
            bind = $mainMod SHIFT, h, exec, cliphist list | fuzzel --dmenu | cliphist delete
            
            bind = $mainMod, left, movefocus, l
            bind = $mainMod, right, movefocus, r
            bind = $mainMod, up, movefocus, u
            bind = $mainMod, down, movefocus, d
            
            bind = $mainMod SHIFT, left, movewindow, l
            bind = $mainMod SHIFT, right, movewindow, r
            bind = $mainMod SHIFT, up, movewindow, u
            bind = $mainMod SHIFT, down, movewindow, d
            
            bind = $mainMod, 1, workspace, 1
            bind = $mainMod, 2, workspace, 2
            bind = $mainMod, 3, workspace, 3
            bind = $mainMod, 4, workspace, 4
            bind = $mainMod, 5, workspace, 5
            bind = $mainMod, 6, workspace, 6
            bind = $mainMod, 7, workspace, 7
            bind = $mainMod, 8, workspace, 8
            bind = $mainMod, 9, workspace, 9
            bind = $mainMod, 0, workspace, 10
            
            bind = $mainMod SHIFT, 1, movetoworkspace, 1
            bind = $mainMod SHIFT, 2, movetoworkspace, 2
            bind = $mainMod SHIFT, 3, movetoworkspace, 3
            bind = $mainMod SHIFT, 4, movetoworkspace, 4
            bind = $mainMod SHIFT, 5, movetoworkspace, 5
            bind = $mainMod SHIFT, 6, movetoworkspace, 6
            bind = $mainMod SHIFT, 7, movetoworkspace, 7
            bind = $mainMod SHIFT, 8, movetoworkspace, 8
            bind = $mainMod SHIFT, 9, movetoworkspace, 9
            bind = $mainMod SHIFT, 0, movetoworkspace, 10
            
            # Move/resize windows with mainMod + LMB/RMB and dragging
            bindm = $mainMod, mouse:272, movewindow
            bindm = $mainMod, mouse:273, resizewindow
          '';
    };
  };
}

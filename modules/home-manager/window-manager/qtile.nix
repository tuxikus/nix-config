{ config, lib, ... }:
{
  options = {
    qtileWallpaper = lib.mkOption {
      type = lib.types.path;
    };
  };
  config = {
    home.file = {
      ".config/qtile/config.py".text = ''
        import os
        import subprocess
        
        from libqtile import bar, layout, qtile, widget, hook
        from libqtile.config import Click, Drag, Group, Key, Match, Screen
        from libqtile.lazy import lazy
        from libqtile.backend.wayland import InputConfig
        
        @hook.subscribe.startup_once
        def autostart():
            home = os.path.expanduser('~/.config/qtile/autostart.sh')
            subprocess.call(home)
        
        mod = "mod4"
        terminal = "ghostty"
        app_launcher = "fuzzel"
        
        keys = [
            Key([mod], "left", lazy.layout.left(), desc="Move focus to left"),
            Key([mod], "right", lazy.layout.right(), desc="Move focus to right"),
            Key([mod], "down", lazy.layout.down(), desc="Move focus down"),
            Key([mod], "up", lazy.layout.up(), desc="Move focus up"),
        
            Key([mod, "shift"], "left", lazy.layout.shuffle_left(), desc="Move window to the left"),
            Key([mod, "shift"], "right", lazy.layout.shuffle_right(), desc="Move window to the right"),
            Key([mod, "shift"], "down", lazy.layout.shuffle_down(), desc="Move window down"),
            Key([mod, "shift"], "up", lazy.layout.shuffle_up(), desc="Move window up"),
        
            # Toggle between split and unsplit sides of stack.
            # Split = all windows displayed
            # Unsplit = 1 window displayed, like Max layout, but still with
            # multiple stack panes
            Key(
                [mod, "shift"],
                "Return",
                lazy.layout.toggle_split(),
                desc="Toggle between split and unsplit sides of stack",
            ),
            Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
            # Toggle between different layouts as defined below
            Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
            Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
            Key(
                [mod],
                "m",
                lazy.window.toggle_fullscreen(),
                desc="Toggle fullscreen on the focused window",
            ),
        
            Key([mod], "d", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
            #Key([mod], "d", lazy.spawn(app_launcher), desc="Spawn fuzzel"),
        ]
        
        # Add key bindings to switch VTs in Wayland.
        # We can't check qtile.core.name in default config as it is loaded before qtile is started
        # We therefore defer the check until the key binding is run by using .when(func=...)
        for vt in range(1, 8):
            keys.append(
                Key(
                    ["control", "mod1"],
                    f"f{vt}",
                    lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
                    desc=f"Switch to VT{vt}",
                )
            )
        
        
        groups = [Group(i) for i in "123456789"]
        
        for i in groups:
            keys.extend(
                [
                    # mod + group number = switch to group
                    Key(
                        [mod],
                        i.name,
                        lazy.group[i.name].toscreen(),
                        desc=f"Switch to group {i.name}",
                    ),
                    # mod + shift + group number = switch to & move focused window to group
                    Key(
                        [mod, "shift"],
                        i.name,
                        lazy.window.togroup(i.name, switch_group=True),
                        desc=f"Switch to & move focused window to group {i.name}",
                    ),
                    # Or, use below if you prefer not to switch to that group.
                    # # mod + shift + group number = move focused window to group
                    # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
                    #     desc="move focused window to group {}".format(i.name)),
                ]
            )
        
        layouts = [
            layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4, margin=5),
            layout.Max(),
            # Try more layouts by unleashing below layouts.
            # layout.Stack(num_stacks=2),
            # layout.Bsp(),
            # layout.Matrix(),
            # layout.MonadTall(),
            # layout.MonadWide(),
            # layout.RatioTile(),
            # layout.Tile(),
            # layout.TreeTab(),
            # layout.VerticalTile(),
            # layout.Zoomy(),
        ]
        
        widget_defaults = dict(
            font="Iosevka Nerd Font",
            fontsize=15,
            padding=10,
        )
        extension_defaults = widget_defaults.copy()
        
        screens = [
            Screen(
                wallpaper="${config.qtileWallpaper}",
                wallpaper_mode="fill",
                top=bar.Bar(
                    [
                        widget.CurrentLayout(),
                        widget.GroupBox(),
                        widget.Prompt(),
                        widget.WindowName(),
                        widget.Chord(
                            chords_colors={
                                "launch": ("#ff0000", "#ffffff"),
                            },
                            name_transform=lambda name: name.upper(),
                        ),
                        # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                        widget.StatusNotifier(),
                        # widget.Systray(),
                        widget.CPU(),
                        widget.Memory(),
                        widget.PulseVolume(),
                        widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                        widget.QuickExit(),
                    ],
                    32,
                    # border_width=[0, 0, 2, 0],  # Draw top and bottom borders
                    # border_color=["000000", "000000", "000000", "000000"]  # Borders are magenta
                ),
                # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
                # By default we handle these events delayed to already improve performance, however your system might still be struggling
                # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
                # x11_drag_polling_rate = 60,
            ),
        ]
        
        # Drag floating layouts.
        mouse = [
            Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
            Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
            Click([mod], "Button2", lazy.window.bring_to_front()),
        ]
        
        dgroups_key_binder = None
        dgroups_app_rules = []  # type: list
        follow_mouse_focus = True
        bring_front_click = False
        floats_kept_above = True
        cursor_warp = False
        floating_layout = layout.Floating(
            float_rules=[
                # Run the utility of `xprop` to see the wm class and name of an X client.
                *layout.Floating.default_float_rules,
                Match(wm_class="confirmreset"),  # gitk
                Match(wm_class="makebranch"),  # gitk
                Match(wm_class="maketag"),  # gitk
                Match(wm_class="ssh-askpass"),  # ssh-askpass
                Match(title="branchdialog"),  # gitk
                Match(title="pinentry"),  # GPG key password entry
            ]
        )
        auto_fullscreen = True
        focus_on_window_activation = "smart"
        reconfigure_screens = True
        
        # If things like steam games want to auto-minimize themselves when losing
        # focus, should we respect this or not?
        auto_minimize = True
        
        # get by running: qtile cmd-obj -o core -f get_inputs
        wl_input_rules = {
           "1133:16519:Logitech G903 LS": InputConfig(accel_profile='flat'),
        }
        
        # xcursor theme (string or None) and size (integer) for Wayland backend
        wl_xcursor_theme = None
        wl_xcursor_size = 24
        
        # XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
        # string besides java UI toolkits; you can see several discussions on the
        # mailing lists, GitHub issues, and other WM documentation that suggest setting
        # this string if your java app doesn't work correctly. We may as well just lie
        # and say that we're a working one by default.
        #
        # We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
        # java that happens to be on java's whitelist.
        wmname = "LG3D"
      '';

      ".config/qtile/autostart.sh" = {
        text = ''
          #!/usr/bin/env sh
          wlr-randr --output DP-3 --mode 2560x1440@144 &
          dunst &
        '';
        executable = true;
      };
    };
  };
}

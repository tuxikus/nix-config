{
  home.file.".config/wezterm/wezterm.lua" = {
    text = ''
      local wezterm = require "wezterm"
      local config = wezterm.config_builder()
      
      config.initial_cols = 120
      config.initial_rows = 28
      
      config.font_size = 15
      config.font = wezterm.font "Iosevka Nerd Font"
      
      config.color_scheme = "3024 (light) (terminal.sexy)"
      
      config.window_padding = {
         left = 10,
         right = 10,
         top = 10,
         bottom = 10,
      }
      
      config.tab_bar_at_bottom = true
      config.use_fancy_tab_bar = false
      config.tab_and_split_indices_are_zero_based = true
      
      config.quick_select_alphabet = "dvorak"
      
      config.default_prog = { "bash" }
      
      config.leader = { key = "C", mods = "CTRL", timeout_milliseconds = 2000 }
      
      config.keys = {
         { mods = "LEADER", key = "0", action = wezterm.action.ActivateTab(0) },
         { mods = "LEADER", key = "1", action = wezterm.action.ActivateTab(1) },
         { mods = "LEADER", key = "2", action = wezterm.action.ActivateTab(2) },
         { mods = "LEADER", key = "3", action = wezterm.action.ActivateTab(3) },
         { mods = "LEADER", key = "4", action = wezterm.action.ActivateTab(4) },
         { mods = "LEADER", key = "5", action = wezterm.action.ActivateTab(5) },
         { mods = "LEADER", key = "6", action = wezterm.action.ActivateTab(6) },
         { mods = "LEADER", key = "7", action = wezterm.action.ActivateTab(7) },
         { mods = "LEADER", key = "8", action = wezterm.action.ActivateTab(8) },
         { mods = "LEADER", key = "9", action = wezterm.action.ActivateTab(9) },
         { mods = "LEADER", key = "s", action = wezterm.action.Search { CaseInSensitiveString = "" } },
         { mods = "LEADER", key = "l", action = wezterm.action.ShowLauncher },
         { mods = "LEADER", key = "q", action = wezterm.action.QuickSelect },
         { mods = "LEADER", key = "|", action = wezterm.action.SplitHorizontal },
         { mods = "LEADER", key = "-", action = wezterm.action.SplitVertical },
         { mods = "LEADER", key = "t", action = wezterm.action.ShowTabNavigator },
         { mods = "LEADER", key = "p", action = wezterm.action.ActivateCommandPalette },
         { mods = "LEADER", key = "c", action = wezterm.action.SpawnTab "CurrentPaneDomain",},
         { mods = "LEADER", key = "x", action = wezterm.action.CloseCurrentPane { confirm = true },},
         { key = "r",
           mods = "LEADER",
           action = wezterm.action.PromptInputLine {
              description = "Enter new name for tab",
              initial_value = "x",
              action = wezterm.action_callback(function(window, pane, line)
                    -- line will be `nil` if they hit escape without entering anything
                    -- An empty string if they just hit enter
                    -- Or the actual line of text they wrote
                    if line then
                       window:active_tab():set_title(line)
                    end
              end),
           },
         },
      }
      
      config.launch_menu = {
         { args = { "top" }, },
         { label = "Bash",
           args = { "bash", "-l" },
           -- cwd = "some/path",
           -- set_environment_variables = { FOO = "bar" },
         }
      }
      
      -- tab bar
      function tab_title(tab_info)
         local title = tab_info.tab_title
         if title and #title > 0 then
            return title
         end
         return tab_info.active_pane.title
      end
      
      wezterm.on(
         "format-tab-title",
         function(tab, tabs, panes, config, hover, max_width)
            local title = tab_title(tab)
            if tab.is_active then
               return {
                  { Background = { Color = "blue" } },
                  { Text = "[" .. title .. "]" },
               }
            end
            if tab.is_last_active then
               return {
                  { Background = { Color = "green" } },
                  { Text = " " .. title .. "*" },
               }
            end
            return title
         end
      )
      
      return config
    '';
  };
}

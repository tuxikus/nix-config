{
  home.file.".config/wezterm/wezterm.lua" = {
    text = ''
      local wezterm = require 'wezterm'
      local config = wezterm.config_builder()
      
      config.initial_cols = 120
      config.initial_rows = 28
      
      config.font_size = 15
      config.font = wezterm.font 'Iosevka Nerd Font'
      
      config.color_scheme = '3024 (light) (terminal.sexy)'
      
      config.window_padding = {
        left = 10,
        right = 10,
        top = 10,
        bottom = 10,
      }
      
      config.use_fancy_tab_bar = false
      config.tab_and_split_indices_are_zero_based = true
      
      config.quick_select_alphabet = 'dvorak'
      
      config.default_prog = { 'bash' }
      
      config.leader = { key = 'C', mods = 'CTRL', timeout_milliseconds = 2000 }
      
      config.keys = {
        { mods = 'LEADER', key = 'l', action = wezterm.action.ShowLauncher },
        { mods = 'LEADER', key = 'p', action = wezterm.action.ActivateCommandPalette },
        { mods = 'LEADER', key = 'c', action = wezterm.action.SpawnTab 'CurrentPaneDomain',},
        { mods = 'LEADER', key = 'x', action = wezterm.action.CloseCurrentPane { confirm = true },},
      }
      
      config.launch_menu = {
        { args = { 'top' }, },
        { label = 'Bash',
          args = { 'bash', '-l' },
          -- cwd = 'some/path',
          -- set_environment_variables = { FOO = 'bar' },
        }
      }
      
      return config
    '';
  };
}

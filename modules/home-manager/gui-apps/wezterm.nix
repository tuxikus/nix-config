{
  home.file.".config/wezterm/wezterm.lua" = {
    text = ''
      local wezterm = require 'wezterm'
      local config = wezterm.config.builder()
      
      config.initial_cols = 120
      config.initial_rows = 28
      
      config.font_size = 12
      config.font = wezterm.font 'Iosevka Nerd Font'
      
      config.color_scheme = '3024 (light) (terminal.sexy)'
      
      config.window_padding = {
        left = 2,
        right = 2,
        top = 0,
        bottom = 0,
      }
      
      config.ui_key_cap_rendering = 'Emacs'
      
      config.quick_select_alphabet = 'dvorak'
      
      config.default_prog = { 'bash' }
      
      config.keys = {
        { key = 'l', mods = 'CTRL|SHIFT', action = wezterm.action.ShowLauncher },
      }
      
      config.launcher_menu = {
        { args = { 'top' }, },
        { label = 'Bash',
          args = { 'bash', '-l' },
          -- cwd = 'some/path',
          -- set_environment_variables = { FOO = 'bar' },
        }'
      }
    '';
  };
}

{
  programs.tmux = {
    enable = true;
    shortcut = "z";
    newSession = true;
    escapeTime = 0;
    terminal = "xterm-256color";

    extraConfig = ''
    # vim like pane resizing
    bind -r C-k resize-pane -U
    bind -r C-j resize-pane -D
    bind -r C-h resize-pane -L
    bind -r C-l resize-pane -R
    
    # vim like pane switching
    bind -r k select-pane -U
    bind -r j select-pane -D
    bind -r h select-pane -L
    bind -r l select-pane -R
    
    unbind Up
    unbind Down
    unbind Left
    unbind Right
    
    unbind C-Up
    unbind C-Down
    unbind C-Left
    unbind C-Right
    
    # easy-to-remember split pane commands
    bind | split-window -h -c "#{pane_current_path}"
    bind - split-window -v -c "#{pane_current_path}"
    bind c new-window -c "#{pane_current_path}"
    '';
  };
}

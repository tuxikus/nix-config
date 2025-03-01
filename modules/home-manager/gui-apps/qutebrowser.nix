{ pkgs, ... }:
let
  qutebrowserConfigLocation = if pkgs.system == "x86_64-linux"
                              then ".config/qutebrowser/config.py"
                              else ".qutebrowser/config.py";
in
{
  home.file.${qutebrowserConfigLocation}.text = ''
    config.load_autoconfig(False)
    c.tabs.position = "left"
    c.content.blocking.method = 'both'
  '';
}

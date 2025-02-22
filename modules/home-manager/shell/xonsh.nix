{ pkgs, ... }:
{
  home.packages = with pkgs; [
    xonsh
  ];

  home.file.".config/xonsh/rc.xsh" = {
    text = ''
      $PROMPT = '{RED}{last_return_code_if_nonzero:[{BOLD_INTENSE_RED}{}{RED}] }{RESET} {YELLOW}{env_name}{RESET}{GREEN} {cwd}{branch_color}{curr_branch: {}}{RESET} {BOLD_BLUE}{prompt_end}{RESET} '

      aliases['ll'] = 'ls -lah'
      aliases['ff'] = 'fastfetch'
      aliases['night-shift-on'] = 'hyprsunset --temperature int(3000) & disown'
      aliases['night-shift-off'] = 'pgrep hyprsunset | xargs kill'
    '';
  };
}
 

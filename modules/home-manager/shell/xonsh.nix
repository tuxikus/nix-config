{ pkgs, ... }:
{
  home.packages = with pkgs; [
    xonsh
  ];

  home.file.".config/xonsh/rc.xsh" = {
    text = ''
	$PROMPT = '{RED}{last_return_code_if_nonzero:[{BOLD_INTENSE_RED}{}{RED}] }{RESET} {YELLOW}{env_name}{RESET}{GREEN} {cwd}{branch_color}{curr_branch: {}}{RESET} {BOLD_BLUE}{prompt_end}{RESET} '
	
	$PATH.append("~/.local/bin/")
	
	aliases['ll'] = 'ls -lah'
	aliases['ff'] = 'fastfetch'
	aliases['pyvc'] = 'python3 -m venv venv'
	aliases['pyva'] = 'source-bash venv/bin/activate'
	aliases['pip-freeze'] = 'python3 -m pip freeze > requirements.txt'
    '';
  };
}

{ lib, config, ...}:
{
  options = {
    projectDirectories = lib.mkOption {
      type = lib.types.str;
    };
  };

  config = {
    home.file.".local/bin/cli-project-switcher" = {
      text = ''
        #!/usr/bin/env python3
        
        from pathlib import Path
        from glob import glob
        
        PROJECT_CONFIG_FILE = Path.home() / ".config" / "cli-project-switcher" / "config"
        
        def check_project_config_file():
            if not PROJECT_CONFIG_FILE.is_file():
                raise FileNotFoundError()
        
        def read_project_dirs():
            with open(PROJECT_CONFIG_FILE, "r") as f:
                content = f.read()
        
            non_empty_lines = []
        
            for line in content.split("\n"):
                if line != "":
                    non_empty_lines.append(line)
        
            return non_empty_lines
        
        def main():
            try:
                check_project_config_file()
            except:
                print(f"{PROJECT_CONFIG_FILE} not found")
                exit(1)
        
            for project_dir in read_project_dirs():
                try:
                    for project in Path(project_dir.replace("~", str(Path.home()))).iterdir():
                        print(project)
                except:
                    pass
        
        if __name__ == "__main__":
            main()
        
      '';
      executable = true;
    };

    home.file.".config/cli-project-switcher/config".text = ''
      ${config.projectDirectories}
    '';
  };
}

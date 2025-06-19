{
  home.file.".local/bin/restore-home-backup" = {
    text = ''
      #!/usr/bin/env bash
      # Title          : restore-home-backup
      # Date           : 2025-06-19
      # Author         : tuxikus
      # Version        : 1.0
      # Description    : Restore backup of home directory
      # Options        : -b backup root
      #                  --help print help
      #                  --version print version
      
      version="1.0"
      backup_root=
      enable_dry_run=
      options=
      excludes="--exclude=lost+found/ --exclude=*/lost+found/ --exclude=.Trash-*/ --exclude=*/.Trash-*/"
      
      print_help() {
          cat <<END_OF_HELP
      ------------------------------------------------------
          [EXAMPLE] restore-home-backup -b /mnt/backup1/
          [EXAMPLE] restore-home-backup -bd /mnt/backup1/
      
          OPTIONS:
          -b          backup root path
          -d          enable dry-run
          --help      print help
          --version   print version
      ------------------------------------------------------
      END_OF_HELP
      }
      
      print_version() {
          echo $version
      }
      
      check_path() {
          if [ ! -d $1 ]; then
            echo "Wrong usage! Not a valid path: $1"
            print_help
            exit 0
          fi
      }
      
      if [ "$#" -eq 0 ]; then
          echo "Illegal number of parameters"
          print_help
          exit 1
      fi
      
      case "$1" in
          --help)
            print_help
            exit 0
            ;;
          --version)
            print_version
            exit 0
            ;;
      esac
      
      while getopts 'b:d' option; do
          case "$option" in
            b) backup_root="$OPTARG";;
            d) enable_dry_run=1;;
            ?)
                print_help
                exit 1
                ;;
          esac
      done
      
      if [[ "$backup_root" == "" ]]; then
          echo "No backup root"
          print_help
          exit 1
      fi
      
      if [[ $enable_dry_run -eq 1 ]]; then
          options="--dry-run"
      fi
      
      check_path $backup_root
      echo "rsync -av $options $excludes $backup_root $HOME/"
      rsync -av $options $excludes $backup_root $HOME/
    '';

    executable = true;
  };
}

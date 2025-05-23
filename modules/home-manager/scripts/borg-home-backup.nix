{
  home.file.".local/bin/borg-home-backup" = {
    text = ''
      #!/usr/bin/env bash
      # Title          : home-backup
      # Date           : 2025-05-23
      # Author         : tuxikus
      # Version        : 1.0
      # Description    : Create backup of home directory via borg
      # Options
      
      version=1.0
      
      print_help() {
          cat <<END_OF_HELP
      ------------------------------------------------------
          [EXAMPLE] borg-home-backup -o /path/to/borg/repo
      
          OPTIONS:
          -o          destination
          --help      print help
          --version   print version
      ------------------------------------------------------
      END_OF_HELP
        }
      
      print_version() {
          echo $version
      }
      
      check_borg_repo() {
          borg list $1 > /dev/null 2>&1
          if ! [ "$?" = "0" ]; then
              echo $1 is not a borg repo
              exit 1
          fi
      }
      
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
      
      if ! [ "$#" = "2" ]; then
          echo "Illegal number of parameters"
          print_help
          exit 1
      fi
      
      while getopts 'o:' option; do
          case "$option" in
              o) destination="$OPTARG";;
              ?)
              print_help
              exit 1
              ;;
          esac
      done
      
      check_borg_repo $destination
      
      borg create --progress --stats $destination::org-$(date +%Y-%m-%d) $HOME/org
      borg create --progress --stats $destination::org-edu-$(date +%Y-%m-%d) $HOME/org-edu
      borg create --progress --stats $destination::projects-$(date +%Y-%m-%d) $HOME/projects
      borg create --progress --stats $destination::pictures-$(date +%Y-%m-%d) $HOME/multimedia/pictures
      borg create --progress --stats $destination::videos-$(date +%Y-%m-%d) $HOME/multimedia/videos
      borg create --progress --stats $destination::books-$(date +%Y-%m-%d) $HOME/multimedia/books
      borg create --progress --stats $destination::comics-$(date +%Y-%m-%d) $HOME/multimedia/comics
      borg create --progress --stats $destination::music-$(date +%Y-%m-%d) $HOME/multimedia/music
      borg create --progress --stats $destination::retro-gaming-$(date +%Y-%m-%d) $HOME/multimedia/retro-gaming
      
      echo "Pruning old backups..."
      
      for prefix in org org-edu projects pictures videos books comics music retro-gaming; do
        borg prune -v --list $destination --prefix $prefix- --keep-daily=7 --keep-weekly=4 --keep-monthly=6
      done
      
      echo "Done!"
    '';

    executable = true;
  };
}

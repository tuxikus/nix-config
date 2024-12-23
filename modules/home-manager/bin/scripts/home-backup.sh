#!/usr/bin/env bash
# Title          : home-backupp
# Date           : 2024-04-11
# Author         : tuxikus
# Version        : 1.1
# Description    : Create backup of home directory
# Options        :  -o destination/output path
#                   -d enable --dry-run
#                   -x enable --delete
#                   --help print help
#                   --version print version

version="1.1"
excludes="--exclude={'*/lost+found/','lost+found/'}"
log_file_name=".backup-log.txt"
enable_delete=
enable_dry_run=
options=
rsync_command=
destination_path=

print_help() {
    cat <<END_OF_HELP
------------------------------------------------------
    [EXAMPLE] home-backup -o /path/to/backup/
    [EXAMPLE] home-backup -do /path/to/backup/
    [EXAMPLE] home-backup -dxo /path/to/backup/

    OPTIONS:
    -o          destination
    -d          enable dry-run
    -x          enable delete
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
        echo "Wrong usage!"
        print_help
        exit 0
    fi
}

write_log() {
    log_file_path="$HOME/${log_file_name}"

    # check if log file exists
    if [ ! -f $log_file_path ]; then
        touch $log_file_path
    fi

    cat >> $log_file_path <<END_OF_LOG
$(date '+%F_%H:%M')_$1
END_OF_LOG
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

while getopts 'o:dx' option; do
    case "$option" in
        o) destination_path="$OPTARG";;
        d) enable_dry_run=1;;
        x) enable_delete=1;;
        ?)
            print_help
            exit 1
            ;;
    esac
done

check_path $destination_path

if [[ $enable_dry_run -eq 1 && $enable_delete -eq 1 ]]; then
    options="--dry-run --delete"
elif [[ $enable_dry_run -eq 1 ]]; then
    options="--dry-run"
elif [[ $enable_delete -eq 1 ]]; then
    options="--delete"
else
    options=""
fi

rsync_command="rsync -av $options $excludes \
                     $HOME/.bookmarks.org \
                     $HOME/.nix-config\
                     $HOME/.backup-log.txt \
                     $HOME/.ppw \
                     $HOME/org \
                     $HOME/org-edu \
                     $HOME/media \
                     $HOME/projects \
                     $destination_path"

eval $rsync_command

if [ -z $enable_dry_run ]; then
    write_log $destination_path
fi

sync

echo "Done!"

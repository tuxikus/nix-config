#!/usr/bin/env bash
# Title          : quick-qemu
# Date           : 2024-04-13
# Author         : tuxikus
# Version        : 1.0
# Description    : Small qemu abstraction script
# Options        :  --help          print help
#                   --version       print version
#                   --install       enable install
#                   --create-disk   create an empty virtual disk
#                   --run           enable run
#                   --run-shared    enable run and file share

version="1.0"
install_enabled=0
run_enabled=0
run_shared_enabled=0
create_disk_enabled=0
disk_name=
disk_size=
disk_path=
iso_path=
qemu_command=
cores=4
mem=8
local_share_path="./share"
mount_tag="hostshare"

print_help() {
    cat <<END_OF_HELP
-----------------------------------------------------
    EXAMPLE:
    quick-qemu --version
    quick-qemu --help
    quick-qemu --create-disk disk.raw 16
    quick-qemu --install disk.raw generic_iso.iso
    quick-qemu --run disk.raw
    quick-qemu --run-shared disk.raw

    OPTIONS:
    --help
    --version
    --install
    --run
    --run-shared
    --create-disk
-----------------------------------------------------
END_OF_HELP
}

print_version() {
    echo $version
}

check_file() {
    if [ ! -f "$1" ]; then
        echo $1 does not exist, please ckeck filename/path.
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
    --install) install_enabled=1;;
    --run) run_enabled=1;;
    --run-shared) run_shared_enabled=1;;
    --create-disk) create_disk_enabled=1;;
    *)
        echo "Wrong usage!"
        exit 1
        ;;
esac

if [[ $create_disk_enabled -eq 1 ]]; then
    disk_name=$2
    disk_size=$3
    qemu_command="qemu-img create -f raw $disk_name ${disk_size}G"
    eval $qemu_command
    exit 0
fi

if [[ $install_enabled -eq 1 ]]; then
    disk_path=$2
    iso_path=$3
    check_file $disk_path
    check_file $iso_path
    qemu_command="qemu-system-x86_64 --enable-kvm -cpu host -smp ${cores} -m ${mem}G -vga virtio -display gtk,gl=on -cdrom ${iso_path} -drive file=${disk_path},format=raw,if=virtio,cache=none"
    eval $qemu_command
    exit 0
fi

if [[ $run_enabled -eq 1 ]]; then
   disk_path=$2
   check_file $disk_path
   qemu_command="qemu-system-x86_64 --enable-kvm -cpu host -smp ${cores} -m ${mem}G -vga virtio -display gtk,gl=on -drive file=${disk_path},format=raw,if=virtio,cache=none"
   eval $qemu_command
   exit 0
fi

if [[ $run_shared_enabled -eq 1 ]]; then
    disk_path=$2
    check_file $disk_path
    #check_file $local_share_path
    qemu_command="qemu-system-x86_64 --enable-kvm -cpu host -smp ${cores} -m ${mem}G -vga virtio -display gtk,gl=on -fsdev local,path=${local_share_path},security_model=mapped-xattr,id=fsdev0 -device virtio-9p-pci,id=fs0,fsdev=fsdev0,mount_tag=${mount_tag} -drive file=${disk_path},format=raw,if=virtio,cache=none"
    eval $qemu_command
    exit 0
fi

echo "Fatal error!"
exit 1

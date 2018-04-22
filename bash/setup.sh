#!/bin/bash

# Installing necessary packages
apt update
apt upgrade
apt install git
apt install vim fonts-powerline vim-gtk3
apt install cmake gcc
apt install build-essential
apt install python3 python3-pip python3-dev python-dev
apt install nodejs nodejs-dev nodejs-doc npm
apt install rsync unison
apt install xdotool
apt install taskwarrior
apt install gnome-tweak-tool
apt install xclip
apt isntall gparted
apt install gksu

# Changing settings for dual boot
timedatectl set-local-rtc 1
echo "Add windows drive to fstab:(This is best done manully)"
echo "run blkid | grep /dev/storage_drive_name to get UUID of windows drive. Then add following lines to fstab file"
echo "# windows partition /dev/sda3\nUUID=**** /media/windows ntfs rw,auto,users,exec 0 0"
echo "Backing up fstab..."
cp /etc/fstab /etc/fstab.old





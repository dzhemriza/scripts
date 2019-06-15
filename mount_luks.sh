#!/bin/bash

if [ `whoami` != "root" ]; then
    echo "You must be root!"
    exit 1
fi

ENCRYPTED_FILE=$1
MOUNT_DIRECTORY=$2

usage()
{
    echo ""
    echo "Invalid argument"
    echo ""
    echo "usage: mount_luks.sh <file_path> <mount_point>"
}

if [ "$ENCRYPTED_FILE" == "" ]; then
    usage

    exit 2
fi

if [ "$MOUNT_DIRECTORY" == "" ]; then
    usage

    exit 3
fi

DEVICE="$(basename $ENCRYPTED_FILE)-device"

cryptsetup luksOpen $ENCRYPTED_FILE "$(basename $ENCRYPTED_FILE)-device"
sudo mount /dev/mapper/$DEVICE  $MOUNT_DIRECTORY

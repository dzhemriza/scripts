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
    echo "usage: umount_luks.sh <file_path> <mount_point>"
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

umount $MOUNT_DIRECTORY
cryptsetup luksClose $DEVICE

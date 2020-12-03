#!/bin/bash
#
# Archives a directory using tar and encrypts it's contents using gpg
# by providing a password
#
# Archive format:
# YYYYMMDD_HHMMSS_NNN_HOSTNAME_archive.
#
# Requires: tar, gpg
#

usage()
{
    echo ""
    echo "usage: archive.sh <directory_1> <directory_2> ..."
    echo ""
}

if [ "$1" == "" ]; then
    usage
    exit 2
fi

TIMESTAMP="$(date +'%Y%d%m_%H%M%S_%N')"

tar -cv "$@" | gpg -v --cipher-algo AES256 -c -o\
                   "${TIMESTAMP}_$(hostname)_archive.tar.gz.gpg"

echo "done."

#!/bin/bash
#
# Unpacks archive created by archive.sh script uses
# tar and gpg to extract the contents
#
# Requires: tar, gpg
#

usage()
{
    echo ""
    echo "usage: unpack_archive.sh <archive_file_name>"
    echo ""
}

if [ "$1" == "" ]; then
    usage
    exit 2
fi

if [ "$2" != "" ]; then
    usage
    exit 3
fi

gpg -d "$1" | tar xvf -

echo "done."

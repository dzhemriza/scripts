#!/bin/bash
#
# Unpacks archive created by archive.sh script uses
# tar and gpg to extract the contents
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

gpg -d "$1" | tar xzvf -

echo "done."

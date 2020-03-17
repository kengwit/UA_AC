#!/bin/sh
#
# genmanifest.sh - generate the top-level MANIFEST file
# No support is provided for this script.
#
# Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: genmanifest.sh 1569 2005-08-19 19:48:17Z cloyce $

if [ -z "$SPEC" ]; then
    echo "SPEC variable is not set!";
    exit 1;
fi

# Tweak this; it's a regexp that recognizes the name of the suite tarball
# that lives in original.src
suitetarballre='/cpu200.*\.t[ab]z$'

# Don't tweak this
novc='( ( -name CVS -o -name .svn ) -prune ) -o'

cd $SPEC
rm -f MANIFEST SUMS.data
touch SUMS.data

if [ -f tools/tools_src.tar.bz2 ]; then
  exclude_toolsrc="grep -v ^tools/src/"
else
  exclude_toolsrc=cat
fi

echo Generating MD5 sums for distribution files
find . $novc \( -type f ! -name MANIFEST ! -name MANIFEST.tmp -print \) | \
  sed 's#^\./##' | \
  egrep -v '(\.ignoreme|\.cvsignore|\.DS_Store)' | \
  egrep -v $suitetarballre | \
  egrep -v '^shrc\.bat$' | \
  egrep -v '^tools/src/buildtools.log/.*buildlog.txt$' | \
  $exclude_toolsrc | \
  sort | \
  xargs specmd5sum --binary -e >> MANIFEST

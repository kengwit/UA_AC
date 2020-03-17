#!/bin/sh

#
#  install.sh - installs the full benchmark tree or just the tools binaries
#  Copyright (c) 1998-2005 by Standard Performance Evaluation Corporation
#
#  Authors:  Christopher Chan-Nui, Cloyce D. Spradling
#

SUITE=cpu2000
UCSUITE=CPU2000

system_call() {
    if [ "0$VERBOSE" -gt 0 ]; then
	echo $*
    fi
    if $*; then 
	true
    else
	echo
	echo "We had a problem with the following command:"
	echo "$*"
	echo
	exit 1;
    fi
}

is_spec_dir() {
    [ -x "$1/bin/runspec\;1"  ] || 
    [ -x "$1/bin/runspec.\;1" ] || 
    [ -x "$1/bin/runspec."    ] || 
    [ -x "$1/bin/runspec"     ] || 
    [ -d "$1/tools/bin"       ] 
}

clear=`tput clear`
echo "${clear}SPEC $UCSUITE Installation"
echo

# We want everything to be world readable
umask 022

# Set the locale, if it isn't already set and the user hasn't forbidden it
if [ -z "$SPEC_INSTALL_LOCALE_OK" ]; then
    if [ -z "$LC_ALL" -o -z "$LC_LANG" ]; then
        LC_ALL=C
        LC_LANG=C
        export LC_ALL LC_LANG
    fi
fi

# Set some flags for later
if [ ! -z "$VERBOSE" ]; then
    VERBOSE=`echo $VERBOSE | tr -c 0-9`
fi
if [ "0$VERBOSE" -gt 0 ]; then
    SPEC_INSTALL_VERBOSE=-v
else
    SPEC_INSTALL_VERBOSE=
fi

# Find top of SPEC heirarchy
if [ -n "$SPEC" ] ; then
    if is_spec_dir "$SPEC"; then
	if [ "$SPEC" != "`pwd`" ]; then
	    echo "The SPEC environment variable is already set to \"$SPEC\","
            echo "which does not match the current directory (\"`pwd`\").  If you continue with the"
            echo "installation, the tools will be installed in \"$SPEC\".  Is this the desired behavior?"
	    echo "Please enter 'yes' or 'no'."
	    read ans
	    ans=`echo $ans | tr YESNO yesno`
	    if [ "$ans" = 1 -o "$ans" = 'y' -o "$ans" = 'yes' ]; then
		true
	    else
                echo
                echo "Okay, \$SPEC is now unset.  Please pay special attention to the next"
                echo "non-blank line.  If it does not contain the location where you would"
                echo "like to install, terminate the install.sh process and try again."
                echo
		SPEC=
	    fi
	fi
    else
	SPEC=
    fi
fi
if [ -z "$SPEC" ]; then
    SPEC=`pwd`
    while [ -n "$SPEC" ]; do
	if is_spec_dir "$SPEC"; then
	    break;
	fi
	# At least some vendors' /bin/sh doesn't like this substitution
	#SPEC=${SPEC%/*}
	# Everyone should still have sed
	SPEC=`echo $SPEC | sed -e 's/\/[^\/]*$//'`
    done
fi
if [ -z "$SPEC" ]; then
    SPEC=`dirname "$0"`
    while [ -n "$SPEC" ]; do
	if is_spec_dir "$SPEC"; then
	    break;
	fi
	# At least some vendors' /bin/sh doesn't like this substitution
	#SPEC=${SPEC%/*}
	# Everyone should still have sed
	SPEC=`echo $SPEC | sed -e 's/\/[^\/]*$//'`
    done
fi
if [ -z "$SPEC" ]; then
    echo "Can't find the top of your $UCSUITE tree!  Please change to the benchmark"
    echo "directory and run this program ($0) again!"
    exit 1
fi
echo "Top of the $UCSUITE tree is '$SPEC'"

if cd "$SPEC" ; then
    true
else
    echo "Huh?  Can't cd into $UCSUITE directory '$SPEC'"
    exit 1
fi

# Find out where to install the executables
arch=
if [ $# -gt 0 ]; then
    arch=$1; shift
fi
if [ $# -gt 0 ]; then
    SPECTARGET=$1; shift
fi

if [ -z "$SPECTARGET" ]; then
    if touch a 2> /dev/null; then 
	rm a
	SPECTARGET=$SPEC
    fi
fi

while [ -z "$SPECTARGET" ]; do
    echo "Enter the directory you wish to install to (e.g. /usr/$SUITE)"
    read SPECTARGET
    mkdir -p $SPECTARGET 2> /dev/null || mkdir $SPECTARGET 2> /dev/null
    if [ ! -d "$SPECTARGET" ]; then
	SPECTARGET=
    fi
done

valid_archs=
archcount=0
if [ "$arch" != 'none' ]; then
    mach=`uname -m 2>/dev/null | sed 's/\./\\./g'`
    os=`uname -s 2>/dev/null | sed 's/\./\\./g'`
    rev=`uname -r 2>/dev/null | sed 's/\./\\./g'`
    proc=`uname -p 2>/dev/null | sed 's/\./\\./g'`
    for tmparch in `ls "$SPEC/tools/bin"`; do
	if [ -n "$mach" -a -n "$os" -a -f "$SPEC/tools/bin/$tmparch/excludearch" ] && 
             grep -i "$mach" "$SPEC/tools/bin/$tmparch/excludearch" >/dev/null 2>&1 ||
             grep -i "$os" "$SPEC/tools/bin/$tmparch/excludearch" >/dev/null 2>&1; then
            # The machine type or OS name was listed in excludearch;
            # don't try running binaries on this system
            true
        elif [ -n "$rev" -a -f "$SPEC/tools/bin/$tmparch/excluderev" ] && 
               grep -i "^$rev\$" "$SPEC/tools/bin/$tmparch/excluderev" >/dev/null 2>&1; then 
            # The OS revision was listed in excluderev;
            # don't try running binaries on this system
            true
        elif [ -n "$proc" -a -f "$SPEC/tools/bin/$tmparch/excludeproc" ] && 
               grep -i "^$proc\$" "$SPEC/tools/bin/$tmparch/excludeproc" >/dev/null 2>&1; then 
            # The processor architecture was listed in excludeproc;
            # don't try running binaries on this system
            true
        elif [ -n "$SPEC_DONT_TELL" ] &&
              echo $SPEC_DONT_TELL | grep -i "^$tmparch\$" >/dev/null 2>&1; then
            # The user wishes to not see this toolset.
            true
        elif [ -x "$SPEC/tools/bin/$tmparch/specbzip2" ] &&
            "$SPEC/tools/bin/$tmparch/specbzip2" -h > /dev/null 2>&1; then
            if [ -n "$valid_archs" ]; then
              valid_archs="$tmparch $valid_archs"
            else
              valid_archs="$tmparch"
            fi
            archcount=`expr $archcount + 1`
        fi
    done
fi

if [ -n "$valid_archs" ]; then
    error=
    if [ -n "$arch" ]; then
	if "$SPEC/tools/bin/$arch/specbzip2" -h > /dev/null 2>&1 ; then
	    true
	elif [ -d "$SPEC/tools/bin/$arch" ]; then
	    error="'${arch}' does not appear to be executable on this machine.";
	    arch=
	else
	    error="Tools for '${arch}' do not exist.";
	    arch=
	fi
    fi
    while [ -z "$arch" ]; do
        if [ $archcount -gt 1 ]; then
          echo
          echo "These appear to be valid toolsets:"
          echo ""
          for i in $valid_archs; do
            descfile="$SPEC/tools/bin/$i/description"
            if [ -f "$descfile" ]; then
              printf "%-29s %s\n" $i "`head -1 "$descfile"`"
              if [ `cat "$descfile" | wc -l` -gt 1 ]; then
                tail +2 "$descfile"
              fi
            else
              echo $i
            fi
            echo ""
          done
          echo "${error}"
          echo "Enter the architecture you are using:"
          read arch
        else
          echo
          echo "There appears to be only one valid toolset:"
          echo ""
          descfile="$SPEC/tools/bin/$valid_archs/description"
          if [ -f "$descfile" ]; then
            printf "%-29s %s\n" $valid_archs "`head -1 "$descfile"`"
            if [ `cat "$descfile" | wc -l` -gt 1 ]; then
              tail +2 "$descfile"
            fi
          else
            echo $valid_archs
          fi
          echo ""
          echo "${error}"
          if [ -z "$SPEC_DONT_ASK" ]; then
            echo "Use this? (y/n)"
            read arch
          else
            echo "Using this toolset..."
          fi
        fi
        if [ "$arch" = "none" ]; then arch=; break; fi
        if [ $archcount -eq 1 -a \( "x$arch" = "xy" -o "x$arch" = "xY" -o -n "$SPEC_DONT_ASK" \) ]; then
          # Strip the whitespace
          arch=`echo $valid_archs | sed 's/ //g'`
        elif [ "x$arch" = "xn" -o "x$arch" = "xN" ]; then
          arch=
          break
        fi
        if [ -x "$SPEC/tools/bin/$arch/specbzip2" ] && "$SPEC/tools/bin/$arch/specbzip2" -h > /dev/null 2>&1 ; then
            true
        else
            error="'${arch}' does not appear to be executable on this machine.";
            arch=
        fi
    done
else
    arch=$valid_archs
fi

# Set up the excludes for the installation.
EXCLUDE=
EXCLUDE_PAT=

echo ""
if [ -z "$arch" ]; then
    echo "${clear}We do not appear to have vendor supplied binaries for your"
    echo "architecture.  You will have to compile the tool binaries"
    echo "by yourself.  Please read the file"
    echo ""
    echo "    $SPECTARGET/docs/tools_build.txt"
    echo ""
    echo "and then try the following."
    echo
    echo "If you wish I can try to perform these steps for you.  However"
    echo "I'm not very intelligent so if anything goes wrong I'm just going"
    echo "to stop.  Want me to try?  (yes/no)"
    read ans
    dobuild=
    if [ "$ans" = "yes" ] || [ "$ans" = "YES" ] || \
	[ "$ans" = "y" ] || [ $ans = "Y" ] || [ $ans = "Yes" ]
    then
	dobuild=1
    else
	echo ""
	echo "There are no working toolsets, and you have elected not to have"
	echo "me try to build them for you.  Exiting."
	echo ""
	exit 1
    fi
elif [ -z "$SPEC_INSTALL_TOOLS" ]; then
    EXCLUDE="$EXCLUDE --exclude=tools/*"
    EXCLUDE_PAT="$EXCLUDE_PAT tools/"
fi

UNCOMPRESS="bzip2 -d"
TAR=tar
if [ -n "$arch" ]; then
    UNCOMPRESS="\"$SPEC/tools/bin/$arch/specbzip2\" -d"
    TAR="\"$SPEC/tools/bin/$arch/spectar\" $EXCLUDE"
fi

# Install things here
# Here's a part where we try to unpack a tarball that lives in original.src
# on the installation media.  If it's not found, it's not necessarily fatal;
# this situation could arise when running install.sh to install tools
# binaries in a benchmark tree that has already been unpacked.
errors=
if [ "$SPEC" != "$SPECTARGET" ]; then
    if [ -z "$SOC" ]; then
	for i in "$SPEC/original.src/${SUITE}.tbz" "$SPEC/original.src/${SUITE}.tbz\;1"; do
	    if [ -f $i ]; then
		SOC=$i
		break
	    fi
	done
    fi

    if [ -z "$SOC" ]; then
	errors="Can't find $SPEC/original.src/${SUITE}.tbz file"
    fi

    if [ -z "$errors" ]; then
        echo "Unpacking $UCSUITE archive..."
        if eval $UNCOMPRESS < "$SOC" | \
              (cd "$SPECTARGET" ; eval $TAR $SPEC_INSTALL_VERBOSE -xf -) ; then
            true
            echo "Done unpacking $UCSUITE archive."
        else
            errors="Error extracting the $UCSUITE archive.
"
        fi
    fi
fi

if [ -n "$errors" ]; then
    echo $errors
    exit 1
fi

MD5PROG=
if [ ! -z "$arch" -a -x "$SPEC/tools/bin/$arch/specmd5sum" ]; then
    MD5PROG="$SPEC/tools/bin/$arch/specmd5sum"
elif [ ! -z "$arch" -a -x tools/bin/$arch/specmd5sum ]; then
    MD5PROG=tools/bin/$arch/specmd5sum
elif [ -x bin/specmd5sum ]; then
    MD5PROG=bin/specmd5sum
fi
if [ -z "$MD5PROG" ]; then
    echo "No specmd5sum found.  Tools are out of date or incomplete."
    echo "Skipping file integrity checks."
    error="Binary tools are incomplete."
elif [ -z "$SPEC_INSTALL_NOCHECK" ]; then
    if $MD5PROG -e >/dev/null 2>&1 </dev/null; then
	echo "Checking the integrity of your source tree..."
	echo
        cat "$SPEC/MANIFEST" > "$SPECTARGET/MANIFEST.tmp.$$"

        # Filter out errors due to newly-rebuilt tools packages
        for i in `find tools/bin -name unbundled -print`; do
          grep -v `dirname "$i"` "$SPECTARGET/MANIFEST.tmp.$$" > "$SPECTARGET/manifest.tmp.$$.1"
          cat "$SPECTARGET/manifest.tmp.$$.1" > "$SPECTARGET/MANIFEST.tmp.$$"
          rm -f "$SPECTARGET/manifest.tmp.$$.1"
        done

        # Filter out names that were not unpacked
	for i in original.src/ $EXCLUDE_PAT; do
          cat "$SPECTARGET/MANIFEST.tmp.$$" | grep -v " $i" > "$SPECTARGET/manifest.tmp.$$.1"
          cat "$SPECTARGET/manifest.tmp.$$.1" > "$SPECTARGET/MANIFEST.tmp.$$"
          rm -f "$SPECTARGET/manifest.tmp.$$.1"
        done

	# Don't do the grep -v ': OK' here because grep will exit successfully
        cat "$SPECTARGET/MANIFEST.tmp.$$" | grep -v '/ORIG$' | (cd "$SPECTARGET"; $MD5PROG -e -c -) > "$SPECTARGET/manifest.check.$$"
	rm -f "$SPECTARGET/MANIFEST.tmp.$$"
        cat "$SPECTARGET/manifest.check.$$" | grep -v ': OK$' >> "$SPECTARGET/manifest.errors.$$"
        echo
	if grep ':' "$SPECTARGET/manifest.errors.$$"; then
	    if [ -d "$SPECTARGET/tools/output" ]; then
	      # The tools have just been built, so some failures are expected
	      echo "Package integrity check failed.  Some failures are to be expected after"
	      echo "building the tools, as the build process modifies some files in the"
	      echo "distribution."
	    else
	      errors="${errors}Package integrity check failed."
	      echo "Package integrity check failed."
	    fi
	else
	    echo "Checksums are all okay."
	fi
	rm -f "$SPECTARGET/manifest.check.$$" "$SPECTARGET/manifest.errors.$$"
    else
	echo "Your specmd5sum program is not up-to-date."
	echo "Skipping file integrity checks."
	error="Binary tools are out of date or incomplete."
    fi
fi

if [ -n "$errors" ]; then
    echo $errors
    exit 1
fi

if [ -z "$arch" ]; then
    if [ -n "$dobuild" ]; then
        echo "Attempting to build the tools for $UCSUITE..."
	cd "$SPECTARGET/tools/src"
	if "$SPECTARGET/tools/src/buildtools"; then
	    true
        else
	    echo "Whoops! I had trouble building your tools.  Please consult"
	    echo "$SPECTARGET/docs/tools_build.txt."
	    exit 1;
	fi
        cd ../..
	. ./shrc
	if "$SPECTARGET/bin/packagetools" `bin/specperl -MConfig -e 'print $Config{"archname"};'`; then
	    true
        else
	    echo "Whoops! I had trouble packaging your tools.  Please consult"
	    echo "$SPECTARGET/docs/tools_build.txt."
	    exit 1;
	fi
    else
	echo "Ok... good luck!"
	exit 1;
    fi
else
    # Install binaries here
    rm -rf bin/lib

    # UNinstall previously installed tools.  This might not get everything,
    # but it'll get everything that might not be overwritten by the next
    # installation.
    if [ -f "$SPECTARGET/SUMS.tools" ]; then
        echo "Removing previous tools installation"
        (cd "$SPECTARGET"; cat SUMS.tools | grep bin/ | awk '{print $4}' | xargs rm -f)
        # At this point bin/lib should be empty, but just to be sure...
        rm -rf "$SPECTARGET/bin/lib"
        rm -f "$SPECTARGET/bin/packagename"
        rm -f "$SPECTARGET/SUMS.tools"
    fi

    echo "Unpacking binary tools for $arch..."
    # Check for the one you really want _last_
    TOOLS=
    for testsuite in cpu2006 ${SUITE}; do
      if [ -f "$SPEC/tools/bin/$arch/${testsuite}tools-$arch.tar.bz2" ]; then
        TOOLS="$SPEC/tools/bin/$arch/${testsuite}tools-$arch.tar.bz2"
        UNCOMPRESS="\"$SPEC/tools/bin/$arch/specbzip2\" -dc"
      fi
    done
    if [ -z "$TOOLS" ]; then
      # This should never happen
      echo "Huh?  There's no binary tools tarball?!"
      exit 1;
    fi

    if eval $UNCOMPRESS < `ls "$TOOLS"` | \
	    (cd "$SPECTARGET"; eval $TAR $SPEC_INSTALL_VERBOSE -xf -); then
        if [ -z "$SPEC_INSTALL_NOCHECK" ]; then
          echo "Checking the integrity of your binary tools..."
          echo
          # Don't do the grep -v ': OK' here because grep will exit successfully
          (cd "$SPECTARGET"; cat SUMS.tools | grep -v '/ORIG$' | $MD5PROG -e -c - > manifest.check.$$)
          if [ $? != 0 ]; then
             cat "$SPECTARGET/manifest.check.$$" | grep -v ': OK$'
             echo
             errors="${errors}Binary tools integrity check failed."
             echo "Binary tools integrity check failed."
          else
             echo "Checksums are all okay."
          fi
          rm -f "$SPECTARGET/manifest.check.$$"
        fi
    else
	errors="${errors}Error extracting the '$arch' binaries tar file.
"
    fi
    echo $arch > "$SPECTARGET/bin/packagename"
fi

cd "$SPECTARGET"

# shrc will add LD_LIBRARY_PATH if it is needed
. ./shrc

if bin/relocate ; then
    true
else
    errors="${errors}Error re-homing the benchmark tools.
"
fi

if [ -z "$errors" ]; then
    echo "Everything looks okay.  cd to $SPECTARGET,"
    echo "source the shrc file and have at it!"
else
    echo $errors
    exit 1
fi

exit 0

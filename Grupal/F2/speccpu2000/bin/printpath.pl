#!/spec/cpu2000/bin/specperl
#!/spec/cpu2000/bin/specperl -d
#!/usr/bin/perl
#
# printpath.pl
# No support is provided for this script.
#
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: printpath.pl 1393 2005-06-14 15:55:21Z cloyce $
# Print an NT path with some carriage returns for legibility
# Include hats so the output can be captured for re-use if desired.
# J.Henning 22 April 1999
#
@path = split ";", $ENV{'PATH'};
$path[1] =~ s/^PATH=//;
print "\nPATH=^\n";
print join (";^\n", @path), "\n";

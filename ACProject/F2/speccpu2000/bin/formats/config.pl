#
#  config.pl - produces config files
#  Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author: Cloyce D. Spradling
#
# $Id: config.pl 1422 2005-06-15 18:37:20Z cloyce $

use strict;

use vars qw($name $extension $bzipprog);

$name      = 'config';
$extension = 'cfg';

sub format {
    my($me, $r, $mode) = @_;
    my $enconfig = '';
    my @output = ();

    # Assemble the rawtxtconfig lines
    if (exists($r->{'rawconfig'}) && # it should
        ref($r->{'rawconfig'}) eq 'ARRAY') { # it should
      $enconfig = join('', @{$r->{'rawconfig'}});
    } else {
      return (undef, []);
    }
    my (undef, $decodedconfig, $txtconfig) = ::decode_decompress($enconfig);

    # Decide which of the three possibilities to use.
    # Basically, choose in descending order of preference:
    # txtconfig (decoded, decompressed)
    # decodedconfig (just decoded)
    # enconfig (the original input)
    my $compconfig = defined($txtconfig) ? $txtconfig : defined($decodedconfig) ? $decodedconfig : $enconfig;
    push @output, split(/(?:\r\n|\n)/, $compconfig, -1);

    # The first line of the stored config file should be a comment labelling
    # the invocation command line, so check for that:
    if ($output[0] !~ /^\# Invocation/) {
        ::Log(0, "WARNING: Contents of rawconfig array are not a configuration file!\n");
        return(undef, []);
    }

    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }

    return (\@output, []);
}

1;

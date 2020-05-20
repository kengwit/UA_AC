#
#  raw.pl - produces RAW output
#  Copyright (C) 1995-2005 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author:  Christopher Chan-Nui
#
# $Id: raw.pl 1422 2005-06-15 18:37:20Z cloyce $

use strict;

use vars qw($name $extension $prefix);

$name      = 'raw';
$extension = 'raw';
$prefix = 'spec.cpu2000';

sub format {
    my($me, $r, $mode) = @_;
    my @output;
    my %dumped = ( 'benchmarklist' => 1, 'datestr' => 1, 'ref_tree_level' => 1,
		   'rawfile' => 1, 'compraw' => 1, 'rawhash' => 1 );
    my @keys = $r->list_keys;
    @keys = grep (!/^(refs|benchmarks)$/, @keys);

    # Dump configuration info
    for my $key (sort main::bytrailingnum @keys) {
	next if exists $dumped{$key};
	next unless ($key =~ /^(hw_|sw_|license_num|test|prepared|machine_name|company_name)/o);
	my $val = $r->accessor($key);
	if (ref $val eq 'ARRAY') {
	    for (my $i = 0; $i < @$val; $i++) {
		push(@output, 
		    sprintf "%s.%s%03d: %s", $prefix, $key, $i, $val->[$i]);
	    }
	} elsif (ref $val eq '') {
	    push (@output, sprintf "%s.%s: %s", $prefix, $key, $val);
	}
	$dumped{$key} = 1;
    }
    # Dump the notes.  We do this separately so that the numbers can be spaced
    # It should always be an array, but just in case it isn't, give it the
    # full treatment.
    my $val = $r->accessor('notes');
    if (ref $val eq 'ARRAY') {
        for (my $i = 0; $i < @$val; $i++) {
	    if ($val->[$i] =~ /[\r\n]/o) {
		my @morenotes = split(/[\r\n]+/, $val->[$i]);
		splice @$val, $i, 1, @morenotes;
		redo;
	    }
            push(@output,
                sprintf "%s.notes%03d: %s", $prefix, $i * 5, $val->[$i]);
        }
    } elsif (ref $val eq '') {
        push (@output, sprintf "%s.notes: %s", $prefix, $val);
    }
    $dumped{'notes'} = 1;

    # Make sure that the suite versions exist and are integrated properly
    if (!grep { /^suitever$/o } @keys) {
	$r->{'suitever'} = $::suite_version;
	push @keys, 'suitever';
    }
    if (!grep { /^toolsver$/o } @keys) {
	$r->{'toolsver'} = $::version;
	push @keys, 'toolsver';
    }
    if (!grep { /^invalid$/o } @keys) {
	# Make sure that invalid results have an invalid tag:
	if (!exists($r->{'invalid'})) {
	    if (exists($r->{'errors'})) {
		if ((ref($r->{'errors'}) eq 'ARRAY') &&
		    (@{$r->{'errors'}}+0) > 0) {
		    $r->{'invalid'} = 1;
		}
	    } else {
		$r->{'invalid'} = 0;
	    }
	} else {
	    $r->{'invalid'} = 0 if (!defined($r->{'invalid'}));
	}
	push @keys, 'invalid';
    }

    # Print a little delimiter
    push @output, '# =============== do not edit below this point ===================';

    # From here on in, we make an MD5 hash of all the lines
    my $ctx = new Digest::MD5;
    my @md5list = (); # Here's where we store the info to be hashed

    # Dump non-configuration, non-result info (leftovers)
    for my $key (sort @keys) {
	next if exists $dumped{$key};
	my $val = $r->accessor($key);
	if (ref $val eq 'ARRAY') {
	    for (my $i = 0; $i < @$val; $i++) {
		push(@md5list, 
		    sprintf "%s.%s%03d: %s", $prefix, $key, $i, $val->[$i]);
	    }
	} elsif (ref $val eq '') {
	    push (@md5list, sprintf "%s.%s: %s", $prefix, $key, $val);
	}
	$dumped{$key} = 1;
    }

    # Dump result info
    for my $bench (sort keys %{$r->{'results'}}) {
	my $benchname = $bench;
	$benchname =~ y/\./_/;
	for my $tune (keys %{$r->{'results'}{$bench}}) {
	    next if ref $r->{'results'}{$bench}{$tune}{'data'} ne 'ARRAY';
	    my @tmp = @{$r->{'results'}{$bench}{$tune}{'data'}};
	    for (my $i = 0; $i < @tmp; $i++) {
		for my $key (sort keys %{$tmp[$i]}) {
		    next if $key eq 'ref' || $key eq 'ref_tree_level' || $key eq 'refs' || $key eq 'tune';
		    if (ref $tmp[$i]->{$key} eq 'ARRAY') {
			my @tmp2 = @{$tmp[$i]->{$key}};
			for (my $j = 0; $j < @tmp2; $j++) {
			    push (@md5list, 
				sprintf "%s.results.%s.%s.%03d.%s%03d: %s", 
				 	$prefix, $benchname, $tune, $i, $key, 
					$j, $tmp[$i]->{$key}->[$j]);
			}
		    } else {
			push (@md5list, 
			    sprintf "%s.results.%s.%s.%03d.%s: %s", 
				    $prefix, $benchname, $tune, $i, $key, 
				    $tmp[$i]->{$key});
		    }
		}
	    }
	}
    }

    foreach my $line (@md5list) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }
    $ctx->add(@md5list);
    push @output, "$prefix.rawhash: ".$ctx->hexdigest();
    push @output, @md5list;
    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }

    # Make sure we save a copy of the rawfile in the result object:
    if (!exists $r->{'compraw'}) {
	my $rawfile = join("\n", @output)."\n";
	my $comprawfile = '';
	eval '$comprawfile = Compress::Zlib::memGzip($rawfile)';
	if ($@) {
	    $r->{'rawfile'} = main::encode_base64($rawfile);
	    $r->{'compraw'} = undef;
	} else {
	    $r->{'rawfile'} = undef;
	    $r->{'compraw'} = main::encode_base64($comprawfile);
	}
    }

    return (\@output, []);
}

1;

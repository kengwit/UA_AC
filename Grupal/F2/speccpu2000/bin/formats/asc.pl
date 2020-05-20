#
#  asc.pl - produces ASCII output
#  Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: asc.pl 1422 2005-06-15 18:37:20Z cloyce $

use strict;

use vars qw($name $extension);

$name      = 'ASCII';
$extension = 'asc';
my $asc_version = '2.1';
my $debug = 0;

sub format {
    my($me, $r, $mode) = @_;
    my (@output, @errors);
    my (%seen, $temp, $bench, $name, @values, @errmsg);
    my @nc = @{$r->{'nc'}};
    $r->{'table'} = 0 if (@nc);
    my $invalid = ($r->{'invalid'} ||
		   ((ref($r->{'errors'}) eq 'ARRAY') && @{$r->{'errors'}}));
    if ($invalid) {
	push (@errors, '#' x 78);
	push (@errors, sprintf ("# %-74s #", '  ' . 'INVALID RUN ' x 6));
	push (@errors, sprintf ("# %-74s #", ''));

	for ($r->errors) {
	    push (@errors, sprintf ("# %-74s #", $_));
	}

	push (@errors, sprintf ("# %-74s #", ''));
	push (@errors, sprintf ("# %-74s #", '  ' . 'INVALID RUN ' x 6));
	push (@errors, '#' x 78);
    }

    push (@output, center( 'SPEC ' . $r->metric .  ' Summary'));
    push @output, center($r->{'hw_vendor'}.' '.$r->{'hw_model'});
    if ($r->{'company_name'} !~ /^(|--)$/ &&
	$r->{'company_name'} ne $r->{'hw_vendor'}) {
	push @output, center("Tested by ".$r->{'company_name'});
    }
    push @output, center( $r->datestr );
    push (@output, '');

    # Note some important stuff
    push @output, sprintf("SPEC License #%-4d  Test date: %-10s   Hardware availability: %s",
			  $r->license_num, $r->test_date, $r->hw_avail);
    push @output, sprintf("Tester: %-34s  Software availability: %s",
			  $r->tester_name, $r->sw_avail);
    push (@output, '');

    # Note the reason for NC, if any.
    if (@nc) {
	push @output, '', '---------------------------------------------------------------------------', '';
	push @output, @nc;
	push @output, '', '---------------------------------------------------------------------------', '', '';
    }

    my $nCopies = 'Ref Time';
    my $rmode = ' Ratio';
    if (istrue($r->rate)) {
	$nCopies = '# Copies';
	$rmode = '  Rate';
    }
    push (@output,
    '                                     Estimated                     Estimated'
    ) if $invalid;
    push (@output,
    '                   Base      Base      Base      Peak      Peak      Peak',
    "   Benchmarks    $nCopies  Run Time  $rmode    $nCopies  Run Time  $rmode",
    '   ------------  --------  --------  --------  --------  --------  --------');

    my $table    = {};
    my $results  = {};
    my $smallest = { 'base' => { 'time' => 2147483647, # Mr. Big Number
				 'ratio' => 2147483647 }, # Mr. Big Number
                     'peak' => { 'time' => 2147483647, # Mr. Big Number
				 'ratio' => 2147483647 } # Mr. Big Number
		 };
    my %benchseen = ();
    my %tuneseen = ();

    # Go through the benchmarks that have results.  We'll catch the missed
    # ones afterward.
    for my $bench (sort keys %{$r->{'results'}}) {
	my $benchres = $r->{'results'}{$bench};
	for my $tune (sort keys %{$r->{'results'}{$bench}}) {
	    $benchres = $r->{'results'}{$bench}{$tune}{'data'};
	    my $tmp;
	    for my $res (@{$benchres}) {
		# If we don't get here, we haven't "seen" them...
		$benchseen{$bench} = 1 unless exists $benchseen{$bench};
		$tuneseen{$tune} = 1 unless exists $benchseen{$tune};
		if (istrue($r->rate)) {
		    $tmp = [ $res->copies,    $res->reported_time, $res->ratio, $res->selected, !$res->valid ];
		} else {
		    $tmp = [ $res->reference, $res->reported_time, $res->ratio, $res->selected, !$res->valid ];
		}
		print "\@tmp = (",join(', ', @{$tmp}),")\n" if ($debug & 2);
		if ($tmp->[1] && ($smallest->{$tune}{'time'}  > $tmp->[1])) {
		    $smallest->{$tune}{'time'}  = $tmp->[1];
		}
		if ($tmp->[2] && ($smallest->{$tune}{'ratio'} > $tmp->[2])) {
		    $smallest->{$tune}{'ratio'} = $tmp->[2];
		}
		if ($debug & 2) {
		    print "smallest->{$tune}{time} = ".$smallest->{$tune}{'time'}."\n";
		    print "smallest->{$tune}{ratio} = ".$smallest->{$tune}{'ratio'}."\n";
		}
		push (@{$table->{$bench}{$tune}}, $tmp);
		if (@{$res->errors}) {
		    $tmp->[4] = 1; # Make it invalid
		}
		if ($res->selected) {
		    $tmp->[3] = '*' unless $tmp->[3] eq 'X';
		    push (@{$results->{$bench}{$tune}}, $tmp);
		    $benchseen{$bench} = 'selected';
		}
	    }
	}
    }
    for my $bench (sort keys %{$r->benchmarks}) {
	next if (exists($benchseen{$bench}) &&
		 ($benchseen{$bench} eq 'selected'));
	for my $tune (sort keys %tuneseen) {
	    my $tmp = [ '', '', '', '', 1 ];
	    push (@{$table->{$bench}{$tune}}, $tmp) unless $benchseen{$bench};
	    push (@{$results->{$bench}{$tune}}, $tmp);
	}
    }
    my $timelog = { 'base' => $smallest->{'base'}{'time'},
		    'peak' => $smallest->{'peak'}{'time'} };
    my $ratiolog = { 'base' => $smallest->{'base'}{'ratio'},
		     'peak' => $smallest->{'peak'}{'ratio'} };
    $timelog->{'base'}  = log($timelog->{'base'})/log(10) if ($timelog->{'base'} > 0);
    $ratiolog->{'base'} = log($ratiolog->{'base'})/log(10) if ($ratiolog->{'base'} > 0);
    $timelog->{'peak'}  = log($timelog->{'peak'})/log(10) if ($timelog->{'peak'} > 0);
    $ratiolog->{'peak'} = log($ratiolog->{'peak'})/log(10) if ($ratiolog->{'peak'} > 0);
    print "\@timelog=($timelog->{'base'}, $timelog->{'peak'})  \@ratiolog=($ratiolog->{'base'}, $ratiolog->{'peak'})\n" if $debug;


    if ($r->table) {
	push (@output, format_table($table, $timelog, $ratiolog, @nc));
	push (@output, '   ' . '=' x 72);
    }
    push (@output, format_table($results, $timelog, $ratiolog, @nc));

    my $est;
    $est = 'Est. ' if ($invalid);

    push (@output, sprintf ("   %-34s%8s", 
			    $est . $r->baseunits,
			    &significant(8, 3, $ratiolog->{'base'}, $r->basemean, @nc)));
    push (@output, sprintf ("   %-34s%38s",
			    $est . $r->peakunits,
			    &significant(8, 3, $ratiolog->{'peak'}, $r->peakmean, @nc)));

    push (@output, format_info('HARDWARE', [ $r->hardware ]));
    push (@output, format_info('SOFTWARE', [ $r->software ]));
    push (@output, format_info('NOTES',    [ $r->notes    ]));

    push @output, @errors;
    unshift @output, @errors;
    push @output, '-----------------------------------------------------------------------------';
    push @output, 'For questions about this result, please contact the tester.';
    push @output, 'For other inquiries, please contact webmaster@spec.org.';
    push @output, 'Copyright 1999-2005 Standard Performance Evaluation Corporation';
    push @output, 'Generated on '.&::ctime(time)." by SPEC CPU2000 ASCII formatter v$asc_version";

    return (\@output, []);
}

sub format_table {
    my ($table, $timelog, $ratiolog, @nc) = @_;
    my @rc;
    for my $benchname (sort keys %$table) {
	my $tr = $table->{$benchname};
	my $array = { 'base' => [], 'peak' => [] };
	$array->{'base'} = [@{$tr->{'base'}}] if ref($tr->{'base'}) eq 'ARRAY';
	$array->{'peak'} = [@{$tr->{'peak'}}] if ref($tr->{'peak'}) eq 'ARRAY';
	my ($base, $peak);

	while (@{$array->{'base'}} || @{$array->{'peak'}}) {
	    my $line = sprintf '   %-12s ', $benchname;
	    for my $tune (qw(base peak)) {
		my $ref = $array->{$tune};
		if (@$ref) {
		    my ($first, $time, $ratio, $selected, $invalid) = @{shift @$ref};
		    $first = sprintf('%8d', $first) if ($first + 0 > 0);
		    $time  = sprintf('%8s', significant(8,3,$timelog->{$tune},$time, @nc)) if ($time + 0 > 0);
		    $ratio = sprintf('%8s', significant(8,3,$ratiolog->{$tune},$ratio, @nc)) if ($ratio + 0 > 0);
		    my $selectchar = $selected ? '*' : ' ';
		    if ($invalid) {
			$selectchar = 'X';
			$ratio = '';
		    }
		    $selectchar = ' ' if (@nc);
		    $line .= sprintf ' %8s  %8s  %8s%s',  
				$first, $time, $ratio, 
				$selectchar;
		} else {
		    $line .= ' ' x 29;
		}
	    }
	    push (@rc, $line);
	}
    }
    return @rc;
}

sub format_info {
    my ($title, $ref) = @_;
    return () if !@$ref;

    my @output;
    push (@output, '', '', center($title), center('-' x length($title)));
    for my $item (@{$ref}) {
	my ($name, @vals) = @$item;
	if (!@vals) {
	    if ($title ne 'NOTES') {
		push (@output, sprintf ('%20.20s: --', $name));
	    } else {
		push (@output, "    --");
	    }
	} else {
	    my $val = shift @vals;
	    if ($title ne 'NOTES') {
		push (@output, sprintf ('%20.20s: %s', $name, $val));
	    } else {
		push (@output, "    $val");
	    }

	    while (@vals) {
		$val = shift @vals;
		if (ref $val eq '') {
		    if ($title ne 'NOTES') {
			push (@output, sprintf ('%20.20s  %s', '', $val));
		    } else {
			push (@output, "    $val");
		    }
		} elsif (ref $val eq 'ARRAY') {
		    unshift (@vals, @{$val});
		}
	    }
	}
    }
    return @output;
}

sub z {
    my @tmp = @_;
    grep (s/([\000-\011\013-\037])/"^".chr(ord($1)+64)/ge, @tmp);
    return @tmp;
}

# significant -- format a floating point value into N significant digits
# width of text string to return
# log of minimum output (how many decimal places do we want)
# log of smallest overall value (how many decimals will we be using for sure)
# value to format
sub significant {
    my ($width, $min_log, $low_log, $value, @nc) = @_;
    print "significant($width, $min_log, $low_log, $value, $debug)\n" if ($debug & 4);
    my ($real_dp, $wanted_dp, $dp, $space, $log);

    return 'NC' if (@nc);
    if ($value == 0) {
	if ($value !~ m/^\s*(\+|-)?[0-9.eE+]/) {
	    print "Returning '$value'\n" if ($debug & 4);
	    return $value;
	}
	$log = 0;
    } else {
	$log = &floor(log($value)/log(10)); 
    }
    $min_log--;
    print "  log=$log  min_log=$min_log\n" if ($debug & 4);
    if ($log > $min_log) {
	# STUPID STUPID HACK which circumvents the whole idea of
	# 3 significant digits.
	if ($value < 1000) {
	    $value = 
		int($value / (10**($log-$min_log))+.5) *
				    (10**($log-$min_log));
	}
    }
    print "  value=$value\n" if ($debug & 4);
    $dp        = ($low_log>=$min_log)?0:3-$low_log;
    $wanted_dp = ($log>=$min_log)?0:$min_log-$log;
    print "  dp=$dp   wanted_dp=$wanted_dp\n" if ($debug & 4);
    if ($dp > $wanted_dp) {
	$space = $dp - $wanted_dp;
	$real_dp = $wanted_dp;
    } else {
	$space = 0;
	$real_dp = $dp;
    }
    if ($real_dp == 0 && $dp > 0) {
	$space++;
    }
    print "  space=$space  real_dp=$real_dp\n" if ($debug & 4);
    my $retval = sprintf('%*.*f%s', $width-$space, $real_dp, $value, ' ' x $space);
    print "sprintf('%*.*f%s', \$width-\$space, \$real_dp, \$value, ' ' x \$space) =\n" if ($debug & 4);
    print "  sprintf('%*.*f%s', ".($width-$space).", $real_dp, $value, ".(' ' x $space).") =\n" if ($debug & 4);
    print "    $retval\n" if ($debug & 4);
    return $retval;
}

sub floor {
    my ($temp) = @_;
    my $inttemp = int($temp);
    if ($temp != $inttemp) { #  This is a bad test.
	if ($temp > 0) {
	    $temp = $inttemp;
	} else {
	    $temp = $inttemp-1;
	}
    }
    return $temp;
}

sub center  { main::center(@_); }
sub jp { main::jp(@_); }
sub istrue { main::istrue(@_); }

1;

#
# parse.pl
#
# Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: parse.pl 1579 2005-08-22 19:37:59Z cloyce $

use strict;

sub parse_commandline {
    my ($config, $cl_opts) = @_;

    if (exists $ENV{'SPEC_RUNSPEC'}) {
	unshift(@ARGV,shellwords($ENV{'SPEC_RUNSPEC'}));
    }

    # Here's a hack:
    # This is called each time before rawformatting a result.
    # So save @ARGV before we nuke it, and reuse it if appropriate
    if (exists $ENV{'SPEC_SAVED_ARGC_DONT_EVER_SET_THIS'}) {
	# Restore @ARGV
	print "Restoring \@ARGV: before = ",join('', @ARGV),"\n" if $debug;
	@ARGV = split(/\|\|/, $ENV{'SPEC_SAVED_ARGC_DONT_EVER_SET_THIS'});
	print "Restoring \@ARGV: after = ",join('', @ARGV),"\n" if $debug;
    } else {
	# Save @ARGV
	print "Saving \@ARGV: before = ",join('', @ARGV),"\n" if $debug;
	$ENV{'SPEC_SAVED_ARGC_DONT_EVER_SET_THIS'} = join('||', @ARGV);
	print "Saving \@ARGV: after = ",join('', @ARGV),"\n" if $debug;
    }

    Getopt::Long::config("no_ignore_case", "bundling");
    my $rc = GetOptions ($cl_opts, qw(action|a=s
			config|c=s
			make_no_clobber|C
			ext|extension|e=s
			help|h|?
			mach|machine|m=s
			iterations|n=i
			output_format|o=s
			rate|r
			size|input|i=s
			tune|tuning|T=s
			users|u=s
			max_active_compares|maxcompares=i
			username|U=s
			rebuild|D
			deletework|d
			unbuffer|f
			ignore_errors|ignoreerror|I
			verbose|debug|v=i
			version|V
			setprocgroup!
			reportable|s!
			strict!
                        loose|l!
                        table!
			rawformat|R
                        nc=s
                        test
			));

    # 'loose', 'strict', and 'reportable' are all really ways of talking
    # about 'reportable', so munge things up properly
    # We don't do a whole lot of checking here because those would only
    # cover strange cases where the user says '--strict --noreportable'
    # or things like that, and our job isn't to help people be not stupid
    if (exists $cl_opts->{'loose'}) {
	$cl_opts->{'reportable'} = 1 - $cl_opts->{'loose'};
	delete $cl_opts->{'loose'};
    }
    if (exists $cl_opts->{'strict'}) {
	$cl_opts->{'reportable'} = $cl_opts->{'strict'};
	delete $cl_opts->{'strict'};
    }

    # Run the Perl test suite, if asked
    if (exists $cl_opts->{'test'} && $cl_opts->{'test'}) {
      print "Running the Perl test suite...\n";
      chdir main::jp($ENV{'SPEC'}, 'bin');
      exec "./specperl", main::jp('test', 'TEST');
    }

    # Try to be reasonably helpful
    if (exists($cl_opts->{'nc'})) {
	if (!defined($::cpu2000_website_formatter) ||
	    $::cpu2000_website_formatter == 0) {
	    die "The --nc flag is not for you.\nStopped";
	} elsif (! -e $cl_opts->{'nc'}) {
	    die "Please specify an existing file for the NC text!\nStopped";
	} else {
	    $cl_opts->{'ncfname'} = $cl_opts->{'nc'};
	    open(TMP, "<$cl_opts->{nc}") || die "Can't open $cl_opts->{nc} for reading: $!\nStopped";
	    my $eol = $/;
	    undef $/;
	    $cl_opts->{'nc'} = [ split(/[\r\n]+/, <TMP>) ];
	    $/ = $eol;
	}
    }
    if (!$rc || istrue($cl_opts->{'help'})) {
	usage();
	exit (!$rc ? 1 : 0);
    }
    return 1;
}

sub validate_options {
    my ($config) = @_;

    if (istrue($config->unbuffer)) {
	$|=1;
    }
    if ($config->ext eq '') {
	Log(0, "Please specify an extension!  (-e or ext= in config file)\n");
	exit 1;
    }

}

sub resolve_choices {
    my ($config) = @_;

    if (istrue($config->{'rawformat'})) {
	# Don't do anything but build the format list and make the list
	# of files to format.
	$config->{'formatlist'} = [ map { $config->formats->{$_} }
				    choose_strings('Output',
						   $config->output_format,
						   keys %{$config->formats}) ];
	$config->{'runlist'} = [ @ARGV ];
    } else {
	my $action = $config->action;
	my $tmp = choose_string($action, @{$config->valid_actions});
	if (!defined $tmp) {
	    &Log(0, "I don't know what type of action '$action' is!\n");
	    exit 1;
	}
	$config->action($action);

	$config->{'formatlist'} = [ map { $config->formats->{$_} }
				    choose_strings('Output',
						   "raw,".$config->output_format,
						   keys %{$config->formats}) ];

	$config->{'tunelist'} = [ choose_strings('Tune', $config->{'tune'}, 
						 @{$config->valid_tunes}) ];

	$config->{'extlist'} = [ split(/,+|\s+/, $config->{'ext'}) ];

	$config->{'machlist'} = [ split(/,+|\s+/, $config->{'mach'}) ];
	$config->{'sizelist'} = [ split(/,+|\s+/, $config->{'size'}) ];
	$config->{'userlist'} = [ split(/,+|\s+/, $config->{'users'}) ];

# Make sure there is at least one entry in each of the categories
	for (qw(tunelist extlist machlist sizelist)) {
	    $config->{$_} = [ 'default' ] if (ref($config->{$_}) ne 'ARRAY' || @{$config->{$_}} == 0);
	}
	my @candidates = (@ARGV);
	if (@candidates+0 < 1 && exists $config->{'runlist'}) {
	    push @candidates, split(/(?:\s+|\s*,\s*)/, $config->{'runlist'});
	}
	my @benchmarks = resolve_user_selection($config, \@candidates);

	if (!@benchmarks) {
	    Log(0, "\nNo benchmarks specified!\n");
	    exit 1;
	}
	$config->{'runlist'} = [ @benchmarks ];
    }
}

sub resolve_user_selection {	## figure out which benchmarks are to be run
                                ## and return them as a list.
    my ($config, $selection) = @_;       ## typically, what is on the command line
    my @benchmarks = ();
    my %benchmark_seen;
    my $sel;
    my $set;

    # First, convert 'all' to ('int', 'fp') as decided by the committee.
    # This makes things nicer in several ways, but less general
    # Position is important, so we search for the index of 'all' so we can
    # use splice.
    for (my $i = 0; $i < @$selection; $i++) {
	if ($selection->[$i] =~ /(\^)?all/io) {
	    my $not = $1;
	    splice(@{$selection}, $i, 1, "${not}int", "${not}fp");
	    Log(24, "'${not}all' replaced by ('${not}int', '${not}fp') at position $i of selection list\n");
	}
    }

    # Find out what benchmarks the user wants to run, knock out duplicates
    my $error = 0;
    for my $sel (@$selection) {
	my @temp = ();
	my $not = 0;
	if ($sel =~ m/^\^(.*)/) { ## if the argument begins with a ^
	    $sel = $1;            ## it implies a NOT of the selection
	    if ($config->reportable) {
		Log(0, "'^' is not allowed for a reportable run, ignoring\n");
	    } else {
		$not = 1;
	    }
	}


	# look for the selection in the list of benchmark sets
	if (exists $config->{'benchsets'}{$sel}) {
	    my $ref = $config->{'benchsets'}{$sel}{'benchmarks'};
	    push (@temp, map { $ref->{$_} } keys %$ref);
	} else {
	    if ($config->reportable) { 
		Log(0, "Individual benchmark selection is not allowed for a reportable run;\n   ignoring '$sel'\n");
		next;
	    } else {
		my $temp = &find_benchmark($config, $sel);
		if (!defined $temp) {
		    Log(0, "Can't find benchmark '$sel'\n");
		} else {
		    @temp = ($temp);
		}
	    }
	} 

	if (!@temp) {
	    Log(0, "Can't parse '$sel' into a benchmark\n");
	    next;
	}

        ## process the temporary list of benchmarks
	for my $bench (sort { $a->benchmark cmp $b->benchmark } @temp) {
	    my $name = $bench->benchmark;
	    if ($not) { ## delete this benchmark from the list
		## don't bother removing it if we haven't added it yet
		next if !$benchmark_seen{$name};
		Log(4, "  '$name' removed\n");
		$benchmark_seen{$name} = 0;
		## remove (filter) it from the benchmarks list
		@benchmarks = grep($name ne $_->benchmark, @benchmarks);
	    } else { ## add it to the benchmark list
		next if $benchmark_seen{$name}; ## skip if we have seen it
		push (@benchmarks, $bench); ## add it to the final list
		$benchmark_seen{$name} = 1; ## flag that we have seen this
		Log(24, "  '$name' added\n");
	    }
	}
    }
    return @benchmarks; ## return the list
}

sub find_benchmark {
    my ($config, $name) = @_;
    my @objs = map { $config->benchmarks->{$_} } keys %{$config->benchmarks};
    if ($name =~ /^(\d+)$/ || $name =~ /^(\d{1,3})\./) {
	for (@objs) {
	    return $_ if ($_->num == $name);
	}
    } else {
	my $match;
	for (@objs) {
	    return $_ if ($_->name eq $name || $_->benchmark eq $name);

	    if ($_->name =~ m/^$name/) {
		return undef if defined $match;
		$match = $_;
	    }
	}

	return $match if defined $match;
    }
    return undef;
}

sub parse_raw {
    my ($fh, $config) = @_;
    my $prefix = $Spec::Format::raw::prefix;
    my $bsname = 'Who knows?';

    # Suck in the raw file
    my @raw = <$fh>;
    foreach my $ln (@raw) {
	if ($ln =~ /$prefix\.name:\s+(\S+)/o) { $bsname = $1; last; }
    };
    my $r = $config->{'benchsets'}->{$bsname};
    my $rawfile = '';
    foreach (@raw) {
	$rawfile .= $_;
	tr/\012\015//d;		# Hooray for Microsoft and NT!
    }
    my $comprawfile = '';
    eval '$comprawfile = Compress::Zlib::memGzip($rawfile)';
    if ($@) {
	$r->{'rawfile'} = main::encode_base64($rawfile);
	$r->{'compraw'} = undef;
    } else {
	$r->{'rawfile'} = undef;
	$r->{'compraw'} = main::encode_base64($comprawfile);
    }

    my ($tag, $value) = ('', undef);
    my (%seen) = (()); # clear, at least
    my $md5record = 0;
    my $ctx = new Digest::MD5;

    # Read in a RAW file from $fh and reconstruct a result object from it.
    while(defined($_ = shift(@raw))) {
	next if /^\s*$/o;
	next if /^\#/o;		# Allow comments in the raw file
	($tag, $value) = /^$prefix\.([^:]+):[ 	](\s*.*)/o;

	# This handles the submission info that the email handler tacks on
	if (defined($::cpu2000_website_formatter) &&
	    $::cpu2000_website_formatter &&
	    defined($::format_for_publication) &&
	    /^-SUBMIT-$/o) {
	    # Handle the submission information specially
	    my $submittedby_line = shift(@raw);
	    my $submitdate_line = shift(@raw);
	    if (!$::format_for_publication) {
		# These aren't appended to the notes on the public side
		push @{$r->{'notes'}}, ( '', $submittedby_line,
					 $submitdate_line,
					 shift(@raw) );
	    }
	    $submittedby_line =~ s/^[^:]+:\s+//;
	    $submitdate_line =~ s/^[^:]+:\s+//;
	    $r->{'Submitted_by'} = $submittedby_line;
	    $r->{'Submit_date'} = $submitdate_line;
	    next;
	}
	next unless (defined($tag) && defined($value) && ($tag ne ''));

	# Check for/add to the MD5 hash
	if ($md5record) {
	    tr/\015\012//d;
	    $ctx->add($_);
	}
	$md5record = 1	if ($tag eq 'rawhash');
	# The What-Goes-Where Heuristic: if $tag has no periods, it is an
	# 'info' thing.  If it does, it is a 'result' thing.  Otherwise
	# things become extremely wibbly.
	if ($tag !~ /\./o) { # 'info' thing
	    if ($tag =~ /^(.*?)(\d+)$/o) { # an ARRAY 'info' thing
		$r->{$1} = [] unless (exists ($r->{$1}) &&
                                      ref($r->{$1}) eq 'ARRAY');
		$r->{$1}->[$2] = $value;
	    } else {
		$r->{$tag} = $value;
	    }
	} else {
	    # a 'result' thing
	    my @comps = split(/\./, $tag);
	    # @comps should have exactly 5 elements, and the first should
	    # be 'results'
	    if (($#comps != 4) || (shift(@comps) ne 'results')) {
		Log(0, "Hey!  Wierd line in raw file:\n$_\n");
		next; # return undef; ?
	    }
	    my ($bench, $tune, $idx, $key) = @comps;
	    if (exists($r->{'iterations'}) && ($idx+1 > $r->{'iterations'})) {
		$r->{'iterations'} = $idx+1;
	    }
	    $bench =~ y/_/./;
	    if (!exists $seen{"$bench|$tune|$idx"}) {
		# Make it.
		$r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx] =
		    new Spec::Config();
		my $tmpref = $r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx];
		# Set up 'refs', so references to members will work
		$tmpref->{'refs'}  = [ $tmpref, $config ];
		# Set up 'errors', so it's there even if there aren't any
		$tmpref->{'errors'} = [];
		# Say what tuning was used
		$tmpref->{'tune'} = $tune;
		# Remember that it was made
		$seen{"$bench|$tune|$idx"} = 1;
	    }
	    my $rref = $r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx];
	    if ($key eq 'reference') {
		if (exists $r->{'reference'}->{$bench}) {
		    if ($r->{'reference'}->{$bench} != $value) {
			Log(0, "Whoa!  Reference time for iteration $idx of $bench doesn't match the\npreviously recorded reference time of".$r->{'reference'}->{$bench}."\n");
		    }
		} else {
		    $r->{'reference'}->{$bench} = $value;
		}
	    }
	    if ($key =~ /^(.*?)(\d+)$/o) {
		# It's an ARRAY 'result' thing
		$rref->{$1}->[$2] = $value;
	    } else {
		# It's a SCALAR 'result' thing
		$rref->{$key} = $value;
	    }
	}
    }

    # Let's make sure some things are set properly
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
    # 'table' is a slightly special case of a variable that the
    # user should be able to override at rawformat time
    if (exists $config->{'table'}) {
	$config->{'table'} = istrue($config->{'table'});
	$r->{'table'} = $config->{'table'} if ($config->{'table'} != istrue($r->{'table'}));
    }

    # Check to see if the MD5s match
    if (exists($r->{'rawhash'})) {
	my $genmd5 = $ctx->hexdigest();
	if ($r->{'rawhash'} !~ /$genmd5/i) {
	    Log(0, "\nError: corrupt result file; unable to format.\n");
            Log(0, "(This problem may occur if you edited your raw file using an editor that\n");
            Log(0, "changed the non-user-editable portions of the file.  If you have a backup of\n");
            Log(0, "the original, please restore it and try a different editor.)\n");
	    return undef;
	}
    }
    if (exists($r->{'suitever'})) {
	if (($r->{'suitever'} ne 'unknown') &&
	    ($r->{'suitever'} < 1.30)) {
	    Log(0, "The version of the CPU2000 suite used to produce this result is older than\n  the latest production version.\n");
	} elsif ($r->{'suitever'} > 100) {
	    Log(0, "The version of the CPU2000 suite used to produce this result is NOT a release\nversion.\n");
        }
    } else {
	Log(0, "The version of the CPU2000 suite used to produce this result is unknown.  It\n  might not be a release version!\n");
        $r->{'suitever'} = 'unknown';
    }
    if (exists($r->{'toolsver'})) {
	if (($r->{'toolsver'} ne 'unknown') &&
	    ($r->{'toolsver'} < 2.02)) {
	    Log(0, "The version of the CPU2000 tools used to produce this result is older than\n  the latest production version.\n");
	}
    } else {
	Log(0, "The version of the CPU2000 tools used to produce this result is unknown.  It\n  might not be a release version!\n");
        $r->{'toolsver'} = 'unknown';
    }
    if (istrue($config->rate) && !istrue($r->rate)) {
	$r->{'rate'} = 1;
	my $peakseen = 0;
	# We are being asked to format a speed result as a 1-way rate result
	foreach my $key (keys %seen) {
	    my ($bench, $tune, $idx) = split(/\|/, $key);
	    $peakseen = 1 if ($tune =~ /peak/io);
	    my $rref = $r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx];
	    if ($rref->size eq 'ref') {
		my $reference = $rref->reference;
		my $reported = $rref->reported_time;
		$rref->{'ratio'} = ($reported)?$reference / $reported:0;
		$rref->{'ratio'} *= $::rate_multiplier;
	    } else {
		$rref->{'reference'} = '--';
		$rref->{'ratio'} = '--';
	    }
	}
	$r->{'basemean'} = $r->calc_mean_rate('base');
	$r->{'peakmean'} = $r->calc_mean_rate('peak') if $peakseen;
    }
    # Compress the notes -- they may have gaps in them
    my ($currnote, $noteptr) = (0, 0);
    if (exists($r->{'notes'}) && ref($r->{'notes'}) eq 'ARRAY') {
      while ($noteptr < @{$r->{'notes'}}+0) {
        if (defined($r->{'notes'}->[$noteptr])) {
          if ($currnote != $noteptr) {
            $r->{'notes'}->[$currnote] = $r->{'notes'}->[$noteptr];
          }
          $currnote++;
        }
        $noteptr++;
      }
    }
    $#{$r->{'notes'}} = $currnote - 1;

    return $r;
}

1;

#
# locate.pm
#
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: locate.pl 1393 2005-06-14 15:55:21Z cloyce $

use strict;
use IO::Dir;

# This is the list of benchmarks that will be used for the Grand Unified
# SPEC CPU2000 Result Formatter. This should be updated whenever the *bset
# files change.
my %bmarklist = ('int' => { 'name' => 'int',
			    'units' => 'SPECint',
			    'metric' => 'CINT2000',
			    'longest_ref' => 100,
			    'benchmarklist' => [qw(
			      164.gzip 175.vpr 176.gcc
			      181.mcf 186.crafty 197.parser
			      252.eon 253.perlbmk 254.gap
			      255.vortex 256.bzip2 300.twolf
			    )]
			   },
		 'fp' => { 'name' => 'fp',
                           'units' => 'SPECfp',
			   'metric' => 'CFP2000',
			   'longest_ref' => 100,
			   'benchmarklist' => [qw(
                             168.wupwise 171.swim 172.mgrid 173.applu
                             177.mesa 178.galgel 179.art
                             183.equake 187.facerec 188.ammp
			     189.lucas 191.fma3d 200.sixtrack
                             301.apsi
                           )]
			  }
		 );

sub locate_benchmarks {
    my ($config) = @_;

    $config->{'formats'} = {} if !exists $config->{'formats'};

    my $benchdir = jp($config->top, $config->benchdir);
    my %seen;
    if (defined $::cpu2000_website_formatter &&
	$::cpu2000_website_formatter) {
	# We're just formatting, so don't bother to look for the
	# benchmark directories; use the hardcoded list instead
	foreach my $bset (keys %bmarklist) {
	    my $setclass = "Spec::Benchset::${bset}";
	    eval "package $setclass; \@${setclass}::ISA = qw(Spec::Benchset);";
	    my $setobj = $setclass->new($config);
	    for my $var (keys %{$bmarklist{$bset}}) {
		next if ($var eq 'benchmarklist');
		$setobj->{$var} = $bmarklist{$bset}->{$var};
	    }
	    #push @{$setobj->refs}, $config;
	    $setobj->{'name'} = $bmarklist{$bset}->{'name'};
	    $config->{'benchsets'}->{$setobj->name} = $setobj;
	    foreach my $bmark (@{$bmarklist{$bset}->{'benchmarklist'}}) {
		$bmark =~ /(\d+).(.*)/o;
		my ($num, $name) = ($1, $2);
		my $bmarkclass = "Spec::Benchmark::${name}";
		eval "package $bmarkclass; \@${bmarkclass}::ISA = qw(Spec::Benchmark); \$${bmarkclass}::benchname = \'$name\'; \$${bmarkclass}::benchnum = $num; \@${bmarkclass}::base_exe = (\'foo\');";
		my $bmobj = $bmarkclass->new("$num.$name", $config, $num, $name);
		push @{$setobj->{'benchmarklist'}}, $bmark;
		$config->{'benchmarks'}->{$bmark} = $bmobj;
	    }
	}
    } else {
	my $dh = new IO::Dir $benchdir;
	if (!defined $dh) {
	    Log(0, "Can't open benchmark directory '$benchdir': $!\n");
	    exit 1;
	}
	while (defined($_ = $dh->read)) { 
	    #print  "Reading '$_', '$benchdir'\n";
	    next if $_ eq '.' || $_ eq '..';
	    my $dir     = jp($benchdir, $_);
	    my $basedir = $_;
	    next if !-d $dir;
	    my $dh2 = new IO::Dir $dir;
	    while (defined($_ = $dh2->read)) { 
		my $topdir = jp ($dir, $_);
#print  "Searching $dir, $benchdir, $_\n";
		if (m/^(\d{3})\.(\S+)$/) {
		    my ($num, $name) = ($1, $2);
		    if ($seen{$name}) {
			Log(0, "'$name' appeared as a benchmark name more than once, ignoring\n");
			next;
		    }
		    my $specdir = jp($topdir, 'Spec');
#		my $pm = jp($specdir, "$name.pm");
		    my $pm = jp($specdir, 'object.pm');
		    if ($name =~ m/\./) {
			Log(0, "benchmark name '$name' is not allowed to have a '.' in it, ignoring\n");
		    } elsif (-d $specdir && -r $pm) {
			eval "package Spec::Benchmark::${name}; \@Spec::Benchmark::${name}::ISA = qw(Spec::Benchmark); require '$pm';";
			if ($@) {
			    Log(0, "Error requiring '$pm': $@\n");
			    next;
			}
			my $class="Spec::Benchmark::${name}";
			if (!$class->can('new')) {
			    Log(0, "No 'new' for class '$class' in '$pm'\n");
			    next;
			}
			my $obj = $class->new($topdir, $config, $num, $name);
			if (!defined($obj) || !ref($obj)) {
			    Log(0, "Error initializing '$pm'\n");
			    next;
			}
			$seen{$name}++;
			$config->{'benchmarks'}{$_} = $obj;
		    }
		} elsif (/^([^\/\\:;]+)\.bset$/o) {
		    my $name = $1;
		    eval "package Spec::Benchset::${name}; \@Spec::Benchset::${name}::ISA = qw(Spec::Benchset); require '$topdir';";
		    if ($@) {
			Log(0, "Error requiring benchset file '$topdir': $@\n");
			next;
		    }
		    my $class="Spec::Benchset::${name}";
		    if (!$class->can('new')) {
			Log(0, "No 'new' for class '$class' in '$topdir'\n");
			next;
		    }
		    my $obj = $class->new($config);
		    if (!defined($obj) || !ref($obj)) {
			Log(0, "Error initializing '$topdir'\n");
			next;
		    }
		    $config->{'benchsets'}{$obj->name} = $obj;
		}
	    }
	}
    }
    for my $set (keys %{$config->{'benchsets'}}) {
	my $obj = $config->{'benchsets'}{$set};
	my $ref = {};
	$config->{'benchsets'}{$set}{'benchmarks'} = $ref;
	my @benchmarks = @{$obj->{'benchmarklist'}};
	for my $bench (@benchmarks) {
	    if (!exists $config->{'benchmarks'}{$bench}) {
		Log(0, "Benchmark Set '$set' calls for non-existant benchmark '$bench'\n");
		$obj->valid(0);
	    }
	    $ref->{$bench} = $config->{'benchmarks'}{$bench};
	}
    }
    $config->{'setobjs'} = [ map {$config->{'benchsets'}{$_}} keys %{$config->{'benchsets'}} ];
}

sub locate_formats($) {
    my ($config, $quiet) = @_;
    Log(2, "Identifying output formats...") unless $quiet;

    my @formats = list_files(jp($config->top, 'bin', 'formats'));
    @formats = grep (/\.p[lm]$/o, @formats);

    for my $pm (@formats) { ## for each format .pl file
	my ($name) = $pm =~ m|([^/]+)\.p[lm]$|o;
	eval "package Spec::Format::${name}; \@Spec::Format::${name}::ISA = qw(Spec::Format); require '$pm';";
	if ($@) {
	    Log(12, "\nError requiring $pm for $name format: $@\nContinuing with output formats...");
	    next;
	}

	my $class= "Spec::Format::${name}";

	if (!$class->can('new')) {
	    Log(12, "\nNo 'new' function for class '$class' in '$pm'\n");
	    next;
	}

	## run the creation method for the object, i.e., "new"
	my $obj = $class->new;
	if (!defined($obj) || !ref($obj)) {
	    Log(12, "\nError initializing format object\n");
	    next;
	}
	$config->{'formats'}{$name} = $obj;
	Log(2, "$name...") unless $quiet;
    }
    Log(2, "\n") unless $quiet;
}

1;

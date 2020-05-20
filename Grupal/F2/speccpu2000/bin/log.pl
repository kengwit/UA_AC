#
# log.pm
#
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: log.pl 1393 2005-06-14 15:55:21Z cloyce $

use strict;
use IO::File;

my $log_handle = new IO::File ">&STDOUT";
my $log_opened = 0;

sub open_log {
    my ($config) = @_;

    # Find a name
    my $dir  = jp($config->top, $config->resultdir);
    my $name = jp($dir, $config->prefix . $config->log);
    # We've got to be careful about races here, too!
    # Some systems lack the ability to lock files that aren't opened for
    # writing.  So we open a file for writing in $SPEC/result, and make sure
    # that everyone can write to it.
    my $origumask = umask;
    my $rc = 0;
    umask 0000;			# Make sure that everyone can write it
    my $fh = new IO::File ">$name.lock";
    umask $origumask;		# Now put it back
    if (!defined $fh) {
	Log(0, "Couldn't open $name.lock for writing: $!\nI'll continue, but your log and results files may be munged if you're doing\nmultiple concurrent runs.  If not, there's nothing to worry about.\n");
    } else {
	# Get an exclusive lock on the lockfile
	eval '$rc = flock($fh, LOCK_EX);';
	if ($@) {
	    Log(0, "Your system doesn't implement file locking, so I can't guarantee that\n");
	    Log(0, "the run number (which determines the names of the log files and results files)\nwill be unique.  This is only an issue if there are multiple concurrent runs\nhappening using this copy of the benchmark.\n");
	}
	if (!$rc) {
	    Log(0, "The log lock file ($name.lock) couldn't be locked, so I can't guarantee that\n");
	    Log(0, "the run number (which determines the names of the log files and results files)\nwill be unique.  This is only an issue if there are multiple concurrent runs\nhappening using this copy of the benchmark.\n");
	}
	$fh->seek(0, 0);	# Probably unnecessary, but it won't hurt.
    }
    my $num  = find_biggest_ext($dir);
    $name = jp($dir, $config->prefix . $config->log . '.' . ++$num);
    $config->{'lognum'} = $num;
    if (!defined($log_handle)) {	# Default open of STDOUT must've failed
	$log_handle = new IO::File;
    }
    if (!$log_handle->open(">$name")) {
	Log(0, "Couldn't open log file '$name': $!\n");
	return 0;
    }
    $log_opened = 1;
    # Unbuffer the log file
    $log_handle->autoflush;

    # Unlock the file so that others may proceed.
    $rc = 0;
    eval '$rc = flock($fh, LOCK_UN);'; 
    if (!$rc && $@ eq '') {
	Log(0, "File locking is implemented on your system, but I couldn't unlock the log\n");
	Log(0, "file lock.  Other runs may wait indefinitely because of this.\n");
    }
    $fh->close();		# It'd close when we leave this sub anyway

    return 1;
}

## ############
## SUB                   LOG
## ############

## The first argument is the log level associated with the call.
## The following arguments hold the information that needs to be
## logged. The global $verbose level needs to be set to determine
## if the message is to be logged. This function does some
## remedial screen wrapping--breaking on character postion of
## the width rather than whitespace.

sub Log {			## The multi-level multi-type log writer.
				## The log statement will output if
				## the global verbosity level is equal to or
				## greater than the level associated with the
				## call to Log. There are messages that
				## are formatted both for the screen
				## and (separately) for the log file.
				## All messages with a level greater than
				## 90 are generated regardless of the
				## verbosity level.
    my ($level, @data) = @_;
    my ($type, @output);
    my $verbose    = $main::global_config->verbose;
    my $line_width = $main::global_config->line_width;
    my $log_line_width = $main::global_config->log_line_width;
    my $log_output    = "";
    my $screen_output = "";
    my $logfile = 0;
    for (@data) {		## handle each data type reference
	my $type = ref($_);
	my $data;

	if ($type eq "ARRAY") {	## arrays referenced are joined w/o separator
				## and then appended to the output
	    $data = join("", @$_);
	} elsif ($type eq "SCALAR") { ## scalars reference are just appended
	    $data = $$_;
	} else {		## if item is not a reference, but only a
				## explicit value, then just append that
				## value
	    $data = $_;
	}
	if ($level >= 100) {
	    $level   -= 100;
	    $logfile  = 1;
	}
	$log_output    .= $data if (($verbose >= $level) || $logfile);
	$screen_output .= $data if ($verbose >= $level);
    }
    if ($line_width > 0) { ## wrap lines for output
	## based on the line width specified
	## in the user configuration
	## screen line wrapping
	@output = split ("\n", $screen_output);
	for (@output) {		
	    while ($_) {
		print substr($_, 0, $line_width), "\n";
		substr($_, 0, $line_width) = "";
	    }
	}
    } else {
	print             $screen_output; ## screen
    }
    if ($log_opened != 0) {
	if ($log_line_width > 0) {
	    ## log line wrapping
	    @output = split ("\n", $log_output);
	    for (@output) {		
		while ($_) {
		    print $log_handle substr($_, 0, $log_line_width), "\n";
		    substr($_, 0, $log_line_width) = "";
		}
	    }
	} else {			## no line width specified
	    print $log_handle $log_output;	  ## log file
	}
    }
}

sub log_header {
    my ($config) = @_;
    my $tune       = join (',', @{$config->tunelist});
    my $action     = $config->action;
    my $verbose    = $config->verbose;
    my $ext        = join (',', @{$config->extlist});
    my $size       = join (',', @{$config->sizelist});
    my $mach       = join (',', @{$config->machlist});
    my $benchmarks = join (',', map {$_->benchmark} @{$config->runlist});
    my $outputs    = join (',', map {$_->name}      @{$config->formatlist});

    Log(140, <<EOV);
Verbosity = $verbose
Action    = $action
Tune      = $tune
Ext       = $ext
Size      = $size
Machine   = $mach
benchmarks= $benchmarks
outputs   = $outputs
EOV
}



1;

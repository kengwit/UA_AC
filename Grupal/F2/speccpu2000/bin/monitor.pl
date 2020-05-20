#
# monitor.pl
#
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: monitor.pl 1393 2005-06-14 15:55:21Z cloyce $

use strict;

sub monitor_shell {
    my ($name, $config) = @_;
    my $cmd = $config->accessor_nowarn($name);

    if (defined($cmd) && ($cmd ne '')) {
	Log(0, "Executing $name: $cmd\n");
	#log_system_noexpand($cmd, $name, $config);
	log_system($cmd, $name, $config);
	if ($?) {
	    Log(0, "$name returned non-zero exit code ($?)\n");
	    exit 1;
        }
    }

    $cmd = $config->accessor_nowarn("${name}_perl");
    if (defined($cmd) && ($cmd ne '')) {
	my $s = new Safe 'tmp';
	if ($global_config->safe_eval()) {
	    $s->permit_only(':base_core', ':base_mem');
	} else {
	    $s->deny_only();
	}
	$s->share('%ENV', '$global_config');
	$s->reval($cmd);
	if ($@) {
	    Log(0, "Error executing ${name}_perl\n", $@);
	}
	no strict 'refs';
	%{*{"main::".$s->root."::"}{HASH}} = ();
    }
}

sub monitor_pre        { monitor_shell('monitor_pre',        @_); }
sub monitor_pre_run    { monitor_shell('monitor_pre_run',    @_); }
sub monitor_pre_bench  { monitor_shell('monitor_pre_bench',  @_); }
sub monitor_post_bench { monitor_shell('monitor_post_bench', @_); }
sub monitor_post       { monitor_shell('monitor_post',       @_); }

1;

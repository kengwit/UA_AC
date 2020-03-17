#
# os.pm
#
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: os.pl 1393 2005-06-14 15:55:21Z cloyce $

use strict;

# Set up a process group if we can
sub setProcGrp($) {
    my ($config) = @_;
    eval "setpgrp;";
    if (!$@) {
	$SIG{'INT'} = 'handle_sigint' 
    } else {
	$config->{'setpgrp_enabled'} = 1;
    }
}

sub initialize_os($) {
    my ($config) = @_;
    setProcGrp($config) if istrue($config->setprocgroup);
}

1;

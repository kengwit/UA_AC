#
# listfile.pm
#
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: listfile.pm 1393 2005-06-14 15:55:21Z cloyce $

package Spec::Listfile::entry;

use strict;

sub new {
    my ($name, $ref) = @_;
    return bless $ref, $name;
}

sub lock {
    my $me = shift;

    $me->{'lock'} = 1;
    return $me;
}

sub unlock {
    my $me = shift;

    $me->{'lock'} = 0;
    return $me;
}

sub remove {
    my $me = shift;
    for (keys %$me) {
	delete $me->{$_};
    }
    return $me;
}

sub delete {
    my $me = shift;

    $me->{'lock'} = 0;
    return $me;
}

sub name {
    my $me = shift;
    return $me->{'name'};
}

sub path {
    my $me = shift;
    return $me->{'dir'};
}

package Spec::Listfile;

use IO::File;
use Fcntl qw(O_CREAT O_RDWR O_EXCL LOCK_EX LOCK_UN);
use strict;

sub jp { main::joinpaths(@_); }

sub new {
    my ($class, $dir, $fname) = @_;
    my $me = bless {}, $class;

    $me->{'dir'}      = $dir;
    $me->{'filename'} = jp($dir,$fname);

    $me->open();

    return $me;
}

sub open {
    my ($me, $fname) = @_;
    my $config = $main::global_config;

    if (defined($fname) && ($fname ne '')) {
	$me->{'filename'} = $fname;
    }
    $fname = $me->{'filename'};

    my $fh = new IO::File;

# If Lockfile doesn't exist, create it.  Force IO::File to use sysopen with
# exclusive create.  This means that there will be only one lock file created,
# of course for NFS all bets are off, and multi-user is at your own risk.
    if ( ! -f "$fname") {
	my @dir = (::dirname($fname));
	if (! -d $dir[0]) {
	    while (! -d $dir[0]) {
		unshift @dir, ::dirname($dir[0]);
	    }
	    foreach my $dir (@dir) {
		mkdir $dir, 00777;
	    }
	}
	# Jeff Reilly wants list files to always be at least mode 666, so
	# we'll do some umask fiddling to accomodate.  We'll use eval, just
        # to be safe.
        my $oldumask;
	eval { $oldumask = umask 00000; };	# We don't care if it fails
	my $rc = $fh->open($fname, O_RDWR|O_CREAT|O_EXCL, 00666);
	eval { umask $oldumask; }; 		# as long as we don't die too
	$fh->close() if ($rc);
    }

# Grab and lock file
    if (! $fh->open("+<$fname")) {
	::Log(0, "Can't open list file '$fname': $!\n");
        # This can't be ignored
        exit 1; 
    }
    flock($fh,LOCK_EX) if ($config->locking);
    $me->{'filehandle'} = $fh;

# Parse the file
    $me->parse();

    return $me;
}


# Read the file data into the object
sub parse {
    my $me = shift;
    my $fh = $me->{'filehandle'};

# Get rid of old data
    $me->{'data'} = {}; 

    seek ($fh, 0, 0); # Read file from beginning

    while (<$fh>) {
	next if m/^\s*#/ || m/^\s*$/;
	last if m/^__END__/;
	my ($name, @data) = split(' ', $_);
	for (@data) {
	    if (m/(\S+)=(\S+)/) {
		$me->{'data'}{$name}{$1}=$2;
	    }
	}
	$me->{'data'}{$name}{'name'} = $name;
	$me->{'data'}{$name}{'lock'} = 0 if !defined $me->{'data'}{$name}{'lock'};
    }
}

sub update {
    my $me = shift;
    my $fh = $me->{'filehandle'};

    seek($fh, 0, 0);
    for my $name (sort keys %{$me->{'data'}}) {
	next if keys %{$me->{'data'}{$name}} == 0;
	next if exists($me->{'data'}) && exists($me->{'data'}->{$name}) &&
	    ($me->{'data'}->{$name}->{'delete'} ne '');
	print $fh "$name";
	for (sort keys %{$me->{'data'}{$name}}) {
	    next if $_ eq 'name';
	    print $fh " $_=",$me->{'data'}{$name}{$_};
	}
	print $fh "\n";
    }
    print $fh "__END__\n";
# Attempt to be nice and truncate the file, this isn't too important
# as the __END__ tag will caues the rest of the file to be ignored.
# eval to keep operating systems without truncate happy.
    eval '$fh->truncate(tell($fh))';
}

sub find_entry {
    my $me = shift;
    my %criteria = (@_);

    loop:
    for my $name (sort keys %{$me->{'data'}}) {
	for my $crit (keys %criteria) {
	    next loop if $me->{'data'}{$name}{$crit} ne $criteria{$crit};
	}
	my $entry = new Spec::Listfile::entry($me->{'data'}{$name});
	return $entry;
    }
    return undef;
}

# Fast way to find an entry by it's name
sub find_entry_name {
    my ($me, $name) = @_;

    if (defined $me->{'data'}{$name}) {
	return new Spec::Listfile::entry($me->{'data'}{$name});
    }
    return undef;
}

sub remove_entry {
    my $me = shift;
    my $entry = $me->find_entry(@_);
    $entry->remove() if ($entry);
}

sub new_entry {
    my $me = shift;
    my %criteria = (@_);
    my $config = $main::global_config;

    my $name = '00000000';
    for (keys %{$me->{'data'}}) {
	$name = $_ if ($name lt $_); 
    }
    if ($name eq '99999999') {
	::Log(0, "Too many working directories!\n");
        exit 1;
    }
    $name++;

    $me->{'data'}{$name}={};  # Make sure there is a hash

    $me->{'data'}{$name}{'dir'}=jp($me->{'dir'},$name);
    for (keys %criteria) {
	$me->{'data'}{$name}{$_}=$criteria{$_};
    }
    $me->{'data'}{$name}{'name'} = $name;
    my $entry = new Spec::Listfile::entry($me->{'data'}{$name});
    return $entry;
}

sub close {
    my $me = shift;
    if (defined $me->{'filehandle'}) {
# Close should unlock the file
#	flock(FILE,LOCK_UN) if ($::spec{'locking'});
	close($me->{'filehandle'});
	delete $me->{'filehandle'};
    }
}

sub DESTROY {
    my $me = shift;
    $me->close();
}

1;

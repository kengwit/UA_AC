#
# format.pm
#
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: format.pm 1585 2005-08-24 23:20:16Z cloyce $
#
package Spec::Format;

sub new {
    my ($class) = @_;
    my $me       = bless {}, $class;

    $me->{'name'}      = ${"${class}::name"};
    $me->{'extension'} = ${"${class}::extension"};
    $me->{'binary'}    = defined(${"${class}::binary"}) ? ${"${class}::binary"} : 0;

    return $me;
}

sub name {
    my $me = shift;
    return $me->{'name'};
}

# Search through the list of references for the data, can assign
# data too if an entry for the data exists.
sub accessor {
    my ($me, $what, @rest) = @_;
    if (! exists $me->{$what}) {
	Log(0, ref($me) . " accessor '$what' not found!\n");
	$DB::signal = $DB::single = 1;
	return undef;
    }
    my $old = $me->{$what};
    if (@rest) {
	if (ref($old) eq 'ARRAY') {
	    $me->{$what} = [ @rest ];
	} elsif (ref($old) eq 'HASH') {
	    $me->{$what} = { @rest };
	} else {
	    $me->{$what} = $rest[0];
	    Log(0, "accessor '$what' passed too much data for scalar!\n")
		if @rest > 1;
	}
    }
    return $old;
}

AUTOLOAD {
    use vars qw($AUTOLOAD);
    my $name;
    my ($pkg,$func) = $AUTOLOAD =~ /(.*)::([^:]+)$/;
    if ($func eq 'DESTROY') {
	eval "package $pkg; sub $func {}";
    } else {
	eval "package $pkg; sub $func { shift->accessor('$func', \@_); }";
    }
    goto &$AUTOLOAD;
}

1;

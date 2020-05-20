#
# config.pl
#
# Copyright (C) 1999-2001 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: config.pl 1393 2005-06-14 15:55:21Z cloyce $
#
package Spec::Config;

use strict;
use IO::File;

# Create a new object
sub new {
    my ($class) = @_;
    my $me = bless {}, $class;

# These are used for accessor lookups.  Initialize to be the base level
    $me->{'refs'} = [ $me ];
    $me->{'ref'} = $me;

    $me->{'ref_tree_level'} = 'Top level Config';

    return $me;
}

# Load a file and merge it with the current data
sub merge {
    my ($me, $filename, $included) = @_;
    my ($include, $name, $value, @vals);
    my $ref = $me;
    my $spec = $main::spec;
    my $continued = 0;
    my $appended = 0;
    my $blockquote = undef;
    my $first_blockquote = undef;
    my $lastref = undef;
    my $md5mode = 0;

    $filename .= ".cfg" if ($filename !~ m/\./);
    $filename = jp($me->top, $me->configdir, $filename);

    $me->{'configpath'} = $filename unless $included;
    my $fh = new IO::File;
    if (!$fh->open($filename)) {
	Log(0, "Can't read config file '$filename': $!\n");
	return 0;
    }

    if ($included) {
	$me->{'rawtxtconfigall'} .= "# ---- Begin inclusion of '$filename'\n";
	$me->{'rawtxtconfig'} .= "# ---- Begin inclusion of '$filename'\n";
    } else {
	$me->{'oldmd5'} =  '';
	$me->{'rawtxtconfigall'} =
	$me->{'rawtxtconfig'} = 
          "# Invocation command line:\n# $0 ".join(' ', @{$me->{'orig_argv'}})."\n############################################################################\n";
    }

    while (<$fh>) {
	last if (m/^__END__$/);
	if (m/^__MD5__$/) {
	    $md5mode = 1;
	    next;
	}

# Save off this line but not protected comments that begin with '#>'
	if ($md5mode) {
	    $me->{'oldmd5'} .= $_       
	} else {
	    $me->{'rawtxtconfigall'} .= $_;
	    if (m/^\s*include:\s*\S+\s*$/i) {
		$me->{'rawtxtconfig'} .= "#$_";
	    } else {
		$me->{'rawtxtconfig'} .= $_ unless m/^\s*\#>/;
	    }
	}
        # Trim comments
        s/\#.*//o;		# The \ makes emacs happy

# Handle <<EOT type blocks in config file
	if (defined $blockquote) {
	    tr/\015\012//d;
	    if ($_ eq $blockquote) {
		undef $blockquote;
	    } elsif ($first_blockquote) {
		$$lastref = $_;
		$first_blockquote=0;
	    } else {
		$$lastref .= "\n$_";
	    }
# Handle continued lines with '\' at end of line
	} elsif ($continued) {
	    tr/\015\012//d;
	    $continued = 0;
	    $appended = 1 if s/\\\\$//;
	    $continued = 1 if s/\\$//;
	    $$lastref .= "\n$_";
# Handle appended lines with '\\' at end of line
	} elsif ($appended) {
	    tr/\015\012//d;
	    $appended = 0;
	    $appended = 1 if s/\\\\$//;
	    $continued = 1 if s/\\$//;
	    $$lastref .= " $_";
# Include a file
	} elsif (($include) = m/^\s*include:\s*(\S+)\s*$/i) {
	    if (! $me->merge($include, 1)) {
		tr/\015\012//d;
		Log(0, "Can't include file '$include'\n");
		return 0;
	    }
# Check to see if the line is in the form of x=x=x=x: or some subset.
# if so, then point the working reference pointer at that data.
	} elsif ((@vals) = 
	    m/^([^#=\s]+)(?:=([^=\s]+))?(?:=([^=\s]+))?(?:=([^=\s]+))?:\s*$/o) {
	    for (my $i = 0; $i < 4; $i++) {
		$vals[$i] = 'default' if $vals[$i] eq '';
		if ($vals[$i] =~ /:/o) {
		    tr/\015\012//d;
		    Log(0, "':' is not allowed in a section name:\n '$vals[$i]' in '$_'\n");
		    $vals[$i] =~ s/://go;
		}
	    }
	    my ($bench, $tune, $ext, $mach) = @vals;
 	    $me->{$bench} = {} if ref($me->{$bench}) ne 'HASH';
	    $me->{$bench}{$tune} = {} if ref($me->{$bench}{$tune}) ne 'HASH';
	    $me->{$bench}{$tune}{$ext} = {} if ref($me->{$bench}{$tune}{$ext}) ne 'HASH';
	    if (ref($me->{$bench}{$tune}{$ext}{$mach}) ne 'HASH') {
		$ref = $me->{$bench}{$tune}{$ext}{$mach} = {};
	    }
	    $ref = \%{$me->{$bench}{$tune}{$ext}{$mach}};
# Named pairs to current working reference
	} elsif (($name, $value) = 
	    m/^\s*([A-Za-z0-9_]+:?(?:[0-9]*|\*))\s*=(.*)/) {
	    if ($name =~ /:/o) {
		tr/\015\012//d;
		Log(0, "':' is not allowed in a variable name:\n '$name' in '$_'\n");
		$name =~ s/://go;
	    }
	    # Everything except notes should have leading whitespace removed
	    $value =~ s/^\s*//o unless ($name =~ /^notes/io);

# If it is the special case %undef% then remove the specified keys
# from the working reference
	    if ($value =~ m/^\s*%undef%\s*$/o) {
		for (find_keys($ref, $name)) {
		    delete $ref->{$_} if exists $ref->{$_};
		}
	    } elsif (!$md5mode && ($name eq 'optmd5' || $name eq 'exemd5')) {
		next;
	    } elsif ($name eq 'rawtxtconfig' || $name eq 'rawtxtconfigall') {
		next; # We don't allow people to overwrite rawtxtconfig
	    } elsif ($name eq 'users') {
		# users is special because it needs to be translated to
		# userlist to make any difference
		$ref->{'userlist'} = [ split(/,+|\s+/, $value) ];
	    } else {
		if ($value=~ s/\s*<<\s*(\S+)\S*$//) {
		    $blockquote = $1;
		    $first_blockquote = 1;
		    $value = '';
		} elsif ($value=~ s/\\\\\s*$//) {
		    $appended  = 1;
		} elsif ($value=~ s/\\\s*$//) {
		    $continued  = 1;
		}
    		$ref->{$name} = $value;
		$lastref = \$ref->{$name};
	    }
	}
    }
    if ($included) {
	$me->{'rawtxtconfigall'} .= "# ---- End inclusion of '$filename'\n";
	$me->{'rawtxtconfig'} .= "# ---- End inclusion of '$filename'\n";
    }
    
    $fh->close();
    1;
}

sub users {
    my $me = shift;
    return 1 if !istrue($me->rate);
    return main::expand_ranges(@{$me->userlist});
}

sub max_users {
    my $me = shift;
    return 1 if !istrue($me->rate);
    return $me->{'max_users'} if exists($me->{'max_users'}) && ($me->{'max_users'} > 0);
    return main::max(main::expand_ranges(@{$me->userlist}));
}

sub bytrailingnum {
    my ($anum) = $a =~ m/([0-9.]+)\s*$/;
    my ($bnum) = $b =~ m/([0-9.]+)\s*$/;
    my $rc =  $anum <=> $bnum;
    return $rc if $rc;
    return $a cmp $b;
}

# search keys of an array based on a limited wild card (*)
# and returns the list of keys that match/contain the pattern
sub find_keys {
    my ($me, $pat) = @_;
    my @temp;
    if ($pat =~ s/\*$//) { # if pattern ends in "*"
	@temp = sort bytrailingnum grep (m/^$pat[0-9.]*/, list_keys($me));
    } else {
	@temp = ($pat) if exists $me->{$pat};
    }
    return @temp;
}

# Return a list of sets that the benchmarks are in
sub benchmark_in_sets {
    my ($me, @benchmarks) = @_;
    my %sets;

    for my $bench (@benchmarks) {
	for my $set (keys %{$me->{'benchsets'}}) {
	    $sets{$set}++ if exists $me->{'benchsets'}{$set}{'benchmarks'}{$bench};
	}
    }
    return keys %sets;
}

sub list_keys {
    my ($me) = @_;
    my %seen;
    my @rc;
    my @refs = ();
    if (ref($me) eq 'HASH') {
	if (exists($me->{'refs'}) && ref($me->{'refs'}) eq 'ARRAY') {
	    @refs = @{$me->{'refs'}};
	} else {
	    @refs = ($me);
	}
    } else {
	@refs = (@{$me->refs});
    }
    for my $hash (@refs) {
	for my $key (keys %$hash) {
#	    next if ref($hash->{$key}) ne '';
	    push (@rc, $key) if !$seen{$key}++;
	}
    }
    return sort @rc;
}

sub match_keys {
    my ($me, @pats) = @_;
    my @rc;
    for my $val (list_keys($me)) {
	for my $pat (@pats) {
	    if ($val =~ m/^${pat}[0-9.]*$/) {
		push (@rc, $val);
		next;
	    }
	}
    }
    return sort bytrailingnum @rc;
}

# Set the search path for the accessor function.  Search from most specific to
# least
sub set_search {
    my ($me, $bench, $tune, $ext, $mach) = @_;
    my @refs;

    my @benchlist;
    my @tunelist;
    my @extlist;
    my @machlist;

    if (ref($bench)) {
	if (ref($bench) ne 'ARRAY') {
	    Log(0, "set_search: bench is a ref other than ARRAY!\n");
	    exit 1;
	}
	@benchlist = (@$bench);
    } else  {
	@benchlist = ($bench);
    }

# Always check the base level of the hash

    my @sets = $me->benchmark_in_sets($bench);
    $me->{'refs'} = [
	reverse ($me, $me->ref_tree(['default', @sets, @benchlist],
				['default', $tune],
				['default', $ext],
				['default', $mach])) ];
    $me->{'ref'} = $me->{$bench}{$tune}{$ext}{$mach};
    $me->{'current_bench'} = $bench;
    $me->{'current_tune'}  = $tune;
    $me->{'current_ext'}   = $ext;
    $me->{'current_mach'}  = $mach;
}

# Not used
sub ref_order {
    my ($me) = @_;
    my @refs = @{$me->{'refs'}};
    printf "ref has %d entries!\n", scalar(@refs);
    for (@refs) {
	if ($_->{'ref_tree_level'} ne '') {
	    print $_->{'ref_tree_level'}, "\n";
	} else {
	    print "Unknown!\n";
	}
    }
}

my @ref_tree_vals = ();
# Build a tree of references
sub ref_tree {
    my ($ref, $leafs, @rest) = @_;
    my %seen;
    my @rc;
    for (@$leafs) {
	next if $seen{$_}++;
#push (@ref_tree_vals, $_);
	$ref->{$_} = { } if ref($ref->{$_}) ne 'HASH';
	push (@rc, $ref->{$_});
#$ref->{$_}{'ref_tree_level'} = join (' ', @ref_tree_vals);
#print $ref->{$_}{'ref_tree_level'}, "\n";
	push (@rc, ref_tree($ref->{$_}, @rest)) if @rest > 0;
#pop(@ref_tree_vals);
    }
    return @rc;
}

sub unshift_ref {
    my ($me, @refs) = @_;
    unshift (@{$me->{'refs'}}, @refs);
    $me->{'ref'} = $me->{'refs'}[0];
}
sub shift_ref {
    my ($me) = @_;
    my $out = shift @{$me->{'refs'}};
    $me->{'ref'} = $me->{'refs'}[0];
    return $out;
}
sub push_ref {
    my ($me, @refs) = @_;
    push (@{$me->{'refs'}}, @refs);
    $me->{'ref'} = $me->{'refs'}[0];
}
sub pop_ref {
    my ($me) = @_;
    my $out = pop @{$me->{'refs'}};
    $me->{'ref'} = $me->{'refs'}[0];
    return $out;
}

# Search through the list of references for the data, can assign
# data too if an entry for the data exists.
sub accessor_combined {
    my ($me, $warn, $what, @rest) = @_;
    my $old;
    for (@{$me->{'refs'}}) {
	if (exists $_->{$what}) {
	    $old = $_->{$what};
	    last;
	}
    }
    if (! defined $old) {
	if ($warn) {
	    my ($pkg, $file, $ln) = caller;
	    Log(0, "WARNING: accessor '$what' not found! called from $pkg @ line $ln in $file for object ".ref($me)."\n");
	    $DB::single=$DB::signal=1;
	}
	return undef if !@rest;
    }
    if (@rest) {
	my $ref = $me->{'refs'}->[0];
	if (ref($old) eq 'ARRAY') {
	    $ref->{$what} = [ @rest ];
	} elsif (ref($old) eq 'HASH') {
	    $ref->{$what} = { @rest };
	} else {
	    $ref->{$what} = $rest[0];
	    Log(0, "accessor '$what' passed too much data for scalar!\n")
		if @rest > 1;
	}
    }
    return $old;
}

sub accessor_backend {
    my ($me, $warn, $what, @rest) = @_;
    my $old;
    for (@{$me->{'refs'}}) {
	if (exists $_->{$what}) {
	    $old = $_->{$what};
	    last;
	}
    }
    if (! defined $old) {
	if ($warn) {
	    my ($pkg, $file, $ln) = caller;
	    Log(0, "WARNING: accessor '$what' not found! called from $pkg @ line $ln in $file for object ".ref($me)."\n");
	    $DB::single=$DB::signal=1;
	}
	return undef if !@rest;
    }
    if (@rest) {
	my $ref = $me->{'refs'}->[0];
	if (ref($old) eq 'ARRAY') {
	    $ref->{$what} = [ @rest ];
	} elsif (ref($old) eq 'HASH') {
	    $ref->{$what} = { @rest };
	} else {
	    $ref->{$what} = $rest[0];
	    Log(0, "accessor '$what' passed too much data for scalar!\n")
		if @rest > 1;
	}
    }
    return $old;
}

sub accessor {
    my $me = shift;
    $me->accessor_backend(1, @_);
}
sub accessor_nowarn {
    my $me = shift;
    $me->accessor_backend(0, @_);
}

# Automagically create new accessor functions for the class
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

# Alias some main:: package functions into our namespace so we don't have to
# keep calling out the package
sub jp { main::jp(@_); }
sub istrue { main::istrue(@_); }
sub Log { main::Log(@_); }

1;

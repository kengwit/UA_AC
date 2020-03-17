$benchnum  = '253';
$benchname = 'perlbmk';
$exename   = 'perlbmk';
$benchlang = 'C';
@base_exe  = ($exename);

@common_sources=qw(av.c deb.c doio.c doop.c dump.c fake_dyna.c globals.c gv.c
		    hv.c mg.c op.c perl.c perlio.c perly.c pp.c pp_ctl.c
		    pp_hot.c pp_sys.c regcomp.c regexec.c run.c scope.c sv.c
		    taint.c toke.c universal.c util.c md5c.c MD5.c);
@nt_sources=qw(nt_perlmain.c win32.c win32io.c win32sck.c perllib.c);
@unix_sources=qw(unix_perlmain.c malloc.c);
$need_math='yes';

sub sources {
    my $me = shift;
    my $os = $me->OS;
    if ($me =~ m/^windows/ && !$ENV{'SPEC_NOT_REALLY_WINDOWS'}) {
	return (@common_sources, @nt_sources);
    } else {
	return (@common_sources, @unix_sources);
    }
}

# jh 7-May-1998: rather than looking for mumble.in, look for mumble.pl
sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;

    my $exe = $me->exe_file;

    for ($me->input_files_base) {
        next if m#/#o;  # Don't descend into subdirs
	if (($name) = m/(.*).pl$/) {
	    if ($name =~ /splitmail|perfect|diffmail/) {
		# Suck the data from a file
		open(IN, "<$name.in") || next;	# Definitely don't die here
		my $params = '';
		while(defined($params = <IN>)) {
		    next if ($params =~ /^(#|$)/o);
		    my @params = split(/\s+/, $params);
		    push @rc, { 'command' => $exe,
				'args' => [ '-I./lib', $_, @params ],
				'output' => join('.', @params).'.out',
				'error' => join('.', @params).'.err',
			      };
		}
	    } else {
              my $cmdhash = { 'command' => $exe,
			      'args'    => [ '-I.', '-I./lib', $_ ], 
                              'output'  => "$name.out",
                              'error'   => "$name.err" };
              if (-f "$name.in") {
                $cmdhash->{'input'} = "$name.in";
              }
              push @rc, $cmdhash;
	    }
	}
    }
    @rc;
}

1;

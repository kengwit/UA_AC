$benchnum  = '189';
$benchname = 'lucas';
$exename   = 'lucas';
$benchlang = 'F';
@base_exe  = ($exename);

@sources=qw(lucas_distrib_spec.f90);

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;

    my $exe = $me->exe_file;
    for ($me->input_files_base) {
	if (($name) = m/(.*).in$/) {
	    push (@rc, { 'command' => $exe, 
			 'args'    => [ ], 
			 'input'   => $_,
			 'output'  => "$name.out",
			 'error'   => "$name.err",
			});
	}
    }
    return @rc;
}

1;

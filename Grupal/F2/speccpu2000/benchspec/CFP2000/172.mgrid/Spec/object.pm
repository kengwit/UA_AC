$benchnum  = '172';
$benchname = 'mgrid';
$exename   = 'mgrid';
$benchlang = 'F77';
@base_exe  = ($exename);

$reltol   = 0.001;
$abstol   = 1.0e-12;

@sources=qw(mgrid.f);

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

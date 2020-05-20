$benchnum  = '187';
$benchname = 'facerec';
$exename   = 'facerec';
$benchlang   = 'F';
@base_exe  = ($exename);

$reltol   = {'hops.out' => .2, 'default' => 0.001};
$abstol   = {'hops.out' => 5, 'default' => 2.e-7};
$skiptol   = {'hops.out' => 4, 'default' => 0};

@sources=qw(cfftb.f cfftf.f cffti.f FaceRec.f90 gaborTrafo.f90
	    imageRoutines.f90 fft2d.f90 graphRoutines.f90
	    parameterRoutines.f90);

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

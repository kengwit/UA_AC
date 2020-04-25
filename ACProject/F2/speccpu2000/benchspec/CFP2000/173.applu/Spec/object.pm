$benchnum  = '173';
$benchname = 'applu';
$exename   = 'applu';
$benchlang = 'F77';
@base_exe  = ($exename);

$reltol   = 0.0001;
$abstol   = undef;

@sources=qw(applu.f);

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

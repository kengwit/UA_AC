$benchnum  = '171';
$benchname = 'swim';
$exename   = 'swim';
$benchlang = 'F77';
@base_exe  = ($exename);

$reltol   = 0.2;
$reltol = {'SWIM7'   => 0.2, 'default' => 0.001};
$abstol   = 1.0e-6;

@sources=qw(swim.f);

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

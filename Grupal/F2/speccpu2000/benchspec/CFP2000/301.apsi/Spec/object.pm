$benchnum  = '301';
$benchname = 'apsi';
$exename   = 'apsi';
$benchlang = 'F77';
@base_exe  = ($exename);

# Testing these

$reltol   = 0.01;
$abstol   = 0.0000001;

# will return later .. $reltol   = 0.08;
# will return later .. $abstol   = 0.0000001;
# will return later .. $skiptol  = 1;

@sources=qw(apsi.f);

sub invoke {
    my ($me) = @_;

    return ({ 'command' => $me->exe_file, 
		    'args'    => [ ], 
		    'output'  => 'apsi.out',
		    'error'   => 'apsi.err',
		});
}

1;

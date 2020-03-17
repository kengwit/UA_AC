$benchnum  = '168';
$benchname = 'wupwise';
$exename   = 'wupwise';
$benchlang = 'F77';
@base_exe  = ($exename);

$reltol = {'te.out'   => 0.005, 'default' => 0.001};
$abstol = undef;

@sources=qw(caxpy.f cscal.f matmul.f rndcnf.f slarnd.f xerbla.f ccopy.f
	    gammul.f muldeo.f rndphi.f su3mul.f cdotc.f init.f muldoe.f
	    scnrm2.f uinith.f lsame.f cgemm.f phinit.f slaran.f wupwise.f);

sub invoke {
    my ($me) = @_;
    my $name = $me->name;
    return ({ 'command' => $me->exe_file, 
		 'args'    => [ ], 
		 'error'   => "$name.err",
		 'output'  => "$name.out",
		});
}

1;

$benchnum  = '178';
$benchname = 'galgel';
$exename   = 'galgel';
$benchlang = 'F';
@base_exe  = ($exename);

$reltol   = 0.01;
$abstol   = 2.e-8;

@sources=qw(modules.f90 ab.f90 arhbt.f90 arhim.f90 bifg21.f90 bifgel.f90
	    bifoag.f90 cyklap.f90 eigQR.f90 farhim.f90 funht.f90 funns.f90
	    galgel.f90 grsyst.f90 htxyl.f90 htxynl.f90 newtap.f90 newtlap.f90
	    nsxyar.f90 nsxyl.f90 nsxynl.f90 pollin.f90 polnel.f90 pro.f90
	    syshtL.f90 syshtN.f90 sysnsL.f90 sysnsN.f90 tempbt.f90 tempo.f90
	    tminit.f90 tnelgo.f90 tsubo.f90 vxrigid.f90 vyfree.f90 xconduc.f90
	    yadiab.f90 lapak.f90);

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

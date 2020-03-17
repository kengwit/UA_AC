$benchnum  = '200';
$benchname = 'sixtrack';
$exename   = 'sixtrack';
$benchlang = 'F77';
@base_exe  = ($exename);

$reltol = 0.0005;
$abstol = 0.0005;

@sources=qw(abend.f adia.f adib.f ambm6.f analie6.f betalf.f block.f blockdis.f
	    caconv.f calrms.f chroma.f cinvar.f clor6.f clorb.f clorb2.f
	    combel.f comnul.f corrorb.f cpart6.f cphase.f cpltwis.f creat6.f
	    ctrbasi6.f dacond6.f dacopd6.f dalie6s.f dalind6.f daprid6.f
	    daten.f datime.f decoup.f detune.f dinv.f dist1.f distance.f
	    dlie6.f eig66.f envada.f envar.f envardis.f envars.f envarsv.f
	    envquad.f errf.f errff.f error.f fexpo6.f fft.f fit.f flush.f
	    forest6.f foxys.f gam6.f gbm6.f gofix6.f hbook.f htal.f htbl.f
	    htls.f htrl.f htul.f hyper6.f igmeta.f intd6.f intepr.f join.f
	    lie6.f linopt.f loesd.f lostpar2.f lostpar3.f lostpart.f maincr.f
	    mainia6.f matrix.f maxmin.f midbloc6.f mul66.f mywwerf.f orbinit.f
	    ord.f orderma6.f phasad.f postpr.f putorb.f qmod.f ranecu.f resb6.f
	    resex.f resl6.f rext6.f ripple.f rmod.f rotatio6.f search.f
	    sinpro.f sixdaco6.f subre.f subsea.f sumpos.f syn_thck.f synoda.f
	    take6.f taked6.f thck4d.f thck6d.f thck6d_a.f thin4d.f thin6d.f
	    thin6d_a.f tra_thck.f tra_thin.f trx6.f tuer6.f umlau6.f umlauf.f
	    write4.f write6.f writebin.f writelin.f wzset.f wzsub.f wzsubf.f
	    wzsubv.f xuflow.f);

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;

    for ($me->input_files_base) {
	if (($name) = m/(.*).in$/) {
	    push (@rc, { 'command' => $me->exe_file, 
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

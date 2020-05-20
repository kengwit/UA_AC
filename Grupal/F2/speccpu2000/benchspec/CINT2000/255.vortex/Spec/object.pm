$benchnum  = '255';
$benchname = 'vortex';
$exename   = 'vortex';
$benchlang = 'C';
@base_exe  = ($exename);

@sources=qw(bitvec.c bmt.c bmt0.c bmt01.c bmt10.c bmtlib.c bmtobj.c core01.c
	    dba.c dbm0.c dbm1.c dbm2.c domain.c draw07.c draw7.c drawlib.c
	    drawobj.c emplib.c empobj.c env0.c env01.c env1.c fm.c gdbm.c
	    grp0.c grp1.c grp2.c hm.c iam.c ifm.c im.c km.c list01.c mem00.c
	    mem01.c mem10.c oa0.c oa1.c oadmp.c obj01.c odbm.c ogrp.c om.c
	    point.c primal.c pstub.c query.c rect.c rects.c sa.c shell.c sm.c
	    testobj.c tm.c trans00.c trans01.c trans10.c trans20.c tree0.c
	    tree00.c tree01.c ut.c vchunk.c vdbm.c voa.c vom.c);
$need_math='yes';

sub invoke {
    my ($me) = @_;
    my @rc;

    my $endian = "unknown";
    my $end = hex($me->endian);
    my $exe = $me->exe_file;
    my $size = $me->size;

    if ($end == 0x1234) { 
	$endian = "lendian";
    } elsif ($end == 0x4321) { 
	$endian = "bendian";
    } elsif ($end == 0x12345678) { 
	$endian = "lendian";
    }

    if($size eq 'test') {

      return ({   'command' => $exe,
                'args'    => [ "$endian.raw" ],
                'output'  => 'vortex.out2',
                'error'   => 'vortex.err',
            });

    } elsif($size eq 'train') {

      return ({   'command' => $exe,
                'args'    => [ "$endian.raw" ],
                'output'  => 'vortex.out2',
                'error'   => 'vortex.err',
            });

    } elsif($size eq 'ref') {

      push (@rc, { 'command' => $exe,
		'args'    => [ "${endian}1.raw" ],
		'output'  => 'vortex1.out2',
		'error'   => 'vortex1.err',
                });

      push (@rc, { 'command' => $exe,
		'args'    => [ "${endian}2.raw" ],
		'output'  => 'vortex2.out2',
		'error'   => 'vortex2.err',
                });

      push (@rc, { 'command' => $exe,
		'args'    => [ "${endian}3.raw" ],
		'output'  => 'vortex3.out2',
		'error'   => 'vortex3.err',
                });
      return @rc;
    }

}

1;


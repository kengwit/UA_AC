$benchnum  = '175';
$benchname = 'vpr';
$exename   = 'vpr' ;
$benchlang = 'C';
@base_exe  = ($exename);

$reltol   = { 'test' => {'costs.out' => 0.10},
              'costs.out' => .05, 
	      'default'   => 0.015};
$abstol   = undef;

@sources=qw(main.c util.c read_netlist.c read_arch.c place.c route.c draw.c
	    graphics.c stats.c segment_stats.c rr_graph.c rr_graph2.c
	    rr_graph_sbox.c rr_graph_util.c rr_graph_timing_params.c
	    rr_graph_area.c check_rr_graph.c check_route.c hash.c
	    read_place.c); 
$need_math='yes';
$bench_flags='-DSPEC_CPU2000';

sub invoke {
    my ($me) = @_;
    my @rc;

    my @place_flags=('-nodisp', '-place_only', '-init_t 5', 
                     '-exit_t 0.005', '-alpha_t 0.9412', 
                     '-inner_num 2'), 

    my @route_flags=('-nodisp', '-route_only', '-route_chan_width 15',
		     '-pres_fac_mult 2', '-acc_fac 1', 
                     '-first_iter_pres_fac 4', 
                     '-initial_pres_fac 8');

    push (@rc, { 'command' => $me->exe_file, 
		    'args'    => [ 'net.in', 'arch.in', 'place.out', 'dum.out',
				    @place_flags ], 
		    'output'  => 'place_log.out',
		    'error'   => 'place_log.err',
		});

    push (@rc, { 'command' => $me->exe_file, 
		    'args'    => [ 'net.in', 'arch.in', 'place.in', 'route.out',
				    @route_flags ], 
		    'output'  => 'route_log.out',
		    'error'   => 'route_log.err',
		});

    return @rc;
}

1;

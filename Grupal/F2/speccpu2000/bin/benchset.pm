#
# benchset.pm
# Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: benchset.pm 1393 2005-06-14 15:55:21Z cloyce $
#

package Spec::Benchset;
use strict;
use vars '@ISA';

@ISA = (qw(Spec::Config));

# Information about what's in the config entries
## these tables are used to give field names to config entries in the report
##               field name     report label         ??
use vars qw(@hardware_info @software_info @extra_info @notes_info);

# All of the hardware_info items are printed
@hardware_info=(
    ['hw_vendor'   , 'Hardware Vendor'   ],
    ['hw_model'    , 'Model Name'        ],
    ['hw_cpu'      , 'CPU'               ],
    ['hw_cpu_mhz'  , 'CPU MHz'           ],
    ['hw_fpu'      , 'FPU'               ],
    ['hw_ncpu'     , 'CPU(s) enabled'    ],
    ['hw_ncpuorder', 'CPU(s) orderable'  ],
    ['hw_parallel' , 'Parallel'          ],
    ['hw_pcache'   , 'Primary Cache'     ],
    ['hw_scache'   , 'Secondary Cache'   ],
    ['hw_tcache'   , 'L3 Cache'          ],
    ['hw_ocache'   , 'Other Cache'       ],
    ['hw_memory'   , 'Memory'            ],
    ['hw_disk'     , 'Disk Subsystem'    ],
    ['hw_other'    , 'Other Hardware'    ],
);

# All of the software_info items are printed
@software_info=(
    ['sw_os'       , 'Operating System' ],
    ['sw_compiler' , 'Compiler'         ],
    ['sw_file'     , 'File System'      ],
    ['sw_state'    , 'System State'     ],
);

# Only *some* of the extra_info items are printed, and only some of the time!
@extra_info = (
	       [ 'Volume', 	        'Volume'         ], # Unused
	       [ 'Issue', 		'Issue'          ], # Unused
	       [ 'Page', 		'Page'           ], # Unused
	       [ 'company_name', 	'Company Name'   ], # Tester's company
	       [ 'machine_name', 	'Machine Name'   ], # Unused
	       [ 'license_num', 	'License Number' ],
	       [ 'tester_name', 	'Tester Name'    ],
	       [ 'test_date', 	        'Test Date'      ],
	       [ 'hw_avail', 	        'Hardware Avail' ],
	       [ 'sw_avail', 	        'Software Avail' ],
	       [ 'longest_ref', 	'Longest Ref'    ], # Never printed
	       [ 'config', 	        'Config'         ], # Unused
	       [ 'prepared_by', 	'Prepared By'    ], # Unused
);

# notes are always printed
@notes_info=(
    ['notes'    , ''    ]
);

sub new {
    my ($class, $config) = @_;
    my $me       = bless {}, $class;

    no strict 'refs';
    $me->{'name'}           = ${"${class}::name"};
    $me->{'units'}          = ${"${class}::units"};
    $me->{'metric'}         = ${"${class}::metric"};
    $me->{'longest_ref'}    = ${"${class}::longest_ref"};
    $me->{'benchmarklist'}  = [@{"${class}::benchmarks"}];
    $me->{'mach'} = 'default';
    $me->{'ext'}  = 'default';
    $me->{'size'} = 'default';
    $me->{'rate'} = 0;
    $me->{'time'} = time;
    $me->{'mean_anyway'} = istrue($config->mean_anyway);
    $me->{'errors'} = [];
    $me->{'calc_errors'} = [];
    $me->{'config'} = $config;
    $me->{'valid'} = 1;

    my $tmp = {};
    for (@{$me->{'benchmarklist'}}) {
	$tmp->{$_}++;
    }
    $me->{'benchmarks'}     = $tmp;
    $me->{'refs'} = [ $me ];

    return $me;
}

sub info_format {
    my ($me, @format) = @_;
    my @rc = ();
    for my $format (@format) {
	next if (ref($format) ne 'ARRAY');
	if (ref($format[0]) eq 'ARRAY') {
	    for my $ref (@$format) {
		my @keys  = $me->match_keys($ref->[0]);
		my @field = ($ref->[1]);
		if (@keys) {
		    for my $key (@keys) {
			my $val = $me->accessor($key);
			my @vals = ($val);
			@vals = @$val if (ref($val) eq 'ARRAY');
			for $val (@vals) {
			    if ($val =~ /^\s*$/o) {
				# It contains only whitespace, which would
				# get stomped by the split.
				push @field, ' ';
			    } else {
				push @field, split(/\n/, $val);
			    }
			}
		    }
		} else {
		    push (@field, '--');
		}
		push (@rc, [@field]);
	    }
	} else {
	    my @keys  = $me->match_keys($format->[0]);
	    my @field = ($format->[1]);
	    if (@keys) {
		for my $key (@keys) {
		    push (@field, split(/\n/, $me->accessor($key)));
		}
	    } else {
		push (@field, '--');
	    }
	    push (@rc, [@field]);
	}
    }
    return @rc;
}

sub hardware {
    my ($me) = @_;
    return $me->info_format(\@hardware_info);
}

sub software {
    my ($me) = @_;
    return $me->info_format(\@software_info);
}

sub notes {
    my ($me) = @_;
    return $me->info_format(\@notes_info);
}

sub baseunits {
    my ($me) = shift;
    my $rate = $me->rate?'_rate':'';
    return $me->units . $rate . '_base2000';
}

sub peakunits {
    my ($me) = shift;
    my $rate = $me->rate?'_rate':'';
    return $me->units . $rate. '2000';
}

sub datestr {
    my ($me) = shift;
    my $tmp = main::ctime($me->{'time'});
    $tmp =~ tr/\015\012//d;
    return $tmp;
}

sub errors {
    my ($me) = shift;
    my @errors;

    push (@errors, @{$me->{'errors'}}) if ref $me->{'errors'} eq 'ARRAY';

    my $ref = $me->{'results'};
    for my $bench (sort keys %$ref) {
	for my $tune (sort keys %{$ref->{$bench}}) {
	    next if ref($ref->{$bench}{$tune}{'data'}) ne 'ARRAY';
	    for my $res (@{$ref->{$bench}{$tune}{'data'}}) {
		for (@{$res->{'errors'}}) {
		    push (@errors, "Error $bench: $_");
		}
	    }
	}
    }

    grep (s/\s+$//,@errors);

    return @errors;
}

sub which_tunes {
    my ($me) = shift;
    my %tunes;
    my $ref = $me->{'results'};
    for my $bench (sort keys %$ref) {
	for my $tune (sort keys %{$ref->{$bench}}) {
	    $tunes{$tune} = 1;
	}
    }
    return sort keys %tunes;
}

sub report {
    my ($me, $benchobjs, $config, $mach, $ext, $size) = @_;
    my $found_one = 0;
    my $longest_ref = $me->{'longest_ref'};
    my $result;
    my $rate = $config->rate;
    # If $config->rawconfig exists, and rawformat is set, assume that it's
    # already been properly munged.
    my $rawconfig = '';
    if (exists($config->{'rawconfig'}) && $config->rawformat) {
	$rawconfig = join("\n", @{$config->rawconfig});
    } else {
	$rawconfig = $config->rawtxtconfig;
	eval '$rawconfig = Compress::Zlib::memGzip($rawconfig)';
	$rawconfig = $config->rawtxtconfig if $@;
	$rawconfig = main::encode_base64($rawconfig);
    }
    $result = bless { 
	'mach'        => '',
	'ext'         => '',
	'size'        => '',
	'rate'        => '',
	'rawconfig'   => [ split ("\n", $rawconfig) ],
	'table'       => $me->config->table,
	'name'        => $me->name,
	'units'       => $me->units,
	'metric'      => $me->metric,
	'longest_ref' => $me->longest_ref,
	'benchmarks'  => $me->benchmarks,
	'mean_anyway' => $me->mean_anyway,
	'tunelist'    => $me->config->tunelist,
	'basepeak'    => $config->{'basepeak'},
    }, ref($me);
    $config = $me->config;	# This probably isn't necessary
    $result->{'refs'} = [ $result ];
    $result->{'valid'} = 1;
    $result->mach ($mach) if defined $mach;
    $result->ext  ($ext)  if defined $ext;
    # $size should always be defined.  But just in case...
    if (defined $size) {
      $result->size($size);
    } else {
      $result->size($me->size);
    }
    $result->rate ($rate) if defined $rate;

    for (keys %{$me->benchmarks}) {
	my $bm =$me->{'benchmarks'}->{$_};
	$result->{'reference'}->{$_} = $bm->reference;
    }

    $config->{'refs'} = [
	reverse ($config, $config->ref_tree(['default', $me->name, 
						sort keys %{$me->benchmarks}],
					    ['default', 'base', 'peak' ],
					    ['default', $ext],
					    ['default', $mach])) ];

    # Copy text data into object
    my @keys = sort main::bytrailingnum $config->list_keys;

    for my $tagref (@hardware_info, @software_info, @extra_info) {
	my $tag = $tagref->[0];
	my @data = ();
	for (@keys) {
	    if (m/^$tag(\d*)$/) {
		push (@data, $config->accessor($_));
	    }
	}
	@data = ('--') unless @data;
	if (@data > 1) {
	    $result->{$tag} = [ @data ];
	} else {
	    $result->{$tag} = $data[0];
	}
    }
      # Notes are a little special.
    if (istrue($config->expand_notes)) {
	$result->{'notes'} = [ map { ce($config->accessor($_), $config) }
			       grep { /^notes([0-9_]*)$/ }
			       sort @keys ];
    } else {
	$result->{'notes'} = [ map { $config->accessor($_) }
			       grep { /^notes([0-9_]*)$/ }
			       sort @keys ];
    }
    # Make sure to put the company name in the notes section if it doesn't
    # match the hw_vendor field
    if (exists $config->{'company_name'} &&
	$config->{'company_name'} ne $config->{'hw_vendor'} &&
	$result->{'notes'}->[0] !~ /^Tested by/) {
	unshift @{$result->{'notes'}}, 'Tested by '.$config->{'company_name'};
    }

    # Now copy benchmark information into object
    for my $bench (@$benchobjs) {
	next if !$result->bench_in($bench);
	$found_one = 1;
	$bench->{'longest_ref'} = $longest_ref;
	$result->add_results($bench);
    }

    $result->{'basemean'} = '--';
    $result->{'peakmean'} = '--';
    return undef if !$found_one;
    # Munge up the results if basepeak is set, and peak was selected to run
    # If global basepeak is 1, we do wholesale base->peak substitution.
    # If global basepeak is 2, we do per-benchmark lowest median selection
    if ($result->{'basepeak'} == 1) {
	Log(10, "Doing base -> peak substitution for full suite basepeak\n");
	if (grep { /^peak$/o } @{$config->tunelist}) {
	    # It doesn't make sense to do this unless they requested a peak
	    # run
	    for my $bench (keys %{$result->benchmarks}) {
		my $benchref = $result->{'results'}{$bench};
		my $tune = 'base';
		# Make sure the data structures exist
		foreach my $tmptune ('base', 'peak') {
		    if (ref($benchref->{$tmptune}) ne 'HASH') {
			$benchref->{$tmptune} = {'data' => [] };
		    }
		}
		@{$benchref->{'peak'}{'data'}} = @{$benchref->{'base'}{'data'}};
	    }
	}
    } elsif ($result->{'basepeak'} == 2) {
	Log(10, "Doing lowest median substitution for some benchmarks (per-benchmark basepeak\n");
	for my $bench (keys %{$result->benchmarks}) {
	    next unless istrue($me->{'benchmarks'}->{$bench}->{'basepeak'});
	    Log(10, "  basepeak lowest median sub for $bench\n");
	    my %tmpmedian = ();
	    my $benchref = $result->{'results'}{$bench};
	    for my $tune (keys %{$benchref}) {
		my @tmpdata = ();
		for my $obj ( @{$benchref->{$tune}{'data'}}) {
		    next if ! $obj->valid;
		    push (@tmpdata, $obj);
		}
		$tmpmedian{$tune} = median_ratio(0, @tmpdata);
	    }
	    my @sortres = sort { $tmpmedian{$a} <=> $tmpmedian{$b} } keys %tmpmedian;
	    for (my $i = 1; $i < @sortres; $i++) {
		Log(89, "Setting $sortres[$i] to $sortres[0] for $bench\n");
		dupe_results($benchref->{$sortres[$i]}->{'data'},
			     $benchref->{$sortres[0]}->{'data'});
	    }
	}
    }
    $result->{'basemean'} = ($result->rate)?$result->calc_mean_rate('base'):
				    $result->calc_mean_speed('base');
    if (grep { /^peak$/o } @{$config->tunelist}) {
	$result->{'peakmean'} = ($result->rate)?$result->calc_mean_rate('peak'):
	    $result->calc_mean_speed('peak');
    } else {
	$result->{'peakmean'} = '--';
    }

    # Check for some basic errors
    $result->add_error("'reportable' flag not set during run") if !istrue($config->reportable);

    my $saw_base = 0;
    for my $tune (@{$config->tunelist}) {
	$saw_base++ if ($tune eq 'base');
	for my $bench ($result->insufficient_data($tune)) {
	    $result->add_error("$bench $tune did not have enough runs!\n");
	}
    }
    if (!$saw_base) {
	$result->add_error("No 'base' runs!  Base measurement required!\n");
    }
    if ($result->size ne 'ref') {
	$result->add_error("Input set must be 'ref' for a valid run (set to '".$result->size."' for this run)\n");
    }

    return $result;
}

sub insufficient_data {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';

    my @which = ();
    for my $bench (keys %{$me->benchmarks}) {
	if (!exists $me->{'results'}{$bench}{$tune} ||
	    ref($me->{'results'}{$bench}{$tune}{'data'}) ne 'ARRAY' ||
	    @{$me->{'results'}{$bench}{$tune}{'data'}}+0 < $main::global_config->min_report_runs) {
	    push (@which, $bench);
	}
    }
    return @which;
}

sub add_results {
    my ($me, $bench) = @_;
    my @tunes = ($bench->tune);
#    @tunes = ('base', 'peak') if $bench->basepeak;

    for my $tune (@tunes) {
	my @tmp;
	if (istrue($me->rate)) {
	    @tmp = $bench->result_list;
	} else {
	    @tmp = $bench->result_list(1);
	}
	push (@{$me->{'results'}{$bench->benchmark}{$tune}{'data'}}, @tmp);
    }
    $me->{'benchmarks'}->{$bench->benchmark}->{'basepeak'} = $bench->basepeak;
}

sub reference {
    my ($me, $bench) = @_;
    return $me->{'reference'}{$bench};
}

sub valid {
    my ($me, $bench, $tune) = @_;
    return 0 unless exists $me->{'results'}{$bench}{$tune};
    my $valid = 0;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	$valid = $_->{'valid'};
    }
    return $valid;
}

sub ratio {
    my ($me, $bench, $tune) = @_;
    my $rc = '';
    my $count = 0;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
	$rc += $_->{'ratio'};
	$count++;
    }
    $rc /= $count if $count;
    return $rc;
}

sub copies {
    my ($me, $bench, $tune) = @_;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
	return $_->{'copies'};
    }
    return '';
}

sub runtime {
    my ($me, $bench, $tune, $round) = @_;
    my $rc = '';
    my $count = 0;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
	$rc += $_->{'reported_time'};
	$count++;
    }
    if ($count) {
	$rc /= $count;
	$rc = int($rc + 0.5) if ($round);
    }
    return $rc;
}


# SIDE EFFECT:  Sets the selected bit for the result if select is nonzero
sub median_ratio {
    my ($select, @objs) = @_;
    my $numobjs = @objs+0;
    return undef if $numobjs <= 0;
    Log(70, "median_ratio(select=$select) for ".$objs[0]->benchmark.'='.$objs[0]->tune.'='.$objs[0]->ext.'='.$objs[0]->mach."\n");
    Log(70, "         ratios and runtimes:\n             ".join("\n             ", map { '[ '.$_->reported_time.' = '.$_->ratio.' ]' } @objs)."\n");
    # Sort by runtime and not ratio; for non 'ref' runs, ratio is always '--'!
    @objs = sort { $a->reported_time <=> $b->reported_time } @objs;
    Log(70, "  sorted ratios and runtimes:\n             ".join("\n             ", map { '[ '.$_->reported_time.' = '.$_->ratio.' ]' } @objs)."\n");
    if (($numobjs > 1) && (($numobjs % 2) == 0)) {
	Log(70, "  Even number of runs! Averaging #".(int($numobjs/2))." and #".(int($numobjs/2)+1)." of $numobjs\n");
	# Even number of runs.  Return the average of the median runs, and
	# mark them both as 'selected'
	my $sel1 = $objs[int($numobjs/2)-1];
	my $sel2 = $objs[int($numobjs/2)];
	if ($select) {
	    $sel1->selected(1);
	    $sel2->selected(1);
	}
	Log(70, "  Returned ratio = ".(($sel1->ratio+$sel2->ratio)/2)."\n");
	return ($sel1->ratio+$sel2->ratio)/2;
    } else {
	Log(70, "  Odd number of runs! Choosing #".(int($numobjs/2)+1)." of $numobjs\n");
	my $sel = $objs[int($numobjs/2)];
	$sel->selected(1) if $select;
	Log(70, "  Returned ratio = ".$sel->ratio."\n");
	return $sel->ratio;
    }
}

sub calc_mean_rate {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';
    my $sufficient = scalar($me->insufficient_data($tune)) == 0;

    my $per_copy = {};
    for my $bench (keys %{$me->benchmarks}) {
	for my $obj ( @{ $me->{'results'}{$bench}{$tune}{'data'} }) {
	    next if ! $obj->valid;
	    push (@{$per_copy->{$obj->copies}{$bench}{'data'}}, $obj);
	}
    }
    my $num_benchmarks = (keys %{$me->benchmarks})+0;
    if ($tune eq 'base') {
	my $nextbest_product = 0;
	my $nextbest_copies  = 0;
	my $best_product = 0;
	my $best_copies  = 0;
	for my $copies (sort { $a <=> $b } keys %$per_copy) {
	    my $copyref = $per_copy->{$copies};
	    my $product = 1;
	    my $count = 0;
	    my $valid = 1;
	    for my $bench (keys %{$me->benchmarks}) {
		if (! exists $per_copy->{$copies}{$bench}) {
		    $valid = 0;
		    next;
		}
		my $benchref = $per_copy->{$copies}{$bench};
		$valid = 0 if (@{$benchref->{'data'}} < $main::global_config->min_report_runs);
		my $tmp = median_ratio(1, @{$benchref->{'data'}});
		if (defined($tmp) && ($tmp > 0)) {
		    $product *= $tmp;
		    $count ++;
		}
	    }
	    if ($count) {
		$product = $product ** (1/$count);
		if (!$valid && $product > $nextbest_product) {
		    $nextbest_product = $product;
		    $nextbest_copies  = $copies;
		}
		if ($product > $best_product) {
		    $best_product = $product;
		    $best_copies  = $copies;
		}
	    }
	}
	if ($best_copies == 0) {
	    $me->valid(0);
	    $me->add_error("There is no set of valid runs with the same # of copies for base rate value");
		return (istrue($main::global_config->mean_anyway) || $sufficient) ? $nextbest_product : '--';
	}
	return (istrue($main::global_config->mean_anyway) || $sufficient) ? $best_product : '--';
    } else {
	my $best_product = {};
	my $best_copies = {};
	for my $copies (keys %$per_copy) {
	    for my $bench (keys %{$me->benchmarks}) {
		next unless exists $per_copy->{$copies}{$bench};
		my $benchref = $per_copy->{$copies}{$bench};
		# X*X*X*X* 
		my $product = median_ratio(1, @{$benchref->{'data'}});
		if (defined($product) && ($product > 0) &&
		    ($product > $best_product->{$bench})) {
		    $best_product->{$bench} = $product;
		    $best_copies->{$bench}  = $copies;
		}
	    }
	}
	my $product = 1;
	my $count = 0;
	for my $bench (keys %{$me->benchmarks}) {
	    if (exists $best_product->{$bench}) {
		$product *= $best_product->{$bench};
		$count ++;
	    } else {
		$me->valid(0);
		$me->add_error("Complete set of valid runs for peak rate unavailable ($bench missing)");
	    }
	}
	if ($count) {
	    $product = $product ** (1/$count);
	} else {
	    $product = 0;
	}
	return (istrue($main::global_config->mean_anyway) || $sufficient) ? $product : '--';
    }
}

sub add_error {
    my $me = shift;
    push (@{$me->{'errors'}}, @_);
}

sub calc_mean_speed {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';
    my $sufficient = $me->insufficient_data($tune) == 0;

    my $product = 1;
    my $count = 0;
    for my $bench (keys %{$me->benchmarks}) {
	my @results = ();
	for my $obj ( @{ $me->{'results'}{$bench}{$tune}{'data'} }) {
	    next if ! $obj->valid || $obj->copies != 1;
	    push (@results, $obj);
	}
	my $tmp = median_ratio(1, @results);
	if (defined $tmp && $tmp > 0) {
	    $product *= $tmp;
	    $count++;
	}
    }
    return 0 if $count == 0;	# '--' means 'no'
    $product = $product ** (1/$count);
    return (istrue($main::global_config->mean_anyway) || $sufficient) ? $product : '--';
}

sub results_list {
    my ($me) = @_;
    my $benchhash = $me->{'results'};
    return () if ref($benchhash) ne 'HASH';
    my @result;
    for my $tune ('base', 'peak') {
	for my $bench (sort keys %$benchhash) {
	    next if ref($benchhash->{$bench}) ne 'HASH';
	    next if !exists $benchhash->{$bench}{$tune};
	    push (@result, @{$benchhash->{$bench}{$tune}{'data'}});
	}
    }
    return @result;
}

sub bench_in {
    my ($me, $bench) = @_;
    return exists $me->benchmarks->{$bench->benchmark} &&
	    $me->mach eq $bench->mach && $me->ext eq $bench->ext &&
	    $me->size eq $bench->size;
}

sub dupe_results {
    my ($dest, $src) = @_;

    return unless (ref($dest) eq 'ARRAY' && ref($src) eq 'ARRAY');

    # Do a deep copy, except for the 'tune', 'ref', and 'refs' members
    for (my $i = 0; $i < @{$src}; $i++) {
	my $href = $src->[$i];
	for my $key (keys %{$href}) {
	    next if ($key =~ /^(tune|ref|refs)$/o);
	    $dest->[$i]->{$key} = $src->[$i]->{$key};
	}
    }
}
    
sub Log     { main::Log(@_); }
sub jp      { main::joinpaths(@_); }
sub ce      { main::command_expand(@_); }
sub istrue  { main::istrue(@_); }
sub src     { my $me = shift; jp($me->path, $me->srcdir); }


package Spec::Benchset::oneresult;
use strict;

sub new {
    my ($set) = @_;
    return bless [$set->reference];
}

sub reference {
    my ($me) = @_;
    $me->[0];
}

1;


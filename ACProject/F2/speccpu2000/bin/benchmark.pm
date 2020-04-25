#
# benchmark.pm
# Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: benchmark.pm 1451 2005-06-22 14:56:18Z cloyce $
#

package Spec::Benchmark;
use strict;
use File::Basename;
use File::stat;
use IO::File;
use IO::Dir;
use Cwd;

use vars '@ISA';

@ISA = (qw(Spec::Config));

# List of things to *not* include in the MD5 hash of options
my %option_md5_ignorelist = (
			     'Volume'         =>  'Volume',
			     'Issue'          =>  'Issue',
			     'Page'           =>  'Page',
			     'company_name'   =>  'Tester Company Name',
			     'machine_name'   =>  'Machine Name',
			     'license_num'    =>  'License Number',
			     'tester_name'    =>  'Tester Name',
			     'test_date'      =>  'Test Date',
			     'longest_ref'    =>  'Longest Ref',
			     'config'         =>  'Config',
			     'prepared_by'    =>  'Cloyce',
			     'abstol' => '',
			     'action' => 'build',
			     'basepeak' => '0',
			     'benchdir' => 'benchspec',
			     'benchmark' => '164.gzip',
			     'bindir' => 'exe',
			     'commanderrfile' => 'speccmds.err',
			     'commandfile' => 'speccmds.cmd',
			     'commandoutfile' => 'speccmds.out',
			     'compwhite' => '',
			     'configdir' => 'config',
			     'datadir' => 'data',
			     'debug' => 0,
			     'deletebinaries' => '0',
			     'deletework' => '0',
			     'difflines' => '10',
			     'dirprot' => '511',
			     'exitvals' => 'spec_exit',
			     'expand_notes' => 0,
			     'fast_rundir_verify' => '0',
			     'fileprot' => '511',
			     'help' => '0',
			     'ignore_errors' => '0',
			     'ignore_sigint' => '0',
			     'inputdir' => 'input',
			     'iterations' => '1',
			     'line_width' => '0',
			     'locking' => '1',
			     'log' => 'log',
			     'log_line_width' => '0',
			     'lognum' => '016',
			     'make_no_clobber' => '0',
			     'makeflags' => '',
			     'max_active_compares' => '10',
			     'max_users' => '1',
			     'mean_anyway' => '0',
			     'min_report_runs' => '3',
			     'minimize_builddirs' => '0',
			     'minimize_rundirs' => '0',
			     'obiwan' => '',
			     'output' => 'asc',
			     'output_format' => 'all',
			     'outputdir' => 'output',
			     'nc' => 0,
			     'path' => '/spec/cpu2000',
			     'rate' => '0',
			     'rawformat' => 0,
			     'rebuild' => '1',
			     'ref_tree_level' => 'Top level Config',
			     'reftime' => 'reftime',
			     'reltol' => '',
			     'resultdir' => 'result',
			     'run' => 'all',
			     'safe_eval' => '1',
			     'setprocgroup' => '1',
			     'sigint' => '2,',
			     'size' => 'ref',
			     'skiptol' => '',
			     'tune' => 'base',
			     'command_add_redirect' => '0',
			     'specrun' => 'specinvoke',
			     'srcdir' => 'src',
			     'subworkdir' => 'work',
			     'table' => '1',
			     'teeout' => 'yes',
			     'teerunout' => '0',
			     'timefile' => 'time',
			     'top' => '/spec/cpu2000',
			     'uid' => '2033',
			     'unbuffer' => '0',
			     'username' => 'channui',
			     'users' => '1',
			     'use_submit_for_speed' => '0',
			     'vendor' => 'anon',
			     'verbose' => '99',
			     'version' => '0',
			     'workdir' => 'run',
			     'workdirinfo' => 'spec.dir',
			     'worklist' => 'list',
			     'optmd5' => '',
			     'exemd5' => '',
			     'reportable' => 'list',
			     'check_md5' => '',
			     'configpath' => '',
			     'oldmd5' => '',
			     'changedmd5' => '',
			     'backup_config' => '',
);

sub new {
    no strict 'refs';
    my ($class, $topdir, $config, $num, $name) = @_;
    my $me       = bless {}, $class;

    $me->{'name'}        = ${"${class}::benchname"};
    $me->{'num'}         = ${"${class}::benchnum"};
    if ($me->{'name'} ne  $name || $me->{'num'} != $num) {
	Log(0, "Benchmark name (".$me->{'num'}.".".$me->{'name'}.") does not match directory name '$topdir'.  Ignoring benchmark\n");
	return undef;
    }
    $me->{'abstol'}      = ${"${class}::abstol"};
    $me->{'reltol'}      = ${"${class}::reltol"};
    $me->{'compwhite'}   = ${"${class}::compwhite"};
    $me->{'obiwan'}      = ${"${class}::obiwan"};
    $me->{'skiptol'}     = ${"${class}::skiptol"};
    $me->{'BENCHLANG'}   = ${"${class}::benchlang"};
    if (defined ${"${class}::bench_flags"}) {
      $me->{'BENCH_FLAGS'} = ${"${class}::bench_flags"};
    }
    $me->{'benchmark'}   = $me->{'num'}.'.'.$me->{'name'};
    $me->{'path'}        = $topdir;
    $me->{'base_exe'}    = [@{"${class}::base_exe"}];
    $me->{'config'}      = $config;
    $me->{'refs'}        = [ $me, $config];
    $me->{'result_list'} = [ ];
    for (qw( abstol reltol compwhite obiwan skiptol )) {
	$me->{$_} = '' if !defined $me->{$_};
    }

    if ($me->{'name'} eq '' || $me->{'num'} eq '' || !@{$me->{'base_exe'}}) {
	return undef;
    }

    return $me;
}

sub per_file_param_val {
    my ($me, $param, $size, $file) = @_;
    my $val = $me->{$param};
    my $result;
    if (ref($val) eq 'HASH') {
	if (ref($val->{$size}) eq 'HASH') {
	    if (exists $val->{$size}{$file}) {
		$result = $val->{$size}{$file};
	    } elsif (exists $val->{$size}{'default'}) {
		$result = $val->{$size}{'default'};
            } elsif (ref ($val->{'default'}) eq 'HASH' && 
                     exists $val->{'default'}{$file}) {
		$result = $val->{'default'}{$file};
	    }
	} 
	if (!defined $result) {
	    if (exists $val->{$size} && ref($val->{$size}) eq '') {
		$result = $val->{$size};
	    } elsif (exists $val->{$file} && ref($val->{$file}) eq '') {
		$result = $val->{$file};
	    } else {
		$result = $val->{'default'};
	    }
	}
    } else {
	$result = $val;
    }
    return $result;
}
sub per_file_param {
    my $val = per_file_param_val(@_);
    return istrue($val)?1:undef;
}

sub compress_whitespace { shift->per_file_param('compress_whitespace', @_); };
sub abstol              { shift->per_file_param_val('abstol', @_); };
sub reltol              { shift->per_file_param_val('reltol', @_); };
sub obiwan              { shift->per_file_param('obiwan', @_); };
sub skiptol             { shift->per_file_param_val('skiptol', @_); };

sub instance {
    my ($me, $tune, $size, $ext, $mach, $users) = @_;
    my $child = bless { %$me }, ref($me);
    my $config = $me->config;
    $child->{'tune'}  = $tune;
    $child->{'ext'}   = $ext;
    $child->{'size'}  = $size;
    $child->{'mach'}  = $mach;
    $child->{'result_list'} = [];

    my $bench = $child->benchmark;
    my @sets = $config->benchmark_in_sets($bench);
    $child->{'refs'} = [ $child,
	reverse ($config, $config->ref_tree(['default', @sets, $bench],
				['default', $tune],
				['default', $ext],
				['default', $mach])) ];
    $child->{'max_users'} = $child->max_users;
    $users = ($config->users)[0] if ($tune eq 'base'); # The number of copies
				# is not allowed to vary on a per-benchmark
				# basis for base runs
    if (defined($users) && ($users > 0)) {
	$child->{'userlist'}  = [$users];
    } else {
	$users = $child->accessor_nowarn('userlist');
	if (defined($users) && ref($users) eq 'ARRAY') {
	    $child->{'userlist'} = $users;
	}
    }
    if ($child->basepeak == 2 &&
	!exists($child->{'basepeak'})) {
	# We've inherited this wierd setting from the top level, so ignore
	# it.
	$child->{'basepeak'} = 0;
    } else {
	$child->{'basepeak'} = istrue($child->basepeak);
    }
    if (istrue($child->{'basepeak'})) {
	$child->{'smarttune'} = 'base';
	$child->{'refs'} = [ $child,
	    reverse ($config, $config->ref_tree(['default', @sets, $bench],
				    ['default', 'base'],
				    ['default', $ext],
				['default', $mach])) ];
    } else {
	$child->{'smarttune'} = $tune;
    }
    return $child;
}

sub descmode {
    my $me = shift;
    return join(' ', $me->benchmark, $me->size, $me->tune, $me->ext, $me->mach);
}

sub input_files_hash {
    my ($me, $size) = @_;
    my $head = jp($me->path, $me->datadir);

    $size = $me->size if ($size eq '');

    my @dirs = (jp($head, $size, $me->inputdir));
    my $dir = jp($head, 'all', $me->inputdir);
    unshift (@dirs, $dir) if -d $dir;

    return main::build_tree_hash($me, @dirs);
}

sub copy_input_files_to {
    my ($me, $fast, @path, $size) = @_;
    my ($files, $dirs) = $me->input_files_hash($size);
    for my $dir (@path) {
	# Create directories
	mkdir $dir, 0755;
	for my $reldir (sort keys %$dirs) {
	    mkdir jp($dir, $reldir), 0755;
	}
	# Copy files
	for my $file (sort keys %$files) {
	    main::copy_file($files->{$file}, jp($dir,$file), !$fast);
	}
    }
}

sub input_files_base {
    my $me = shift;
    my ($hash) = $me->input_files_hash;
    return sort keys %$hash;
}

sub input_files {
    my $me = shift;
    my ($hash) = $me->input_files_hash;
    return sort map { $hash->{$_} } keys %$hash;
}

sub input_files_abs {
    my $me = shift;
    my $head   = jp($me->path, $me->datadir);
    my ($hash) = $me->input_files_hash;
    return sort map { jp($head, $hash->{$_}) } keys %$hash;
}

sub output_files_hash {
    my ($me, $size) = @_;
    my $head = jp($me->path, $me->datadir);

    $size = $me->size if ($size eq '');

    my @dirs = (jp($head, $size, $me->outputdir));
    my $dir = jp($head, 'all', $me->outputdir);
    unshift (@dirs, $dir) if -d $dir;

    return main::build_tree_hash($me, @dirs);
}

sub output_files_base {
    my $me = shift;
    my ($hash) = $me->output_files_hash;
    return sort keys %$hash;
}

sub output_files {
    my $me = shift;
    my ($hash) = $me->output_files_hash;
    return sort map { $hash->{$_} } keys %$hash;
}

sub output_files_abs {
    my $me = shift;
    my $head   = jp($me->path, $me->datadir);
    my ($hash) = $me->output_files_hash;
    return sort map { jp($head, $hash->{$_}) } keys %$hash;
}

sub exe_files {
    my $me    = shift;
    my $tune  = $me->smarttune;
    my $ext   = $me->ext;
    my $fdocommand = $me->accessor_nowarn('fdocommand');
    if (defined($fdocommand) && ($fdocommand ne '')) {
	return @{$me->base_exe};
    } else {
#	return map { "${_}_$tune$mach.$ext" } @{$me->base_exe};
	return map { "${_}_$tune.$ext" } @{$me->base_exe};
    }
}

sub exe_file {
    my $me = shift;
    return ($me->exe_files)[0];
}

sub exe_files_abs {
    my $me = shift;
    my $head   = jp($me->path, $me->bindir);
    return sort map { jp($head, $_) } $me->exe_files;
}

sub reference {
    my $me = shift;
    my $file = jp($me->path, $me->datadir, $me->size, 'reftime');
    if (!-f $file) {
	Log(0, "No file $file\n");
	return 1;
    };
    my $ref = (main::read_file($file))[1];
    if (($me->size eq 'ref') && ($ref == 0)) {
	Log(0, "ref reference size for ".$me->descmode." == 0\n");
	return 1;
    };
    return $ref;
}


sub Log     { main::Log(@_); }
sub jp      { main::joinpaths(@_); }
sub istrue  { main::istrue(@_); }
sub src     { my $me = shift; jp($me->path, $me->srcdir); }

sub check_md5 {
    my $me = shift;
    return 1 if $me->reportable;
    return $me->accessor_nowarn('check_md5');
}

sub make {
    my $me = shift;
    return 'specmake' if $me->reportable;
    return $me->accessor_nowarn('make');
}

sub make_no_clobber {
    my $me = shift;
    return 0 if $me->reportable;
    return $me->accessor_nowarn('make_no_clobber');
}

# Check to make sure that the input set exists.
sub check_size {
    my ($me) = @_;
    my $dir = jp($me->path, $me->datadir, $me->size);

    return -d $dir && ($me->size ne 'ref' || -f jp($dir, $me->reftime)) &&
	   -d jp($dir, $me->inputdir) && -d jp($dir, $me->outputdir);
}

sub check_exe () {
    my ($me) = @_;
    # If there are no MD5 hashes then, we will definitely fail the compare
    if ($me->check_md5 && ($me->accessor_nowarn('optmd5') eq '' || 
	$me->accessor_nowarn('exemd5') eq '')) {
	return 0;
    }

    # Build a hash of the executeable files
    my $ctx = new Digest::MD5;
    for my $name ($me->exe_files_abs) {
	return 0 if !-f $name;
	my $fh = new IO::File "<$name";
	if (!defined $fh) {
	    Log (0, "Can't open file '$name' for reading: $!\n");
	}
	$ctx->addfile($fh);
    }
    if ($me->check_md5) {
	my $md5exe = $ctx->hexdigest;
	my $md5opt = option_md5($me);
	if ($md5opt ne $me->accessor_nowarn('optmd5') ||
	    $md5exe ne $me->accessor_nowarn('exemd5')) {
#	    print "Options: ".$md5opt ne $me->accessor_nowarn('optmd5') ? "yes":"no","\n";
#	    print "    Exe: ".$md5exe ne $me->accessor_nowarn('exemd5') ? "yes":"no","\n";
	    return 0;
	}
    }
    return 1;
}

sub form_makefile {
    my $me = shift;
    my @list;

    my $tune  = $me->smarttune;
    my $ext   = $me->ext;
    my $bench = $me->benchmark;

    push (@list, "TUNE=". $tune);
    push (@list, "EXT=".  $ext);
    push (@list, "");

    for ($me->list_keys) {
# Don't want references in makefile
	my $val = $me->accessor($_);
	if (ref ($val) eq '' && $val!~m/\n/) {
	    push (@list, sprintf ('%-16s = %s', $_, $val));
	}
    }

# Add vendor makefile stuff at the end
    if ($me->accessor_nowarn('vendor_makefile') ne '') {
	push (@list, $me->vendor_makefile);
    }

# Log our new makefile to the logfile
    my $temp = join ("\n", @list) . "\n";
    return $temp;
}

sub write_makefile {
    my ($me, $filename) = @_;

    my $temp = $me->form_makefile;

    Log (150, "Wrote to makefile '$filename':\n", \$temp);

# Write our new makefile out
    my $fh = new IO::File;
    if (!$fh->open(">$filename")) {
	Log(0, "Can't write makefile '$filename': $!\n");
	exit 1;
    }
    print $fh $temp;
    $fh->close();
}

sub option_md5 {
    my $me = shift;
    my $md5 = Digest::MD5->new;

    Log(30, "option_md5 list contains:\n");
    for my $key (sort $me->list_keys) {
        # Don't allow descriptive text
	next if $key =~ m/^(hw_|sw_|notes|submit)/; 
	next if exists $option_md5_ignorelist{$key};

	my $val = $me->accessor($key);
	next if ref($val) ne '';    # Don't allow array references
	next if $val =~ m/\n/;      # Don't allow carriage returns
	Log(30, "  $key=$val\n");
	$md5->add($key, '=', $val, "\n")
    }
    return $md5->hexdigest;
}

sub build {
    my ($me, $directory) = @_;
    my ($fdo, $pass1, $pass2, $pass3);
    my $rc;
    my $bench = $me->benchmark;
    my @pass;
    my $makefile_md5;
    my $valid_build = 1;
    my $compile_options = '';
    my $path = $directory->path;

    # Get a pointer to where we update our build status info
    my $md5ref    = $me->config;
    for my $key ($me->benchmark, $me->smarttune, $me->ext, $me->mach) {
	if (!exists $md5ref->{$key} || ref($md5ref->{$key} ne 'HASH')) {
	    $md5ref->{$key} = {};
	}
	$md5ref = $md5ref->{$key};
    }
    $md5ref->{'changedmd5'} = 0;


    # First things first, remove any existing binaries with these names,
    # this make sure that if the build fails any existing binaries are erased
    for my $file ($me->exe_files_abs) {
	if (-f $file && !unlink $file) {
	    Log(0, "Can't remove file '$file': $!\n");
	    exit 1;
	}
    }

    my $langref = {};
    if ($me->BENCHLANG eq 'F77' && $me->smarttune eq 'base') {
	$langref->{'BENCHLANG'} = 'F';
	$langref->{'FPBASE'} = 'yes';
    }
    $langref->{'commandexe'} = ($me->exe_files)[0];
    $langref->{'baseexe'} = ($me->base_exe)[0]->[0];
    $me->unshift_ref($langref);

    # This is just for testing purposes
    if ( ! $me->make_no_clobber || 
	 ! -f &jp ( $path, 'Makefile' )) {
	if (!rmpath($path)) {	# It's probably not there
            &main::mkpath($path);
	}

	if ($me->VPATH_INCLUDE    eq '' && $me->VPATH_CINCLUDE   eq '' &&
	    $me->VPATH_CXXINCLUDE eq '' && $me->VPATH_FINCLUDE   eq '' &&
	    $me->VPATH_F90INCLUDE eq '' && !istrue($me->USE_VPATH)) {
	  main::copy_tree($me->src(), $directory->path());
	  my $srcalt = jp($me->src(), 'src.alt', $me->srcalt);
	  if ($me->srcalt ne '') {
	    Log(3, "Attempting to use src.alt from '$srcalt' ... ");
	    if (-d $srcalt) {
	      main::copy_tree($srcalt, $directory->path());
	      Log(3, "okay\n");
	    } else {
	      Log(3, "not a directory!  Ignored.\n");
	    }
	  }
	}
	my $origmakefile = jp($me->src,'Makefile.cpu2000');
	$origmakefile = jp($me->src,'Makefile') if (!-f $origmakefile);
	main::copy_file($origmakefile, 
			jp($path,'Makefile'), 1);
    } else {
	$valid_build = 0;
    }

    if (!chdir($path)) {
	Log(0, "Couldn't chdir to $path: $!\n");
    }
    $me->write_makefile(jp($path, $me->specmake));

    my $compile_start = time;  ## used by the following log statement
    Log(160, "  Compile for '$bench' started at:".&::ctime($compile_start)." ($compile_start)\n");

    my $make = $me->make . ' ' . $me->makeflags;

# Check to see if feedback is being used
    if (istrue($me->feedback)) {
	for ($me->list_keys) {
	    if (m/^fdo_\S+(\d+)/    && $me->accessor_nowarn($&) ne '') { 
		$pass[$1] = 1; $fdo = 1; 
	    }
	    if (m/^PASS(\d+)_(\S+)/ && $me->accessor_nowarn($&) ne '')    { 
		my ($pass, $what) = ($1, $2);
		$what =~ m/^(\S*)(FLAGS|OPTIMIZE)/;
		if ($1 ne 'LD' && $1 ne $me->BENCHLANG) {
		    next;
		}
		$pass[$pass] = 1; $fdo = 1; 
	    }
	}
    }
# Set up some default values for FDO, don't set these if the user has
# overridden them
    my (@commands) = ('fdo_pre0');
    my $tmp = {
	    'fdo_run1'         => '$command',
	};
    for (my $i = 1; $i < @pass; $i++) {
	if ($pass[$i]) {
	    if ($i != 1) {
		$tmp->{"fdo_make_clean$i"}="$make FDO=PASS$i fdoclean";
	    }
	    if (($i < (@pass-1)) && !exists($tmp->{"fdo_run$i"})) {
		$tmp->{"fdo_run$i"} = '$command';
	    }
	    $tmp->{"fdo_make_pass$i"} ="$make FDO=PASS$i build";
	    push (@commands, "fdo_pre_make$i", "fdo_make_clean$i",
			     "fdo_make_pass$i", "fdo_post_make$i",
			     "fdo_pre$i", "fdo_run$i", "fdo_post$i");
	}
    }
    $me->push_ref($tmp);
    my %replacements = (
	    'benchmark' => $me->benchmark,
	    'benchtop'  => $me->path,
	    'benchnum'  => $me->num,
	    'benchname' => $me->name,
	    'spectop'   => $me->top,
    );

    if (main::log_system("$make clean", 'make', $me, \%replacements)) {
	$tmp = "Error with make clean!\n";
	Log(0, "  $tmp") if $rc;
	$me->pop_ref;
	$me->shift_ref;
	$me->release($directory);
	$me->compile_error_result($tmp);
	return 1;
    }

    if ($fdo) {
	$me->unshift_ref({ 'size'          => 'train' });
	$me->copy_input_files_to(istrue($me->fast_rundir_verify), $path);
	$me->shift_ref();

	for my $cmd (@commands) {
	    my $val = $me->accessor_nowarn($cmd);
	    my ($pass) =  $cmd =~ m/(\d+)$/;
	    next if $val =~ m/^\s*$/;
	    if ($cmd =~ /^fdo_run/) {
		$me->unshift_ref({
		    'size'          => 'train',
		    'dirlist'       => [ $directory ],
		    'fdocommand'    => $val });
		Log(3, "Training ", $me->benchmark, "\n");
		$rc = $me->run_benchmark(1, $cmd);
		$me->shift_ref();
		$compile_options .= "RUN$pass: $val";
		Log(0, "  Error with train!\n") if $rc;
	    } else {
		$rc=main::log_system($val, $cmd, $me, \%replacements);
		$compile_options .= "$cmd: $val";
		if ($cmd =~ m/^fdo_make_pass/) {
		    $rc=&main::log_system("$make FDO=PASS$pass options",
					  "options$pass",
					  $me, \%replacements);
		    my $fh = new IO::File "<options$pass.out";
		    if (defined $fh) {
			while (<$fh>) {
			    s/:/$pass:/;
			    $compile_options .= $_;
			}
			$fh->close();
		    }
		}
		if ($rc) {
		    $tmp = "Error with $cmd!\n";
		    Log(0, "  $tmp");
		    $me->pop_ref;
		    $me->shift_ref;
		    $me->release($directory);
		    $me->compile_error_result($tmp);
                    log_finish($bench, $compile_start);
		    return 1;
		}
	    }
	}
    } else {
	$rc = main::log_system("$make build", 'make', $me, \%replacements);
	if (!$rc) {
	    $rc = main::log_system("$make options", 'options', $me, \%replacements);
	    my $fh = new IO::File "<options.out";
	    if (defined $fh) {
		while (<$fh>) {
		    $compile_options .= $_;
		}
		$fh->close();
	    }
	}

	if ($rc) {
	    $tmp = "Error with make!\n";
	    Log(0, "  $tmp") if $rc;
	    $me->pop_ref;
	    $me->shift_ref;
	    $me->release($directory);
	    $me->compile_error_result($tmp);
            log_finish($bench, $compile_start);
	    return 1;
	}
    }
    $me->pop_ref;
    $me->shift_ref;

    log_finish($bench, $compile_start);

    my @unmade = ();
    my $os_ext = $me->os_exe_ext;
    for my $name (@{$me->base_exe}) {
	push (@unmade, $name) if ! -f $name && ! -f "$name$os_ext";
    }
    if (@unmade) {
	my $tmp = "Some files did not appear to be built: ". join(', ',@unmade). "\n";
	Log(0, "  $tmp");
	$me->release($directory);
	$me->compile_error_result($tmp);
	return 1;
    }


    # Well we made it all the way here, so the executable(s) must be built
    # Copy them to the exe directory
    my $tune  = $me->smarttune;
    my $ext   = $me->ext;

    my $ctx = new Digest::MD5;
    my $head = jp($me->path, $me->bindir);
    if ( ! -d $head ) {
	mkdir $head, 00777;
    }
    for my $name (@{$me->base_exe}) {
	my $sname = $name;
	$sname .= $os_ext if ! -f $name && -f "$name$os_ext";
	&main::copy_file($sname, jp($head, "${name}_$tune.$ext"), 1);
	my $fh = new IO::File "<$sname";
	if (!defined $fh) {
	    Log(0, "Can't open file '$sname': $!\n");
	}
	$ctx->addfile($fh);
    }
    my $md5opt = option_md5($me);
    my $md5exe = $ctx->hexdigest;
    eval '$tmp = Compress::Zlib::memGzip($compile_options)';
    $tmp = $compile_options if $@;
    my $compile_options_64 = main::encode_base64($tmp);

    if ($md5ref->{'optmd5'} ne $md5opt) {
	$md5ref->{'optmd5'} = $md5opt;
	$md5ref->{'changedmd5'}++;
    }
    if ($md5ref->{'exemd5'} ne $md5exe) {
	$md5ref->{'exemd5'} = $md5exe;
	$md5ref->{'changedmd5'}++;
    }
    $md5ref->{'valid_build'} = $valid_build?"yes":"no";
    $md5ref->{'compile_options'} = $compile_options_64;
    $md5ref->{'rawcompile_options'} = $compile_options;

    $me->{'dirlist'} = [] unless (ref($me->{'dirlist'}) eq 'ARRAY');
    if ((istrue($me->minimize_rundirs) && ($directory->{'type'} eq 'run')) ||
	(istrue($me->minimize_builddirs) && ($directory->{'type'} eq 'build'))) {
	push @{$me->{'dirlist'}}, $directory;
    } else {
	$me->release($directory);
    }

    return 0;
}

sub log_finish {
    my ($bench, $compile_start) = @_;

    my $compile_finish = time;  ## used by the following log statement
    my $elapsed_time = $compile_finish - $compile_start;
    &main::Log(160, "  Compile for '$bench' ended at:".&main::ctime($compile_finish)." ($compile_finish)\n");
    &main::Log(160, "  Elapsed compile for '$bench': ".&main::to_hms($elapsed_time)." ($elapsed_time)\n");
}

sub compile_error_result {
    my $me = shift @_;
    my $result = new Spec::Config();

    $result->{'valid'} = 0;
    $result->{'errors'} = [ @_ ];
    $result->{'tune'} = $me->tune;
    $result->{'mach'} = $me->mach;
    $result->{'ext'} = $me->ext;
    $result->{'benchmark'} = $me->benchmark;
    $result->{'reference'}     = $me->reference;

    $result->{'reported_sec'}  = '--';
    $result->{'reported_nsec'} = '--';
    $result->{'reported_time'} = '--';
    $result->{'ratio'} = '--';
    $result->{'selected'} = 0;
    $result->{'copies'} = 1;

    push (@{$me->{'result_list'}}, $result);

    return $me;
}

sub link_rundirs {
    my ($me, $owner) = @_;
    $me->{'dirlist'} = $owner->{'dirlist'};
    $me->{'dirlist_is_copy'} = 1;
}

sub setup_rundirs {
    my ($me, $numdirs) = @_;
    my $rc;
    my $tune  = $me->smarttune;
    my $ext   = $me->ext;
    my $mach   = $me->mach;
    my $size   = $me->size;
    my $nonuke = exists($ENV{'SPEC_CPU2000_NO_RUNDIR_NUKE'}) ? 1 : 0;

    my (@dirs) = $me->reserve($nonuke, $numdirs,
			      'type'=>'run', 'tune'=>$tune, 'ext' => $ext,
			      'mach' => $mach, 'size'=>$size,
			      'username' => $me->username);
    my $sizepath = jp($me->path, $me->datadir, $me->size, 'input');
    my $allpath  = jp($me->path, $me->datadir, 'all', 'input');

    # Quick check for "bad" directories
    for my $dir (@dirs) {
	# They're bad if we say they are
	if (istrue($me->deletework)) {
	    $dir->{'bad'} = 1;
	}
	# Any directories that don't exist are obviously bad
	if (!-d $dir->path) {
	    main::mkpath($dir->path);
	    $dir->{'bad'} = 1;
	}
    }

    # Check to see which directories are ok
    my $fast = istrue($me->fast_rundir_verify);
    for my $reffile ($me->input_files_abs) {
	# We can't just do basename here, because sometimes there are files
	# in subdirs
	my $short    = $reffile;
	$short       =~ s%^($sizepath|$allpath)/%%i;
	my $refsize  = stat($reffile)->size;
	my $refdigest;
	$refdigest = main::md5filedigest($reffile) if !$fast;

	for my $dir (@dirs) {
	    next if $dir->{'bad'};
	    my $target = jp($dir->path, $short);
	    if (!-f $target ||
		-s $target != $refsize ||
		(!$fast && ($refdigest ne main::md5filedigest($target)))) {
		$dir->{'bad'} = 1;
	    }
	}
    }

    # Here's the "anything not explicitly permitted is forbidden" section
    # Remove output files from directories which are ok
    for my $dir (@dirs) {
	next if $dir->{'bad'};
	my $basepath = $dir->path;
	my @tmpdir = ($basepath);
	my @files = ();
	while (defined($_ = shift(@tmpdir))) {
	    my $dh = new IO::Dir $_;
	    next unless defined $dh;
	    foreach my $file ($dh->read) {
		next if ($file eq '.' || $file eq '..');
		$file = jp($_, $file);
		if ( -d $file ) {
		    push @tmpdir, $file;
		} else {
		    push @files, $file;
		}
	    }
	}
	@files = sort map { s%^$basepath/%%i; $_ } @files;
	my @okfiles = sort map { s%^($sizepath|$allpath)/%%i; $_ } ($me->output_files_base, $me->input_files_base);
	for my $reffile (@files) {
	    next if grep { $reffile eq $_ } @okfiles;
	    my $target = jp($dir->path, $reffile);
	    next if !-f $target;
	    if (!unlink ($target)) {
		$dir->{'bad'} = 1;
		next;
	    }
	}
	my $dh = new IO::Dir $dir->path;
	if (!defined $dh) {
	    $dir->{'bad'} = 1;
	    next;
	}
	while (defined($_ = $dh->read)) { 
	    next if !m/\.(out|err|cmp|mis)$/;
	    my $target = jp($dir->path, $_);
	    if (!unlink ($target)) {
		$dir->{'bad'} = 1;
		last;
	    }
	}
    }

    my $needed_setup = 0;
    # Now rebuild all directories which are not okay
    for my $dir (@dirs) {
	next if !$dir->{'bad'};
	$needed_setup = 1;
	delete $dir->{'bad'};
	my $path = $dir->path();
	if (!rmpath($path)) {
	    &main::mkpath($path);
	}

	# Copy input files
	$me->copy_input_files_to($fast, $path);
    }

    # Copy executables to first directory
    for my $file ($me->exe_files_abs) {
	main::copy_file($file, $dirs[0]->path, 1);
    }

    $me->{'dirlist'} = [ @dirs ];
    return $needed_setup;
}

sub cleanup_rundirs {
    my ($me, $numdirs) = @_;
    $numdirs = @{$me->{'dirlist'}} if ($numdirs <= 0);
    my @output_files = $me->output_files_base;
    for (my $i = 0; $i < $numdirs; $i++) {
	my $dir = $me->{'dirlist'}[$i]->path;
	my $file;

	# Remove explicit output files
	for $file (map { jp($dir, $_) } @output_files) {
	    unlink $file;
	}

	# Remove any files ending in .err or .out
	my $dh = new IO::Dir "$dir";
	while (defined($file = $dh->read)) {
	    next if $file !~ m/\.(cmp|out|err|mis)$/i;
	    my $absfile = jp($dir, $file);
	    unlink $absfile;
	}
    }
}

sub rmpath {
    # Remove the contents of a given directory.  Doesn't actually remove the
    # directory itself.
    my ($path) = @_;
    # Remove the contents of the given path
    my $dh = new IO::Dir $path;
    return 0 if (!defined $dh);	# Fail quietly; there's nothing to do.
    while (defined($_ = $dh->read)) { 
	my $target = jp($path, $_);
	if (-d $target) {
	    next if ($target =~ /\/\.{1,2}/o);
	    rmpath($target);
	    rmdir($target);
	} else {
	    if (!unlink ($target)) {
		Log(0, "Couldn't unlink $target - tree removal aborted\n");
		return 0;
	    }
	}
    }
    return 1;
}

sub delete_binaries {
    my ($me, $all) = @_;

    my $head = jp($me->path, $me->bindir);
    if ($all) {
	rmpath($head);
    } else {
	my $tune  = $me->smarttune;
	my $ext   = $me->ext;
#	my $mach  = $me->mach;
#	if ($mach eq 'default') {
#	    $mach = '';
#	} elsif ($mach ne '') {
#	    $mach = "_$mach";
#	}
	for my $name (@{$me->base_exe}) {
#	    unlink(jp($head, "${name}_$tune$mach.$ext"));
	    unlink(jp($head, "${name}_$tune.$ext"));
	}
    }
}

sub delete_rundirs {
    my ($me, $all) = @_;

    my @attributes = ();

    if ($all) {
	my $dir = jp($me->{'path'}, $me->workdir);
	rmpath($dir);
    } else {
	@attributes = ([
	    'username'=>$me->username, 'size'=>$me->size, 'ext'=>$me->ext,
	    'tune'=>$me->smarttune, 'mach'=>$me->mach,
	], [
	    'username'=>$me->username, 'type'=>'build', 'ext'=>$me->ext
	]);

	my $file = $me->lock_listfile();
	my $entry;
	for my $attr (@attributes) {
	    while (1) {
		$entry = $file->find_entry(@$attr);
		last if !$entry;
		rmpath($entry->path);
		rmdir($entry->path);
		$entry->remove();
	    }
	}
	$file->update();
	$file->close();
    }
}

sub remove_rundirs {
    my ($me) = @_;

    if ($me->{'dirlist_is_copy'}) {
	delete $me->{'dirlist_is_copy'};
    } else {
	if (ref($me->{'dirlist'}) eq 'ARRAY') {
	    my @dirs = @{$me->{'dirlist'}};
	    for my $dirobj (@dirs) {
		rmpath($dirobj->path);
	    }
	    $me->release(@dirs);
	} else {
	    Log(3, "No list of directories to remove for ".$me->descmode."\n");
	}
    }
    $me->{'dirlist'} = [];
}

sub release_rundirs {
    my ($me) = @_;

    if ($me->{'dirlist_is_copy'}) {
	delete $me->{'dirlist_is_copy'};
    } elsif (ref($me->{'dirlist'}) eq 'ARRAY') {
	my @dirs = @{$me->{'dirlist'}};
	$me->release(@dirs);
    }
    $me->{'dirlist'} = [] unless (istrue($me->minimize_rundirs));
}

sub reserve {
    my ($me, $nonuke, $num, @attributes) = @_;
    $num = 1 if ($num eq '');
    if (@attributes == 0) {
	@attributes = ( 'username'=>$me->username,   'ext'=>$me->ext,
			'tune'=>$me->smarttune, 'mach'=>$me->mach, );
    }

    my $file = $me->lock_listfile();
    my @entries;

    for (my $i = 0; $i < $num; $i++ ) {
	my $entry = $file->find_entry('lock'=>0, @attributes);
	if (!$entry || $nonuke) {
	    $entry = $file->new_entry('lock'=>0, 'username'=>$me->username, @attributes);
	}
	push (@entries, $entry);
	$entry->lock($me->username);
    }
    $file->update();
    $file->close();
    push (@{$me->{'entries'}}, @entries);

    return @entries;
}

sub release {
    my ($me, @dirs) = @_;

    my $file = $me->lock_listfile();

    for my $dir (@dirs) {
	my $entry = $file->find_entry_name($dir->name);
	if ($entry) {
	    $entry->unlock($dir->name);
	} else {
	    Log(0, "WARNING: release: Bogus entry in entries list\n");
	}
    }

    $file->update();
    $file->close();
}

sub ce { main::command_expand(@_) };

sub run_benchmark {
    my ($me, $num_users) = @_;
    my $noisy = istrue($me->teeout) && istrue($me->teerunout);
    my @submitcmds = map {$me->accessor($_)} $me->find_keys('submit*');
    Log(40, "Submit commands for ".$me->descmode.":\n".join("\n",@submitcmds)."\n");
    my $submit = 0;
    my $origwd = main::cwd();

    my @dirs = @{$me->dirlist}[0..$num_users-1];
    my $error = 0;

    my $path = $dirs[0]->path;
    chdir($path);

    $me->unshift_ref({ 'iter' => 0, 'command' => '', 'commandexe' => '',
		       'usernum' => 0, });
    $me->push_ref   ({ 'fdocommand' => '', 'monitor_wrapper' => '',
		       'monitor_specrun_wrapper' => '', });
    my @newcmds;

    for my $dir (@dirs) {
	push (@newcmds, '-u ' . $dir->path);
    }
    for my $obj ($me->invoke) {
	$me->iter($me->iter+1);
	my $command = jp('..', basename($path), $obj->{'command'});
	$me->accessor_nowarn('commandexe', $command);
	$command .= ' ' . join (' ', @{$obj->{'args'}}) if @{$obj->{'args'}};
	if ($me->command_add_redirect) {
	    $command .= ' < '.$obj->{'input'} if ($obj->{'input'} ne '');
	    $command .= ' > '.$obj->{'output'} if ($obj->{'output'} ne '');
	    $command .= ' 2> '.$obj->{'error'} if ($obj->{'error'} ne '');
	}
	$me->command($command);

	## expand variables and values in the command line
	if ($me->fdocommand ne '') {
	    $command = ce($me->fdocommand, $me);
	    $me->command($command);
	} elsif ($me->monitor_wrapper ne '') {
	    $command = ce($me->monitor_wrapper, $me);
	    $me->command($command);
	}

	my $submit = 0;
	$me->usernum($submit);
	if (@submitcmds &&
	    (istrue($me->rate) || istrue($me->use_submit_for_speed))) {
	    $command = ce($submitcmds[$submit++%@submitcmds], $me);
	    $me->command($command);
	}
	my $opts = '';
	$opts .= '-i '. $obj->{'input'}  .' ' if (exists $obj->{'input'});
	$opts .= '-o '. $obj->{'output'} .' ' if (exists $obj->{'output'});
	$opts .= '-e '. $obj->{'error'}  .' ' if (exists $obj->{'error'});
	push (@newcmds, "$opts$command");
    }

    Log(150, "Commands to run: \n    ", join ("\n    ", @newcmds), "\n");

    my $absrunfile = jp($path, $me->commandfile);
    my $resfile    = jp($path, $me->commandoutfile);
    {
	my $fh = new IO::File ">$absrunfile";
	if (defined($fh)) {
	    print $fh join ("\n", @newcmds), "\n";
	    $fh->close;
	} else {
	    Log(0, "Error opening $absrunfile for writing!\n");
	    exit 1;
	}
    }

    my @specrun = (jp($me->top, 'bin', $me->specrun),
		'-d', $path,
		'-e', $me->commanderrfile,
		'-o', $me->commandoutfile,
		'-f', $me->commandfile,
		);
    push @specrun, '-r' if $me->command_add_redirect;
    my $command =join (' ', @specrun);
    $me->command($command);
    if ($me->monitor_specrun_wrapper ne '') {
	$command = ce($me->monitor_specrun_wrapper, $me);
    }
    main::monitor_pre_bench($me);

    my %oldENV = %ENV;
    main::munge_environment($me) if $me->env_vars;
    Log(191, "Specinvoke: $command\n");

    my $start = time;
    my $rc;
    if ($me->monitor_specrun_wrapper ne '') {
	$rc = system $command;
    } else {
	$rc = system @specrun;
    }
    my $stop = time;
    my $elapsed = $stop-$start;
    %ENV = %oldENV if $me->env_vars;

    main::monitor_post_bench($me);

    $me->pop_ref();
    $me->shift_ref();

    return $rc if $me->accessor_nowarn('fdocommand') ne '';

    my $result = new Spec::Config();
    $result->{'valid'} = 1;
    $result->{'errors'} = [];
    if (defined($rc) && $rc) {
	$result->{'valid'} = 0;
	Log(0, $me->specrun . " returned with non-zero return code($rc)\n");
	push (@{$result->{'errors'}}, $me->specrun . " returned with non-zero return code($rc)\n");
    }
    $result->{'tune'} = $me->tune;
    $result->{'mach'} = $me->mach;
    $result->{'ext'} = $me->ext;
    $result->{'benchmark'} = $me->benchmark;

    my $fh = new IO::File "<$resfile";
    if (defined $fh) {
	my $error = 0;
	my @child_times = ( );
	while (<$fh>) {
	    if (m/child finished:\s*(\d+),\s*(\d+),\s*(\d+),\s*(\d+),\s*(\d+),\s*\d+,\s*(\d+)/) {
		my ($num, $ssec, $snsec, $esec, $ensec, $rc) =
		    ($1, $2, $3, $4, $5, $6);
		if ($rc != 0) {
		    $error = 1;
		    $result->{'valid'} = 0;
		    Log(0, "Child returned with invalid return code($1)\n");
		}
		$child_times[$num] = { 'time' => 0 } unless defined($child_times[$num]);
		$child_times[$num]->{'lastline'} = "user $num finished \@ ".main::ctime($ssec+($snsec/1000000000)).".  Total elapsed time: ";
		$child_times[$num]->{'time'} += $esec + ($ensec/1000000000);
	    }
	    if (m/runs elapsed time:\s*(\d+),\s*(\d+)/) {
		$result->{'reported_sec'}  = $1;
		$result->{'reported_nsec'} = $2;
	    }
	}
	foreach my $ref (@child_times) {
	    next unless defined($ref);
	    if (ref($ref) ne 'HASH') {
		Log(0, "Non-HASH ref found in child stats: $ref\n");
	    } else {
		Log(125, $ref->{'lastline'}.$ref->{'time'}."\n");
	    }
	}
	if ($error) {
	    push (@{$result->{'errors'}}, "Child returned with invalid return code\n");
	}
	$fh->close;
    } else {
	$result->{'valid'} = 0;
	Log(0, "couldn't open specrun result file '$resfile'\n");
	push (@{$result->{'errors'}}, "couldn't open specrun result file\n");
    }

# Now make sure that the results compared!
    if ($me->action eq 'run') {
	$result->{'valid'} = 0;
    } else {
	my $size    = $me->size;
	my $comparedir  = $dirs[0]->path;
	my $comparename = jp($comparedir, $me->comparefile);

	if ($me->pre_compare(@dirs)) {
	    Log(0, "pre_compare for " . $me->benchmark . " failed!\n");
	};

	my $comparecmd = new IO::File ">$comparename";
	if (!defined $comparecmd) {
	    $result->{'valid'} = 0;
	    push (@{$result->{'errors'}}, "Unable to write compare file");
	} else {
	    my $num_output_files = 0;
	    for my $obj (@dirs) {
		my $path = $obj->path;
		my $basecmd = "-c $path ";

		Log(145, "comparing files in '$path'\n");
		for my $absname ($me->output_files_abs) {
		    my $relname = basename($absname);
		    my $cw      = $me->compress_whitespace($size, $relname);
		    my $abstol  = $me->abstol ($size, $relname);
		    my $reltol  = $me->reltol ($size, $relname);
		    my $obiwan  = $me->obiwan ($size, $relname);
		    my $skiptol = $me->skiptol($size, $relname);

		    Log(150, "comparing '$relname' with cw='$cw', abstol='$abstol', reltol='$reltol',obiwan='$obiwan',skiptol='$skiptol'\n");

		    my $cmd = $basecmd . "-o $relname.cmp " . jp($me->top, 'bin', $me->specdiff) . 
			    ' -m -l ' . $me->difflines . ' ';
		    $cmd .= ' -c ' if defined($cw) && $cw;
		    $cmd .= " -a $abstol " if defined($abstol) && ($abstol ne '');
		    $cmd .= " -r $reltol " if defined($reltol) && ($reltol ne '');
		    $cmd .= " -o " if defined($obiwan) && $obiwan;
		    $cmd .= " -s $skiptol " if defined($skiptol) && ($skiptol ne '');
		    $cmd .= $absname . ' ' . $relname;
		    print $comparecmd "$cmd\n";
		    $num_output_files++;
		}
	    }
	    $comparecmd->close();

	    my $num_compares = $me->max_active_compares;
	    # If max_active_compares isn't set, this will ensure that we
	    # do one compare (at a time) per run directory
	    $num_compares = @dirs+0 if $num_compares == 0;
	    # If we try to run more compares than the total number of output
	    # files to compare (users * output files), then one will exit,
	    # and the compare will fail, even if everything else is okay.
	    if ($num_compares > $num_output_files) {
		$num_compares = $num_output_files;
	    }
	    my @specrun = (jp($me->top, 'bin', $me->specrun),
			'-E',
			'-d', $comparedir,
			'-c', $num_compares,
			'-e', $me->compareerrfile,
			'-o', $me->compareoutfile,
			'-f', $me->comparefile,
			);
	    Log(191, 'Specinvoke: ', join (' ', @specrun), "\n");
	    $rc = system @specrun;
	    if ($rc) {
		$result->{'valid'} = 0;
		push (@{$result->{'errors'}}, "Output miscompare");
		my $logged = 0;
		for my $obj (@dirs) {
		    my $file;
		    my $dh = new IO::Dir $obj->path;
		    while (defined($file = $dh->read)) {
			next if $file !~ m/\.mis$/i;
			my ($basename) = $file =~ m/(.*)\.mis$/;
			my ($misname) = jp($obj->path, $file);

			Log (0, "*** Miscompare of $basename, see $misname\n");
			$logged = 1;
			my $fh = new IO::File "<$misname";
			if (!defined $fh) {
			    Log (0, "Can't open miscompare file!\n");
			} else {
			    while (<$fh>) {
				Log (120, $_);
			    }
			    $fh->close();
			}
		    }
		}
		Log(0, "Compare command returned $rc!\n") unless $logged;
	    }
	}
    }

    my $reported_sec  = $result->{'reported_sec'};
    my $reported_nsec = $result->{'reported_nsec'};
    my $reported = $reported_sec + $reported_nsec / 1000000000;
    if ($me->size eq 'ref') {
	my $reference = $me->reference;

	$result->{'reference'}     = $reference;
	$result->{'ratio'}         = (defined($reported) && $reported) ? $reference / $reported : 0;
	if (istrue($me->rate)) {
	    $result->{'ratio'} *= $num_users * $::rate_multiplier;
	} else {
	    $result->{'ratio'} *= $::speed_multiplier;
	}
    } else {
	$result->{'reference'}     = '--';
	$result->{'ratio'}         = '--';
    }
    $result->{'reported_time'} = $reported;
    $result->{'selected'}      = 0;
    $result->{'copies'}        = $num_users;
    $result->{'benchmark'}     = $me->benchmark;

    &Log (155, "Benchmark Times:\n",
              '  Start:    ', &main::ctime($start), " ($start)\n",
              '  Stop:     ', &main::ctime($stop),  " ($stop)\n",
              '  Elapsed:  ', &main::to_hms($elapsed), " ($elapsed)\n",
              '  Reported: ', "$reported_sec $reported_nsec $reported\n");


    push (@{$me->{'result_list'}}, $result);
    chdir($origwd);
    return $result;
}

sub pre_compare {
    return 0;
}

sub result_list {
    my ($me, $copies) = @_;
    if (defined $copies) {
	return grep ($_->copies == $copies, @{$me->{'result_list'}});
    } else {
	return @{$me->{'result_list'}};
    }
}

sub ratio {
    my ($me, $num_users) = @_;
    my @res = @{$me->{'result_list'}};
    if (defined $num_users) {
	@res = grep ($_->{'copies'} == $num_users, @res);
    }
    @res = sort { $a->{'ratio'} <=> $b->{'ratio'} } @{$me->{'result_list'}};
    if (@res % 2) {
	return $res[(@res-1)/2]; # Odd # results, return the median ratio
    } else {
	return ($res[@res/2-1] + $res[@res/2]/2); # Return the average of the
	                                          # two middle values
    }
}

sub copies {
    my ($me) = @_;
}

sub lock_listfile {
    my $me = shift;

    my $dir       = jp($me->{'path'}, $me->workdir);
    my $file      = jp($dir,          $me->worklist);
    my $obj = new Spec::Listfile($dir, $file);
    $me->{'listfile'} = $obj;
    return $obj;
}


1;

#
# vars.pl
#
# Copyright (C) 1995-2005 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: vars.pl 1423 2005-06-15 18:47:00Z cloyce $

##############################################################################
# Initialize Variables
##############################################################################

use strict;
use Config;
use Cwd;
use UNIVERSAL;

# Customize the name of the suite and the multipliers here
$::year = '2000';
$::suite = 'CPU'.$::year;

# lcsuite should match the name of at least one of your benchsets.  If not,
# be sure to edit resolve_user_selection in parse.pl to do the right thing.
$::lcsuite = lc($::suite);

# Ratio multipliers vs. reference machine
$::speed_multiplier = 100;      # See osgcpu-2358
$::rate_multiplier = 1.16;      # See osgcpu-2359

my $nonvolatile_config = {
    'benchdir'   => 'benchspec',     # benchmark directory
    'resultdir'  => 'result',        # result directory
    'configdir'  => 'config',        # configuration directory
    'log'        => 'log',           # extra error logging file
    'dirprot'    => 0777,            # directory protection
    'fileprot'   => 0777,            # directory protection
    'workdir'    => 'run',           # Where to actually do the runs
    'worklist'   => 'list',          # Where to actually do the runs
    'exitvals'   => 'spec_exit',     # File containing return codes
    'datadir'    => 'data',          # directory containing input sets
    'inputdir'   => 'input',         # dir under datadir containing input
    'outputdir'  => 'output',        # dir under datadir containing output
    'reftime'    => 'reftime',       # file containing reference for input
    'bindir'     => 'exe',           # directory containing executables
    'srcdir'     => 'src',           # directory containing source
    'specmake'   => 'Makefile.spec', # Override file for Makefile
    'specdiff'   => 'specdiff',      # Number of lines of difference
    'min_report_runs' => 3,          # Minimum number of runs to be valid
    'rawfile'    => '',	             # Hint to the formatter (word to your mother)
    'orig_argv'  => [ @ARGV ],
    'orig_env'   =>  { %ENV },
    'help'       => 0,
    'version'    => 0,
    'output'     => 'asc',
    'valid_actions'    => [ qw(build setup run validate clean realclean trash
			       clobber nuke) ],
    'valid_tunes'      => [ qw(base peak) ],
    'vendor_makefiles' => 0,
    'specrun'          => 'specinvoke',
};

my $default_config = {
    'compile_error'       => 0,               # had problems compiling?
    'reportable'          => 0,               # run follows all reporting rules
    'check_md5'           => 1,               # check saved md5 sums
    'prefix'              => '',              # prefix to prepend to files
    'sigint'              =>  undef,          # Signal # of SIGINT
    'ignore_sigint'       =>  0,              # Ignore SIGINT
    'make'                => 'specmake',      # Name of make executable (override in benchmark.pm)
    'vendor'              => 'anon',          # Vendor/Version for filenames
    'action'              => 'validate',      # Default action
    'run'                 => 'all',           # What benchmarks to run
    'config'              => 'default.cfg',   # configuration file
    'mach'                => 'default',       # Vendor/Version for filenames
    'ext'                 => 'none',          # default extension for executable
    'size'                => 'ref',           # Size of input set
    'tune'                => 'base',          # default tuning level
    'output_format'       => 'all',           # output format
    'rawformat'           => 0,               # just do rawformat
    'nc'                  => 0,               # format as non-compliant
    'srcalt'              => '',              # approved source mods
    'expand_notes'        => 0,               # do var expansion in notes
    'max_active_compares' => 0,               # Max # of parallel compares
    'difflines'           => 10,              # Number of lines of difference
    'subworkdir'          => 'work',          # prefix for dir under workdir
    'endian'              => $Config{'byteorder'},
    'ignore_errors'       => 0,               # Ignore certain errors
    'mean_anyway'         => 0,               # Calculate mean even if invalid
    'setprocgroup'        => 1,               # Set the process group
    'verbose'             => 5,               # Verbosity level
    'deletework'          => 0,               # Delete existing working dirs
    'deletebinaries'      => 0,               # Delete existing binaries
    'rate'                => 0,               # Rate vs Speed
    'unbuffer'            => 0,               # Unbuffer STDOUT
    'line_width'          => 0,               # line wrap width
    'log_line_width'      => 0,               # line wrap width for logfile
    'feedback'            => 1,               # Default to allow feedback
    'users'               => 1,               # Number of users
    'uid'                 => $<,              # User ID of the user
    'rebuild'             => 0,               # Rebuild binaries even if they
                                              # already exist
    'env_vars'            => 0,               # Allow environment to be
                                              # overriden by ENV_*
    'locking'             => 1,               # Try to lock files
    'os_exe_ext'          => '',	      # Some OSes (NT) create
                                              # executables with specific
				              # extesions
    'makeflags'           => '',	      # Extra flags for make (like -j)
    'OS'                  => 'unix',	      # OS type
    'teeout'              => 0,               # Run output through 'tee' so
				              # you can see it on the screen
    'teerunout'           => 0,               # Run output through 'tee' so
				              # you can see it on the screen
    'minimize_rundirs'    => 0,		      # Try to keep working disk size
				              # down as small as possible
    'minimize_builddirs'  => 0,		      # Try to keep working disk size
				              # down as small as possible
    'backup_config'       => 1,		      # Whether to keep backup config
				              # file left over from updating
    				              # MD5s
    'fast_rundir_verify'  => 0,		      # Don't MD5 the files in the
				              # dirs, just check size/date
    'make_no_clobber'     => 0,               # Don't blow away directory when
				              # building executables
    'basepeak'            => 0,		      # Use base binary for peak
				              # measurements
    'iterations'          => 3,		      # Number of iterations to run
    'commandfile'         => 'speccmds.cmd',  # Name of command file
    'commanderrfile'      => 'speccmds.err',  # Name of command error file
    'commandoutfile'      => 'speccmds.out',  # Name of command output file
    'comparefile'         => 'compare.cmd',   # Name of compare file
    'compareerrfile'      => 'compare.err',   # Name of compare error file
    'compareoutfile'      => 'compare.out',   # Name of compare output file
    'table'               => 1,               # Produce a table of results
    'safe_eval'           => 1,               # Very strict opcode mask for
				              # string expansion
    'command_add_redirect' => 0,
    'use_submit_for_speed'=> 0,
    'VPATH_INCLUDE'       => '',
    'VPATH_CINCLUDE'      => '',
    'VPATH_CXXINCLUDE'    => '',
    'VPATH_FINCLUDE'      => '',
    'VPATH_F90INCLUDE'    => '',
    'USE_VPATH'           => 0,
};

sub initialize_variables {
    my ($config) = @_;

    for (keys %$default_config) {
	$config->{$_} = $default_config->{$_};
    }
    for (keys %$nonvolatile_config) {
	$config->{$_} = $nonvolatile_config->{$_};
    }

    my $name = '';
    $name = $ENV{'SPECUSER'}     if ($name eq '') && (exists $ENV{'SPECUSER'});
    $name = $ENV{'USER'}         if ($name eq '') && (exists $ENV{'USER'});
    $name = $ENV{'USERNAME'}     if ($name eq '') && (exists $ENV{'USERNAME'});
    $name = $ENV{'LOGNAME'}      if ($name eq '') && (exists $ENV{'LOGNAME'});
    $name = eval q|getpwuid $config->{'uid'}| if ($name eq '');
    $name = $config->{'uid'}     if ($name eq '');
    $name = 'default'            if ($name eq '');
    $config->{'username'} = $name;

    # Check to see if OS was specified in environment
    $config->{'OS'} = lc($ENV{'OS'}) if exists($ENV{'OS'}) && ($ENV{'OS'} ne '');
    if ($config->{'OS'} =~ /^windows/) {
	$config->{'os_exe_ext'} = '.exe';
	$config->{'ignore_sigint'} = 1;
	$config->{'specdiff'} = 'specdiff.bat';
	$nonvolatile_config->{'specdiff'} = 'specdiff.bat';
    }

    # See where the top of the SPEC tree is
    $config->{'top'} = $ENV{'SPEC'};
    $config->{'top'} = cwd if $ENV{'SPEC'} eq '' || ! -d $ENV{'SPEC'};
    $config->{'specrun'} = jp($config->top, $config->specrun);

    # Check to see if sigint is defined in the Config data
    {
	my @nums = split(" ", $Config{'sig_num'});
	my @names = split(" ", $Config{'sig_name'});
	while (@nums && @names) {
	    my $num = shift @nums;
	    my $name = shift @names;
	    if ($name eq 'INT') {
		$config->{'sigint'} = $num;
		last;
	    }
	}
    }
}
sub finalize_config {
    my ($config, $cl_opts) = @_;
# Command line options override config file options
    for ( keys %$cl_opts) {
	next if $_ eq 'ref' || $_ eq 'refs';
	$config->{$_} = $cl_opts->{$_};
    }

# Make sure none of the unchangeble constants changed
    for ( keys %$nonvolatile_config) {
	$config->{$_} = $nonvolatile_config->{$_};
    }
}

1;

__END__

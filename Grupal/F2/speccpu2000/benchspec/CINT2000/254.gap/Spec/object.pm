$benchnum  = '254';
$benchname = 'gap';
$exename   = 'gap';
$benchlang = 'C';
@base_exe  = ($exename);

$reltol   = 0.01;
$abstol   = undef;

@sources=qw(agcollec.c eval.c integer.c range.c statemen.c vector.c aggroup.c finfield.c list.c rational.c string.c word.c blister.c function.c pcpresen.c read.c system.c coding.c gap.c permutat.c record.c tietze.c costab.c gasman.c plist.c scanner.c unknown.c cyclotom.c idents.c polynom.c set.c vecffe.c);
$need_math='yes';

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;

    my $exe = $me->exe_file;
    my $size = $me->size;

    if ($size eq 'ref') {
        @opts=qw(-l ./ -q -m 192M);
    } elsif ($size eq 'train') {
        @opts=qw(-l ./ -q -m 128M);
    } elsif ($size eq 'test') {
        @opts=qw(-l ./ -q -m 64M);
    } else {
        main::Log(0, $me->benchmark . ": Unable to process size '$size'\n");
    }



    for ($me->input_files_base) {
	if (($name) = m/(.*).in$/) {
	    push (@rc, { 'command' => $exe, 
			 'args'    => [ @opts ],
			 'input'   => $_,
			 'output'  => "$name.out",
			 'error'   => "$name.err",
			});
	}
    }
    return @rc;
}

1;

$benchnum  = '176';
$benchname = 'gcc';
$exename   = 'cc1';
$benchlang = 'C';
@base_exe  = ($exename);

@sources=qw(c-parse.c c-lang.c c-lex.c c-pragma.c c-decl.c c-typeck.c
	    c-convert.c c-aux-info.c c-common.c c-iterate.c toplev.c version.c
	    tree.c print-tree.c stor-layout.c fold-const.c function.c stmt.c
	    expr.c calls.c expmed.c explow.c optabs.c varasm.c rtl.c
	    print-rtl.c rtlanal.c emit-rtl.c real.c dbxout.c sdbout.c
	    dwarfout.c xcoffout.c integrate.c jump.c cse.c loop.c unroll.c
	    flow.c stupid.c combine.c regclass.c local-alloc.c global.c
	    reload.c reload1.c caller-save.c insn-peep.c reorg.c sched.c
	    final.c recog.c reg-stack.c insn-opinit.c insn-recog.c
	    insn-extract.c insn-output.c insn-emit.c insn-attrtab.c m88k.c
	    getpwd.c convert.c bc-emit.c bc-optab.c obstack.c);
$need_math='yes';

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;
    my $exe  = $me->exe_file;

    for ($me->input_files_base) {
	if (($name) = m/(.*).i$/) {
	    push (@rc, { 'command' => $exe, 
			 'args'    => [ $_, "-o $name.s" ], 
			 'output'  => "$name.out",
			 'error'   => "$name.err",
			});
	}
    }
    @rc;
}

1;

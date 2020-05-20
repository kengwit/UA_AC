	file	 "166.i"
data

; cc1 (2.7.2.2) arguments: -O -fdefer-pop -fomit-frame-pointer
; -fcse-follow-jumps -fcse-skip-blocks -fexpensive-optimizations
; -fthread-jumps -fstrength-reduce -funroll-loops -fwritable-strings
; -fpeephole -fforce-mem -ffunction-cse -finline-functions -finline
; -freg-struct-return -fdelayed-branch -frerun-cse-after-loop
; -fschedule-insns -fschedule-insns2 -fcommon -fgnu-linker -m88110 -m88100
; -m88000 -mno-ocs-debug-info -mno-ocs-frame-position -mcheck-zero-division

gcc2_compiled.:
	align	 4
_ptrace_nelt:
	word	 0
	align	 4
_twolev_nelt:
	word	 3
	align	 4
_twolev_config:
	word	 1
	word	 1024
	word	 8
	align	 4
_ruu_include_spec:
	word	 1
	align	 4
_RUU_size:
	word	 8
	align	 4
_LSQ_size:
	word	 4
	align	 4
_mem_nelt:
	word	 2
	align	 4
_mem_lat:
	word	 18
	word	 2
	align	 4
_pcstat_nelt:
	word	 0
	global	 _fu_config
	align	 4
_fu_config:
	word	 @LC0
	word	 4
	word	 0
	word	 1
	word	 1
	word	 1
	zero	 4
	zero	 240
	word	 @LC1
	word	 1
	word	 0
	word	 2
	word	 3
	word	 1
	zero	 4
	word	 3
	word	 20
	word	 19
	zero	 4
	zero	 224
	word	 @LC2
	word	 2
	word	 0
	word	 10
	word	 1
	word	 1
	zero	 4
	word	 11
	word	 1
	word	 1
	zero	 4
	zero	 224
	word	 @LC3
	word	 4
	word	 0
	word	 4
	word	 2
	word	 1
	zero	 4
	word	 5
	word	 2
	word	 1
	zero	 4
	word	 6
	word	 2
	word	 1
	zero	 4
	zero	 208
	word	 @LC4
	word	 1
	word	 0
	word	 7
	word	 4
	word	 1
	zero	 4
	word	 8
	word	 12
	word	 12
	zero	 4
	word	 9
	word	 24
	word	 24
	zero	 4
	zero	 208
	align	 8
@LC4:
	string	 "FP-MULT/DIV\000"
	align	 8
@LC3:
	string	 "FP-adder\000"
	align	 8
@LC2:
	string	 "memory-port\000"
	align	 8
@LC1:
	string	 "integer-MULT/DIV\000"
	align	 8
@LC0:
	string	 "integer-ALU\000"
	align	 8
_sim_num_insn:
	word	 0
	word	 0
	align	 8
_sim_total_insn:
	word	 0
	word	 0
	align	 8
_sim_num_refs:
	word	 0
	word	 0
	align	 8
_sim_total_refs:
	word	 0
	word	 0
	align	 8
_sim_num_loads:
	word	 0
	word	 0
	align	 8
_sim_total_loads:
	word	 0
	word	 0
	align	 8
_sim_num_branches:
	word	 0
	word	 0
	align	 8
_sim_total_branches:
	word	 0
	word	 0
	align	 8
_sim_cycle:
	word	 0
	word	 0
	align	 4
_inst_seq:
	word	 0
	align	 4
_ptrace_seq:
	word	 0
	align	 4
_spec_mode:
	word	 0
	align	 4
_ruu_fetch_issue_delay:
	word	 0
	align	 4
_pred_perfect:
	word	 0
	align	 4
_fu_pool:
	word	 0
	align	 8
@LC5:
	string	 "chunks > 0\000"
	align	 8
@LC6:
	string	 "sim-outorder.c\000"
	align	 8
@LC7:
	string	 "mem_access_latency\000"
	align	 8
@LC8:
	string	 "sim-outorder.c\000"
	align	 8
@LC9:
	string	 "il1_access_fn\000"
	align	 8
@LC10:
	string	 "writes to instruction memory not supported\000"
	align	 8
@LC11:
	string	 "sim-outorder.c\000"
	align	 8
@LC12:
	string	 "il1_access_fn\000"
	align	 8
@LC13:
	string	 "writes to instruction memory not supported\000"
	align	 8
@LC14:
	string	 "sim-outorder.c\000"
	align	 8
@LC15:
	string	 "il2_access_fn\000"
	align	 8
@LC16:
	string	 "writes to instruction memory not supported\000"
	align	 8
@LC17:
	string	 "phy_page_ptr\000"
	align	 8
@LC18:
	string	 "sim-outorder.c\000"
	align	 8
@LC19:
	string	 "itlb_access_fn\000"
	align	 8
@LC20:
	string	 "phy_page_ptr\000"
	align	 8
@LC21:
	string	 "sim-outorder.c\000"
	align	 8
@LC22:
	string	 "dtlb_access_fn\000"
	align	 8
@LC23:
	string	 "sim-outorder: This simulator implements a very de"
	string	 "tailed out-of-order issue\nsuperscalar processor "
	string	 "with a two-level memory system and speculative\ne"
	string	 "xecution support.  This simulator is a performanc"
	string	 "e simulator, tracking the\nlatency of all pipelin"
	string	 "e operations.\n\000"
	align	 8
@LC24:
	string	 "-ptrace\000"
	align	 8
@LC25:
	string	 "generate pipetrace, i.e., <fname|stdout|stderr> <"
	string	 "range>\000"
	align	 8
@LC26:
	string	 "  Pipetrace range arguments are formatted as foll"
	string	 "ows:\n\n    {{@|#}<start>}:{{@|#|+}<end>}\n\n  Bo"
	string	 "th ends of the range are optional, if neither are"
	string	 " specified, the entire\n  execution is traced.  R"
	string	 "anges that start with a `@' designate an address\n"
	string	 "  range to be traced, those that start with an `#"
	string	 "' designate a cycle count\n  range.  All other ra"
	string	 "nge values represent an instruction count range. "
	string	 " The\n  second argument, if specified with a `+',"
	string	 " indicates a value relative\n  to the first argum"
	string	 "ent, e.g., 1000:+100 == 1000:1100.  Program symbo"
	string	 "ls may\n  be used in all contexts.\n\n    Example"
	string	 "s:   -ptrace FOO.trc #0:#1000\n                -p"
	string	 "trace BAR.trc @2000:\n                -ptrace BLA"
	string	 "H.trc :1500\n                -ptrace UXXE.trc :\n"
	string	 "                -ptrace FOOBAR.trc @main:+278\n\000"
	align	 8
@LC27:
	string	 "-max:inst\000"
	align	 8
@LC28:
	string	 "maximum number of inst's to execute\000"
	align	 8
@LC29:
	string	 "-fastfwd\000"
	align	 8
@LC30:
	string	 "number of insts skipped before timing starts\000"
	align	 8
@LC31:
	string	 "-fetch:ifqsize\000"
	align	 8
@LC32:
	string	 "instruction fetch queue size (in insts)\000"
	align	 8
@LC33:
	string	 "-fetch:mplat\000"
	align	 8
@LC34:
	string	 "extra branch mis-prediction latency\000"
	align	 8
@LC35:
	string	 "-bpred\000"
	align	 8
@LC36:
	string	 "branch predictor type {nottaken|taken|perfect|bim"
	string	 "od|2lev}\000"
	align	 8
@LC37:
	string	 "bimod\000"
	align	 8
@LC38:
	string	 "-bpred:bimod\000"
	align	 8
@LC39:
	string	 "bimodal predictor BTB size\000"
	align	 8
@LC40:
	string	 "-bpred:2lev\000"
	align	 8
@LC41:
	string	 "2-level predictor config (<l1size> <l2size> <hist"
	string	 "_size>)\000"
	align	 8
@LC42:
	string	 "-decode:width\000"
	align	 8
@LC43:
	string	 "instruction decode B/W (insts/cycle)\000"
	align	 8
@LC44:
	string	 "-issue:width\000"
	align	 8
@LC45:
	string	 "instruction issue B/W (insts/cycle)\000"
	align	 8
@LC46:
	string	 "-issue:inorder\000"
	align	 8
@LC47:
	string	 "run pipeline with in-order issue\000"
	align	 8
@LC48:
	string	 "-issue:wrongpath\000"
	align	 8
@LC49:
	string	 "issue instructions down wrong execution paths\000"
	align	 8
@LC50:
	string	 "-commit:width\000"
	align	 8
@LC51:
	string	 "instruction commit B/W (insts/cycle)\000"
	align	 8
@LC52:
	string	 "-ruu:size\000"
	align	 8
@LC53:
	string	 "register update unit (RUU) size\000"
	align	 8
@LC54:
	string	 "-lsq:size\000"
	align	 8
@LC55:
	string	 "load/store queue (LSQ) size\000"
	align	 8
@LC56:
	string	 "-cache:dl1\000"
	align	 8
@LC57:
	string	 "l1 data cache config, i.e., {<config>|none}\000"
	align	 8
@LC58:
	string	 "dl1:128:32:4:l\000"
	align	 8
@LC59:
	string	 "  The cache config parameter <config> has the fol"
	string	 "lowing format:\n\n    <name>:<nsets>:<bsize>:<ass"
	string	 "oc>:<repl>\n\n    <name>   - name of the cache be"
	string	 "ing defined\n    <nsets>  - number of sets in the"
	string	 " cache\n    <bsize>  - block size of the cache\n "
	string	 "   <assoc>  - associativity of the cache\n    <re"
	string	 "pl>   - block replacement strategy, 'l'-LRU, 'f'-"
	string	 "FIFO, 'r'-random\n\n    Examples:   -cache:dl1 dl"
	string	 "1:4096:32:1:l\n                -dtlb dtlb:128:409"
	string	 "6:32:r\n\000"
	align	 8
@LC60:
	string	 "-cache:dl1lat\000"
	align	 8
@LC61:
	string	 "l1 data cache hit latency (in cycles)\000"
	align	 8
@LC62:
	string	 "-cache:dl2\000"
	align	 8
@LC63:
	string	 "l2 data cache config, i.e., {<config>|none}\000"
	align	 8
@LC64:
	string	 "ul2:1024:64:4:l\000"
	align	 8
@LC65:
	string	 "-cache:dl2lat\000"
	align	 8
@LC66:
	string	 "l2 data cache hit latency (in cycles)\000"
	align	 8
@LC67:
	string	 "-cache:il1\000"
	align	 8
@LC68:
	string	 "l1 inst cache config, i.e., {<config>|dl1|dl2|non"
	string	 "e}\000"
	align	 8
@LC69:
	string	 "il1:512:32:1:l\000"
	align	 8
@LC70:
	string	 "  Cache levels can be unified by pointing a level"
	string	 " of the instruction cache\n  hierarchy at the dat"
	string	 "a cache hiearchy using the \"dl1\" and \"dl2\" ca"
	string	 "che\n  configuration arguments.  Most sensible co"
	string	 "mbinations are supported, e.g.,\n\n    A unified "
	string	 "l2 cache (il2 is pointed at dl2):\n      -cache:i"
	string	 "l1 il1:128:64:1:l -cache:il2 dl2\n      -cache:dl"
	string	 "1 dl1:256:32:1:l -cache:dl2 ul2:1024:64:2:l\n\n  "
	string	 "  Or, a fully unified cache hierarchy (il1 pointe"
	string	 "d at dl1):\n      -cache:il1 dl1\n      -cache:dl"
	string	 "1 ul1:256:32:1:l -cache:dl2 ul2:1024:64:2:l\n\000"
	align	 8
@LC71:
	string	 "-cache:il1lat\000"
	align	 8
@LC72:
	string	 "l1 instruction cache hit latency (in cycles)\000"
	align	 8
@LC73:
	string	 "-cache:il2\000"
	align	 8
@LC74:
	string	 "l2 instruction cache config, i.e., {<config>|dl2|"
	string	 "none}\000"
	align	 8
@LC75:
	string	 "dl2\000"
	align	 8
@LC76:
	string	 "-cache:il2lat\000"
	align	 8
@LC77:
	string	 "l2 instruction cache hit latency (in cycles)\000"
	align	 8
@LC78:
	string	 "-cache:flush\000"
	align	 8
@LC79:
	string	 "flush caches on system calls\000"
	align	 8
@LC80:
	string	 "-cache:icompress\000"
	align	 8
@LC81:
	string	 "convert 64-bit inst addresses to 32-bit inst equi"
	string	 "valents\000"
	align	 8
@LC82:
	string	 "-mem:lat\000"
	align	 8
@LC83:
	string	 "memory access latency (<first_chunk> <inter_chunk"
	string	 ">)\000"
	align	 8
@LC84:
	string	 "-mem:width\000"
	align	 8
@LC85:
	string	 "memory access bus width (in bytes)\000"
	align	 8
@LC86:
	string	 "-tlb:itlb\000"
	align	 8
@LC87:
	string	 "instruction TLB config, i.e., {<config>|none}\000"
	align	 8
@LC88:
	string	 "itlb:16:4096:4:l\000"
	align	 8
@LC89:
	string	 "-tlb:dtlb\000"
	align	 8
@LC90:
	string	 "data TLB config, i.e., {<config>|none}\000"
	align	 8
@LC91:
	string	 "dtlb:32:4096:4:l\000"
	align	 8
@LC92:
	string	 "-tlb:lat\000"
	align	 8
@LC93:
	string	 "inst/data TLB miss latency (in cycles)\000"
	align	 8
@LC94:
	string	 "-res:ialu\000"
	align	 8
@LC95:
	string	 "total number of integer ALU's available\000"
	align	 8
@LC96:
	string	 "-res:imult\000"
	align	 8
@LC97:
	string	 "total number of integer multiplier/dividers avail"
	string	 "able\000"
	align	 8
@LC98:
	string	 "-res:memport\000"
	align	 8
@LC99:
	string	 "total number of memory system ports available (to"
	string	 " CPU)\000"
	align	 8
@LC100:
	string	 "-res:fpalu\000"
	align	 8
@LC101:
	string	 "total number of floating point ALU's available\000"
	align	 8
@LC102:
	string	 "-res:fpmult\000"
	align	 8
@LC103:
	string	 "total number of floating point multiplier/divider"
	string	 "s available\000"
	align	 8
@LC104:
	string	 "-pcstat\000"
	align	 8
@LC105:
	string	 "profile stat(s) against text addr's (mult uses ok"
	string	 ")\000"
	align	 8
@LC106:
	string	 "-bugcompat\000"
	align	 8
@LC107:
	string	 "operate in backward-compatible bugs mode (for tes"
	string	 "ting only)\000"
text
	align	 8
	global	 _sim_reg_options
_sim_reg_options:
	subu	 r31,r31,80
	or.u	 r3,r0,hi16(@LC23)
	st.d	 r24,r31,56
	or	 r24,r0,r2
	st	 r1,r31,64
@Ltb0:
	bsr.n	 _opt_reg_header
	or	 r3,r3,lo16(@LC23)
	st	 r0,r31,32
	or	 r2,r0,r24
	or	 r6,r0,2
	or	 r8,r0,0
	or	 r9,r0,0
	or.u	 r3,r0,hi16(@LC24)
	or.u	 r4,r0,hi16(@LC25)
	or.u	 r5,r0,hi16(_ptrace_opts)
	or.u	 r7,r0,hi16(_ptrace_nelt)
	or	 r3,r3,lo16(@LC24)
	or	 r4,r4,lo16(@LC25)
	or	 r5,r5,lo16(_ptrace_opts)
	or	 r7,r7,lo16(_ptrace_nelt)
	bsr.n	 _opt_reg_string_list
	st	 r0,r31,36
	or.u	 r3,r0,hi16(@LC26)
	or	 r2,r0,r24
	bsr.n	 _opt_reg_note
	or	 r3,r3,lo16(@LC26)
	or	 r2,r0,r24
	or	 r6,r0,0
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC27)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC28)
	or.u	 r5,r0,hi16(_max_insts)
	or	 r3,r3,lo16(@LC27)
	or	 r4,r4,lo16(@LC28)
	bsr.n	 _opt_reg_uint
	or	 r5,r5,lo16(_max_insts)
	or	 r2,r0,r24
	or	 r6,r0,0
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC29)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC30)
	or.u	 r5,r0,hi16(_fastfwd_count)
	or	 r3,r3,lo16(@LC29)
	or	 r4,r4,lo16(@LC30)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_fastfwd_count)
	or	 r2,r0,r24
	or	 r6,r0,4
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC31)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC32)
	or.u	 r5,r0,hi16(_ruu_ifq_size)
	or	 r3,r3,lo16(@LC31)
	or	 r4,r4,lo16(@LC32)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_ruu_ifq_size)
	or	 r2,r0,r24
	or	 r6,r0,3
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC33)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC34)
	or.u	 r5,r0,hi16(_ruu_branch_penalty)
	or	 r3,r3,lo16(@LC33)
	or	 r4,r4,lo16(@LC34)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_ruu_branch_penalty)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r3,r0,hi16(@LC35)
	or.u	 r4,r0,hi16(@LC36)
	or.u	 r5,r0,hi16(_pred_type)
	or.u	 r6,r0,hi16(@LC37)
	or	 r3,r3,lo16(@LC35)
	or	 r4,r4,lo16(@LC36)
	or	 r5,r5,lo16(_pred_type)
	bsr.n	 _opt_reg_string
	or	 r6,r6,lo16(@LC37)
	or	 r2,r0,r24
	or	 r6,r0,2048
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC38)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC39)
	or.u	 r5,r0,hi16(_btb_size)
	or	 r3,r3,lo16(@LC38)
	or	 r4,r4,lo16(@LC39)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_btb_size)
	st	 r0,r31,32
	or	 r2,r0,r24
	or	 r9,r0,1
	or.u	 r3,r0,hi16(@LC40)
	or.u	 r4,r0,hi16(@LC41)
	or.u	 r13,r0,hi16(_twolev_nelt)
	or.u	 r5,r0,hi16(_twolev_config)
	ld	 r6,r13,lo16(_twolev_nelt)
	or	 r3,r3,lo16(@LC40)
	or	 r4,r4,lo16(@LC41)
	or	 r5,r5,lo16(_twolev_config)
	or	 r7,r13,lo16(_twolev_nelt)
	or	 r8,r0,r5
	bsr.n	 _opt_reg_int_list
	st	 r0,r31,36
	or	 r2,r0,r24
	or	 r6,r0,4
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC42)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC43)
	or.u	 r5,r0,hi16(_ruu_decode_width)
	or	 r3,r3,lo16(@LC42)
	or	 r4,r4,lo16(@LC43)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_ruu_decode_width)
	or	 r2,r0,r24
	or	 r6,r0,4
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC44)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC45)
	or.u	 r5,r0,hi16(_ruu_issue_width)
	or	 r3,r3,lo16(@LC44)
	or	 r4,r4,lo16(@LC45)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_ruu_issue_width)
	or	 r2,r0,r24
	or	 r6,r0,0
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC46)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC47)
	or.u	 r5,r0,hi16(_ruu_inorder_issue)
	or	 r3,r3,lo16(@LC46)
	or	 r4,r4,lo16(@LC47)
	bsr.n	 _opt_reg_flag
	or	 r5,r5,lo16(_ruu_inorder_issue)
	or	 r2,r0,r24
	or	 r6,r0,1
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC48)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC49)
	or.u	 r5,r0,hi16(_ruu_include_spec)
	or	 r3,r3,lo16(@LC48)
	or	 r4,r4,lo16(@LC49)
	bsr.n	 _opt_reg_flag
	or	 r5,r5,lo16(_ruu_include_spec)
	or	 r2,r0,r24
	or	 r6,r0,4
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC50)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC51)
	or.u	 r5,r0,hi16(_ruu_commit_width)
	or	 r3,r3,lo16(@LC50)
	or	 r4,r4,lo16(@LC51)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_ruu_commit_width)
	or	 r2,r0,r24
	or	 r6,r0,16
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC52)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC53)
	or.u	 r5,r0,hi16(_RUU_size)
	or	 r3,r3,lo16(@LC52)
	or	 r4,r4,lo16(@LC53)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_RUU_size)
	or	 r2,r0,r24
	or	 r6,r0,8
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC54)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC55)
	or.u	 r5,r0,hi16(_LSQ_size)
	or	 r3,r3,lo16(@LC54)
	or	 r4,r4,lo16(@LC55)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_LSQ_size)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r3,r0,hi16(@LC56)
	or.u	 r4,r0,hi16(@LC57)
	or.u	 r5,r0,hi16(_cache_dl1_opt)
	or.u	 r6,r0,hi16(@LC58)
	or	 r3,r3,lo16(@LC56)
	or	 r4,r4,lo16(@LC57)
	or	 r5,r5,lo16(_cache_dl1_opt)
	bsr.n	 _opt_reg_string
	or	 r6,r6,lo16(@LC58)
	or.u	 r3,r0,hi16(@LC59)
	or	 r2,r0,r24
	bsr.n	 _opt_reg_note
	or	 r3,r3,lo16(@LC59)
	or	 r2,r0,r24
	or	 r6,r0,1
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC60)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC61)
	or.u	 r5,r0,hi16(_cache_dl1_lat)
	or	 r3,r3,lo16(@LC60)
	or	 r4,r4,lo16(@LC61)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_cache_dl1_lat)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r3,r0,hi16(@LC62)
	or.u	 r4,r0,hi16(@LC63)
	or.u	 r5,r0,hi16(_cache_dl2_opt)
	or.u	 r6,r0,hi16(@LC64)
	or	 r3,r3,lo16(@LC62)
	or	 r4,r4,lo16(@LC63)
	or	 r5,r5,lo16(_cache_dl2_opt)
	bsr.n	 _opt_reg_string
	or	 r6,r6,lo16(@LC64)
	or	 r2,r0,r24
	or	 r6,r0,6
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC65)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC66)
	or.u	 r5,r0,hi16(_cache_dl2_lat)
	or	 r3,r3,lo16(@LC65)
	or	 r4,r4,lo16(@LC66)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_cache_dl2_lat)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r3,r0,hi16(@LC67)
	or.u	 r4,r0,hi16(@LC68)
	or.u	 r5,r0,hi16(_cache_il1_opt)
	or.u	 r6,r0,hi16(@LC69)
	or	 r3,r3,lo16(@LC67)
	or	 r4,r4,lo16(@LC68)
	or	 r5,r5,lo16(_cache_il1_opt)
	bsr.n	 _opt_reg_string
	or	 r6,r6,lo16(@LC69)
	or.u	 r3,r0,hi16(@LC70)
	or	 r2,r0,r24
	bsr.n	 _opt_reg_note
	or	 r3,r3,lo16(@LC70)
	or	 r2,r0,r24
	or	 r6,r0,1
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC71)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC72)
	or.u	 r5,r0,hi16(_cache_il1_lat)
	or	 r3,r3,lo16(@LC71)
	or	 r4,r4,lo16(@LC72)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_cache_il1_lat)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r3,r0,hi16(@LC73)
	or.u	 r4,r0,hi16(@LC74)
	or.u	 r5,r0,hi16(_cache_il2_opt)
	or.u	 r6,r0,hi16(@LC75)
	or	 r3,r3,lo16(@LC73)
	or	 r4,r4,lo16(@LC74)
	or	 r5,r5,lo16(_cache_il2_opt)
	bsr.n	 _opt_reg_string
	or	 r6,r6,lo16(@LC75)
	or	 r2,r0,r24
	or	 r6,r0,6
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC76)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC77)
	or.u	 r5,r0,hi16(_cache_il2_lat)
	or	 r3,r3,lo16(@LC76)
	or	 r4,r4,lo16(@LC77)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_cache_il2_lat)
	or	 r2,r0,r24
	or	 r6,r0,0
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC78)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC79)
	or.u	 r5,r0,hi16(_flush_on_syscalls)
	or	 r3,r3,lo16(@LC78)
	or	 r4,r4,lo16(@LC79)
	bsr.n	 _opt_reg_flag
	or	 r5,r5,lo16(_flush_on_syscalls)
	or	 r2,r0,r24
	or	 r6,r0,0
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC80)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC81)
	or.u	 r5,r0,hi16(_compress_icache_addrs)
	or	 r3,r3,lo16(@LC80)
	or	 r4,r4,lo16(@LC81)
	bsr.n	 _opt_reg_flag
	or	 r5,r5,lo16(_compress_icache_addrs)
	st	 r0,r31,32
	or	 r2,r0,r24
	or	 r9,r0,1
	or.u	 r3,r0,hi16(@LC82)
	or.u	 r4,r0,hi16(@LC83)
	or.u	 r13,r0,hi16(_mem_nelt)
	or.u	 r5,r0,hi16(_mem_lat)
	ld	 r6,r13,lo16(_mem_nelt)
	or	 r3,r3,lo16(@LC82)
	or	 r4,r4,lo16(@LC83)
	or	 r5,r5,lo16(_mem_lat)
	or	 r7,r13,lo16(_mem_nelt)
	or	 r8,r0,r5
	bsr.n	 _opt_reg_int_list
	st	 r0,r31,36
	or	 r2,r0,r24
	or	 r6,r0,8
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC84)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC85)
	or.u	 r5,r0,hi16(_mem_bus_width)
	or	 r3,r3,lo16(@LC84)
	or	 r4,r4,lo16(@LC85)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_mem_bus_width)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r3,r0,hi16(@LC86)
	or.u	 r4,r0,hi16(@LC87)
	or.u	 r5,r0,hi16(_itlb_opt)
	or.u	 r6,r0,hi16(@LC88)
	or	 r3,r3,lo16(@LC86)
	or	 r4,r4,lo16(@LC87)
	or	 r5,r5,lo16(_itlb_opt)
	bsr.n	 _opt_reg_string
	or	 r6,r6,lo16(@LC88)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r3,r0,hi16(@LC89)
	or.u	 r4,r0,hi16(@LC90)
	or.u	 r5,r0,hi16(_dtlb_opt)
	or.u	 r6,r0,hi16(@LC91)
	or	 r3,r3,lo16(@LC89)
	or	 r4,r4,lo16(@LC90)
	or	 r5,r5,lo16(_dtlb_opt)
	bsr.n	 _opt_reg_string
	or	 r6,r6,lo16(@LC91)
	or	 r2,r0,r24
	or	 r6,r0,30
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC92)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC93)
	or.u	 r5,r0,hi16(_tlb_miss_lat)
	or	 r3,r3,lo16(@LC92)
	or	 r4,r4,lo16(@LC93)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_tlb_miss_lat)
	or	 r2,r0,r24
	or	 r7,r0,1
	or	 r8,r0,0
	or.u	 r25,r0,hi16(_fu_config+4)
	or.u	 r3,r0,hi16(@LC94)
	or.u	 r4,r0,hi16(@LC95)
	or.u	 r5,r0,hi16(_res_ialu)
	ld	 r6,r25,lo16(_fu_config+4)
	or	 r3,r3,lo16(@LC94)
	or	 r4,r4,lo16(@LC95)
	or	 r5,r5,lo16(_res_ialu)
	bsr.n	 _opt_reg_int
	or	 r25,r25,lo16(_fu_config+4)
	ld	 r6,r25,268
	or	 r2,r0,r24
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC96)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC97)
	or.u	 r5,r0,hi16(_res_imult)
	or	 r3,r3,lo16(@LC96)
	or	 r4,r4,lo16(@LC97)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_res_imult)
	ld	 r6,r25,536
	or	 r2,r0,r24
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC98)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC99)
	or.u	 r5,r0,hi16(_res_memport)
	or	 r3,r3,lo16(@LC98)
	or	 r4,r4,lo16(@LC99)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_res_memport)
	ld	 r6,r25,804
	or	 r2,r0,r24
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC100)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC101)
	or.u	 r5,r0,hi16(_res_fpalu)
	or	 r3,r3,lo16(@LC100)
	or	 r4,r4,lo16(@LC101)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_res_fpalu)
	ld	 r6,r25,1072
	or	 r2,r0,r24
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC102)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC103)
	or.u	 r5,r0,hi16(_res_fpmult)
	or	 r3,r3,lo16(@LC102)
	or	 r4,r4,lo16(@LC103)
	bsr.n	 _opt_reg_int
	or	 r5,r5,lo16(_res_fpmult)
	st	 r0,r31,32
	or	 r2,r0,r24
	or	 r6,r0,8
	or	 r8,r0,0
	or	 r9,r0,0
	or.u	 r3,r0,hi16(@LC104)
	or.u	 r4,r0,hi16(@LC105)
	or.u	 r5,r0,hi16(_pcstat_vars)
	or.u	 r7,r0,hi16(_pcstat_nelt)
	or	 r13,r0,1
	or	 r3,r3,lo16(@LC104)
	or	 r4,r4,lo16(@LC105)
	or	 r5,r5,lo16(_pcstat_vars)
	or	 r7,r7,lo16(_pcstat_nelt)
	bsr.n	 _opt_reg_string_list
	st	 r13,r31,36
	or	 r2,r0,r24
	or	 r6,r0,0
	or	 r7,r0,1
	or.u	 r3,r0,hi16(@LC106)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC107)
	or.u	 r5,r0,hi16(_bugcompat_mode)
	or	 r3,r3,lo16(@LC106)
	or	 r4,r4,lo16(@LC107)
	bsr.n	 _opt_reg_flag
	or	 r5,r5,lo16(_bugcompat_mode)
@Lte0:
	ld	 r1,r31,64
	ld.d	 r24,r31,56
	jmp.n	 r1
	addu	 r31,r31,80

data
	align	 8
@LC108:
	string	 "sim-outorder.c\000"
	align	 8
@LC109:
	string	 "sim_check_options\000"
	align	 8
@LC110:
	string	 "bad fast forward count: %d\000"
	align	 8
@LC111:
	string	 "sim-outorder.c\000"
	align	 8
@LC112:
	string	 "sim_check_options\000"
	align	 8
@LC113:
	string	 "inst fetch queue size must be positive > 0 and a "
	string	 "power of two\000"
	align	 8
@LC114:
	string	 "sim-outorder.c\000"
	align	 8
@LC115:
	string	 "sim_check_options\000"
	align	 8
@LC116:
	string	 "mis-prediction penalty must be at least 1 cycle\000"
	align	 8
@LC117:
	string	 "perfect\000"
	align	 8
@LC118:
	string	 "taken\000"
	align	 8
@LC119:
	string	 "nottaken\000"
	align	 8
@LC120:
	string	 "bimod\000"
	align	 8
@LC121:
	string	 "2lev\000"
	align	 8
@LC122:
	string	 "sim-outorder.c\000"
	align	 8
@LC123:
	string	 "sim_check_options\000"
	align	 8
@LC124:
	string	 "bad 2-level predictor config (<l1size> <l2size> <"
	string	 "hist_size>)\000"
	align	 8
@LC125:
	string	 "sim-outorder.c\000"
	align	 8
@LC126:
	string	 "sim_check_options\000"
	align	 8
@LC127:
	string	 "cannot parse predictor type `%s'\000"
	align	 8
@LC128:
	string	 "sim-outorder.c\000"
	align	 8
@LC129:
	string	 "sim_check_options\000"
	align	 8
@LC130:
	string	 "issue width must be positive non-zero and a power"
	string	 " of two\000"
	align	 8
@LC131:
	string	 "sim-outorder.c\000"
	align	 8
@LC132:
	string	 "sim_check_options\000"
	align	 8
@LC133:
	string	 "issue width must be positive non-zero and a power"
	string	 " of two\000"
	align	 8
@LC134:
	string	 "sim-outorder.c\000"
	align	 8
@LC135:
	string	 "sim_check_options\000"
	align	 8
@LC136:
	string	 "commit width must be positive non-zero\000"
	align	 8
@LC137:
	string	 "sim-outorder.c\000"
	align	 8
@LC138:
	string	 "sim_check_options\000"
	align	 8
@LC139:
	string	 "RUU size must be a positive number > 1 and a powe"
	string	 "r of two\000"
	align	 8
@LC140:
	string	 "sim-outorder.c\000"
	align	 8
@LC141:
	string	 "sim_check_options\000"
	align	 8
@LC142:
	string	 "LSQ size must be a positive number > 1 and a powe"
	string	 "r of two\000"
	align	 8
@LC143:
	string	 "none\000"
	align	 8
@LC144:
	string	 "none\000"
	align	 8
@LC145:
	string	 "sim-outorder.c\000"
	align	 8
@LC146:
	string	 "sim_check_options\000"
	align	 8
@LC147:
	string	 "the l1 data cache must defined if the l2 cache is"
	string	 " defined\000"
	align	 8
@LC148:
	string	 "%[^:]:%d:%d:%d:%c\000"
	align	 8
@LC149:
	string	 "sim-outorder.c\000"
	align	 8
@LC150:
	string	 "sim_check_options\000"
	align	 8
@LC151:
	string	 "bad l1 D-cache parms: <name>:<nsets>:<bsize>:<ass"
	string	 "oc>:<repl>\000"
	align	 8
@LC152:
	string	 "none\000"
	align	 8
@LC153:
	string	 "%[^:]:%d:%d:%d:%c\000"
	align	 8
@LC154:
	string	 "sim-outorder.c\000"
	align	 8
@LC155:
	string	 "sim_check_options\000"
	align	 8
@LC156:
	string	 "bad l2 D-cache parms: <name>:<nsets>:<bsize>:<ass"
	string	 "oc>:<repl>\000"
	align	 8
@LC157:
	string	 "none\000"
	align	 8
@LC158:
	string	 "none\000"
	align	 8
@LC159:
	string	 "sim-outorder.c\000"
	align	 8
@LC160:
	string	 "sim_check_options\000"
	align	 8
@LC161:
	string	 "the l1 inst cache must defined if the l2 cache is"
	string	 " defined\000"
	align	 8
@LC162:
	string	 "dl1\000"
	align	 8
@LC163:
	string	 "sim-outorder.c\000"
	align	 8
@LC164:
	string	 "sim_check_options\000"
	align	 8
@LC165:
	string	 "I-cache l1 cannot access D-cache l1 as it's undef"
	string	 "ined\000"
	align	 8
@LC166:
	string	 "none\000"
	align	 8
@LC167:
	string	 "sim-outorder.c\000"
	align	 8
@LC168:
	string	 "sim_check_options\000"
	align	 8
@LC169:
	string	 "the l1 inst cache must defined if the l2 cache is"
	string	 " defined\000"
	align	 8
@LC170:
	string	 "dl2\000"
	align	 8
@LC171:
	string	 "sim-outorder.c\000"
	align	 8
@LC172:
	string	 "sim_check_options\000"
	align	 8
@LC173:
	string	 "I-cache l1 cannot access D-cache l2 as it's undef"
	string	 "ined\000"
	align	 8
@LC174:
	string	 "none\000"
	align	 8
@LC175:
	string	 "sim-outorder.c\000"
	align	 8
@LC176:
	string	 "sim_check_options\000"
	align	 8
@LC177:
	string	 "the l1 inst cache must defined if the l2 cache is"
	string	 " defined\000"
	align	 8
@LC178:
	string	 "%[^:]:%d:%d:%d:%c\000"
	align	 8
@LC179:
	string	 "sim-outorder.c\000"
	align	 8
@LC180:
	string	 "sim_check_options\000"
	align	 8
@LC181:
	string	 "bad l1 I-cache parms: <name>:<nsets>:<bsize>:<ass"
	string	 "oc>:<repl>\000"
	align	 8
@LC182:
	string	 "none\000"
	align	 8
@LC183:
	string	 "dl2\000"
	align	 8
@LC184:
	string	 "sim-outorder.c\000"
	align	 8
@LC185:
	string	 "sim_check_options\000"
	align	 8
@LC186:
	string	 "I-cache l2 cannot access D-cache l2 as it's undef"
	string	 "ined\000"
	align	 8
@LC187:
	string	 "%[^:]:%d:%d:%d:%c\000"
	align	 8
@LC188:
	string	 "sim-outorder.c\000"
	align	 8
@LC189:
	string	 "sim_check_options\000"
	align	 8
@LC190:
	string	 "bad l2 I-cache parms: <name>:<nsets>:<bsize>:<ass"
	string	 "oc>:<repl>\000"
	align	 8
@LC191:
	string	 "none\000"
	align	 8
@LC192:
	string	 "%[^:]:%d:%d:%d:%c\000"
	align	 8
@LC193:
	string	 "sim-outorder.c\000"
	align	 8
@LC194:
	string	 "sim_check_options\000"
	align	 8
@LC195:
	string	 "bad TLB parms: <name>:<nsets>:<page_size>:<assoc>"
	string	 ":<repl>\000"
	align	 8
@LC196:
	string	 "none\000"
	align	 8
@LC197:
	string	 "%[^:]:%d:%d:%d:%c\000"
	align	 8
@LC198:
	string	 "sim-outorder.c\000"
	align	 8
@LC199:
	string	 "sim_check_options\000"
	align	 8
@LC200:
	string	 "bad TLB parms: <name>:<nsets>:<page_size>:<assoc>"
	string	 ":<repl>\000"
	align	 8
@LC201:
	string	 "sim-outorder.c\000"
	align	 8
@LC202:
	string	 "sim_check_options\000"
	align	 8
@LC203:
	string	 "l1 data cache latency must be greater than zero\000"
	align	 8
@LC204:
	string	 "sim-outorder.c\000"
	align	 8
@LC205:
	string	 "sim_check_options\000"
	align	 8
@LC206:
	string	 "l2 data cache latency must be greater than zero\000"
	align	 8
@LC207:
	string	 "sim-outorder.c\000"
	align	 8
@LC208:
	string	 "sim_check_options\000"
	align	 8
@LC209:
	string	 "l1 instruction cache latency must be greater than"
	string	 " zero\000"
	align	 8
@LC210:
	string	 "sim-outorder.c\000"
	align	 8
@LC211:
	string	 "sim_check_options\000"
	align	 8
@LC212:
	string	 "l2 instruction cache latency must be greater than"
	string	 " zero\000"
	align	 8
@LC213:
	string	 "sim-outorder.c\000"
	align	 8
@LC214:
	string	 "sim_check_options\000"
	align	 8
@LC215:
	string	 "bad memory access latency (<first_chunk> <inter_c"
	string	 "hunk>)\000"
	align	 8
@LC216:
	string	 "sim-outorder.c\000"
	align	 8
@LC217:
	string	 "sim_check_options\000"
	align	 8
@LC218:
	string	 "all memory access latencies must be greater than "
	string	 "zero\000"
	align	 8
@LC219:
	string	 "sim-outorder.c\000"
	align	 8
@LC220:
	string	 "sim_check_options\000"
	align	 8
@LC221:
	string	 "memory bus width must be positive non-zero and a "
	string	 "power of two\000"
	align	 8
@LC222:
	string	 "sim-outorder.c\000"
	align	 8
@LC223:
	string	 "sim_check_options\000"
	align	 8
@LC224:
	string	 "TLB miss latency must be greater than zero\000"
	align	 8
@LC225:
	string	 "sim-outorder.c\000"
	align	 8
@LC226:
	string	 "sim_check_options\000"
	align	 8
@LC227:
	string	 "number of integer ALU's must be greater than zero"
	string	 "\000"
	align	 8
@LC228:
	string	 "sim-outorder.c\000"
	align	 8
@LC229:
	string	 "sim_check_options\000"
	align	 8
@LC230:
	string	 "number of integer ALU's must be <= MAX_INSTS_PER_"
	string	 "CLASS\000"
	align	 8
@LC231:
	string	 "sim-outorder.c\000"
	align	 8
@LC232:
	string	 "sim_check_options\000"
	align	 8
@LC233:
	string	 "number of integer multiplier/dividers must be gre"
	string	 "ater than zero\000"
	align	 8
@LC234:
	string	 "sim-outorder.c\000"
	align	 8
@LC235:
	string	 "sim_check_options\000"
	align	 8
@LC236:
	string	 "number of integer mult/div's must be <= MAX_INSTS"
	string	 "_PER_CLASS\000"
	align	 8
@LC237:
	string	 "sim-outorder.c\000"
	align	 8
@LC238:
	string	 "sim_check_options\000"
	align	 8
@LC239:
	string	 "number of memory system ports must be greater tha"
	string	 "n zero\000"
	align	 8
@LC240:
	string	 "sim-outorder.c\000"
	align	 8
@LC241:
	string	 "sim_check_options\000"
	align	 8
@LC242:
	string	 "number of memory system ports must be <= MAX_INST"
	string	 "S_PER_CLASS\000"
	align	 8
@LC243:
	string	 "sim-outorder.c\000"
	align	 8
@LC244:
	string	 "sim_check_options\000"
	align	 8
@LC245:
	string	 "number of floating point ALU's must be greater th"
	string	 "an zero\000"
	align	 8
@LC246:
	string	 "sim-outorder.c\000"
	align	 8
@LC247:
	string	 "sim_check_options\000"
	align	 8
@LC248:
	string	 "number of floating point ALU's must be <= MAX_INS"
	string	 "TS_PER_CLASS\000"
	align	 8
@LC249:
	string	 "sim-outorder.c\000"
	align	 8
@LC250:
	string	 "sim_check_options\000"
	align	 8
@LC251:
	string	 "number of floating point multiplier/dividers must"
	string	 " be > zero\000"
	align	 8
@LC252:
	string	 "sim-outorder.c\000"
	align	 8
@LC253:
	string	 "sim_check_options\000"
	align	 8
@LC254:
	string	 "number of FP mult/div's must be <= MAX_INSTS_PER_"
	string	 "CLASS\000"
text
	align	 8
	global	 _sim_check_options
_sim_check_options:
	or.u	 r13,r0,hi16(_fastfwd_count)
	ld	 r6,r13,lo16(_fastfwd_count)
	subu	 r31,r31,240
	st	 r1,r31,80
	st	 r21,r31,60
	set	 r13,r0,30<1>
	st.d	 r24,r31,72
	cmp	 r13,r6,r13
	bb1.n	 ls,r13,@L149
	st.d	 r22,r31,64
@Ltb1:
	or	 r4,r0,819
	or.u	 r2,r0,hi16(@LC108)
	or.u	 r3,r0,hi16(@LC109)
	or.u	 r5,r0,hi16(@LC110)
	or	 r2,r2,lo16(@LC108)
	or	 r3,r3,lo16(@LC109)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC110)
	align	 4
@L149:
	or.u	 r13,r0,hi16(_ruu_ifq_size)
	ld	 r12,r13,lo16(_ruu_ifq_size)
	bcnd.n	 le0,r12,@L151
	subu	 r13,r12,1
	and	 r13,r12,r13
	bcnd.n	 eq0,r13,@L150
	or.u	 r13,r0,hi16(_ruu_branch_penalty)
@L151:
	or.u	 r2,r0,hi16(@LC111)
	or	 r4,r0,822
	or.u	 r3,r0,hi16(@LC112)
	or.u	 r5,r0,hi16(@LC113)
	or	 r2,r2,lo16(@LC111)
	or	 r3,r3,lo16(@LC112)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC113)
	align	 4
@L150:
	ld	 r13,r13,lo16(_ruu_branch_penalty)
	bcnd.n	 gt0,r13,@L152
	or.u	 r25,r0,hi16(_pred_type)
	or.u	 r2,r0,hi16(@LC114)
	or	 r4,r0,825
	or.u	 r3,r0,hi16(@LC115)
	or.u	 r5,r0,hi16(@LC116)
	or	 r2,r2,lo16(@LC114)
	or	 r3,r3,lo16(@LC115)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC116)
	align	 4
@L152:
	or.u	 r3,r0,hi16(@LC117)
	ld	 r2,r25,lo16(_pred_type)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC117)
	bcnd.n	 ne0,r2,@L153
	or.u	 r3,r0,hi16(@LC118)
	or.u	 r13,r0,hi16(_pred)
	or.u	 r12,r0,hi16(_pred_perfect)
	st	 r0,r13,lo16(_pred)
	or	 r13,r0,1
	br.n	 @L154
	st	 r13,r12,lo16(_pred_perfect)
	align	 4
@L153:
	ld	 r2,r25,lo16(_pred_type)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC118)
	bcnd.n	 ne0,r2,@L155
	or.u	 r3,r0,hi16(@LC119)
	or	 r2,r0,2
	or	 r3,r0,0
	or	 r4,r0,0
	br.n	 @L224
	or	 r5,r0,0
	align	 4
@L155:
	ld	 r2,r25,lo16(_pred_type)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC119)
	bcnd.n	 ne0,r2,@L157
	or.u	 r3,r0,hi16(@LC120)
	or	 r2,r0,3
	or	 r3,r0,0
	or	 r4,r0,0
	br.n	 @L224
	or	 r5,r0,0
	align	 4
@L157:
	ld	 r2,r25,lo16(_pred_type)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC120)
	bcnd.n	 ne0,r2,@L159
	or.u	 r3,r0,hi16(@LC121)
	or	 r2,r0,1
	or.u	 r13,r0,hi16(_btb_size)
	or	 r4,r0,0
	ld	 r3,r13,lo16(_btb_size)
	br.n	 @L224
	or	 r5,r0,0
	align	 4
@L159:
	ld	 r2,r25,lo16(_pred_type)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC121)
	bcnd.n	 ne0,r2,@L161
	or	 r4,r0,859
	or.u	 r13,r0,hi16(_twolev_nelt)
	ld	 r13,r13,lo16(_twolev_nelt)
	cmp	 r13,r13,3
	bb0.n	 ne,r13,@L162
	or.u	 r13,r0,hi16(_twolev_config)
	or.u	 r2,r0,hi16(@LC122)
	or	 r4,r0,852
	or.u	 r3,r0,hi16(@LC123)
	or.u	 r5,r0,hi16(@LC124)
	or	 r2,r2,lo16(@LC122)
	or	 r3,r3,lo16(@LC123)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC124)
	align	 4
@L162:
	ld	 r3,r13,lo16(_twolev_config)
	or	 r13,r13,lo16(_twolev_config)
	ld	 r4,r13,4
	ld	 r5,r13,8
	or	 r2,r0,0
@L224:
	bsr	 _bpred_create
	or.u	 r13,r0,hi16(_pred)
	br.n	 @L154
	st	 r2,r13,lo16(_pred)
	align	 4
@L161:
	ld	 r6,r25,lo16(_pred_type)
	or.u	 r2,r0,hi16(@LC125)
	or.u	 r3,r0,hi16(@LC126)
	or.u	 r5,r0,hi16(@LC127)
	or	 r2,r2,lo16(@LC125)
	or	 r3,r3,lo16(@LC126)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC127)
	align	 4
@L154:
	or.u	 r13,r0,hi16(_ruu_decode_width)
	ld	 r12,r13,lo16(_ruu_decode_width)
	bcnd.n	 le0,r12,@L165
	subu	 r13,r12,1
	and	 r13,r12,r13
	bcnd.n	 eq0,r13,@L164
	or.u	 r13,r0,hi16(_ruu_issue_width)
@L165:
	or.u	 r2,r0,hi16(@LC128)
	or	 r4,r0,862
	or.u	 r3,r0,hi16(@LC129)
	or.u	 r5,r0,hi16(@LC130)
	or	 r2,r2,lo16(@LC128)
	or	 r3,r3,lo16(@LC129)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC130)
	align	 4
@L164:
	ld	 r12,r13,lo16(_ruu_issue_width)
	bcnd.n	 le0,r12,@L167
	subu	 r13,r12,1
	and	 r13,r12,r13
	bcnd.n	 eq0,r13,@L166
	or.u	 r13,r0,hi16(_ruu_commit_width)
@L167:
	or.u	 r2,r0,hi16(@LC131)
	or	 r4,r0,865
	or.u	 r3,r0,hi16(@LC132)
	or.u	 r5,r0,hi16(@LC133)
	or	 r2,r2,lo16(@LC131)
	or	 r3,r3,lo16(@LC132)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC133)
	align	 4
@L166:
	ld	 r13,r13,lo16(_ruu_commit_width)
	bcnd.n	 gt0,r13,@L168
	or.u	 r13,r0,hi16(_RUU_size)
	or.u	 r2,r0,hi16(@LC134)
	or	 r4,r0,868
	or.u	 r3,r0,hi16(@LC135)
	or.u	 r5,r0,hi16(@LC136)
	or	 r2,r2,lo16(@LC134)
	or	 r3,r3,lo16(@LC135)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC136)
	align	 4
@L168:
	ld	 r12,r13,lo16(_RUU_size)
	cmp	 r13,r12,1
	bb0.n	 gt,r13,@L170
	subu	 r13,r12,1
	and	 r13,r12,r13
	bcnd.n	 eq0,r13,@L169
	or.u	 r13,r0,hi16(_LSQ_size)
@L170:
	or.u	 r2,r0,hi16(@LC137)
	or	 r4,r0,871
	or.u	 r3,r0,hi16(@LC138)
	or.u	 r5,r0,hi16(@LC139)
	or	 r2,r2,lo16(@LC137)
	or	 r3,r3,lo16(@LC138)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC139)
	align	 4
@L169:
	ld	 r12,r13,lo16(_LSQ_size)
	cmp	 r13,r12,1
	bb0.n	 gt,r13,@L172
	subu	 r13,r12,1
	and	 r13,r12,r13
	bcnd.n	 eq0,r13,@L171
	or.u	 r25,r0,hi16(_cache_dl1_opt)
@L172:
	or.u	 r2,r0,hi16(@LC140)
	or	 r4,r0,874
	or.u	 r3,r0,hi16(@LC141)
	or.u	 r5,r0,hi16(@LC142)
	or	 r2,r2,lo16(@LC140)
	or	 r3,r3,lo16(@LC141)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC142)
	align	 4
@L171:
	or.u	 r3,r0,hi16(@LC143)
	ld	 r2,r25,lo16(_cache_dl1_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC143)
	bcnd.n	 ne0,r2,@L173
	or.u	 r3,r0,hi16(@LC148)
	or.u	 r13,r0,hi16(_cache_dl2_opt)
	or.u	 r3,r0,hi16(@LC144)
	ld	 r2,r13,lo16(_cache_dl2_opt)
	or.u	 r13,r0,hi16(_cache_dl1)
	or	 r3,r3,lo16(@LC144)
	bsr.n	 _strcmp
	st	 r0,r13,lo16(_cache_dl1)
	bcnd.n	 eq0,r2,@L227
	or.u	 r13,r0,hi16(_cache_dl2)
	or.u	 r2,r0,hi16(@LC145)
	or	 r4,r0,883
	or.u	 r3,r0,hi16(@LC146)
	or.u	 r5,r0,hi16(@LC147)
	or	 r2,r2,lo16(@LC145)
	or	 r3,r3,lo16(@LC146)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC147)
	align	 4
@L173:
	ld	 r2,r25,lo16(_cache_dl1_opt)
	addu	 r4,r31,88
	addu	 r22,r31,216
	addu	 r21,r31,220
	addu	 r24,r31,224
	addu	 r23,r31,228
	or	 r3,r3,lo16(@LC148)
	or	 r5,r0,r22
	or	 r6,r0,r21
	or	 r7,r0,r24
	bsr.n	 _sscanf
	or	 r8,r0,r23
	cmp	 r2,r2,5
	bb0.n	 ne,r2,@L176
	or.u	 r2,r0,hi16(@LC149)
	or	 r4,r0,890
	or.u	 r3,r0,hi16(@LC150)
	or.u	 r5,r0,hi16(@LC151)
	or	 r2,r2,lo16(@LC149)
	or	 r3,r3,lo16(@LC150)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC151)
	align	 4
@L176:
	bsr.n	 _cache_char2policy
	ld.b	 r2,r31,228
	or	 r8,r0,r2
	addu	 r2,r31,88
	ld	 r3,r31,216
	or	 r5,r0,0
	ld	 r4,r31,220
	or.u	 r13,r0,hi16(_cache_dl1_lat)
	ld	 r7,r31,224
	or.u	 r9,r0,hi16(_dl1_access_fn)
	ld	 r13,r13,lo16(_cache_dl1_lat)
	or	 r6,r0,0
	or	 r9,r9,lo16(_dl1_access_fn)
	bsr.n	 _cache_create
	st	 r13,r31,32
	or.u	 r13,r0,hi16(_cache_dl1)
	or.u	 r25,r0,hi16(_cache_dl2_opt)
	st	 r2,r13,lo16(_cache_dl1)
	or.u	 r3,r0,hi16(@LC152)
	ld	 r2,r25,lo16(_cache_dl2_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC152)
	bcnd.n	 ne0,r2,@L177
	addu	 r4,r31,88
	or.u	 r13,r0,hi16(_cache_dl2)
@L227:
	br.n	 @L175
	st	 r0,r13,lo16(_cache_dl2)
	align	 4
@L177:
	ld	 r2,r25,lo16(_cache_dl2_opt)
	or	 r5,r0,r22
	or	 r6,r0,r21
	or	 r7,r0,r24
	or.u	 r3,r0,hi16(@LC153)
	or	 r8,r0,r23
	bsr.n	 _sscanf
	or	 r3,r3,lo16(@LC153)
	cmp	 r2,r2,5
	bb0.n	 ne,r2,@L179
	or.u	 r2,r0,hi16(@LC154)
	or	 r4,r0,903
	or.u	 r3,r0,hi16(@LC155)
	or.u	 r5,r0,hi16(@LC156)
	or	 r2,r2,lo16(@LC154)
	or	 r3,r3,lo16(@LC155)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC156)
	align	 4
@L179:
	bsr.n	 _cache_char2policy
	ld.b	 r2,r31,228
	or	 r8,r0,r2
	addu	 r2,r31,88
	ld	 r3,r31,216
	or	 r5,r0,0
	ld	 r4,r31,220
	or.u	 r13,r0,hi16(_cache_dl2_lat)
	ld	 r7,r31,224
	or.u	 r9,r0,hi16(_dl2_access_fn)
	ld	 r13,r13,lo16(_cache_dl2_lat)
	or	 r6,r0,0
	or	 r9,r9,lo16(_dl2_access_fn)
	bsr.n	 _cache_create
	st	 r13,r31,32
	or.u	 r13,r0,hi16(_cache_dl2)
	st	 r2,r13,lo16(_cache_dl2)
@L175:
	or.u	 r25,r0,hi16(_cache_il1_opt)
	or.u	 r3,r0,hi16(@LC157)
	ld	 r2,r25,lo16(_cache_il1_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC157)
	bcnd.n	 ne0,r2,@L180
	or.u	 r3,r0,hi16(@LC162)
	or.u	 r13,r0,hi16(_cache_il2_opt)
	or.u	 r3,r0,hi16(@LC158)
	ld	 r2,r13,lo16(_cache_il2_opt)
	or.u	 r13,r0,hi16(_cache_il1)
	or	 r3,r3,lo16(@LC158)
	bsr.n	 _strcmp
	st	 r0,r13,lo16(_cache_il1)
	bcnd.n	 eq0,r2,@L228
	or.u	 r13,r0,hi16(_cache_il2)
	or.u	 r2,r0,hi16(@LC159)
	or	 r4,r0,917
	or.u	 r3,r0,hi16(@LC160)
	or.u	 r5,r0,hi16(@LC161)
	or	 r2,r2,lo16(@LC159)
	or	 r3,r3,lo16(@LC160)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC161)
	align	 4
@L180:
	ld	 r2,r25,lo16(_cache_il1_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC162)
	bcnd.n	 ne0,r2,@L183
	or.u	 r13,r0,hi16(_cache_dl1)
	ld	 r12,r13,lo16(_cache_dl1)
	bcnd.n	 ne0,r12,@L184
	or.u	 r13,r0,hi16(_cache_il2_opt)
	or.u	 r2,r0,hi16(@LC163)
	or	 r4,r0,923
	or.u	 r3,r0,hi16(@LC164)
	or.u	 r5,r0,hi16(@LC165)
	or	 r2,r2,lo16(@LC163)
	or	 r3,r3,lo16(@LC164)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC165)
	align	 4
@L184:
	or.u	 r3,r0,hi16(@LC166)
	ld	 r2,r13,lo16(_cache_il2_opt)
	or.u	 r13,r0,hi16(_cache_il1)
	or	 r3,r3,lo16(@LC166)
	bsr.n	 _strcmp
	st	 r12,r13,lo16(_cache_il1)
	bcnd.n	 eq0,r2,@L228
	or.u	 r13,r0,hi16(_cache_il2)
	or.u	 r2,r0,hi16(@LC167)
	or	 r4,r0,928
	or.u	 r3,r0,hi16(@LC168)
	or.u	 r5,r0,hi16(@LC169)
	or	 r2,r2,lo16(@LC167)
	or	 r3,r3,lo16(@LC168)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC169)
	align	 4
@L183:
	or.u	 r3,r0,hi16(@LC170)
	ld	 r2,r25,lo16(_cache_il1_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC170)
	bcnd.n	 ne0,r2,@L187
	addu	 r4,r31,88
	or.u	 r13,r0,hi16(_cache_dl2)
	ld	 r12,r13,lo16(_cache_dl2)
	bcnd.n	 ne0,r12,@L188
	or.u	 r13,r0,hi16(_cache_il2_opt)
	or.u	 r2,r0,hi16(@LC171)
	or	 r4,r0,934
	or.u	 r3,r0,hi16(@LC172)
	or.u	 r5,r0,hi16(@LC173)
	or	 r2,r2,lo16(@LC171)
	or	 r3,r3,lo16(@LC172)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC173)
	align	 4
@L188:
	or.u	 r3,r0,hi16(@LC174)
	ld	 r2,r13,lo16(_cache_il2_opt)
	or.u	 r13,r0,hi16(_cache_il1)
	or	 r3,r3,lo16(@LC174)
	bsr.n	 _strcmp
	st	 r12,r13,lo16(_cache_il1)
	bcnd.n	 eq0,r2,@L228
	or.u	 r13,r0,hi16(_cache_il2)
	or.u	 r2,r0,hi16(@LC175)
	or	 r4,r0,939
	or.u	 r3,r0,hi16(@LC176)
	or.u	 r5,r0,hi16(@LC177)
	or	 r2,r2,lo16(@LC175)
	or	 r3,r3,lo16(@LC176)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC177)
	align	 4
@L187:
	ld	 r2,r25,lo16(_cache_il1_opt)
	or.u	 r3,r0,hi16(@LC178)
	addu	 r22,r31,216
	addu	 r21,r31,220
	addu	 r24,r31,224
	addu	 r23,r31,228
	or	 r3,r3,lo16(@LC178)
	or	 r5,r0,r22
	or	 r6,r0,r21
	or	 r7,r0,r24
	bsr.n	 _sscanf
	or	 r8,r0,r23
	cmp	 r2,r2,5
	bb0.n	 ne,r2,@L191
	or.u	 r2,r0,hi16(@LC179)
	or	 r4,r0,946
	or.u	 r3,r0,hi16(@LC180)
	or.u	 r5,r0,hi16(@LC181)
	or	 r2,r2,lo16(@LC179)
	or	 r3,r3,lo16(@LC180)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC181)
	align	 4
@L191:
	bsr.n	 _cache_char2policy
	ld.b	 r2,r31,228
	or	 r8,r0,r2
	addu	 r2,r31,88
	ld	 r3,r31,216
	or	 r5,r0,0
	ld	 r4,r31,220
	or.u	 r13,r0,hi16(_cache_il1_lat)
	ld	 r7,r31,224
	or.u	 r9,r0,hi16(_il1_access_fn)
	ld	 r13,r13,lo16(_cache_il1_lat)
	or	 r6,r0,0
	or	 r9,r9,lo16(_il1_access_fn)
	bsr.n	 _cache_create
	st	 r13,r31,32
	or.u	 r13,r0,hi16(_cache_il1)
	or.u	 r25,r0,hi16(_cache_il2_opt)
	st	 r2,r13,lo16(_cache_il1)
	or.u	 r3,r0,hi16(@LC182)
	ld	 r2,r25,lo16(_cache_il2_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC182)
	bcnd.n	 ne0,r2,@L192
	or.u	 r13,r0,hi16(_cache_il2)
@L228:
	br.n	 @L182
	st	 r0,r13,lo16(_cache_il2)
	align	 4
@L192:
	or.u	 r3,r0,hi16(@LC183)
	ld	 r2,r25,lo16(_cache_il2_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC183)
	bcnd.n	 ne0,r2,@L194
	addu	 r4,r31,88
	or.u	 r13,r0,hi16(_cache_dl2)
	ld	 r12,r13,lo16(_cache_dl2)
	bcnd.n	 ne0,r12,@L195
	or.u	 r13,r0,hi16(_cache_il2)
	or.u	 r2,r0,hi16(@LC184)
	or	 r4,r0,957
	or.u	 r3,r0,hi16(@LC185)
	or.u	 r5,r0,hi16(@LC186)
	or	 r2,r2,lo16(@LC184)
	or	 r3,r3,lo16(@LC185)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC186)
	align	 4
@L195:
	br.n	 @L182
	st	 r12,r13,lo16(_cache_il2)
	align	 4
@L194:
	ld	 r2,r25,lo16(_cache_il2_opt)
	or	 r5,r0,r22
	or	 r6,r0,r21
	or	 r7,r0,r24
	or.u	 r3,r0,hi16(@LC187)
	or	 r8,r0,r23
	bsr.n	 _sscanf
	or	 r3,r3,lo16(@LC187)
	cmp	 r2,r2,5
	bb0.n	 ne,r2,@L197
	or.u	 r2,r0,hi16(@LC188)
	or	 r4,r0,965
	or.u	 r3,r0,hi16(@LC189)
	or.u	 r5,r0,hi16(@LC190)
	or	 r2,r2,lo16(@LC188)
	or	 r3,r3,lo16(@LC189)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC190)
	align	 4
@L197:
	bsr.n	 _cache_char2policy
	ld.b	 r2,r31,228
	or	 r8,r0,r2
	addu	 r2,r31,88
	ld	 r3,r31,216
	or	 r5,r0,0
	ld	 r4,r31,220
	or.u	 r13,r0,hi16(_cache_il2_lat)
	ld	 r7,r31,224
	or.u	 r9,r0,hi16(_il2_access_fn)
	ld	 r13,r13,lo16(_cache_il2_lat)
	or	 r6,r0,0
	or	 r9,r9,lo16(_il2_access_fn)
	bsr.n	 _cache_create
	st	 r13,r31,32
	or.u	 r13,r0,hi16(_cache_il2)
	st	 r2,r13,lo16(_cache_il2)
@L182:
	or.u	 r25,r0,hi16(_itlb_opt)
	or.u	 r3,r0,hi16(@LC191)
	ld	 r2,r25,lo16(_itlb_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC191)
	bcnd.n	 ne0,r2,@L198
	addu	 r4,r31,88
	or.u	 r13,r0,hi16(_itlb)
	br.n	 @L199
	st	 r0,r13,lo16(_itlb)
	align	 4
@L198:
	ld	 r2,r25,lo16(_itlb_opt)
	addu	 r5,r31,216
	addu	 r6,r31,220
	addu	 r7,r31,224
	or.u	 r3,r0,hi16(@LC192)
	addu	 r8,r31,228
	bsr.n	 _sscanf
	or	 r3,r3,lo16(@LC192)
	cmp	 r2,r2,5
	bb0.n	 ne,r2,@L200
	or.u	 r2,r0,hi16(@LC193)
	or	 r4,r0,979
	or.u	 r3,r0,hi16(@LC194)
	or.u	 r5,r0,hi16(@LC195)
	or	 r2,r2,lo16(@LC193)
	or	 r3,r3,lo16(@LC194)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC195)
	align	 4
@L200:
	bsr.n	 _cache_char2policy
	ld.b	 r2,r31,228
	or	 r8,r0,r2
	addu	 r2,r31,88
	or	 r5,r0,0
	ld	 r3,r31,216
	or	 r6,r0,4
	ld	 r4,r31,220
	or.u	 r9,r0,hi16(_itlb_access_fn)
	ld	 r7,r31,224
	or	 r13,r0,1
	or	 r9,r9,lo16(_itlb_access_fn)
	bsr.n	 _cache_create
	st	 r13,r31,32
	or.u	 r13,r0,hi16(_itlb)
	st	 r2,r13,lo16(_itlb)
@L199:
	or.u	 r25,r0,hi16(_dtlb_opt)
	or.u	 r3,r0,hi16(@LC196)
	ld	 r2,r25,lo16(_dtlb_opt)
	bsr.n	 _mystricmp
	or	 r3,r3,lo16(@LC196)
	bcnd.n	 ne0,r2,@L201
	addu	 r4,r31,88
	or.u	 r13,r0,hi16(_dtlb)
	br.n	 @L202
	st	 r0,r13,lo16(_dtlb)
	align	 4
@L201:
	ld	 r2,r25,lo16(_dtlb_opt)
	addu	 r5,r31,216
	addu	 r6,r31,220
	addu	 r7,r31,224
	or.u	 r3,r0,hi16(@LC197)
	addu	 r8,r31,228
	bsr.n	 _sscanf
	or	 r3,r3,lo16(@LC197)
	cmp	 r2,r2,5
	bb0.n	 ne,r2,@L203
	or.u	 r2,r0,hi16(@LC198)
	or	 r4,r0,993
	or.u	 r3,r0,hi16(@LC199)
	or.u	 r5,r0,hi16(@LC200)
	or	 r2,r2,lo16(@LC198)
	or	 r3,r3,lo16(@LC199)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC200)
	align	 4
@L203:
	bsr.n	 _cache_char2policy
	ld.b	 r2,r31,228
	or	 r8,r0,r2
	addu	 r2,r31,88
	or	 r5,r0,0
	ld	 r3,r31,216
	or	 r6,r0,4
	ld	 r4,r31,220
	or.u	 r9,r0,hi16(_dtlb_access_fn)
	ld	 r7,r31,224
	or	 r13,r0,1
	or	 r9,r9,lo16(_dtlb_access_fn)
	bsr.n	 _cache_create
	st	 r13,r31,32
	or.u	 r13,r0,hi16(_dtlb)
	st	 r2,r13,lo16(_dtlb)
@L202:
	or.u	 r13,r0,hi16(_cache_dl1_lat)
	ld	 r13,r13,lo16(_cache_dl1_lat)
	bcnd.n	 gt0,r13,@L204
	or.u	 r13,r0,hi16(_cache_dl2_lat)
	or.u	 r2,r0,hi16(@LC201)
	or	 r4,r0,1001
	or.u	 r3,r0,hi16(@LC202)
	or.u	 r5,r0,hi16(@LC203)
	or	 r2,r2,lo16(@LC201)
	or	 r3,r3,lo16(@LC202)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC203)
	align	 4
@L204:
	ld	 r13,r13,lo16(_cache_dl2_lat)
	bcnd.n	 gt0,r13,@L205
	or.u	 r13,r0,hi16(_cache_il1_lat)
	or.u	 r2,r0,hi16(@LC204)
	or	 r4,r0,1004
	or.u	 r3,r0,hi16(@LC205)
	or.u	 r5,r0,hi16(@LC206)
	or	 r2,r2,lo16(@LC204)
	or	 r3,r3,lo16(@LC205)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC206)
	align	 4
@L205:
	ld	 r13,r13,lo16(_cache_il1_lat)
	bcnd.n	 gt0,r13,@L206
	or.u	 r13,r0,hi16(_cache_il2_lat)
	or.u	 r2,r0,hi16(@LC207)
	or	 r4,r0,1007
	or.u	 r3,r0,hi16(@LC208)
	or.u	 r5,r0,hi16(@LC209)
	or	 r2,r2,lo16(@LC207)
	or	 r3,r3,lo16(@LC208)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC209)
	align	 4
@L206:
	ld	 r13,r13,lo16(_cache_il2_lat)
	bcnd.n	 gt0,r13,@L207
	or.u	 r13,r0,hi16(_mem_nelt)
	or.u	 r2,r0,hi16(@LC210)
	or	 r4,r0,1010
	or.u	 r3,r0,hi16(@LC211)
	or.u	 r5,r0,hi16(@LC212)
	or	 r2,r2,lo16(@LC210)
	or	 r3,r3,lo16(@LC211)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC212)
	align	 4
@L207:
	ld	 r13,r13,lo16(_mem_nelt)
	cmp	 r13,r13,2
	bb0.n	 ne,r13,@L208
	or.u	 r2,r0,hi16(@LC213)
	or	 r4,r0,1013
	or.u	 r3,r0,hi16(@LC214)
	or.u	 r5,r0,hi16(@LC215)
	or	 r2,r2,lo16(@LC213)
	or	 r3,r3,lo16(@LC214)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC215)
	align	 4
@L208:
	or.u	 r13,r0,hi16(_mem_lat)
	ld	 r12,r13,lo16(_mem_lat)
	bcnd.n	 le0,r12,@L210
	or	 r13,r13,lo16(_mem_lat)
	ld	 r13,r13,4
	bcnd.n	 gt0,r13,@L209
	or.u	 r13,r0,hi16(_mem_bus_width)
@L210:
	or.u	 r2,r0,hi16(@LC216)
	or	 r4,r0,1016
	or.u	 r3,r0,hi16(@LC217)
	or.u	 r5,r0,hi16(@LC218)
	or	 r2,r2,lo16(@LC216)
	or	 r3,r3,lo16(@LC217)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC218)
	align	 4
@L209:
	ld	 r12,r13,lo16(_mem_bus_width)
	bcnd.n	 le0,r12,@L212
	subu	 r13,r12,1
	and	 r13,r12,r13
	bcnd.n	 eq0,r13,@L211
	or.u	 r13,r0,hi16(_tlb_miss_lat)
@L212:
	or.u	 r2,r0,hi16(@LC219)
	or	 r4,r0,1019
	or.u	 r3,r0,hi16(@LC220)
	or.u	 r5,r0,hi16(@LC221)
	or	 r2,r2,lo16(@LC219)
	or	 r3,r3,lo16(@LC220)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC221)
	align	 4
@L211:
	ld	 r13,r13,lo16(_tlb_miss_lat)
	bcnd.n	 gt0,r13,@L213
	or.u	 r13,r0,hi16(_res_ialu)
	or.u	 r2,r0,hi16(@LC222)
	or	 r4,r0,1022
	or.u	 r3,r0,hi16(@LC223)
	or.u	 r5,r0,hi16(@LC224)
	or	 r2,r2,lo16(@LC222)
	or	 r3,r3,lo16(@LC223)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC224)
	align	 4
@L213:
	ld	 r11,r13,lo16(_res_ialu)
	bcnd.n	 gt0,r11,@L214
	cmp	 r13,r11,8
	or.u	 r2,r0,hi16(@LC225)
	or	 r4,r0,1025
	or.u	 r3,r0,hi16(@LC226)
	or.u	 r5,r0,hi16(@LC227)
	or	 r2,r2,lo16(@LC225)
	or	 r3,r3,lo16(@LC226)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC227)
	align	 4
@L214:
	bb0.n	 gt,r13,@L215
	or.u	 r13,r0,hi16(_res_imult)
	or.u	 r2,r0,hi16(@LC228)
	or	 r4,r0,1027
	or.u	 r3,r0,hi16(@LC229)
	or.u	 r5,r0,hi16(@LC230)
	or	 r2,r2,lo16(@LC228)
	or	 r3,r3,lo16(@LC229)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC230)
	align	 4
@L215:
	or.u	 r12,r0,hi16(_fu_config)
	ld	 r10,r13,lo16(_res_imult)
	or	 r12,r12,lo16(_fu_config)
	bcnd.n	 gt0,r10,@L216
	st	 r11,r12,4
	or.u	 r2,r0,hi16(@LC231)
	or	 r4,r0,1031
	or.u	 r3,r0,hi16(@LC232)
	or.u	 r5,r0,hi16(@LC233)
	or	 r2,r2,lo16(@LC231)
	or	 r3,r3,lo16(@LC232)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC233)
	align	 4
@L216:
	cmp	 r13,r10,8
	bb0.n	 gt,r13,@L217
	or.u	 r13,r0,hi16(_res_memport)
	or.u	 r2,r0,hi16(@LC234)
	or	 r4,r0,1033
	or.u	 r3,r0,hi16(@LC235)
	or.u	 r5,r0,hi16(@LC236)
	or	 r2,r2,lo16(@LC234)
	or	 r3,r3,lo16(@LC235)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC236)
	align	 4
@L217:
	ld	 r11,r13,lo16(_res_memport)
	bcnd.n	 gt0,r11,@L218
	st	 r10,r12,272
	or.u	 r2,r0,hi16(@LC237)
	or	 r4,r0,1037
	or.u	 r3,r0,hi16(@LC238)
	or.u	 r5,r0,hi16(@LC239)
	or	 r2,r2,lo16(@LC237)
	or	 r3,r3,lo16(@LC238)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC239)
	align	 4
@L218:
	cmp	 r13,r11,8
	bb0.n	 gt,r13,@L219
	or.u	 r13,r0,hi16(_res_fpalu)
	or.u	 r2,r0,hi16(@LC240)
	or	 r4,r0,1039
	or.u	 r3,r0,hi16(@LC241)
	or.u	 r5,r0,hi16(@LC242)
	or	 r2,r2,lo16(@LC240)
	or	 r3,r3,lo16(@LC241)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC242)
	align	 4
@L219:
	ld	 r10,r13,lo16(_res_fpalu)
	bcnd.n	 gt0,r10,@L220
	st	 r11,r12,540
	or.u	 r2,r0,hi16(@LC243)
	or	 r4,r0,1043
	or.u	 r3,r0,hi16(@LC244)
	or.u	 r5,r0,hi16(@LC245)
	or	 r2,r2,lo16(@LC243)
	or	 r3,r3,lo16(@LC244)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC245)
	align	 4
@L220:
	cmp	 r13,r10,8
	bb0.n	 gt,r13,@L221
	or.u	 r13,r0,hi16(_res_fpmult)
	or.u	 r2,r0,hi16(@LC246)
	or	 r4,r0,1045
	or.u	 r3,r0,hi16(@LC247)
	or.u	 r5,r0,hi16(@LC248)
	or	 r2,r2,lo16(@LC246)
	or	 r3,r3,lo16(@LC247)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC248)
	align	 4
@L221:
	ld	 r11,r13,lo16(_res_fpmult)
	bcnd.n	 gt0,r11,@L222
	st	 r10,r12,808
	or.u	 r2,r0,hi16(@LC249)
	or	 r4,r0,1049
	or.u	 r3,r0,hi16(@LC250)
	or.u	 r5,r0,hi16(@LC251)
	or	 r2,r2,lo16(@LC249)
	or	 r3,r3,lo16(@LC250)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC251)
	align	 4
@L222:
	cmp	 r13,r11,8
	bb0.n	 gt,r13,@L223
	or.u	 r2,r0,hi16(@LC252)
	or	 r4,r0,1051
	or.u	 r3,r0,hi16(@LC253)
	or.u	 r5,r0,hi16(@LC254)
	or	 r2,r2,lo16(@LC252)
	or	 r3,r3,lo16(@LC253)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC254)
	align	 4
@L223:
	st	 r11,r12,1076
@Lte1:
	ld	 r1,r31,80
	ld	 r21,r31,60
	ld.d	 r24,r31,72
	ld.d	 r22,r31,64
	jmp.n	 r1
	addu	 r31,r31,240

	align	 8
	global	 _sim_aux_config
_sim_aux_config:
@Ltb2:
@Lte2:
	jmp	 r1

data
	align	 8
@LC255:
	string	 "sim_num_insn\000"
	align	 8
@LC256:
	string	 "total number of instructions committed\000"
	align	 8
@LC257:
	string	 "sim_num_refs\000"
	align	 8
@LC258:
	string	 "total number of loads and stores committed\000"
	align	 8
@LC259:
	string	 "sim_num_loads\000"
	align	 8
@LC260:
	string	 "total number of loads committed\000"
	align	 8
@LC261:
	string	 "sim_num_stores\000"
	align	 8
@LC262:
	string	 "total number of stores committed\000"
	align	 8
@LC263:
	string	 "sim_num_refs - sim_num_loads\000"
	align	 8
@LC264:
	string	 "sim_num_branches\000"
	align	 8
@LC265:
	string	 "total number of branches committed\000"
	align	 8
@LC266:
	string	 "sim_elapsed_time\000"
	align	 8
@LC267:
	string	 "total simulation time in seconds\000"
	align	 8
@LC268:
	string	 "sim_inst_rate\000"
	align	 8
@LC269:
	string	 "simulation speed (in insts/sec)\000"
	align	 8
@LC270:
	string	 "sim_num_insn / sim_elapsed_time\000"
	align	 8
@LC271:
	string	 "sim_total_insn\000"
	align	 8
@LC272:
	string	 "total number of instructions executed\000"
	align	 8
@LC273:
	string	 "sim_total_refs\000"
	align	 8
@LC274:
	string	 "total number of loads and stores executed\000"
	align	 8
@LC275:
	string	 "sim_total_loads\000"
	align	 8
@LC276:
	string	 "total number of loads executed\000"
	align	 8
@LC277:
	string	 "sim_total_stores\000"
	align	 8
@LC278:
	string	 "total number of stores executed\000"
	align	 8
@LC279:
	string	 "sim_total_refs - sim_total_loads\000"
	align	 8
@LC280:
	string	 "sim_total_branches\000"
	align	 8
@LC281:
	string	 "total number of branches executed\000"
	align	 8
@LC282:
	string	 "sim_cycle\000"
	align	 8
@LC283:
	string	 "total simulation time in cycles\000"
	align	 8
@LC284:
	string	 "sim_IPC\000"
	align	 8
@LC285:
	string	 "instructions per cycle\000"
	align	 8
@LC286:
	string	 "sim_num_insn / sim_cycle\000"
	align	 8
@LC287:
	string	 "sim_CPI\000"
	align	 8
@LC288:
	string	 "cycles per instruction\000"
	align	 8
@LC289:
	string	 "sim_cycle / sim_num_insn\000"
	align	 8
@LC290:
	string	 "sim_exec_BW\000"
	align	 8
@LC291:
	string	 "total instructions (mis-spec + committed) per cyc"
	string	 "le\000"
	align	 8
@LC292:
	string	 "sim_total_insn / sim_cycle\000"
	align	 8
@LC293:
	string	 "sim_IPB\000"
	align	 8
@LC294:
	string	 "instruction per branch\000"
	align	 8
@LC295:
	string	 "sim_num_insn / sim_num_branches\000"
	align	 8
@LC296:
	string	 "sim-outorder.c\000"
	align	 8
@LC297:
	string	 "sim_reg_stats\000"
	align	 8
@LC298:
	string	 "cannot locate any statistic named `%s'\000"
	align	 8
@LC299:
	string	 "sim-outorder.c\000"
	align	 8
@LC300:
	string	 "sim_reg_stats\000"
	align	 8
@LC301:
	string	 "`-pcstat' statistical variable `%s' is not an int"
	string	 "egral type\000"
	align	 8
@LC302:
	string	 "sim-outorder.c\000"
	align	 8
@LC303:
	string	 "sim_reg_stats\000"
	align	 8
@LC304:
	string	 "bad stat class\000"
	align	 8
@LC305:
	string	 "%s_by_pc\000"
	align	 8
@LC306:
	string	 "%s (by text address)\000"
	align	 8
@LC307:
	string	 "0x%lx %lu %.2f\000"
text
	align	 8
	global	 _sim_reg_stats
_sim_reg_stats:
	or	 r6,r0,0
	or	 r7,r0,0
	subu	 r31,r31,1104
	or	 r8,r0,0
	st	 r1,r31,64
	or.u	 r3,r0,hi16(@LC255)
	st.d	 r24,r31,56
	or.u	 r4,r0,hi16(@LC256)
	st.d	 r22,r31,48
	or.u	 r5,r0,hi16(_sim_num_insn)
	st.d	 r20,r31,40
	or	 r21,r0,r2
	or	 r3,r3,lo16(@LC255)
	or	 r4,r4,lo16(@LC256)
	or	 r5,r5,lo16(_sim_num_insn)
	bsr.n	 _stat_reg_llong
	st.d	 r18,r31,32
@Ltb3:
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC257)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC258)
	or.u	 r5,r0,hi16(_sim_num_refs)
	or	 r3,r3,lo16(@LC257)
	or	 r4,r4,lo16(@LC258)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_num_refs)
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC259)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC260)
	or.u	 r5,r0,hi16(_sim_num_loads)
	or	 r3,r3,lo16(@LC259)
	or	 r4,r4,lo16(@LC260)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_num_loads)
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC261)
	or	 r6,r0,0
	or.u	 r4,r0,hi16(@LC262)
	or.u	 r5,r0,hi16(@LC263)
	or	 r3,r3,lo16(@LC261)
	or	 r4,r4,lo16(@LC262)
	bsr.n	 _stat_reg_formula
	or	 r5,r5,lo16(@LC263)
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC264)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC265)
	or.u	 r5,r0,hi16(_sim_num_branches)
	or	 r3,r3,lo16(@LC264)
	or	 r4,r4,lo16(@LC265)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_num_branches)
	or	 r2,r0,r21
	or	 r6,r0,0
	or.u	 r3,r0,hi16(@LC266)
	or	 r7,r0,0
	or.u	 r4,r0,hi16(@LC267)
	or.u	 r5,r0,hi16(_sim_elapsed_time)
	or	 r3,r3,lo16(@LC266)
	or	 r4,r4,lo16(@LC267)
	bsr.n	 _stat_reg_int
	or	 r5,r5,lo16(_sim_elapsed_time)
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC268)
	or	 r6,r0,0
	or.u	 r4,r0,hi16(@LC269)
	or.u	 r5,r0,hi16(@LC270)
	or	 r3,r3,lo16(@LC268)
	or	 r4,r4,lo16(@LC269)
	bsr.n	 _stat_reg_formula
	or	 r5,r5,lo16(@LC270)
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC271)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC272)
	or.u	 r5,r0,hi16(_sim_total_insn)
	or	 r3,r3,lo16(@LC271)
	or	 r4,r4,lo16(@LC272)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_total_insn)
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC273)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC274)
	or.u	 r5,r0,hi16(_sim_total_refs)
	or	 r3,r3,lo16(@LC273)
	or	 r4,r4,lo16(@LC274)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_total_refs)
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC275)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC276)
	or.u	 r5,r0,hi16(_sim_total_loads)
	or	 r3,r3,lo16(@LC275)
	or	 r4,r4,lo16(@LC276)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_total_loads)
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC277)
	or	 r6,r0,0
	or.u	 r4,r0,hi16(@LC278)
	or.u	 r5,r0,hi16(@LC279)
	or	 r3,r3,lo16(@LC277)
	or	 r4,r4,lo16(@LC278)
	bsr.n	 _stat_reg_formula
	or	 r5,r5,lo16(@LC279)
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC280)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC281)
	or.u	 r5,r0,hi16(_sim_total_branches)
	or	 r3,r3,lo16(@LC280)
	or	 r4,r4,lo16(@LC281)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_total_branches)
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC282)
	or	 r8,r0,0
	or.u	 r4,r0,hi16(@LC283)
	or.u	 r5,r0,hi16(_sim_cycle)
	or	 r3,r3,lo16(@LC282)
	or	 r4,r4,lo16(@LC283)
	bsr.n	 _stat_reg_llong
	or	 r5,r5,lo16(_sim_cycle)
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC284)
	or	 r6,r0,0
	or.u	 r4,r0,hi16(@LC285)
	or.u	 r5,r0,hi16(@LC286)
	or	 r3,r3,lo16(@LC284)
	or	 r4,r4,lo16(@LC285)
	bsr.n	 _stat_reg_formula
	or	 r5,r5,lo16(@LC286)
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC287)
	or	 r6,r0,0
	or.u	 r4,r0,hi16(@LC288)
	or.u	 r5,r0,hi16(@LC289)
	or	 r3,r3,lo16(@LC287)
	or	 r4,r4,lo16(@LC288)
	bsr.n	 _stat_reg_formula
	or	 r5,r5,lo16(@LC289)
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC290)
	or	 r6,r0,0
	or.u	 r4,r0,hi16(@LC291)
	or.u	 r5,r0,hi16(@LC292)
	or	 r3,r3,lo16(@LC290)
	or	 r4,r4,lo16(@LC291)
	bsr.n	 _stat_reg_formula
	or	 r5,r5,lo16(@LC292)
	or	 r2,r0,r21
	or.u	 r3,r0,hi16(@LC293)
	or	 r6,r0,0
	or.u	 r4,r0,hi16(@LC294)
	or.u	 r5,r0,hi16(@LC295)
	or	 r3,r3,lo16(@LC293)
	or	 r4,r4,lo16(@LC294)
	bsr.n	 _stat_reg_formula
	or	 r5,r5,lo16(@LC295)
	or.u	 r13,r0,hi16(_pred)
	ld	 r2,r13,lo16(_pred)
	bcnd.n	 eq0,r2,@L273
	or.u	 r13,r0,hi16(_cache_il1)
	bsr.n	 _bpred_reg_stats
	or	 r3,r0,r21
	or.u	 r13,r0,hi16(_cache_il1)
@L273:
	ld	 r2,r13,lo16(_cache_il1)
	bcnd.n	 eq0,r2,@L254
	or.u	 r13,r0,hi16(_cache_dl1)
	ld	 r13,r13,lo16(_cache_dl1)
	cmp	 r13,r2,r13
	bb0.n	 ne,r13,@L254
	or.u	 r13,r0,hi16(_cache_dl2)
	ld	 r13,r13,lo16(_cache_dl2)
	cmp	 r13,r2,r13
	bb0.n	 ne,r13,@L274
	or.u	 r13,r0,hi16(_cache_il2)
	bsr.n	 _cache_reg_stats
	or	 r3,r0,r21
@L254:
	or.u	 r13,r0,hi16(_cache_il2)
@L274:
	ld	 r2,r13,lo16(_cache_il2)
	bcnd.n	 eq0,r2,@L255
	or.u	 r13,r0,hi16(_cache_dl1)
	ld	 r13,r13,lo16(_cache_dl1)
	cmp	 r13,r2,r13
	bb0.n	 ne,r13,@L255
	or.u	 r13,r0,hi16(_cache_dl2)
	ld	 r13,r13,lo16(_cache_dl2)
	cmp	 r13,r2,r13
	bb0.n	 ne,r13,@L275
	or.u	 r13,r0,hi16(_cache_dl1)
	bsr.n	 _cache_reg_stats
	or	 r3,r0,r21
@L255:
	or.u	 r13,r0,hi16(_cache_dl1)
@L275:
	ld	 r2,r13,lo16(_cache_dl1)
	bcnd.n	 eq0,r2,@L276
	or.u	 r13,r0,hi16(_cache_dl2)
	bsr.n	 _cache_reg_stats
	or	 r3,r0,r21
	or.u	 r13,r0,hi16(_cache_dl2)
@L276:
	ld	 r2,r13,lo16(_cache_dl2)
	bcnd.n	 eq0,r2,@L277
	or.u	 r13,r0,hi16(_itlb)
	bsr.n	 _cache_reg_stats
	or	 r3,r0,r21
	or.u	 r13,r0,hi16(_itlb)
@L277:
	ld	 r2,r13,lo16(_itlb)
	bcnd.n	 eq0,r2,@L278
	or.u	 r13,r0,hi16(_dtlb)
	bsr.n	 _cache_reg_stats
	or	 r3,r0,r21
	or.u	 r13,r0,hi16(_dtlb)
@L278:
	ld	 r2,r13,lo16(_dtlb)
	bcnd.n	 eq0,r2,@L279
	or.u	 r13,r0,hi16(_pcstat_nelt)
	bsr.n	 _cache_reg_stats
	or	 r3,r0,r21
	or.u	 r13,r0,hi16(_pcstat_nelt)
@L279:
	ld	 r13,r13,lo16(_pcstat_nelt)
	bcnd.n	 le0,r13,@L261
	or	 r24,r0,0
	addu	 r20,r31,584
	or.u	 r13,r0,hi16(_pcstat_vars)
	or.u	 r12,r0,hi16(_pcstat_lastvals)
	or	 r19,r13,lo16(_pcstat_vars)
	or	 r18,r12,lo16(_pcstat_lastvals)
@L263:
	ld	 r3,r19[r24]
	bsr.n	 _stat_find_stat
	or	 r2,r0,r21
	or	 r25,r0,r2
	bcnd	 ne0,r25,@L264
	ld	 r6,r19[r24]
	or.u	 r2,r0,hi16(@LC296)
	or	 r4,r0,1154
	or.u	 r3,r0,hi16(@LC297)
	or.u	 r5,r0,hi16(@LC298)
	or	 r2,r2,lo16(@LC296)
	or	 r3,r3,lo16(@LC297)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC298)
	align	 4
@L264:
	ld	 r12,r25,16
	cmp	 r13,r12,1
	bb1.n	 ls,r13,@L265
	cmp	 r13,r12,2
	bb0.n	 ne,r13,@L280
	or.u	 r13,r0,hi16(_pcstat_stats)
	ld	 r6,r25,4
	or.u	 r2,r0,hi16(@LC299)
	or	 r4,r0,1159
	or.u	 r3,r0,hi16(@LC300)
	or.u	 r5,r0,hi16(@LC301)
	or	 r2,r2,lo16(@LC299)
	or	 r3,r3,lo16(@LC300)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC301)
	align	 4
@L265:
	or.u	 r13,r0,hi16(_pcstat_stats)
@L280:
	or	 r13,r13,lo16(_pcstat_stats)
	st	 r25,r13[r24]
	ld	 r12,r25,16
	bcnd.n	 ne0,r12,@L266
	cmp	 r13,r12,1
	ld	 r13,r25,24
	ld	 r23,r0,r13
	br.n	 @L267
	ext	 r22,r23,0<31>
	align	 4
@L266:
	bb1.n	 ne,r13,@L268
	cmp	 r13,r12,2
	ld	 r13,r25,24
	ld	 r13,r0,r13
	or	 r23,r0,r13
	br.n	 @L267
	or	 r22,r0,0
	align	 4
@L268:
	bb1.n	 ne,r13,@L270
	or.u	 r2,r0,hi16(@LC302)
	ld	 r13,r25,24
	ld.d	 r22,r0,r13
	br.n	 @L281
	st.d	 r22,r18[r24]
	align	 4
@L270:
	or	 r4,r0,1163
	or.u	 r3,r0,hi16(@LC303)
	or.u	 r5,r0,hi16(@LC304)
	or	 r2,r2,lo16(@LC302)
	or	 r3,r3,lo16(@LC303)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC304)
	align	 4
@L267:
	st.d	 r22,r18[r24]
@L281:
	ld	 r4,r25,4
	or.u	 r3,r0,hi16(@LC305)
	addu	 r2,r31,72
	bsr.n	 _sprintf
	or	 r3,r3,lo16(@LC305)
	ld	 r4,r25,8
	or.u	 r3,r0,hi16(@LC306)
	or	 r2,r0,r20
	bsr.n	 _sprintf
	or	 r3,r3,lo16(@LC306)
	or	 r2,r0,r21
	addu	 r3,r31,72
	or	 r4,r0,r20
	or	 r5,r0,0
	or	 r6,r0,3
	or.u	 r7,r0,hi16(@LC307)
	or	 r8,r0,0
	bsr.n	 _stat_reg_sdist
	or	 r7,r7,lo16(@LC307)
	or.u	 r13,r0,hi16(_pcstat_sdists)
	or	 r13,r13,lo16(_pcstat_sdists)
	st	 r2,r13[r24]
	or.u	 r13,r0,hi16(_pcstat_nelt)
	ld	 r13,r13,lo16(_pcstat_nelt)
	addu	 r24,r24,1
	cmp	 r13,r24,r13
	bb1	 lt,r13,@L263
@L261:
@Lte3:
	ld	 r1,r31,64
	ld.d	 r24,r31,56
	ld.d	 r22,r31,48
	ld.d	 r20,r31,40
	ld.d	 r18,r31,32
	jmp.n	 r1
	addu	 r31,r31,1104

data
	align	 8
@LC308:
	string	 "sim-outorder.c\000"
	align	 8
@LC309:
	string	 "sim_init\000"
	align	 8
@LC310:
	string	 "bad pipetrace args, use: <fname|stdout|stderr> <r"
	string	 "ange>\000"
	align	 8
@LC311:
	string	 "sim-outorder.c\000"
	align	 8
@LC312:
	string	 "sim_init\000"
	align	 8
@LC313:
	string	 "cannot do fast decoding, too many opcodes\000"
	align	 8
@LC314:
	string	 "fu-pool\000"
text
	align	 8
	global	 _sim_init
_sim_init:
	or	 r10,r0,0
	or	 r11,r0,0
	or.u	 r13,r0,hi16(_ptrace_nelt)
	ld	 r9,r13,lo16(_ptrace_nelt)
	subu	 r31,r31,48
	or.u	 r13,r0,hi16(_sim_num_insn)
	st	 r1,r31,36
@Ltb4:
	st.d	 r10,r13,lo16(_sim_num_insn)
	or.u	 r13,r0,hi16(_sim_num_refs)
	cmp	 r12,r9,2
	bb1.n	 ne,r12,@L294
	st.d	 r10,r13,lo16(_sim_num_refs)
	or.u	 r13,r0,hi16(_ptrace_opts)
	ld	 r2,r13,lo16(_ptrace_opts)
	or	 r13,r13,lo16(_ptrace_opts)
	ld	 r3,r13,4
	bsr.n	 _ptrace_open
	addu	 r1,r1,@L315
@L316:
	align	 4
@L294:
	bcnd.n	 eq0,r9,@L313
	or.u	 r13,r0,hi16(_ld_text_base)
	or.u	 r2,r0,hi16(@LC308)
	or	 r4,r0,1226
	or.u	 r3,r0,hi16(@LC309)
	or.u	 r5,r0,hi16(@LC310)
	or	 r2,r2,lo16(@LC308)
	or	 r3,r3,lo16(@LC309)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC310)
	align	 4
@L298:
	or.u	 r13,r0,hi16(_ld_text_base)
@L313:
	ld	 r8,r13,lo16(_ld_text_base)
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r12,r8,r13
	cmp	 r11,r8,r12
	bb0.n	 lo,r11,@L300
	or.u	 r13,r0,hi16(_mem_table)
	or	 r6,r0,r12
	or	 r7,r13,lo16(_mem_table)
	or.u	 r13,r0,hi16(_ss_mask2op)
	subu	 r12,r6,r8
	or	 r5,r13,lo16(_ss_mask2op)
	bb1.n	 ge,r11,@L306
	mask	 r12,r12,15
	bcnd.n	 eq0,r12,@L314
	extu	 r9,r8,15<16>
	cmp	 r13,r12,9
	bb1	 ge,r13,@L302
@L306:
	extu	 r10,r8,15<16>
	ld	 r13,r7[r10]
	mask	 r11,r8,65535
	addu	 r13,r13,r11
	ld	 r12,r0,r13
	ld	 r9,r13,4
	st	 r12,r31,40
	st	 r9,r31,44
	mask	 r13,r12,255
	ld	 r13,r5[r13]
	and	 r12,r12,0xff00
	or	 r12,r12,r13
	st	 r12,r31,40
	ld	 r13,r7[r10]
	addu	 r13,r13,r11
	addu	 r8,r8,8
	st	 r12,r0,r13
	cmp	 r12,r8,r6
	bb0.n	 lo,r12,@L300
	st	 r9,r13,4
@L302:
	extu	 r9,r8,15<16>
@L314:
	ld	 r13,r7[r9]
	mask	 r11,r8,65535
	addu	 r13,r13,r11
	ld	 r12,r0,r13
	ld	 r10,r13,4
	st	 r12,r31,40
	st	 r10,r31,44
	mask	 r13,r12,255
	ld	 r13,r5[r13]
	and	 r12,r12,0xff00
	or	 r12,r12,r13
	st	 r12,r31,40
	ld	 r13,r7[r9]
	addu	 r13,r13,r11
	addu	 r11,r8,8
	st	 r10,r13,4
	extu	 r10,r11,15<16>
	st	 r12,r0,r13
	ld	 r13,r7[r10]
	mask	 r11,r11,65535
	addu	 r13,r13,r11
	ld	 r12,r0,r13
	ld	 r9,r13,4
	st	 r12,r31,40
	st	 r9,r31,44
	mask	 r13,r12,255
	ld	 r13,r5[r13]
	and	 r12,r12,0xff00
	or	 r12,r12,r13
	st	 r12,r31,40
	ld	 r13,r7[r10]
	addu	 r13,r13,r11
	addu	 r8,r8,16
	st	 r12,r0,r13
	cmp	 r12,r8,r6
	bb1.n	 lo,r12,@L302
	st	 r9,r13,4
@L300:
	or	 r4,r0,5
	or.u	 r2,r0,hi16(@LC314)
	or.u	 r3,r0,hi16(_fu_config)
	or	 r2,r2,lo16(@LC314)
	bsr.n	 _res_create_pool
	or	 r3,r3,lo16(_fu_config)
	or.u	 r13,r0,hi16(_fu_pool)
	st	 r2,r13,lo16(_fu_pool)
	bsr.n	 _rslink_init
	or	 r2,r0,2048
	bsr	 _tracer_init
	bsr	 _fetch_init
	bsr	 _cv_init
	bsr	 _eventq_init
	bsr	 _readyq_init
	bsr	 _ruu_init
	bsr	 _lsq_init
	or.u	 r2,r0,hi16(_simoo_reg_obj)
	or.u	 r3,r0,hi16(_simoo_mem_obj)
	or.u	 r4,r0,hi16(_simoo_mstate_obj)
	or	 r2,r2,lo16(_simoo_reg_obj)
	or	 r3,r3,lo16(_simoo_mem_obj)
	bsr.n	 _dlite_init
	or	 r4,r4,lo16(_simoo_mstate_obj)
@Lte4:
	ld	 r1,r31,36
	jmp.n	 r1
	addu	 r31,r31,48
	def	 @L315,@L298-@L316

	align	 8
	global	 _sim_aux_stats
_sim_aux_stats:
@Ltb5:
@Lte5:
	jmp	 r1

	align	 8
	global	 _sim_uninit
_sim_uninit:
	or.u	 r13,r0,hi16(_ptrace_nelt)
	ld	 r13,r13,lo16(_ptrace_nelt)
	subu	 r31,r31,48
	bcnd.n	 le0,r13,@L322
	st	 r1,r31,36
@Ltb6:
	bsr	 _ptrace_close
@L322:
@Lte6:
	ld	 r1,r31,36
	jmp.n	 r1
	addu	 r31,r31,48

data
	align	 8
@LC315:
	string	 "sim-outorder.c\000"
	align	 8
@LC316:
	string	 "ruu_init\000"
	align	 8
@LC317:
	string	 "out of virtual memory\000"
text
	align	 8
_ruu_init:
	or.u	 r13,r0,hi16(_RUU_size)
	ld	 r2,r13,lo16(_RUU_size)
	subu	 r31,r31,48
	st	 r1,r31,36
@Ltb7:
	bsr.n	 _calloc
	or	 r3,r0,96
	or.u	 r13,r0,hi16(_RUU)
	bcnd.n	 ne0,r2,@L326
	st	 r2,r13,lo16(_RUU)
	or.u	 r2,r0,hi16(@LC315)
	or.u	 r3,r0,hi16(@LC316)
	or.u	 r5,r0,hi16(@LC317)
	or	 r4,r0,1362
	or	 r2,r2,lo16(@LC315)
	or	 r3,r3,lo16(@LC316)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC317)
	align	 4
@L326:
@Lte7:
	ld	 r1,r31,36
	or.u	 r13,r0,hi16(_RUU_num)
	st	 r0,r13,lo16(_RUU_num)
	or.u	 r13,r0,hi16(_RUU_tail)
	st	 r0,r13,lo16(_RUU_tail)
	or.u	 r13,r0,hi16(_RUU_head)
	addu	 r31,r31,48
	jmp.n	 r1
	st	 r0,r13,lo16(_RUU_head)

data
	align	 8
@LC318:
	string	 "idx: %2d: opcode: %s, inst: `\000"
	align	 8
@LC319:
	string	 "       opcode: %s, inst: `\000"
	align	 8
@LC320:
	string	 "'\n\000"
	align	 8
@LC321:
	string	 "         PC: 0x%08x, NPC: 0x%08x (pred_PC: 0x%08x"
	string	 ")\n\000"
	align	 8
@LC322:
	string	 "         in_LSQ: %s, ea_comp: %s, recover_inst: %"
	string	 "s\n\000"
	align	 8
@LC323:
	string	 "t\000"
	align	 8
@LC324:
	string	 "f\000"
	align	 8
@LC325:
	string	 "t\000"
	align	 8
@LC326:
	string	 "f\000"
	align	 8
@LC327:
	string	 "t\000"
	align	 8
@LC328:
	string	 "f\000"
	align	 8
@LC329:
	string	 "         spec_mode: %s, addr: 0x%08x, tag: 0x%08x"
	string	 "\n\000"
	align	 8
@LC330:
	string	 "t\000"
	align	 8
@LC331:
	string	 "f\000"
	align	 8
@LC332:
	string	 "         seq: 0x%08x, ptrace_seq: 0x%08x\n\000"
	align	 8
@LC333:
	string	 "         queued: %s, issued: %s, completed: %s\n\000"
	align	 8
@LC334:
	string	 "t\000"
	align	 8
@LC335:
	string	 "f\000"
	align	 8
@LC336:
	string	 "t\000"
	align	 8
@LC337:
	string	 "f\000"
	align	 8
@LC338:
	string	 "t\000"
	align	 8
@LC339:
	string	 "f\000"
	align	 8
@LC340:
	string	 "         operands ready: %s\n\000"
	align	 8
@LC341:
	string	 "t\000"
	align	 8
@LC342:
	string	 "f\000"
	align	 8
@LC343:
	string	 "** RUU state **\n\000"
	align	 8
@LC344:
	string	 "RUU_head: %d, RUU_tail: %d\n\000"
	align	 8
@LC345:
	string	 "RUU_num: %d\n\000"
	align	 8
@LC346:
	string	 "sim-outorder.c\000"
	align	 8
@LC347:
	string	 "lsq_init\000"
	align	 8
@LC348:
	string	 "out of virtual memory\000"
text
	align	 8
_lsq_init:
	or.u	 r13,r0,hi16(_LSQ_size)
	ld	 r2,r13,lo16(_LSQ_size)
	subu	 r31,r31,48
	st	 r1,r31,36
@Ltb8:
	bsr.n	 _calloc
	or	 r3,r0,96
	or.u	 r13,r0,hi16(_LSQ)
	bcnd.n	 ne0,r2,@L378
	st	 r2,r13,lo16(_LSQ)
	or.u	 r2,r0,hi16(@LC346)
	or.u	 r3,r0,hi16(@LC347)
	or.u	 r5,r0,hi16(@LC348)
	or	 r4,r0,1472
	or	 r2,r2,lo16(@LC346)
	or	 r3,r3,lo16(@LC347)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC348)
	align	 4
@L378:
@Lte8:
	ld	 r1,r31,36
	or.u	 r13,r0,hi16(_LSQ_num)
	st	 r0,r13,lo16(_LSQ_num)
	or.u	 r13,r0,hi16(_LSQ_tail)
	st	 r0,r13,lo16(_LSQ_tail)
	or.u	 r13,r0,hi16(_LSQ_head)
	addu	 r31,r31,48
	jmp.n	 r1
	st	 r0,r13,lo16(_LSQ_head)

data
	align	 8
@LC349:
	string	 "** LSQ state **\n\000"
	align	 8
@LC350:
	string	 "LSQ_head: %d, LSQ_tail: %d\n\000"
	align	 8
@LC351:
	string	 "LSQ_num: %d\n\000"
	align	 8
_RSLINK_NULL:
	word	 0
	word	 0
	word	 0
	zero	 12
	align	 8
@LC352:
	string	 "sim-outorder.c\000"
	align	 8
@LC353:
	string	 "rslink_init\000"
	align	 8
@LC354:
	string	 "out of virtual memory\000"
text
	align	 8
_rslink_init:
	subu	 r31,r31,80
	st	 r1,r31,64
	st	 r21,r31,44
	or.u	 r13,r0,hi16(_rslink_free_list)
	st.d	 r24,r31,56
	or	 r21,r0,r2
	st.d	 r22,r31,48
@Ltb9:
	or	 r24,r0,0
	bcnd.n	 le0,r21,@L417
	st	 r0,r13,lo16(_rslink_free_list)
	or.u	 r23,r0,hi16(@LC352)
	or.u	 r22,r0,hi16(@LC353)
	mask	 r12,r21,3
	bcnd.n	 eq0,r12,@L419
	or	 r25,r0,r13
	cmp	 r13,r12,1
	bb0.n	 gt,r13,@L424
	cmp	 r13,r12,2
	bb0.n	 gt,r13,@L425
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,24
	bcnd.n	 ne0,r2,@L428
	or	 r24,r0,1
	or	 r2,r23,lo16(@LC352)
	or	 r3,r22,lo16(@LC353)
	or.u	 r5,r0,hi16(@LC354)
	or	 r4,r0,1587
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC354)
	align	 4
@L428:
	ld	 r13,r25,lo16(_rslink_free_list)
	st	 r13,r0,r2
	st	 r2,r25,lo16(_rslink_free_list)
@L425:
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,24
	bcnd	 ne0,r2,@L431
	or	 r2,r23,lo16(@LC352)
	or	 r3,r22,lo16(@LC353)
	or.u	 r5,r0,hi16(@LC354)
	or	 r4,r0,1587
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC354)
	align	 4
@L431:
	ld	 r13,r25,lo16(_rslink_free_list)
	st	 r13,r0,r2
	addu	 r24,r24,1
	st	 r2,r25,lo16(_rslink_free_list)
@L424:
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,24
	bcnd	 ne0,r2,@L434
	or	 r2,r23,lo16(@LC352)
	or	 r3,r22,lo16(@LC353)
	or.u	 r5,r0,hi16(@LC354)
	or	 r4,r0,1587
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC354)
	align	 4
@L434:
	ld	 r13,r25,lo16(_rslink_free_list)
	addu	 r24,r24,1
	st	 r13,r0,r2
	cmp	 r13,r24,r21
	bb0.n	 lt,r13,@L417
	st	 r2,r25,lo16(_rslink_free_list)
@L419:
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,24
	or	 r12,r0,r2
	bcnd.n	 ne0,r12,@L437
	or	 r2,r0,1
	or	 r2,r23,lo16(@LC352)
	or	 r3,r22,lo16(@LC353)
	or.u	 r5,r0,hi16(@LC354)
	or	 r4,r0,1587
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC354)
	align	 4
@L437:
	ld	 r13,r25,lo16(_rslink_free_list)
	st	 r13,r0,r12
	or	 r3,r0,24
	bsr.n	 _calloc
	st	 r12,r25,lo16(_rslink_free_list)
	or	 r12,r0,r2
	bcnd.n	 ne0,r12,@L440
	or	 r2,r0,1
	or	 r2,r23,lo16(@LC352)
	or	 r3,r22,lo16(@LC353)
	or.u	 r5,r0,hi16(@LC354)
	or	 r4,r0,1587
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC354)
	align	 4
@L440:
	ld	 r13,r25,lo16(_rslink_free_list)
	st	 r13,r0,r12
	or	 r3,r0,24
	bsr.n	 _calloc
	st	 r12,r25,lo16(_rslink_free_list)
	or	 r12,r0,r2
	bcnd.n	 ne0,r12,@L443
	or	 r2,r0,1
	or	 r2,r23,lo16(@LC352)
	or	 r3,r22,lo16(@LC353)
	or.u	 r5,r0,hi16(@LC354)
	or	 r4,r0,1587
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC354)
	align	 4
@L443:
	ld	 r13,r25,lo16(_rslink_free_list)
	st	 r13,r0,r12
	or	 r3,r0,24
	bsr.n	 _calloc
	st	 r12,r25,lo16(_rslink_free_list)
	bcnd	 ne0,r2,@L446
	or	 r2,r23,lo16(@LC352)
	or	 r3,r22,lo16(@LC353)
	or.u	 r5,r0,hi16(@LC354)
	or	 r4,r0,1587
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC354)
	align	 4
@L446:
	ld	 r13,r25,lo16(_rslink_free_list)
	addu	 r24,r24,4
	st	 r13,r0,r2
	cmp	 r13,r24,r21
	bb1.n	 lt,r13,@L419
	st	 r2,r25,lo16(_rslink_free_list)
@L417:
@Lte9:
	ld	 r1,r31,64
	ld	 r21,r31,44
	ld.d	 r24,r31,56
	ld.d	 r22,r31,48
	jmp.n	 r1
	addu	 r31,r31,80

	align	 8
_eventq_init:
@Ltb10:
	or.u	 r9,r0,hi16(_event_queue)
	jmp.n	 r1
	st	 r0,r9,lo16(_event_queue)
@Lte10:

data
	align	 8
@LC355:
	string	 "** event queue state **\n\000"
	align	 8
@LC356:
	string	 "idx: %2d: @ %.0f\n\000"
	align	 8
@LC357:
	string	 "sim-outorder.c\000"
	align	 8
@LC358:
	string	 "eventq_queue_event\000"
	align	 8
@LC359:
	string	 "event completed\000"
	align	 8
@LC360:
	string	 "sim-outorder.c\000"
	align	 8
@LC361:
	string	 "eventq_queue_event\000"
	align	 8
@LC362:
	string	 "event occurred in the past\000"
	align	 8
@LC363:
	string	 "sim-outorder.c\000"
	align	 8
@LC364:
	string	 "eventq_queue_event\000"
	align	 8
@LC365:
	string	 "out of rs links\000"
text
	align	 8
_readyq_init:
@Ltb11:
	or.u	 r9,r0,hi16(_ready_queue)
	jmp.n	 r1
	st	 r0,r9,lo16(_ready_queue)
@Lte11:

data
	align	 8
@LC366:
	string	 "** ready queue state **\n\000"
	align	 8
@LC367:
	string	 "sim-outorder.c\000"
	align	 8
@LC368:
	string	 "readyq_enqueue\000"
	align	 8
@LC369:
	string	 "node is already queued\000"
	align	 8
@LC370:
	string	 "sim-outorder.c\000"
	align	 8
@LC371:
	string	 "readyq_enqueue\000"
	align	 8
@LC372:
	string	 "out of rs links\000"
	align	 4
_CVLINK_NULL:
	word	 0
	word	 0
text
	align	 8
_cv_init:
@Ltb12:
	or	 r10,r0,0
	or	 r11,r0,0
	or.u	 r5,r0,hi16(_CVLINK_NULL)
	or	 r12,r0,0
	or.u	 r9,r0,hi16(_create_vector_rt)
	or.u	 r8,r0,hi16(_spec_create_vector_rt)
	or	 r4,r5,lo16(_CVLINK_NULL)
	or	 r2,r9,lo16(_create_vector_rt)
	or	 r3,r8,lo16(_spec_create_vector_rt)
	or.u	 r9,r0,hi16(_spec_create_vector)
	or.u	 r8,r0,hi16(_create_vector)
	or	 r13,r9,lo16(_spec_create_vector)
	or	 r6,r8,lo16(_create_vector)
@L566:
	ld	 r9,r5,lo16(_CVLINK_NULL)
	ld	 r8,r4,4
	st	 r9,r0,r6
	st	 r8,r6,4
	st.d	 r10,r2[r12]
	ld	 r9,r5,lo16(_CVLINK_NULL)
	ld	 r8,r4,4
	st	 r9,r0,r13
	st	 r8,r13,4
	st.d	 r10,r3[r12]
	ld	 r9,r5,lo16(_CVLINK_NULL)
	ld	 r8,r4,4
	st	 r9,r6,8
	addu	 r7,r12,1
	st	 r8,r6,12
	st.d	 r10,r2[r7]
	ld	 r9,r5,lo16(_CVLINK_NULL)
	ld	 r8,r4,4
	addu	 r6,r6,16
	st	 r9,r13,8
	addu	 r12,r12,2
	st	 r8,r13,12
	addu	 r13,r13,16
	cmp	 r9,r12,69
	bb1.n	 le,r9,@L566
	st.d	 r10,r3[r7]
	or.u	 r8,r0,hi16(_use_spec_cv)
	or	 r9,r8,lo16(_use_spec_cv)
	st	 r0,r9,8
	st	 r0,r9,4
	jmp.n	 r1
	st	 r0,r8,lo16(_use_spec_cv)
@Lte12:

data
	align	 4
_dep_names:
	word	 @LC373
	word	 @LC374
	word	 @LC375
	word	 @LC376
	word	 @LC377
	word	 @LC378
	word	 @LC379
	word	 @LC380
	word	 @LC381
	word	 @LC382
	word	 @LC383
	word	 @LC384
	word	 @LC385
	word	 @LC386
	word	 @LC387
	word	 @LC388
	word	 @LC389
	word	 @LC390
	word	 @LC391
	word	 @LC392
	word	 @LC393
	word	 @LC394
	word	 @LC395
	word	 @LC396
	word	 @LC397
	word	 @LC398
	word	 @LC399
	word	 @LC400
	word	 @LC401
	word	 @LC402
	word	 @LC403
	word	 @LC404
	word	 @LC405
	word	 @LC406
	word	 @LC407
	word	 @LC408
	word	 @LC409
	word	 @LC410
	word	 @LC411
	word	 @LC412
	word	 @LC413
	word	 @LC414
	word	 @LC415
	word	 @LC416
	word	 @LC417
	word	 @LC418
	word	 @LC419
	word	 @LC420
	word	 @LC421
	word	 @LC422
	word	 @LC423
	word	 @LC424
	word	 @LC425
	word	 @LC426
	word	 @LC427
	word	 @LC428
	word	 @LC429
	word	 @LC430
	word	 @LC431
	word	 @LC432
	word	 @LC433
	word	 @LC434
	word	 @LC435
	word	 @LC436
	word	 @LC437
	word	 @LC438
	word	 @LC439
	word	 @LC440
	word	 @LC441
	word	 @LC442
	align	 8
@LC442:
	string	 "n/a\000"
	align	 8
@LC441:
	string	 "n/a\000"
	align	 8
@LC440:
	string	 "$tmp\000"
	align	 8
@LC439:
	string	 "$fcc\000"
	align	 8
@LC438:
	string	 "$lo\000"
	align	 8
@LC437:
	string	 "$hi\000"
	align	 8
@LC436:
	string	 "$f31\000"
	align	 8
@LC435:
	string	 "$f30\000"
	align	 8
@LC434:
	string	 "$f29\000"
	align	 8
@LC433:
	string	 "$f28\000"
	align	 8
@LC432:
	string	 "$f27\000"
	align	 8
@LC431:
	string	 "$f26\000"
	align	 8
@LC430:
	string	 "$f25\000"
	align	 8
@LC429:
	string	 "$f24\000"
	align	 8
@LC428:
	string	 "$f23\000"
	align	 8
@LC427:
	string	 "$f22\000"
	align	 8
@LC426:
	string	 "$f21\000"
	align	 8
@LC425:
	string	 "$f20\000"
	align	 8
@LC424:
	string	 "$f19\000"
	align	 8
@LC423:
	string	 "$f18\000"
	align	 8
@LC422:
	string	 "$f17\000"
	align	 8
@LC421:
	string	 "$f16\000"
	align	 8
@LC420:
	string	 "$f15\000"
	align	 8
@LC419:
	string	 "$f14\000"
	align	 8
@LC418:
	string	 "$f13\000"
	align	 8
@LC417:
	string	 "$f12\000"
	align	 8
@LC416:
	string	 "$f11\000"
	align	 8
@LC415:
	string	 "$f10\000"
	align	 8
@LC414:
	string	 "$f9\000"
	align	 8
@LC413:
	string	 "$f8\000"
	align	 8
@LC412:
	string	 "$f7\000"
	align	 8
@LC411:
	string	 "$f6\000"
	align	 8
@LC410:
	string	 "$f5\000"
	align	 8
@LC409:
	string	 "$f4\000"
	align	 8
@LC408:
	string	 "$f3\000"
	align	 8
@LC407:
	string	 "$f2\000"
	align	 8
@LC406:
	string	 "$f1\000"
	align	 8
@LC405:
	string	 "$f0\000"
	align	 8
@LC404:
	string	 "$r31\000"
	align	 8
@LC403:
	string	 "$r30\000"
	align	 8
@LC402:
	string	 "$r29\000"
	align	 8
@LC401:
	string	 "$r28\000"
	align	 8
@LC400:
	string	 "$r27\000"
	align	 8
@LC399:
	string	 "$r26\000"
	align	 8
@LC398:
	string	 "$r25\000"
	align	 8
@LC397:
	string	 "$r24\000"
	align	 8
@LC396:
	string	 "$r23\000"
	align	 8
@LC395:
	string	 "$r22\000"
	align	 8
@LC394:
	string	 "$r21\000"
	align	 8
@LC393:
	string	 "$r20\000"
	align	 8
@LC392:
	string	 "$r19\000"
	align	 8
@LC391:
	string	 "$r18\000"
	align	 8
@LC390:
	string	 "$r17\000"
	align	 8
@LC389:
	string	 "$r16\000"
	align	 8
@LC388:
	string	 "$r15\000"
	align	 8
@LC387:
	string	 "$r14\000"
	align	 8
@LC386:
	string	 "$r13\000"
	align	 8
@LC385:
	string	 "$r12\000"
	align	 8
@LC384:
	string	 "$r11\000"
	align	 8
@LC383:
	string	 "$r10\000"
	align	 8
@LC382:
	string	 "$r9\000"
	align	 8
@LC381:
	string	 "$r8\000"
	align	 8
@LC380:
	string	 "$r7\000"
	align	 8
@LC379:
	string	 "$r6\000"
	align	 8
@LC378:
	string	 "$r5\000"
	align	 8
@LC377:
	string	 "$r4\000"
	align	 8
@LC376:
	string	 "$r3\000"
	align	 8
@LC375:
	string	 "$r2\000"
	align	 8
@LC374:
	string	 "$r1\000"
	align	 8
@LC373:
	string	 "n/a\000"
	align	 8
@LC443:
	string	 "** create vector state **\n\000"
	align	 8
@LC444:
	string	 "[%4s]: from architected reg file\n\000"
	align	 8
@LC445:
	string	 "[%4s]: from %s, idx: %d\n\000"
	align	 8
@LC446:
	string	 "LSQ\000"
	align	 8
@LC447:
	string	 "RUU\000"
	align	 8
@LC448:
	string	 "sim-outorder.c\000"
	align	 8
@LC449:
	string	 "ruu_commit\000"
	align	 8
@LC450:
	string	 "RUU out of sync with LSQ\000"
	align	 8
@LC451:
	string	 "sim-outorder.c\000"
	align	 8
@LC452:
	string	 "ruu_commit\000"
	align	 8
@LC453:
	string	 "functional unit already in use\000"
	align	 8
@LC454:
	string	 "CT\000"
	align	 8
@LC455:
	string	 "CT\000"
	align	 8
@LC456:
	string	 "sim-outorder.c\000"
	align	 8
@LC457:
	string	 "ruu_recover\000"
	align	 8
@LC458:
	string	 "empty RUU\000"
	align	 8
@LC459:
	string	 "sim-outorder.c\000"
	align	 8
@LC460:
	string	 "ruu_recover\000"
	align	 8
@LC461:
	string	 "RUU head and tail broken\000"
	align	 8
@LC462:
	string	 "sim-outorder.c\000"
	align	 8
@LC463:
	string	 "ruu_recover\000"
	align	 8
@LC464:
	string	 "RUU and LSQ out of sync\000"
	align	 8
@LC465:
	string	 "sim-outorder.c\000"
	align	 8
@LC466:
	string	 "ruu_writeback\000"
	align	 8
@LC467:
	string	 "inst completed and !ready, !issued, or completed\000"
	align	 8
@LC468:
	string	 "sim-outorder.c\000"
	align	 8
@LC469:
	string	 "ruu_writeback\000"
	align	 8
@LC470:
	string	 "mis-predicted load or store?!?!?\000"
	align	 8
@LC471:
	string	 "WB\000"
	align	 8
@LC472:
	string	 "sim-outorder.c\000"
	align	 8
@LC473:
	string	 "ruu_writeback\000"
	align	 8
@LC474:
	string	 "output dependence already satisfied\000"
	align	 8
@LC475:
	string	 "sim-outorder.c\000"
	align	 8
@LC476:
	string	 "lsq_refresh\000"
	align	 8
@LC477:
	string	 "STD unknown array overflow, increase MAX_STD_UNKN"
	string	 "OWNS\000"
	align	 8
@LC478:
	string	 "sim-outorder.c\000"
	align	 8
@LC479:
	string	 "ruu_issue\000"
	align	 8
@LC480:
	string	 "issued inst !ready, issued, or completed\000"
	align	 8
@LC481:
	string	 "sim-outorder.c\000"
	align	 8
@LC482:
	string	 "ruu_issue\000"
	align	 8
@LC483:
	string	 "store creates result\000"
	align	 8
@LC484:
	string	 "sim-outorder.c\000"
	align	 8
@LC485:
	string	 "ruu_issue\000"
	align	 8
@LC486:
	string	 "mis-predicted store\000"
	align	 8
@LC487:
	string	 "WB\000"
	align	 8
@LC488:
	string	 "sim-outorder.c\000"
	align	 8
@LC489:
	string	 "ruu_issue\000"
	align	 8
@LC490:
	string	 "functional unit already in use\000"
	align	 8
@LC491:
	string	 "EX\000"
	align	 8
@LC492:
	string	 "EX\000"
	align	 8
@LC493:
	string	 "EX\000"
	align	 8
@LC494:
	string	 "** speculative register contents **\n\000"
	align	 8
@LC495:
	string	 "spec_mode: %s\n\000"
	align	 8
@LC496:
	string	 "t\000"
	align	 8
@LC497:
	string	 "f\000"
	align	 8
@LC498:
	string	 "[%4s]: %12d/0x%08x\n\000"
	align	 8
@LC499:
	string	 "[%4s]: %12d/0x%08x/%f ([%4s] as double: %f)\n\n\000"
	align	 8
@LC500:
	string	 "[ $hi]: %12d/0x%08x\n\000"
	align	 8
@LC501:
	string	 "[ $lo]: %12d/0x%08x\n\000"
	align	 8
@LC502:
	string	 "[$fcc]: 0x%08x\n\000"
	align	 4
_bucket_free_list:
	word	 0
	align	 8
@LC503:
	string	 "sim-outorder.c\000"
	align	 8
@LC504:
	string	 "tracer_recover\000"
	align	 8
@LC505:
	string	 "cannot recover unless in speculative mode\000"
text
	align	 8
_tracer_recover:
	or.u	 r12,r0,hi16(_spec_mode)
	ld	 r13,r12,lo16(_spec_mode)
	subu	 r31,r31,64
	st	 r1,r31,48
	st	 r23,r31,36
	bcnd.n	 ne0,r13,@L1017
	st.d	 r24,r31,40
@Ltb13:
	or.u	 r2,r0,hi16(@LC503)
	or.u	 r3,r0,hi16(@LC504)
	or.u	 r5,r0,hi16(@LC505)
	or	 r4,r0,2724
	or	 r2,r2,lo16(@LC503)
	or	 r3,r3,lo16(@LC504)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC505)
	align	 4
@L1017:
	or.u	 r13,r0,hi16(_use_spec_R)
	st	 r0,r12,lo16(_spec_mode)
	st	 r0,r13,lo16(_use_spec_R)
	or.u	 r13,r0,hi16(_use_spec_F)
	st	 r0,r13,lo16(_use_spec_F)
	or.u	 r13,r0,hi16(_use_spec_HI)
	or	 r10,r0,0
	or.u	 r8,r0,hi16(_bucket_free_list)
	st	 r0,r13,lo16(_use_spec_HI)
	or.u	 r13,r0,hi16(_use_spec_LO)
	or.u	 r12,r0,hi16(_use_spec_FCC)
	st	 r0,r13,lo16(_use_spec_LO)
	or.u	 r13,r0,hi16(_store_htable)
	st	 r0,r12,lo16(_use_spec_FCC)
	or	 r7,r13,lo16(_store_htable)
@L1031:
	ld	 r11,r7[r10]
	bcnd.n	 eq0,r11,@L1062
	addu	 r9,r10,1
@L1048:
	ld	 r12,r0,r11
	ld	 r13,r8,lo16(_bucket_free_list)
	st	 r13,r0,r11
	st	 r11,r8,lo16(_bucket_free_list)
	or	 r11,r0,r12
	bcnd.n	 ne0,r11,@L1048
	addu	 r9,r10,1
@L1062:
	st	 r0,r7[r10]
	ld	 r11,r7[r9]
	bcnd	 eq0,r11,@L1051
@L1052:
	ld	 r12,r0,r11
	ld	 r13,r8,lo16(_bucket_free_list)
	st	 r13,r0,r11
	st	 r11,r8,lo16(_bucket_free_list)
	or	 r11,r0,r12
	bcnd	 ne0,r11,@L1052
@L1051:
	st	 r0,r7[r9]
	addu	 r9,r10,2
	ld	 r11,r7[r9]
	bcnd	 eq0,r11,@L1055
@L1056:
	ld	 r12,r0,r11
	ld	 r13,r8,lo16(_bucket_free_list)
	st	 r13,r0,r11
	st	 r11,r8,lo16(_bucket_free_list)
	or	 r11,r0,r12
	bcnd	 ne0,r11,@L1056
@L1055:
	st	 r0,r7[r9]
	addu	 r9,r10,3
	ld	 r11,r7[r9]
	bcnd	 eq0,r11,@L1059
@L1060:
	ld	 r12,r0,r11
	ld	 r13,r8,lo16(_bucket_free_list)
	st	 r13,r0,r11
	st	 r11,r8,lo16(_bucket_free_list)
	or	 r11,r0,r12
	bcnd	 ne0,r11,@L1060
@L1059:
	addu	 r10,r10,4
	cmp	 r13,r10,31
	bb1.n	 le,r13,@L1031
	st	 r0,r7[r9]
	or.u	 r11,r0,hi16(_ptrace_active)
	ld	 r13,r11,lo16(_ptrace_active)
	bcnd.n	 eq0,r13,@L1038
	or.u	 r12,r0,hi16(_fetch_num)
	ld	 r13,r12,lo16(_fetch_num)
	bcnd.n	 eq0,r13,@L1038
	or	 r23,r0,r11
	or.u	 r25,r0,hi16(_fetch_head)
	or	 r24,r0,r12
@L1041:
	ld	 r13,r23,lo16(_ptrace_active)
	bcnd.n	 eq0,r13,@L1042
	or.u	 r13,r0,hi16(_fetch_data)
	ld	 r12,r25,lo16(_fetch_head)
	mul	 r12,r12,24
	ld	 r13,r13,lo16(_fetch_data)
	addu	 r13,r13,r12
	bsr.n	 ___ptrace_endinst
	ld	 r2,r13,20
@L1042:
	or.u	 r12,r0,hi16(_ruu_ifq_size)
	ld	 r13,r25,lo16(_fetch_head)
	ld	 r11,r12,lo16(_ruu_ifq_size)
	ld	 r12,r24,lo16(_fetch_num)
	addu	 r13,r13,1
	subu	 r11,r11,1
	subu	 r12,r12,1
	st	 r12,r24,lo16(_fetch_num)
	and	 r13,r13,r11
	bcnd.n	 ne0,r12,@L1041
	st	 r13,r25,lo16(_fetch_head)
@L1038:
@Lte13:
	ld	 r1,r31,48
	ld	 r23,r31,36
	ld.d	 r24,r31,40
	or.u	 r13,r0,hi16(_fetch_num)
	st	 r0,r13,lo16(_fetch_num)
	or.u	 r13,r0,hi16(_fetch_head)
	st	 r0,r13,lo16(_fetch_head)
	or.u	 r13,r0,hi16(_fetch_tail)
	st	 r0,r13,lo16(_fetch_tail)
	or.u	 r13,r0,hi16(_recover_PC)
	ld	 r12,r13,lo16(_recover_PC)
	or.u	 r13,r0,hi16(_fetch_regs_PC)
	st	 r12,r13,lo16(_fetch_regs_PC)
	or.u	 r13,r0,hi16(_fetch_pred_PC)
	addu	 r31,r31,64
	jmp.n	 r1
	st	 r12,r13,lo16(_fetch_pred_PC)

	align	 8
_tracer_init:
@Ltb14:
	or.u	 r9,r0,hi16(_spec_mode)
	st	 r0,r9,lo16(_spec_mode)
	or.u	 r9,r0,hi16(_use_spec_R)
	st	 r0,r9,lo16(_use_spec_R)
	or.u	 r9,r0,hi16(_use_spec_F)
	st	 r0,r9,lo16(_use_spec_F)
	or.u	 r9,r0,hi16(_use_spec_HI)
	or	 r7,r0,31
	st	 r0,r9,lo16(_use_spec_HI)
	or.u	 r9,r0,hi16(_use_spec_LO)
	or.u	 r8,r0,hi16(_use_spec_FCC)
	st	 r0,r9,lo16(_use_spec_LO)
	or.u	 r9,r0,hi16(_store_htable)
	st	 r0,r8,lo16(_use_spec_FCC)
	or	 r6,r9,lo16(_store_htable)
@L1093:
	subu	 r9,r7,1
	st	 r0,r6[r7]
	st	 r0,r6[r9]
	subu	 r9,r7,2
	st	 r0,r6[r9]
	subu	 r9,r7,3
	st	 r0,r6[r9]
	subu	 r9,r7,4
	st	 r0,r6[r9]
	subu	 r9,r7,5
	st	 r0,r6[r9]
	subu	 r9,r7,6
	st	 r0,r6[r9]
	subu	 r9,r7,7
	st	 r0,r6[r9]
	subu	 r9,r7,8
	st	 r0,r6[r9]
	subu	 r9,r7,9
	st	 r0,r6[r9]
	subu	 r9,r7,10
	st	 r0,r6[r9]
	subu	 r9,r7,11
	st	 r0,r6[r9]
	subu	 r9,r7,12
	st	 r0,r6[r9]
	subu	 r9,r7,13
	st	 r0,r6[r9]
	subu	 r9,r7,14
	st	 r0,r6[r9]
	subu	 r9,r7,15
	subu	 r7,r7,16
	bcnd.n	 ge0,r7,@L1093
	st	 r0,r6[r9]
	jmp	 r1
@Lte14:

data
	align	 8
@LC506:
	string	 "sim-outorder.c\000"
	align	 8
@LC507:
	string	 "spec_mem_access\000"
	align	 8
@LC508:
	string	 "out of virtual memory\000"
	align	 8
@LC509:
	string	 "sim-outorder.c\000"
	align	 8
@LC510:
	string	 "spec_mem_access\000"
	align	 8
@LC511:
	string	 "access size not supported in mis-speculative mode"
	string	 "\000"
	align	 8
@LC512:
	string	 "** speculative memory contents **\n\000"
	align	 8
@LC513:
	string	 "spec_mode: %s\n\000"
	align	 8
@LC514:
	string	 "t\000"
	align	 8
@LC515:
	string	 "f\000"
	align	 8
@LC516:
	string	 "[0x%08x]: %12.0f/0x%08x:%08x\n\000"
	align	 8
@LC517:
	string	 "sim-outorder.c\000"
	align	 8
@LC518:
	string	 "simoo_mem_obj\000"
	align	 8
@LC519:
	string	 "bogus access type\000"
text
	align	 8
_simoo_mem_obj:
	subu	 r31,r31,80
	st.d	 r24,r31,56
	or	 r24,r0,r3
	st.d	 r22,r31,48
	or	 r22,r0,r4
	st.d	 r20,r31,40
	or	 r21,r0,r5
	bcnd.n	 ne0,r2,@L1271
	st	 r1,r31,64
@Ltb15:
	br.n	 @L1272
	or	 r23,r0,0
	align	 4
@L1271:
	cmp	 r13,r2,1
	bb1.n	 ne,r13,@L1273
	or.u	 r2,r0,hi16(@LC517)
	br.n	 @L1272
	or	 r23,r0,1
	align	 4
@L1273:
	or	 r4,r0,3012
	or.u	 r3,r0,hi16(@LC518)
	or.u	 r5,r0,hi16(@LC519)
	or	 r2,r2,lo16(@LC517)
	or	 r3,r3,lo16(@LC518)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC519)
	align	 4
@L1272:
	or	 r2,r0,r23
	or	 r3,r0,r24
	or	 r4,r0,r21
	bsr.n	 _mem_valid
	or	 r5,r0,0
	bcnd.n	 ne0,r2,@L1339
	or.u	 r13,r0,hi16(_spec_mode)
	ld	 r13,r13,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1276
	subu	 r12,r21,1
	and	 r13,r21,r12
	bcnd.n	 ne0,r13,@L1338
	and	 r13,r24,r12
	bcnd.n	 ne0,r13,@L1338
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L1281
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	subu	 r12,r0,r23
	cmp	 r13,r24,r13
	subu	 r12,r0,r12
	extu	 r13,r13,1<hs>
	or	 r13,r13,r12
	bcnd.n	 eq0,r13,@L1345
	extu	 r13,r24,0<24>
@L1281:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb0.n	 hs,r13,@L1338
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L1338
	extu	 r13,r24,0<24>
@L1345:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L1284
	or	 r11,r0,0
@L1286:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L1285
	bcnd	 eq0,r11,@L1284
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L1284
	st	 r10,r12[r25]
	align	 4
@L1285:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L1286
@L1284:
	cmp	 r12,r10,0
	subu	 r13,r0,r23
	extu	 r12,r12,1<eq>
	subu	 r13,r0,r13
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1290
	or.u	 r20,r0,hi16(_bucket_free_list)
	ld	 r13,r20,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L1346
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L1291
	st	 r2,r20,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L1291:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L1346:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L1290
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r13,r0,hi16(_store_htable)
	or	 r13,r13,lo16(_store_htable)
	ld	 r12,r13[r25]
	st	 r12,r0,r10
	st	 r10,r13[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L1290:
	cmp	 r13,r21,2
	bb0	 ne,r13,@L1304
	bb1.n	 gt,r13,@L1337
	cmp	 r13,r21,4
	cmp	 r13,r21,1
	bb0.n	 ne,r13,@L1295
	or.u	 r2,r0,hi16(@LC509)
	br	 @L1347
	align	 4
@L1337:
	bb0.n	 ne,r13,@L1313
	cmp	 r13,r21,8
	bb0.n	 ne,r13,@L1322
	or.u	 r2,r0,hi16(@LC509)
	br	 @L1347
	align	 4
@L1295:
	bcnd	 ne0,r23,@L1296
	bcnd	 eq0,r10,@L1297
	ld.bu	 r13,r10,8
@L1341:
	br.n	 @L1338
	st.b	 r13,r0,r22
	align	 4
@L1297:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1299
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1302
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1302:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L1338
	st.b	 r13,r0,r22
	align	 4
@L1299:
	br.n	 @L1341
	or	 r13,r0,0
	align	 4
@L1296:
	ld.bu	 r13,r0,r22
	br.n	 @L1338
	st.b	 r13,r10,8
	align	 4
@L1304:
	bcnd	 ne0,r23,@L1305
	bcnd	 eq0,r10,@L1306
	ld.hu	 r13,r10,8
@L1342:
	br.n	 @L1338
	st.h	 r13,r0,r22
	align	 4
@L1306:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1308
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1311
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1311:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L1338
	st.h	 r13,r0,r22
	align	 4
@L1308:
	br.n	 @L1342
	or	 r13,r0,0
	align	 4
@L1305:
	ld.hu	 r13,r0,r22
	br.n	 @L1338
	st.h	 r13,r10,8
	align	 4
@L1313:
	bcnd	 ne0,r23,@L1314
	bcnd	 eq0,r10,@L1315
	ld	 r13,r10,8
@L1343:
	br.n	 @L1338
	st	 r13,r0,r22
	align	 4
@L1315:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1317
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1320
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1320:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L1338
	st	 r13,r0,r22
	align	 4
@L1317:
	br.n	 @L1343
	or	 r13,r0,0
	align	 4
@L1314:
	ld	 r13,r0,r22
	br.n	 @L1338
	st	 r13,r10,8
	align	 4
@L1322:
	bcnd	 ne0,r23,@L1323
	bcnd	 eq0,r10,@L1324
	ld	 r13,r10,8
	st	 r13,r0,r22
	ld	 r13,r10,12
@L1344:
	br.n	 @L1338
	st	 r13,r22,4
	align	 4
@L1324:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1326
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1329
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1329:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L1348
	st	 r13,r0,r22
	align	 4
@L1326:
	or	 r13,r0,0
	st	 r13,r0,r22
@L1348:
	addu	 r24,r24,4
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1330
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1333
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1333:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L1338
	st	 r13,r22,4
	align	 4
@L1330:
	br.n	 @L1344
	or	 r13,r0,0
	align	 4
@L1323:
	ld	 r13,r0,r22
	st	 r13,r10,8
	ld	 r13,r22,4
	br.n	 @L1338
	st	 r13,r10,12
	align	 4
@L1347:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L1276:
	or	 r2,r0,r23
	or	 r3,r0,r24
	or	 r4,r0,r22
	bsr.n	 _mem_access
	or	 r5,r0,r21
@L1338:
	or	 r2,r0,0
@L1339:
@Lte15:
	ld	 r1,r31,64
	ld.d	 r24,r31,56
	ld.d	 r22,r31,48
	ld.d	 r20,r31,40
	jmp.n	 r1
	addu	 r31,r31,80

data
	align	 8
@LC520:
	string	 "sim-outorder.c\000"
	align	 8
@LC521:
	string	 "ruu_link_idep\000"
	align	 8
@LC522:
	string	 "out of rs links\000"
	align	 8
@LC523:
	string	 "register number out of range\000"
	align	 8
@LC524:
	string	 "register number out of range\000"
	align	 8
@LC525:
	string	 "sim-outorder.c\000"
	align	 8
@LC526:
	string	 "simoo_reg_obj\000"
	align	 8
@LC527:
	string	 "bogus register bank\000"
	align	 8
@LC528:
	string	 "sim-outorder.c\000"
	align	 8
@LC529:
	string	 "simoo_reg_obj\000"
	align	 8
@LC530:
	string	 "bogus access type\000"
text
	align	 8
_simoo_reg_obj:
	subu	 r31,r31,48
	cmp	 r13,r4,31
	bb1.n	 ls,r13,@L1648
	st	 r1,r31,36
@Ltb16:
	or.u	 r2,r0,hi16(@LC523)
	br.n	 @L1723
	or	 r2,r2,lo16(@LC523)
	align	 4
@L1648:
	cmp	 r13,r2,1
	bb0.n	 ls,r13,@L1649
	or.u	 r13,r0,hi16(@L1721)
	cmp	 r12,r3,7
	bb0.n	 ls,r12,@L1720
	or	 r13,r13,lo16(@L1721)
	ld	 r13,r13[r3]
	jmp	 r13
	align	 4
@L1721:
	word	 @L1651
	word	 @L1662
	word	 @L1673
	word	 @L1684
	word	 @L1696
	word	 @L1703
	word	 @L1710
	word	 @L1717
	align	 4
@L1651:
	bcnd.n	 ne0,r2,@L1652
	or.u	 r13,r0,hi16(_spec_mode)
	bcnd.n	 ge0,r4,@L1655
	or	 r10,r0,r4
	addu	 r10,r4,31
@L1655:
	ext	 r10,r10,0<5>
	or.u	 r12,r0,hi16(_use_spec_R)
	or	 r13,r0,1
	mak	 r11,r10,0<5>
	or	 r12,r12,lo16(_use_spec_R)
	subu	 r11,r4,r11
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L1653
	or.u	 r13,r0,hi16(_spec_regs_R)
	br.n	 @L1725
	or	 r13,r13,lo16(_spec_regs_R)
	align	 4
@L1653:
	or.u	 r13,r0,hi16(_regs_R)
	br.n	 @L1725
	or	 r13,r13,lo16(_regs_R)
	align	 4
@L1652:
	ld	 r13,r13,lo16(_spec_mode)
	bcnd	 eq0,r13,@L1658
	bcnd.n	 ge0,r4,@L1660
	or	 r9,r0,r4
	addu	 r9,r4,31
@L1660:
	ext	 r9,r9,0<5>
	or.u	 r10,r0,hi16(_use_spec_R)
	or	 r13,r0,1
	mak	 r11,r9,0<5>
	or	 r10,r10,lo16(_use_spec_R)
	subu	 r11,r4,r11
	ld	 r12,r10[r9]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	st	 r12,r10[r9]
	or.u	 r13,r0,hi16(_spec_regs_R)
	ld	 r12,r0,r5
	or	 r13,r13,lo16(_spec_regs_R)
	br.n	 @L1722
	st	 r12,r13[r4]
	align	 4
@L1658:
	or.u	 r13,r0,hi16(_regs_R)
	ld	 r12,r0,r5
	or	 r13,r13,lo16(_regs_R)
	br.n	 @L1722
	st	 r12,r13[r4]
	align	 4
@L1662:
	bcnd.n	 ne0,r2,@L1663
	or.u	 r13,r0,hi16(_spec_mode)
	and	 r13,r4,0xfffe
	bcnd.n	 ge0,r13,@L1666
	or	 r10,r0,r13
	addu	 r10,r13,31
@L1666:
	ext	 r10,r10,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mak	 r11,r10,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	subu	 r11,r13,r11
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1664
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L1725
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L1664:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L1725:
	ld	 r4,r13[r4]
	br.n	 @L1722
	st	 r4,r0,r5
	align	 4
@L1663:
	ld	 r13,r13,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1669
	and	 r13,r4,0xfffe
	bcnd.n	 ge0,r13,@L1671
	or	 r9,r0,r13
	addu	 r9,r13,31
@L1671:
	ext	 r9,r9,0<5>
	or.u	 r10,r0,hi16(_use_spec_F)
	mak	 r11,r9,0<5>
	or	 r10,r10,lo16(_use_spec_F)
	subu	 r11,r13,r11
	or	 r13,r0,1
	ld	 r12,r10[r9]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	st	 r12,r10[r9]
	or.u	 r13,r0,hi16(_spec_regs_F)
	ld	 r12,r0,r5
	or	 r13,r13,lo16(_spec_regs_F)
	br.n	 @L1722
	st	 r12,r13[r4]
	align	 4
@L1669:
	or.u	 r13,r0,hi16(_regs_F)
	ld	 r12,r0,r5
	or	 r13,r13,lo16(_regs_F)
	br.n	 @L1722
	st	 r12,r13[r4]
	align	 4
@L1673:
	bcnd.n	 ne0,r2,@L1674
	or.u	 r13,r0,hi16(_spec_mode)
	and	 r13,r4,0xfffe
	bcnd.n	 ge0,r13,@L1677
	or	 r10,r0,r13
	addu	 r10,r13,31
@L1677:
	ext	 r10,r10,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mak	 r11,r10,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	subu	 r11,r13,r11
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1675
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L1726
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L1675:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L1726:
	ld	 r4,r13[r4]
	br.n	 @L1722
	st	 r4,r0,r5
	align	 4
@L1674:
	ld	 r13,r13,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1680
	and	 r13,r4,0xfffe
	bcnd.n	 ge0,r13,@L1682
	or	 r9,r0,r13
	addu	 r9,r13,31
@L1682:
	ext	 r9,r9,0<5>
	or.u	 r10,r0,hi16(_use_spec_F)
	mak	 r12,r9,0<5>
	or	 r10,r10,lo16(_use_spec_F)
	subu	 r12,r13,r12
	or	 r13,r0,1
	ld	 r11,r10[r9]
	mak	 r13,r13,r12
	or.u	 r12,r0,hi16(_spec_regs_F)
	or	 r11,r11,r13
	st	 r11,r10[r9]
	or	 r12,r12,lo16(_spec_regs_F)
	ld	 r5,r0,r5
	br.n	 @L1722
	st	 r5,r12[r4]
	align	 4
@L1680:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r5,r0,r5
	br.n	 @L1722
	st	 r5,r13[r4]
	align	 4
@L1684:
	cmp	 r13,r4,15
	bb1	 ls,r13,@L1685
	or.u	 r2,r0,hi16(@LC524)
	br.n	 @L1723
	or	 r2,r2,lo16(@LC524)
	align	 4
@L1685:
	bcnd.n	 ne0,r2,@L1686
	or.u	 r13,r0,hi16(_spec_mode)
	mak	 r4,r4,0<1>
	and	 r13,r4,0xfffe
	bcnd.n	 ge0,r13,@L1689
	or	 r10,r0,r13
	addu	 r10,r13,31
@L1689:
	ext	 r10,r10,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mak	 r11,r10,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	subu	 r11,r13,r11
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1687
	or.u	 r13,r0,hi16(_spec_regs_F)
	ext	 r12,r4,0<1>
	br.n	 @L1727
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L1687:
	or.u	 r13,r0,hi16(_regs_F)
	ext	 r12,r4,0<1>
	or	 r13,r13,lo16(_regs_F)
@L1727:
	ld.d	 r12,r13[r12]
	fsub.sds r13,r12,r0
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1686:
	ld	 r13,r13,lo16(_spec_mode)
	bcnd	 eq0,r13,@L1692
	mak	 r4,r4,0<1>
	and	 r13,r4,0xfffe
	bcnd.n	 ge0,r13,@L1694
	or	 r9,r0,r13
	addu	 r9,r13,31
@L1694:
	ext	 r9,r9,0<5>
	or.u	 r10,r0,hi16(_use_spec_F)
	mak	 r11,r9,0<5>
	or	 r10,r10,lo16(_use_spec_F)
	subu	 r11,r13,r11
	or	 r13,r0,1
	ld	 r12,r10[r9]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	st	 r12,r10[r9]
	or.u	 r11,r0,hi16(_spec_regs_F)
	ld	 r13,r0,r5
	ext	 r10,r4,0<1>
	or	 r11,r11,lo16(_spec_regs_F)
	fsub.dss r12,r13,r0
	br.n	 @L1722
	st.d	 r12,r11[r10]
	align	 4
@L1692:
	or.u	 r13,r0,hi16(_regs_F)
	ld	 r12,r0,r5
	or	 r13,r13,lo16(_regs_F)
	fsub.dss r10,r12,r0
	br.n	 @L1722
	st.d	 r10,r13[r4]
	align	 4
@L1696:
	bcnd.n	 ne0,r2,@L1697
	or.u	 r13,r0,hi16(_spec_mode)
	or.u	 r13,r0,hi16(_use_spec_HI)
	ld	 r13,r13,lo16(_use_spec_HI)
	bcnd.n	 eq0,r13,@L1698
	or.u	 r13,r0,hi16(_spec_regs_HI)
	ld	 r13,r13,lo16(_spec_regs_HI)
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1698:
	or.u	 r13,r0,hi16(_regs_HI)
	ld	 r13,r13,lo16(_regs_HI)
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1697:
	ld	 r13,r13,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1701
	or.u	 r12,r0,hi16(_use_spec_HI)
	or	 r13,r0,1
	st	 r13,r12,lo16(_use_spec_HI)
	ld	 r12,r0,r5
	or.u	 r13,r0,hi16(_spec_regs_HI)
	br.n	 @L1722
	st	 r12,r13,lo16(_spec_regs_HI)
	align	 4
@L1701:
	ld	 r12,r0,r5
	or.u	 r13,r0,hi16(_regs_HI)
	br.n	 @L1722
	st	 r12,r13,lo16(_regs_HI)
	align	 4
@L1703:
	bcnd.n	 ne0,r2,@L1704
	or.u	 r13,r0,hi16(_spec_mode)
	or.u	 r13,r0,hi16(_use_spec_LO)
	ld	 r13,r13,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L1705
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1705:
	or.u	 r13,r0,hi16(_regs_LO)
	ld	 r13,r13,lo16(_regs_LO)
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1704:
	ld	 r13,r13,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1708
	or.u	 r12,r0,hi16(_use_spec_LO)
	or	 r13,r0,1
	st	 r13,r12,lo16(_use_spec_LO)
	ld	 r12,r0,r5
	or.u	 r13,r0,hi16(_spec_regs_LO)
	br.n	 @L1722
	st	 r12,r13,lo16(_spec_regs_LO)
	align	 4
@L1708:
	ld	 r12,r0,r5
	or.u	 r13,r0,hi16(_regs_LO)
	br.n	 @L1722
	st	 r12,r13,lo16(_regs_LO)
	align	 4
@L1710:
	bcnd.n	 ne0,r2,@L1711
	or.u	 r13,r0,hi16(_spec_mode)
	or.u	 r13,r0,hi16(_use_spec_FCC)
	ld	 r13,r13,lo16(_use_spec_FCC)
	bcnd.n	 eq0,r13,@L1712
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	ld	 r13,r13,lo16(_spec_regs_FCC)
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1712:
	or.u	 r13,r0,hi16(_regs_FCC)
	ld	 r13,r13,lo16(_regs_FCC)
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1711:
	ld	 r13,r13,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1715
	or.u	 r12,r0,hi16(_use_spec_FCC)
	or	 r13,r0,1
	st	 r13,r12,lo16(_use_spec_FCC)
	ld	 r12,r0,r5
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	br.n	 @L1722
	st	 r12,r13,lo16(_spec_regs_FCC)
	align	 4
@L1715:
	ld	 r12,r0,r5
	or.u	 r13,r0,hi16(_regs_FCC)
	br.n	 @L1722
	st	 r12,r13,lo16(_regs_FCC)
	align	 4
@L1717:
	bcnd.n	 ne0,r2,@L1718
	or.u	 r13,r0,hi16(_regs_PC)
	ld	 r13,r13,lo16(_regs_PC)
	br.n	 @L1722
	st	 r13,r0,r5
	align	 4
@L1718:
	ld	 r12,r0,r5
	br.n	 @L1722
	st	 r12,r13,lo16(_regs_PC)
	align	 4
@L1720:
	or.u	 r2,r0,hi16(@LC525)
	or	 r4,r0,3315
	or.u	 r3,r0,hi16(@LC526)
	or.u	 r5,r0,hi16(@LC527)
	or	 r2,r2,lo16(@LC525)
	or	 r3,r3,lo16(@LC526)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC527)
	align	 4
@L1649:
	or.u	 r2,r0,hi16(@LC528)
	or	 r4,r0,3319
	or.u	 r3,r0,hi16(@LC529)
	or.u	 r5,r0,hi16(@LC530)
	or	 r2,r2,lo16(@LC528)
	or	 r3,r3,lo16(@LC529)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC530)
	align	 4
@L1722:
	or	 r2,r0,0
@L1723:
@Lte16:
	ld	 r1,r31,36
	jmp.n	 r1
	addu	 r31,r31,48

data
	align	 8
_last_op:
	word	 0
	word	 0
	word	 0
	zero	 12
	align	 8
@LC531:
	string	 "sim-outorder.c\000"
	align	 8
@LC532:
	string	 "ruu_dispatch\000"
	align	 8
@LC533:
	string	 "drained and speculative\000"
	align	 8
@LC534:
	string	 "ss.def\000"
	align	 8
@LC535:
	string	 "ruu_dispatch\000"
	align	 8
@LC536:
	string	 "bad jump alignment\000"
	align	 8
@LC537:
	string	 "ss.def\000"
	align	 8
@LC538:
	string	 "ruu_dispatch\000"
	align	 8
@LC539:
	string	 "bad jump alignment\000"
	align	 8
@LC540:
	string	 "ss.def\000"
	align	 8
@LC541:
	string	 "ruu_dispatch\000"
	align	 8
@LC542:
	string	 "bad INT register alignment\000"
	align	 8
@LC543:
	string	 "ss.def\000"
	align	 8
@LC544:
	string	 "ruu_dispatch\000"
	align	 8
@LC545:
	string	 "bad FP register alignment\000"
	align	 8
@LC546:
	string	 "ss.def\000"
	align	 8
@LC547:
	string	 "ruu_dispatch\000"
	align	 8
@LC548:
	string	 "bad INT register alignment\000"
	align	 8
@LC549:
	string	 "ss.def\000"
	align	 8
@LC550:
	string	 "ruu_dispatch\000"
	align	 8
@LC551:
	string	 "bad FP register alignment\000"
	align	 8
@LC552:
	string	 "ss.def\000"
	align	 8
@LC553:
	string	 "ruu_dispatch\000"
	align	 8
@LC554:
	string	 "bad INT register alignment\000"
	align	 8
@LC555:
	string	 "ss.def\000"
	align	 8
@LC556:
	string	 "ruu_dispatch\000"
	align	 8
@LC557:
	string	 "bad FP register alignment\000"
	align	 8
@LC558:
	string	 "ss.def\000"
	align	 8
@LC559:
	string	 "ruu_dispatch\000"
	align	 8
@LC560:
	string	 "bad INT register alignment\000"
	align	 8
@LC561:
	string	 "ss.def\000"
	align	 8
@LC562:
	string	 "ruu_dispatch\000"
	align	 8
@LC563:
	string	 "bad FP register alignment\000"
	align	 8
@LC564:
	string	 "ss.def\000"
	align	 8
@LC565:
	string	 "ruu_dispatch\000"
	align	 8
@LC566:
	string	 "+ overflow\000"
	align	 8
@LC567:
	string	 "ss.def\000"
	align	 8
@LC568:
	string	 "ruu_dispatch\000"
	align	 8
@LC569:
	string	 "+ underflow\000"
	align	 8
@LC570:
	string	 "ss.def\000"
	align	 8
@LC571:
	string	 "ruu_dispatch\000"
	align	 8
@LC572:
	string	 "+ overflow\000"
	align	 8
@LC573:
	string	 "ss.def\000"
	align	 8
@LC574:
	string	 "ruu_dispatch\000"
	align	 8
@LC575:
	string	 "+ underflow\000"
	align	 8
@LC576:
	string	 "ss.def\000"
	align	 8
@LC577:
	string	 "ruu_dispatch\000"
	align	 8
@LC578:
	string	 "- overflow\000"
	align	 8
@LC579:
	string	 "ss.def\000"
	align	 8
@LC580:
	string	 "ruu_dispatch\000"
	align	 8
@LC581:
	string	 "- underflow\000"
	align	 8
@LC582:
	string	 "ss.def\000"
	align	 8
@LC583:
	string	 "ruu_dispatch\000"
	align	 8
@LC584:
	string	 "divide by 0\000"
	align	 8
@LC585:
	string	 "ss.def\000"
	align	 8
@LC586:
	string	 "ruu_dispatch\000"
	align	 8
@LC587:
	string	 "divide by 0\000"
	align	 8
@LC588:
	string	 "ss.def\000"
	align	 8
@LC589:
	string	 "ruu_dispatch\000"
	align	 8
@LC590:
	string	 "bad FP register alignment\000"
	align	 8
@LC591:
	string	 "ss.def\000"
	align	 8
@LC592:
	string	 "ruu_dispatch\000"
	align	 8
@LC593:
	string	 "bad FP register alignment\000"
	align	 8
@LC594:
	string	 "ss.def\000"
	align	 8
@LC595:
	string	 "ruu_dispatch\000"
	align	 8
@LC596:
	string	 "bad FP register alignment\000"
	align	 8
@LC597:
	string	 "ss.def\000"
	align	 8
@LC598:
	string	 "ruu_dispatch\000"
	align	 8
@LC599:
	string	 "bad FP register alignment\000"
	align	 8
@LC600:
	string	 "ss.def\000"
	align	 8
@LC601:
	string	 "ruu_dispatch\000"
	align	 8
@LC602:
	string	 "bad FP register alignment\000"
	align	 8
@LC603:
	string	 "ss.def\000"
	align	 8
@LC604:
	string	 "ruu_dispatch\000"
	align	 8
@LC605:
	string	 "bad FP register alignment\000"
	align	 8
@LC606:
	string	 "ss.def\000"
	align	 8
@LC607:
	string	 "ruu_dispatch\000"
	align	 8
@LC608:
	string	 "bad FP register alignment\000"
	align	 8
@LC609:
	string	 "ss.def\000"
	align	 8
@LC610:
	string	 "ruu_dispatch\000"
	align	 8
@LC611:
	string	 "bad FP register alignment\000"
	align	 8
@LC612:
	string	 "ss.def\000"
	align	 8
@LC613:
	string	 "ruu_dispatch\000"
	align	 8
@LC614:
	string	 "bad FP register alignment\000"
	align	 8
@LC615:
	string	 "ss.def\000"
	align	 8
@LC616:
	string	 "ruu_dispatch\000"
	align	 8
@LC617:
	string	 "bad FP register alignment\000"
	align	 8
@LC618:
	string	 "ss.def\000"
	align	 8
@LC619:
	string	 "ruu_dispatch\000"
	align	 8
@LC620:
	string	 "bad FP register alignment\000"
	align	 8
@LC621:
	string	 "ss.def\000"
	align	 8
@LC622:
	string	 "ruu_dispatch\000"
	align	 8
@LC623:
	string	 "bad FP register alignment\000"
	align	 8
@LC624:
	string	 "ss.def\000"
	align	 8
@LC625:
	string	 "ruu_dispatch\000"
	align	 8
@LC626:
	string	 "bad FP register alignment\000"
	align	 8
@LC627:
	string	 "ss.def\000"
	align	 8
@LC628:
	string	 "ruu_dispatch\000"
	align	 8
@LC629:
	string	 "bad FP register alignment\000"
	align	 8
@LC630:
	string	 "ss.def\000"
	align	 8
@LC631:
	string	 "ruu_dispatch\000"
	align	 8
@LC632:
	string	 "bad FP register alignment\000"
	align	 8
@LC633:
	string	 "ss.def\000"
	align	 8
@LC634:
	string	 "ruu_dispatch\000"
	align	 8
@LC635:
	string	 "bad FP register alignment\000"
	align	 8
@LC636:
	string	 "ss.def\000"
	align	 8
@LC637:
	string	 "ruu_dispatch\000"
	align	 8
@LC638:
	string	 "bad FP register alignment\000"
	align	 8
@LC639:
	string	 "ss.def\000"
	align	 8
@LC640:
	string	 "ruu_dispatch\000"
	align	 8
@LC641:
	string	 "bad FP register alignment\000"
	align	 8
@LC642:
	string	 "ss.def\000"
	align	 8
@LC643:
	string	 "ruu_dispatch\000"
	align	 8
@LC644:
	string	 "bad FP register alignment\000"
	align	 8
@LC645:
	string	 "ss.def\000"
	align	 8
@LC646:
	string	 "ruu_dispatch\000"
	align	 8
@LC647:
	string	 "bad FP register alignment\000"
	align	 8
@LC648:
	string	 "ss.def\000"
	align	 8
@LC649:
	string	 "ruu_dispatch\000"
	align	 8
@LC650:
	string	 "bad FP register alignment\000"
	align	 8
@LC651:
	string	 "ss.def\000"
	align	 8
@LC652:
	string	 "ruu_dispatch\000"
	align	 8
@LC653:
	string	 "divide by 0\000"
	align	 8
@LC654:
	string	 "ss.def\000"
	align	 8
@LC655:
	string	 "ruu_dispatch\000"
	align	 8
@LC656:
	string	 "bad FP register alignment\000"
	align	 8
@LC657:
	string	 "ss.def\000"
	align	 8
@LC658:
	string	 "ruu_dispatch\000"
	align	 8
@LC659:
	string	 "bad FP register alignment\000"
	align	 8
@LC660:
	string	 "ss.def\000"
	align	 8
@LC661:
	string	 "ruu_dispatch\000"
	align	 8
@LC662:
	string	 "bad FP register alignment\000"
	align	 8
@LC663:
	string	 "ss.def\000"
	align	 8
@LC664:
	string	 "ruu_dispatch\000"
	align	 8
@LC665:
	string	 "divide by 0\000"
	align	 8
@LC666:
	string	 "ss.def\000"
	align	 8
@LC667:
	string	 "ruu_dispatch\000"
	align	 8
@LC668:
	string	 "bad FP register alignment\000"
	align	 8
@LC669:
	string	 "ss.def\000"
	align	 8
@LC670:
	string	 "ruu_dispatch\000"
	align	 8
@LC671:
	string	 "bad FP register alignment\000"
	align	 8
@LC672:
	string	 "ss.def\000"
	align	 8
@LC673:
	string	 "ruu_dispatch\000"
	align	 8
@LC674:
	string	 "bad FP register alignment\000"
	align	 8
@LC675:
	string	 "ss.def\000"
	align	 8
@LC676:
	string	 "ruu_dispatch\000"
	align	 8
@LC677:
	string	 "bad FP register alignment\000"
	align	 8
@LC678:
	string	 "ss.def\000"
	align	 8
@LC679:
	string	 "ruu_dispatch\000"
	align	 8
@LC680:
	string	 "bad FP register alignment\000"
	align	 8
@LC681:
	string	 "ss.def\000"
	align	 8
@LC682:
	string	 "ruu_dispatch\000"
	align	 8
@LC683:
	string	 "bad FP register alignment\000"
	align	 8
@LC684:
	string	 "ss.def\000"
	align	 8
@LC685:
	string	 "ruu_dispatch\000"
	align	 8
@LC686:
	string	 "bad FP register alignment\000"
	align	 8
@LC687:
	string	 "ss.def\000"
	align	 8
@LC688:
	string	 "ruu_dispatch\000"
	align	 8
@LC689:
	string	 "bad FP register alignment\000"
	align	 8
@LC690:
	string	 "ss.def\000"
	align	 8
@LC691:
	string	 "ruu_dispatch\000"
	align	 8
@LC692:
	string	 "bad FP register alignment\000"
	align	 8
@LC693:
	string	 "ss.def\000"
	align	 8
@LC694:
	string	 "ruu_dispatch\000"
	align	 8
@LC695:
	string	 "bad FP register alignment\000"
	align	 8
@LC696:
	string	 "ss.def\000"
	align	 8
@LC697:
	string	 "ruu_dispatch\000"
	align	 8
@LC698:
	string	 "bad FP register alignment\000"
	align	 8
@LC699:
	string	 "ss.def\000"
	align	 8
@LC700:
	string	 "ruu_dispatch\000"
	align	 8
@LC701:
	string	 "bad FP register alignment\000"
	align	 8
@LC702:
	string	 "ss.def\000"
	align	 8
@LC703:
	string	 "ruu_dispatch\000"
	align	 8
@LC704:
	string	 "bad FP register alignment\000"
	align	 8
@LC705:
	string	 "ss.def\000"
	align	 8
@LC706:
	string	 "ruu_dispatch\000"
	align	 8
@LC707:
	string	 "bad FP register alignment\000"
	align	 8
@LC708:
	string	 "ss.def\000"
	align	 8
@LC709:
	string	 "ruu_dispatch\000"
	align	 8
@LC710:
	string	 "bad FP register alignment\000"
	align	 8
@LC711:
	string	 "ss.def\000"
	align	 8
@LC712:
	string	 "ruu_dispatch\000"
	align	 8
@LC713:
	string	 "bad FP register alignment\000"
	align	 8
@LC714:
	string	 "ss.def\000"
	align	 8
@LC715:
	string	 "ruu_dispatch\000"
	align	 8
@LC716:
	string	 "bad FP register alignment\000"
	align	 8
@LC717:
	string	 "ss.def\000"
	align	 8
@LC718:
	string	 "ruu_dispatch\000"
	align	 8
@LC719:
	string	 "bad FP register alignment\000"
	align	 8
@LC720:
	string	 "ss.def\000"
	align	 8
@LC721:
	string	 "ruu_dispatch\000"
	align	 8
@LC722:
	string	 "bad FP register alignment\000"
	align	 8
@LC723:
	string	 "ss.def\000"
	align	 8
@LC724:
	string	 "ruu_dispatch\000"
	align	 8
@LC725:
	string	 "bad FP register alignment\000"
	align	 8
@LC726:
	string	 "ss.def\000"
	align	 8
@LC727:
	string	 "ruu_dispatch\000"
	align	 8
@LC728:
	string	 "bad FP register alignment\000"
	align	 8
@LC729:
	string	 "ss.def\000"
	align	 8
@LC730:
	string	 "ruu_dispatch\000"
	align	 8
@LC731:
	string	 "bad FP register alignment\000"
	align	 8
@LC732:
	string	 "ss.def\000"
	align	 8
@LC733:
	string	 "ruu_dispatch\000"
	align	 8
@LC734:
	string	 "bad FP register alignment\000"
	align	 8
@LC735:
	string	 "ss.def\000"
	align	 8
@LC736:
	string	 "ruu_dispatch\000"
	align	 8
@LC737:
	string	 "bad FP register alignment\000"
	align	 8
@LC738:
	string	 "ss.def\000"
	align	 8
@LC739:
	string	 "ruu_dispatch\000"
	align	 8
@LC740:
	string	 "bad FP register alignment\000"
	align	 8
@LC741:
	string	 "ss.def\000"
	align	 8
@LC742:
	string	 "ruu_dispatch\000"
	align	 8
@LC743:
	string	 "bad FP register alignment\000"
	align	 8
@LC744:
	string	 "ss.def\000"
	align	 8
@LC745:
	string	 "ruu_dispatch\000"
	align	 8
@LC746:
	string	 "bad FP register alignment\000"
	align	 8
@LC747:
	string	 "ss.def\000"
	align	 8
@LC748:
	string	 "ruu_dispatch\000"
	align	 8
@LC749:
	string	 "bad FP register alignment\000"
	align	 8
@LC750:
	string	 "ss.def\000"
	align	 8
@LC751:
	string	 "ruu_dispatch\000"
	align	 8
@LC752:
	string	 "bad FP register alignment\000"
	align	 8
@LC753:
	string	 "ss.def\000"
	align	 8
@LC754:
	string	 "ruu_dispatch\000"
	align	 8
@LC755:
	string	 "bad FP register alignment\000"
	align	 8
@LC756:
	string	 "ss.def\000"
	align	 8
@LC757:
	string	 "ruu_dispatch\000"
	align	 8
@LC758:
	string	 "bad FP register alignment\000"
	align	 8
@LC759:
	string	 "ss.def\000"
	align	 8
@LC760:
	string	 "ruu_dispatch\000"
	align	 8
@LC761:
	string	 "bad FP register alignment\000"
	align	 8
@LC762:
	string	 "ss.def\000"
	align	 8
@LC763:
	string	 "ruu_dispatch\000"
	align	 8
@LC764:
	string	 "bad FP register alignment\000"
	align	 8
@LC765:
	string	 "ss.def\000"
	align	 8
@LC766:
	string	 "ruu_dispatch\000"
	align	 8
@LC767:
	string	 "bad FP register alignment\000"
	align	 8
@LC768:
	string	 "ss.def\000"
	align	 8
@LC769:
	string	 "ruu_dispatch\000"
	align	 8
@LC770:
	string	 "bad FP register alignment\000"
	align	 8
@LC771:
	string	 "ss.def\000"
	align	 8
@LC772:
	string	 "ruu_dispatch\000"
	align	 8
@LC773:
	string	 "bad FP register alignment\000"
	align	 8
@LC774:
	string	 "ss.def\000"
	align	 8
@LC775:
	string	 "ruu_dispatch\000"
	align	 8
@LC776:
	string	 "bad FP register alignment\000"
	align	 8
@LC777:
	string	 "ss.def\000"
	align	 8
@LC778:
	string	 "ruu_dispatch\000"
	align	 8
@LC779:
	string	 "bad FP register alignment\000"
	align	 8
@LC780:
	string	 "ss.def\000"
	align	 8
@LC781:
	string	 "ruu_dispatch\000"
	align	 8
@LC782:
	string	 "bad FP register alignment\000"
	align	 8
@LC783:
	string	 "ss.def\000"
	align	 8
@LC784:
	string	 "ruu_dispatch\000"
	align	 8
@LC785:
	string	 "bad FP register alignment\000"
	align	 8
@LC786:
	string	 "ss.def\000"
	align	 8
@LC787:
	string	 "ruu_dispatch\000"
	align	 8
@LC788:
	string	 "speculative syscall\000"
	align	 8
@LC789:
	string	 "ss.def\000"
	align	 8
@LC790:
	string	 "ruu_dispatch\000"
	align	 8
@LC791:
	string	 "bad INT register alignment\000"
	align	 8
@LC792:
	string	 "ss.def\000"
	align	 8
@LC793:
	string	 "ruu_dispatch\000"
	align	 8
@LC794:
	string	 "bad FP register alignment\000"
	align	 8
@LC795:
	string	 "ss.def\000"
	align	 8
@LC796:
	string	 "ruu_dispatch\000"
	align	 8
@LC797:
	string	 "bad FP register alignment\000"
	align	 8
@LC798:
	string	 "ss.def\000"
	align	 8
@LC799:
	string	 "ruu_dispatch\000"
	align	 8
@LC800:
	string	 "bad INT register alignment\000"
	align	 8
@LC801:
	string	 "internal ld/st\000"
	align	 8
@LC802:
	string	 "DA\000"
	align	 8
@LC803:
	string	 "DA\000"
	align	 8
@LC804:
	string	 "sim-outorder.c\000"
	align	 8
@LC805:
	string	 "ruu_dispatch\000"
	align	 8
@LC806:
	string	 "bad stat class\000"
text
	align	 8
_ruu_dispatch:
	or.u	 r13,r0,hi16(_ruu_decode_width)
	ld	 r11,r13,lo16(_ruu_decode_width)
	subu	 r31,r31,256
	st	 r1,r31,80
	st.d	 r24,r31,72
	st.d	 r22,r31,64
	st.d	 r20,r31,56
	or	 r22,r0,0
	st.d	 r18,r31,48
	or.u	 r13,r0,hi16(_spec_regs_LO)
	st.d	 r14,r31,32
	or.u	 r14,r0,hi16(_use_spec_R)
	st.d	 r16,r31,40
@Ltb17:
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	st	 r0,r31,148
	bcnd.n	 le0,r11,@L1731
	or	 r18,r14,lo16(_use_spec_R)
	or.u	 r13,r0,hi16(_RUU_num)
	or.u	 r12,r0,hi16(_RUU_size)
	ld	 r13,r13,lo16(_RUU_num)
	ld	 r12,r12,lo16(_RUU_size)
	cmp	 r13,r13,r12
	bb0.n	 lt,r13,@L1731
	or.u	 r13,r0,hi16(_LSQ_num)
	or.u	 r12,r0,hi16(_LSQ_size)
	ld	 r13,r13,lo16(_LSQ_num)
	ld	 r12,r12,lo16(_LSQ_size)
	cmp	 r13,r13,r12
	bb0.n	 lt,r13,@L1731
	or.u	 r13,r0,hi16(_fetch_num)
	br	 @L9339
	align	 4
@L8656:
	ld	 r13,r13,lo16(_ruu_include_spec)
	bcnd.n	 ne0,r13,@L9340
	or.u	 r13,r0,hi16(_ruu_inorder_issue)
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L1731
	or.u	 r13,r0,hi16(_ruu_inorder_issue)
@L9340:
	ld	 r13,r13,lo16(_ruu_inorder_issue)
	bcnd.n	 eq0,r13,@L1734
	or.u	 r13,r0,hi16(_last_op+4)
	ld	 r11,r13,lo16(_last_op+4)
	bcnd.n	 eq0,r11,@L1734
	or	 r13,r13,lo16(_last_op+4)
	ld	 r13,r13,4
	ld	 r12,r11,44
	cmp	 r13,r13,r12
	bb1.n	 ne,r13,@L9341
	or.u	 r13,r0,hi16(_fetch_head)
	ld	 r13,r11,84
	bcnd	 eq0,r13,@L1731
	ld	 r13,r11,88
	bcnd	 eq0,r13,@L1731
	ld	 r13,r11,92
	bcnd	 eq0,r13,@L1731
@L1734:
	or.u	 r13,r0,hi16(_fetch_head)
@L9341:
	ld	 r12,r13,lo16(_fetch_head)
	or.u	 r13,r0,hi16(_fetch_data)
	mul	 r12,r12,24
	ld	 r11,r13,lo16(_fetch_data)
	addu	 r11,r11,r12
	ld	 r9,r0,r11
	ld	 r13,r11,4
	st	 r9,r31,88
	st	 r13,r31,92
	ld	 r10,r11,8
	ld	 r12,r11,12
	or.u	 r13,r0,hi16(_regs_PC)
	st	 r10,r13,lo16(_regs_PC)
	mask	 r9,r9,255
	st	 r9,r31,156
	ld	 r14,r11,16
	or.u	 r13,r0,hi16(_pred_PC)
	or.u	 r15,r0,hi16(_ss_op2flags)
	st	 r12,r13,lo16(_pred_PC)
	or	 r15,r15,lo16(_ss_op2flags)
	st	 r14,r31,204
	ld	 r13,r15[r9]
	addu	 r10,r10,8
	st	 r10,r31,196
	ld	 r11,r11,20
	bb0.n	 (31-20),r13,@L1736
	st	 r11,r31,212
	or.u	 r13,r0,hi16(_RUU_num)
	ld	 r13,r13,lo16(_RUU_num)
	bcnd	 ne0,r13,@L1731
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1736
	or.u	 r2,r0,hi16(@LC531)
	or	 r4,r0,3400
	or.u	 r3,r0,hi16(@LC532)
	or.u	 r5,r0,hi16(@LC533)
	or	 r2,r2,lo16(@LC531)
	or	 r3,r3,lo16(@LC532)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC533)
	align	 4
@L1736:
	st	 r0,r31,220
	or.u	 r15,r0,hi16(_regs_R)
	st	 r0,r15,lo16(_regs_R)
	or.u	 r14,r0,hi16(_spec_regs_R)
	ld	 r15,r31,156
	or	 r24,r0,0
	or.u	 r13,r0,hi16(@L8444)
	subu	 r11,r15,1
	st	 r0,r14,lo16(_spec_regs_R)
	cmp	 r12,r11,118
	bb0.n	 ls,r12,@L8443
	or	 r13,r13,lo16(@L8444)
	ld	 r13,r13[r11]
	jmp	 r13
	align	 4
@L8444:
	word	 @L8778
	word	 @L1741
	word	 @L1742
	word	 @L1745
	word	 @L1754
	word	 @L1765
	word	 @L1772
	word	 @L1779
	word	 @L1784
	word	 @L1789
	word	 @L1794
	word	 @L1799
	word	 @L1804
	word	 @L1809
	word	 @L1956
	word	 @L2103
	word	 @L2250
	word	 @L2397
	word	 @L2544
	word	 @L2821
	word	 @L2968
	word	 @L3251
	word	 @L3323
	word	 @L3395
	word	 @L3477
	word	 @L3559
	word	 @L3641
	word	 @L3794
	word	 @L3943
	word	 @L4025
	word	 @L4178
	word	 @L4315
	word	 @L4452
	word	 @L4603
	word	 @L4754
	word	 @L4905
	word	 @L5056
	word	 @L5207
	word	 @L5486
	word	 @L5637
	word	 @L5928
	word	 @L6012
	word	 @L6096
	word	 @L6180
	word	 @L6337
	word	 @L6490
	word	 @L6574
	word	 @L6731
	word	 @L6882
	word	 @L6966
	word	 @L7117
	word	 @L7201
	word	 @L7236
	word	 @L7259
	word	 @L7270
	word	 @L7277
	word	 @L7312
	word	 @L7323
	word	 @L7409
	word	 @L7475
	word	 @L7560
	word	 @L7613
	word	 @L7620
	word	 @L7627
	word	 @L7634
	word	 @L7641
	word	 @L7652
	word	 @L7659
	word	 @L7670
	word	 @L7677
	word	 @L7688
	word	 @L7695
	word	 @L7706
	word	 @L7713
	word	 @L7724
	word	 @L7731
	word	 @L7742
	word	 @L7771
	word	 @L7802
	word	 @L7813
	word	 @L7820
	word	 @L7831
	word	 @L7838
	word	 @L7861
	word	 @L7884
	word	 @L7907
	word	 @L7930
	word	 @L7953
	word	 @L7976
	word	 @L8015
	word	 @L8054
	word	 @L8069
	word	 @L8084
	word	 @L8099
	word	 @L8114
	word	 @L8129
	word	 @L8144
	word	 @L8159
	word	 @L8174
	word	 @L8189
	word	 @L8204
	word	 @L8219
	word	 @L8234
	word	 @L8253
	word	 @L8272
	word	 @L8291
	word	 @L8310
	word	 @L8329
	word	 @L8348
	word	 @L8363
	word	 @L8378
	word	 @L8778
	word	 @L8382
	word	 @L8385
	word	 @L8392
	word	 @L8778
	word	 @L8414
	word	 @L8421
	word	 @L8778
	align	 4
@L1741:
	or.u	 r13,r0,hi16(_regs_PC)
	ld	 r12,r31,92
	ld	 r13,r13,lo16(_regs_PC)
	st	 r0,r31,164
	st	 r0,r31,172
	or	 r17,r0,0
	st	 r0,r31,180
	mak	 r12,r12,26<2>
	mask.u	 r13,r13,0xf000
	st	 r0,r31,188
	or	 r13,r13,r12
	br.n	 @L1739
	st	 r13,r31,196
	align	 4
@L1742:
	ld	 r12,r31,92
	or	 r14,r0,31
	st	 r14,r31,164
	st	 r0,r31,172
	or.u	 r13,r0,hi16(_regs_PC)
	st	 r0,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r10,r13,lo16(_regs_PC)
	or	 r17,r0,0
	ld	 r11,r15,lo16(_spec_mode)
	mak	 r12,r12,26<2>
	mask.u	 r13,r10,0xf000
	st	 r0,r31,188
	or	 r13,r13,r12
	bcnd.n	 eq0,r11,@L1743
	st	 r13,r31,196
	or.u	 r14,r0,hi16(_use_spec_R)
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r12,r14,lo16(_use_spec_R)
	or	 r15,r15,lo16(_spec_regs_R)
	addu	 r13,r10,8
	st	 r13,r15,124
	or.u	 r12,r12,0x8000
	br.n	 @L1739
	st	 r12,r14,lo16(_use_spec_R)
	align	 4
@L1743:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	addu	 r13,r10,8
	br.n	 @L1739
	st	 r13,r14,124
	align	 4
@L1745:
	ld	 r12,r31,92
	or	 r13,r0,1
	st	 r0,r31,164
	extu	 r15,r12,0<24>
	st	 r0,r31,172
	extu	 r12,r12,0<29>
	st	 r15,r31,180
	mask	 r11,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or	 r17,r0,0
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1748
	st	 r0,r31,188
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r15]
	mask	 r13,r13,7
	bcnd.n	 ne0,r13,@L9342
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L1747
	align	 4
@L1748:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r15]
	mask	 r13,r13,7
	bcnd.n	 eq0,r13,@L1747
	or.u	 r15,r0,hi16(_spec_mode)
@L9342:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L1747
	or.u	 r2,r0,hi16(@LC534)
	or	 r4,r0,235
	or.u	 r3,r0,hi16(@LC535)
	or.u	 r5,r0,hi16(@LC536)
	or	 r2,r2,lo16(@LC534)
	or	 r3,r3,lo16(@LC535)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC536)
	align	 4
@L1747:
	ld	 r12,r31,92
	extu	 r4,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r4,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 ne0,r12,@L9343
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9344
	or.u	 r15,r0,hi16(_regs_R)
	align	 4
@L1754:
	ld	 r11,r31,92
	extu	 r14,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	st	 r0,r31,172
	extu	 r11,r11,8<8>
	st	 r14,r31,180
	mask	 r10,r14,31
	ld	 r12,r18[r12]
	or	 r17,r0,0
	mak	 r13,r13,r10
	st	 r0,r31,188
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1757
	st	 r11,r31,164
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r14]
	mask	 r13,r13,7
	bcnd.n	 ne0,r13,@L9345
	or.u	 r14,r0,hi16(_spec_mode)
	br.n	 @L9346
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L1757:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r14]
	mask	 r13,r13,7
	bcnd.n	 eq0,r13,@L1756
	or.u	 r14,r0,hi16(_spec_mode)
@L9345:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8664
	or.u	 r2,r0,hi16(@LC537)
	or	 r4,r0,240
	or.u	 r3,r0,hi16(@LC538)
	or.u	 r5,r0,hi16(@LC539)
	or	 r2,r2,lo16(@LC537)
	or	 r3,r3,lo16(@LC538)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC539)
	align	 4
@L1756:
	or.u	 r15,r0,hi16(_spec_mode)
@L9346:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1761
	or.u	 r13,r0,hi16(_regs_PC)
@L8664:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	or	 r13,r0,1
	extu	 r10,r11,3<5>
	mask	 r11,r11,31
	ld	 r12,r18[r10]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	or.u	 r13,r0,hi16(_regs_PC)
	st	 r12,r18[r10]
	ld	 r13,r13,lo16(_regs_PC)
	or.u	 r14,r0,hi16(_spec_regs_R)
	ld.bu	 r12,r31,94
	or	 r14,r14,lo16(_spec_regs_R)
	addu	 r13,r13,8
	br.n	 @L1762
	st	 r13,r14[r12]
	align	 4
@L1761:
	ld	 r13,r13,lo16(_regs_PC)
	or.u	 r15,r0,hi16(_regs_R)
	ld.bu	 r12,r31,94
	or	 r15,r15,lo16(_regs_R)
	addu	 r13,r13,8
	st	 r13,r15[r12]
@L1762:
	ld	 r12,r31,92
	extu	 r4,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r4,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1763
	or.u	 r14,r0,hi16(_spec_regs_R)
@L9343:
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r4,r14[r4]
	br.n	 @L1739
	st	 r4,r31,196
	align	 4
@L1763:
	or.u	 r15,r0,hi16(_regs_R)
@L9344:
	or	 r15,r15,lo16(_regs_R)
	ld	 r4,r15[r4]
	br.n	 @L1739
	st	 r4,r31,196
	align	 4
@L1765:
	ld	 r11,r31,92
	or	 r13,r0,1
	st	 r0,r31,164
	extu	 r14,r11,0<24>
	st	 r0,r31,172
	extu	 r12,r11,0<29>
	st	 r14,r31,180
	mask	 r10,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r17,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1768
	st	 r0,r31,188
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L8780
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L1768:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L8780:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1770
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
	bb0.n	 ne,r13,@L1808
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L1770:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb1.n	 ne,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L1808
	align	 4
@L1772:
	ld	 r11,r31,92
	or	 r13,r0,1
	st	 r0,r31,164
	extu	 r14,r11,0<24>
	st	 r0,r31,172
	extu	 r12,r11,0<29>
	st	 r14,r31,180
	mask	 r10,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r17,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1775
	st	 r0,r31,188
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L8781
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L1775:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L8781:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1777
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
	bb1.n	 ne,r13,@L1808
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L1777:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb0.n	 ne,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L1808
	align	 4
@L1779:
	ld	 r12,r31,92
	or	 r13,r0,1
	st	 r0,r31,164
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r12,r12,0<29>
	st	 r14,r31,180
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or	 r17,r0,0
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1782
	st	 r0,r31,188
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 le0,r13,@L1808
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L1782:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 gt0,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L1808
	align	 4
@L1784:
	ld	 r12,r31,92
	or	 r13,r0,1
	st	 r0,r31,164
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r12,r12,0<29>
	st	 r14,r31,180
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or	 r17,r0,0
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1787
	st	 r0,r31,188
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 gt0,r13,@L1808
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L1787:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 le0,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L1808
	align	 4
@L1789:
	ld	 r12,r31,92
	or	 r13,r0,1
	st	 r0,r31,164
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r12,r12,0<29>
	st	 r14,r31,180
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or	 r17,r0,0
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1792
	st	 r0,r31,188
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 lt0,r13,@L1808
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L1792:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 ge0,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L1808
	align	 4
@L1794:
	ld	 r12,r31,92
	or	 r13,r0,1
	st	 r0,r31,164
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r12,r12,0<29>
	st	 r14,r31,180
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or	 r17,r0,0
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1797
	st	 r0,r31,188
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 ge0,r13,@L1808
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L1797:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r14]
	bcnd.n	 lt0,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L1808
	align	 4
@L1799:
	or.u	 r13,r0,hi16(_use_spec_FCC)
	ld	 r13,r13,lo16(_use_spec_FCC)
	st	 r0,r31,164
	st	 r0,r31,172
	or	 r14,r0,66
	st	 r14,r31,180
	or	 r17,r0,0
	bcnd.n	 eq0,r13,@L1802
	st	 r0,r31,188
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	ld	 r13,r13,lo16(_spec_regs_FCC)
	br	 @L8782
	align	 4
@L1802:
	or.u	 r13,r0,hi16(_regs_FCC)
	ld	 r13,r13,lo16(_regs_FCC)
@L8782:
	bcnd.n	 ne0,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L1808
	align	 4
@L1804:
	or.u	 r13,r0,hi16(_use_spec_FCC)
	ld	 r13,r13,lo16(_use_spec_FCC)
	st	 r0,r31,164
	st	 r0,r31,172
	or	 r15,r0,66
	st	 r15,r31,180
	or	 r17,r0,0
	bcnd.n	 eq0,r13,@L1807
	st	 r0,r31,188
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	ld	 r13,r13,lo16(_spec_regs_FCC)
	bcnd.n	 ne0,r13,@L1808
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L1807:
	or.u	 r13,r0,hi16(_regs_FCC)
	ld	 r13,r13,lo16(_regs_FCC)
	bcnd.n	 eq0,r13,@L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
@L1808:
	ld.h	 r13,r31,94
	or.u	 r12,r0,hi16(_regs_PC)
	mak	 r13,r13,0<2>
	ld	 r12,r12,lo16(_regs_PC)
	addu	 r13,r13,8
	addu	 r12,r12,r13
	br.n	 @L1739
	st	 r12,r31,196
	align	 4
@L1809:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r14,r12,8<16>
	bcnd.n	 eq0,r9,@L1810
	st	 r14,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L1810:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L1812
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L1814
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9348
	addu	 r13,r13,r12
	align	 4
@L1814:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9348:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1813
	st	 r13,r15[r9]
	align	 4
@L1812:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L1816
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9349
	addu	 r13,r13,r12
	align	 4
@L1816:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9349:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L1813:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1818
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r21,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L1820
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9350
	addu	 r24,r13,r3
	align	 4
@L1820:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9350:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1822
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L1828
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9351
	extu	 r13,r24,0<24>
@L1828:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L1823
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L1823
	extu	 r13,r24,0<24>
@L9351:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L1831
	or	 r11,r0,0
@L1833:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L1832
	bcnd	 eq0,r11,@L1831
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L1831
	st	 r10,r12[r25]
	align	 4
@L1832:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L1833
@L1831:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L1837
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9352
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L1838
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L1838:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9352:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L1837
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L1837:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L1851
	bb0.n	 gt,r13,@L1842
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L1860
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L1869
	or.u	 r2,r0,hi16(@LC509)
	br	 @L1882
	align	 4
@L1842:
	bcnd	 eq0,r10,@L1844
	ld.bu	 r13,r10,8
@L8786:
	br.n	 @L1823
	st.b	 r13,r31,96
	align	 4
@L1844:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1846
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1849
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1849:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L1823
	st.b	 r13,r31,96
	align	 4
@L1846:
	br.n	 @L8786
	or	 r13,r0,0
	align	 4
@L1851:
	bcnd	 eq0,r10,@L1853
	ld.hu	 r13,r10,8
@L8787:
	br.n	 @L1823
	st.h	 r13,r31,96
	align	 4
@L1853:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1855
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1858
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1858:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L1823
	st.h	 r13,r31,96
	align	 4
@L1855:
	br.n	 @L8787
	or	 r13,r0,0
	align	 4
@L1860:
	bcnd	 eq0,r10,@L1862
	ld	 r13,r10,8
@L8788:
	br.n	 @L1823
	st	 r13,r31,96
	align	 4
@L1862:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1864
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1867
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1867:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L1823
	st	 r13,r31,96
	align	 4
@L1864:
	br.n	 @L8788
	or	 r13,r0,0
	align	 4
@L1869:
	bcnd	 eq0,r10,@L1871
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,96
	br.n	 @L1823
	st	 r12,r31,100
	align	 4
@L1871:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1873
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1876
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1876:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9353
	addu	 r23,r24,4
	align	 4
@L1873:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9353:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L1877
	st	 r12,r31,96
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L1880
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L1880:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L1823
	st	 r13,r31,100
	align	 4
@L1877:
	or	 r13,r0,0
	br.n	 @L1823
	st	 r13,r31,100
	align	 4
@L1882:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L1822:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,96
	bsr.n	 _mem_access
	or	 r5,r0,1
@L1823:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld.b	 r13,r31,96
	br.n	 @L8789
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L1818:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r21,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L1885
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9354
	addu	 r24,r13,r3
	align	 4
@L1885:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9354:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1887
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L1893
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9355
	extu	 r13,r24,0<24>
@L1893:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L1888
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L1888
	extu	 r13,r24,0<24>
@L9355:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L1896
	or	 r11,r0,0
@L1898:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L1897
	bcnd	 eq0,r11,@L1896
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L1896
	st	 r10,r12[r25]
	align	 4
@L1897:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L1898
@L1896:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L1902
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9356
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L1903
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L1903:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9356:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L1902
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L1902:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L1916
	bb0.n	 gt,r13,@L1907
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L1925
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L1934
	or.u	 r2,r0,hi16(@LC509)
	br	 @L1947
	align	 4
@L1907:
	bcnd	 eq0,r10,@L1909
	ld.bu	 r13,r10,8
@L8791:
	br.n	 @L1888
	st.b	 r13,r31,96
	align	 4
@L1909:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1911
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1914
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1914:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L1888
	st.b	 r13,r31,96
	align	 4
@L1911:
	br.n	 @L8791
	or	 r13,r0,0
	align	 4
@L1916:
	bcnd	 eq0,r10,@L1918
	ld.hu	 r13,r10,8
@L8792:
	br.n	 @L1888
	st.h	 r13,r31,96
	align	 4
@L1918:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1920
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1923
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1923:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L1888
	st.h	 r13,r31,96
	align	 4
@L1920:
	br.n	 @L8792
	or	 r13,r0,0
	align	 4
@L1925:
	bcnd	 eq0,r10,@L1927
	ld	 r13,r10,8
@L8793:
	br.n	 @L1888
	st	 r13,r31,96
	align	 4
@L1927:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1929
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1932
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1932:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L1888
	st	 r13,r31,96
	align	 4
@L1929:
	br.n	 @L8793
	or	 r13,r0,0
	align	 4
@L1934:
	bcnd	 eq0,r10,@L1936
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,96
	br.n	 @L1888
	st	 r12,r31,100
	align	 4
@L1936:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1938
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1941
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1941:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9357
	addu	 r23,r24,4
	align	 4
@L1938:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9357:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L1942
	st	 r12,r31,96
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L1945
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L1945:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L1888
	st	 r13,r31,100
	align	 4
@L1942:
	or	 r13,r0,0
	br.n	 @L1888
	st	 r13,r31,100
	align	 4
@L1947:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L1887:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,96
	bsr.n	 _mem_access
	or	 r5,r0,1
@L1888:
	or.u	 r15,r0,hi16(_regs_R)
	ld.b	 r13,r31,96
	or	 r15,r15,lo16(_regs_R)
@L8789:
	st	 r13,r15[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1950
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L1950:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L1956:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L1957
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L1957:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L1959
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L1961
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9360
	addu	 r13,r13,r12
	align	 4
@L1961:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9360:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1960
	st	 r13,r14[r9]
	align	 4
@L1959:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L1963
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9361
	addu	 r13,r13,r12
	align	 4
@L1963:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9361:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L1960:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1965
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r21,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L1967
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9362
	addu	 r24,r13,r3
	align	 4
@L1967:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r24,r13,r3
@L9362:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L1969
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L1975
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9363
	extu	 r13,r24,0<24>
@L1975:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L1970
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L1970
	extu	 r13,r24,0<24>
@L9363:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L1978
	or	 r11,r0,0
@L1980:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L1979
	bcnd	 eq0,r11,@L1978
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L1978
	st	 r10,r12[r25]
	align	 4
@L1979:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L1980
@L1978:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L1984
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9364
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L1985
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L1985:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9364:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L1984
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L1984:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L1998
	bb0.n	 gt,r13,@L1989
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L2007
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L2016
	or.u	 r2,r0,hi16(@LC509)
	br	 @L2029
	align	 4
@L1989:
	bcnd	 eq0,r10,@L1991
	ld.bu	 r13,r10,8
@L8801:
	br.n	 @L1970
	st.b	 r13,r31,97
	align	 4
@L1991:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L1993
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L1996
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L1996:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L1970
	st.b	 r13,r31,97
	align	 4
@L1993:
	br.n	 @L8801
	or	 r13,r0,0
	align	 4
@L1998:
	bcnd	 eq0,r10,@L2000
	ld.hu	 r13,r10,8
@L8802:
	br.n	 @L1970
	st.h	 r13,r31,97
	align	 4
@L2000:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2002
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2005
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2005:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L1970
	st.h	 r13,r31,97
	align	 4
@L2002:
	br.n	 @L8802
	or	 r13,r0,0
	align	 4
@L2007:
	bcnd	 eq0,r10,@L2009
	ld	 r13,r10,8
@L8803:
	br.n	 @L1970
	st	 r13,r31,97
	align	 4
@L2009:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2011
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2014
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2014:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L1970
	st	 r13,r31,97
	align	 4
@L2011:
	br.n	 @L8803
	or	 r13,r0,0
	align	 4
@L2016:
	bcnd	 eq0,r10,@L2018
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,97
	br.n	 @L1970
	st	 r12,r31,101
	align	 4
@L2018:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2020
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2023
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2023:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9365
	addu	 r23,r24,4
	align	 4
@L2020:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9365:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L2024
	st	 r12,r31,97
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L2027
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L2027:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L1970
	st	 r13,r31,101
	align	 4
@L2024:
	or	 r13,r0,0
	br.n	 @L1970
	st	 r13,r31,101
	align	 4
@L2029:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L1969:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,97
	bsr.n	 _mem_access
	or	 r5,r0,1
@L1970:
	or.u	 r14,r0,hi16(_spec_regs_R)
	ld.bu	 r13,r31,97
	br.n	 @L8804
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L1965:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r21,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L2032
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9366
	addu	 r24,r13,r3
	align	 4
@L2032:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r24,r13,r3
@L9366:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2034
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2040
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9367
	extu	 r13,r24,0<24>
@L2040:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2035
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2035
	extu	 r13,r24,0<24>
@L9367:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2043
	or	 r11,r0,0
@L2045:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2044
	bcnd	 eq0,r11,@L2043
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2043
	st	 r10,r12[r25]
	align	 4
@L2044:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2045
@L2043:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2049
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9368
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2050
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2050:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9368:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2049
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2049:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L2063
	bb0.n	 gt,r13,@L2054
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L2072
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L2081
	or.u	 r2,r0,hi16(@LC509)
	br	 @L2094
	align	 4
@L2054:
	bcnd	 eq0,r10,@L2056
	ld.bu	 r13,r10,8
@L8806:
	br.n	 @L2035
	st.b	 r13,r31,97
	align	 4
@L2056:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2058
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2061
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2061:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L2035
	st.b	 r13,r31,97
	align	 4
@L2058:
	br.n	 @L8806
	or	 r13,r0,0
	align	 4
@L2063:
	bcnd	 eq0,r10,@L2065
	ld.hu	 r13,r10,8
@L8807:
	br.n	 @L2035
	st.h	 r13,r31,97
	align	 4
@L2065:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2067
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2070
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2070:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2035
	st.h	 r13,r31,97
	align	 4
@L2067:
	br.n	 @L8807
	or	 r13,r0,0
	align	 4
@L2072:
	bcnd	 eq0,r10,@L2074
	ld	 r13,r10,8
@L8808:
	br.n	 @L2035
	st	 r13,r31,97
	align	 4
@L2074:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2076
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2079
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2079:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L2035
	st	 r13,r31,97
	align	 4
@L2076:
	br.n	 @L8808
	or	 r13,r0,0
	align	 4
@L2081:
	bcnd	 eq0,r10,@L2083
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,97
	br.n	 @L2035
	st	 r12,r31,101
	align	 4
@L2083:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2085
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2088
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2088:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9369
	addu	 r23,r24,4
	align	 4
@L2085:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9369:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L2089
	st	 r12,r31,97
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L2092
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L2092:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L2035
	st	 r13,r31,101
	align	 4
@L2089:
	or	 r13,r0,0
	br.n	 @L2035
	st	 r13,r31,101
	align	 4
@L2094:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2034:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,97
	bsr.n	 _mem_access
	or	 r5,r0,1
@L2035:
	or.u	 r14,r0,hi16(_regs_R)
	ld.bu	 r13,r31,97
	or	 r14,r14,lo16(_regs_R)
@L8804:
	st	 r13,r14[r21]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2097
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L2097:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L2101:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L2103:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r14,r12,8<16>
	bcnd.n	 eq0,r9,@L2104
	st	 r14,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L2104:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L2106
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L2108
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9371
	addu	 r13,r13,r12
	align	 4
@L2108:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9371:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L2107
	st	 r13,r15[r9]
	align	 4
@L2106:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+20)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+20)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L2110
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9372
	addu	 r13,r13,r12
	align	 4
@L2110:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9372:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L2107:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2112
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r22,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L2114
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9373
	addu	 r24,r13,r3
	align	 4
@L2114:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9373:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2116
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L2117
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2122
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9374
	extu	 r13,r24,0<24>
@L2122:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2117
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2117
	extu	 r13,r24,0<24>
@L9374:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2125
	or	 r11,r0,0
@L2127:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2126
	bcnd	 eq0,r11,@L2125
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2125
	st	 r10,r12[r25]
	align	 4
@L2126:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2127
@L2125:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2145
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9375
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2132
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2132:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9375:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2145
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2145:
	bcnd	 eq0,r10,@L2147
	ld.hu	 r13,r10,8
@L8815:
	br.n	 @L2117
	st.h	 r13,r31,98
	align	 4
@L2147:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2149
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2152
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2152:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2117
	st.h	 r13,r31,98
	align	 4
@L2149:
	br.n	 @L8815
	or	 r13,r0,0
	align	 4
@L2116:
	or	 r3,r0,r24
	addu	 r4,r31,98
	bsr.n	 _mem_access
	or	 r5,r0,2
@L2117:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld.h	 r13,r31,98
	br.n	 @L8816
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L2112:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r22,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L2179
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9376
	addu	 r24,r13,r3
	align	 4
@L2179:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9376:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2181
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L2182
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2187
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9377
	extu	 r13,r24,0<24>
@L2187:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2182
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2182
	extu	 r13,r24,0<24>
@L9377:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2190
	or	 r11,r0,0
@L2192:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2191
	bcnd	 eq0,r11,@L2190
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2190
	st	 r10,r12[r25]
	align	 4
@L2191:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2192
@L2190:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2210
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9378
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2197
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2197:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9378:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2210
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2210:
	bcnd	 eq0,r10,@L2212
	ld.hu	 r13,r10,8
@L8818:
	br.n	 @L2182
	st.h	 r13,r31,98
	align	 4
@L2212:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2214
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2217
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2217:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2182
	st.h	 r13,r31,98
	align	 4
@L2214:
	br.n	 @L8818
	or	 r13,r0,0
	align	 4
@L2181:
	or	 r3,r0,r24
	addu	 r4,r31,98
	bsr.n	 _mem_access
	or	 r5,r0,2
@L2182:
	or.u	 r15,r0,hi16(_regs_R)
	ld.h	 r13,r31,98
	or	 r15,r15,lo16(_regs_R)
@L8816:
	st	 r13,r15[r22]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2244
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L2244:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L2250:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L2251
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L2251:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L2253
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L2255
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9379
	addu	 r13,r13,r12
	align	 4
@L2255:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9379:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L2254
	st	 r13,r14[r9]
	align	 4
@L2253:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+20)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+20)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L2257
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9380
	addu	 r13,r13,r12
	align	 4
@L2257:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9380:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L2254:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2259
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r22,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L2261
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9381
	addu	 r24,r13,r3
	align	 4
@L2261:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r24,r13,r3
@L9381:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2263
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L2264
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2269
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9382
	extu	 r13,r24,0<24>
@L2269:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2264
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2264
	extu	 r13,r24,0<24>
@L9382:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2272
	or	 r11,r0,0
@L2274:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2273
	bcnd	 eq0,r11,@L2272
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2272
	st	 r10,r12[r25]
	align	 4
@L2273:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2274
@L2272:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2292
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9383
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2279
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2279:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9383:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2292
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2292:
	bcnd	 eq0,r10,@L2294
	ld.hu	 r13,r10,8
@L8824:
	br.n	 @L2264
	st.h	 r13,r31,100
	align	 4
@L2294:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2296
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2299
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2299:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2264
	st.h	 r13,r31,100
	align	 4
@L2296:
	br.n	 @L8824
	or	 r13,r0,0
	align	 4
@L2263:
	or	 r3,r0,r24
	addu	 r4,r31,100
	bsr.n	 _mem_access
	or	 r5,r0,2
@L2264:
	or.u	 r14,r0,hi16(_spec_regs_R)
	ld.hu	 r13,r31,100
	br.n	 @L8825
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L2259:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r22,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L2326
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9384
	addu	 r24,r13,r3
	align	 4
@L2326:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r24,r13,r3
@L9384:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2328
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L2329
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2334
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9385
	extu	 r13,r24,0<24>
@L2334:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2329
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2329
	extu	 r13,r24,0<24>
@L9385:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2337
	or	 r11,r0,0
@L2339:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2338
	bcnd	 eq0,r11,@L2337
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2337
	st	 r10,r12[r25]
	align	 4
@L2338:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2339
@L2337:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2357
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9386
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2344
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2344:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9386:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2357
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2357:
	bcnd	 eq0,r10,@L2359
	ld.hu	 r13,r10,8
@L8827:
	br.n	 @L2329
	st.h	 r13,r31,100
	align	 4
@L2359:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2361
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2364
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2364:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2329
	st.h	 r13,r31,100
	align	 4
@L2361:
	br.n	 @L8827
	or	 r13,r0,0
	align	 4
@L2328:
	or	 r3,r0,r24
	addu	 r4,r31,100
	bsr.n	 _mem_access
	or	 r5,r0,2
@L2329:
	or.u	 r14,r0,hi16(_regs_R)
	ld.hu	 r13,r31,100
	or	 r14,r14,lo16(_regs_R)
@L8825:
	st	 r13,r14[r22]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2391
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L2391:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L2397:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r14,r12,8<16>
	bcnd.n	 eq0,r9,@L2398
	st	 r14,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L2398:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L2400
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L2402
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9387
	addu	 r13,r13,r12
	align	 4
@L2402:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9387:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L2401
	st	 r13,r15[r9]
	align	 4
@L2400:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L2404
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9388
	addu	 r13,r13,r12
	align	 4
@L2404:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9388:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L2401:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2406
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r21,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L2408
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9389
	addu	 r24,r13,r3
	align	 4
@L2408:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9389:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2410
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L2411
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2416
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9390
	extu	 r13,r24,0<24>
@L2416:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2411
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2411
	extu	 r13,r24,0<24>
@L9390:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2419
	or	 r11,r0,0
@L2421:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2420
	bcnd	 eq0,r11,@L2419
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2419
	st	 r10,r12[r25]
	align	 4
@L2420:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2421
@L2419:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2425
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9391
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2426
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2426:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9391:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2425
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2425:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L2439
	bb1.n	 gt,r13,@L2448
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L2470
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L2432
	ld.bu	 r13,r10,8
@L8833:
	br.n	 @L2411
	st.b	 r13,r31,104
	align	 4
@L2432:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2434
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2437
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2437:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L2411
	st.b	 r13,r31,104
	align	 4
@L2434:
	br.n	 @L8833
	or	 r13,r0,0
	align	 4
@L2439:
	bcnd	 eq0,r10,@L2441
	ld.hu	 r13,r10,8
@L8834:
	br.n	 @L2411
	st.h	 r13,r31,104
	align	 4
@L2441:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2443
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2446
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2446:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2411
	st.h	 r13,r31,104
	align	 4
@L2443:
	br.n	 @L8834
	or	 r13,r0,0
	align	 4
@L2448:
	bcnd	 eq0,r10,@L2450
	ld	 r13,r10,8
@L8835:
	br.n	 @L2411
	st	 r13,r31,104
	align	 4
@L2450:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2452
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2455
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2455:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L2411
	st	 r13,r31,104
	align	 4
@L2452:
	br.n	 @L8835
	or	 r13,r0,0
	align	 4
@L2470:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2410:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L2411:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r13,r31,104
	br.n	 @L8836
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L2406:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r21,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L2473
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9392
	addu	 r24,r13,r3
	align	 4
@L2473:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9392:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2475
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L2476
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2481
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9393
	extu	 r13,r24,0<24>
@L2481:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2476
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2476
	extu	 r13,r24,0<24>
@L9393:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2484
	or	 r11,r0,0
@L2486:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2485
	bcnd	 eq0,r11,@L2484
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2484
	st	 r10,r12[r25]
	align	 4
@L2485:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2486
@L2484:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2490
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9394
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2491
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2491:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9394:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2490
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2490:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L2504
	bb1.n	 gt,r13,@L2513
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L2535
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L2497
	ld.bu	 r13,r10,8
@L8838:
	br.n	 @L2476
	st.b	 r13,r31,104
	align	 4
@L2497:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2499
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2502
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2502:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L2476
	st.b	 r13,r31,104
	align	 4
@L2499:
	br.n	 @L8838
	or	 r13,r0,0
	align	 4
@L2504:
	bcnd	 eq0,r10,@L2506
	ld.hu	 r13,r10,8
@L8839:
	br.n	 @L2476
	st.h	 r13,r31,104
	align	 4
@L2506:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2508
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2511
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2511:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2476
	st.h	 r13,r31,104
	align	 4
@L2508:
	br.n	 @L8839
	or	 r13,r0,0
	align	 4
@L2513:
	bcnd	 eq0,r10,@L2515
	ld	 r13,r10,8
@L8840:
	br.n	 @L2476
	st	 r13,r31,104
	align	 4
@L2515:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2517
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2520
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2520:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L2476
	st	 r13,r31,104
	align	 4
@L2517:
	br.n	 @L8840
	or	 r13,r0,0
	align	 4
@L2535:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2475:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L2476:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r13,r31,104
	or	 r15,r15,lo16(_regs_R)
@L8836:
	st	 r13,r15[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2538
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L2538:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L2544:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	extu	 r13,r12,0<16>
	st	 r0,r31,172
	mask	 r13,r13,254
	bcnd.n	 eq0,r9,@L2545
	st	 r13,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L2545:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L2547
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L2549
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9395
	addu	 r13,r13,r12
	align	 4
@L2549:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9395:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L2548
	st	 r13,r15[r9]
	align	 4
@L2547:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L2551
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9396
	addu	 r13,r13,r12
	align	 4
@L2551:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9396:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L2548:
	ld.hu	 r13,r31,92
	bb0.n	 (31-31),r13,@L2554
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L2554
	or.u	 r2,r0,hi16(@LC540)
	or	 r4,r0,323
	or.u	 r3,r0,hi16(@LC541)
	or.u	 r5,r0,hi16(@LC542)
	or	 r2,r2,lo16(@LC540)
	or	 r3,r3,lo16(@LC541)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC542)
	align	 4
@L2554:
	ld	 r13,r31,92
	extu	 r10,r13,0<24>
	or	 r12,r0,1
	extu	 r13,r13,0<29>
	mask	 r11,r10,31
	ld	 r13,r18[r13]
	mak	 r12,r12,r11
	or.u	 r11,r0,hi16(_temp_bs)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L2557
	or	 r12,r11,lo16(_temp_bs)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r11,r14[r10]
	br.n	 @L9397
	or.u	 r14,r0,hi16(_spec_mode)
	align	 4
@L2557:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r11,r15[r10]
	or.u	 r14,r0,hi16(_spec_mode)
@L9397:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2559
	st	 r11,r0,r12
	ld.bu	 r11,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r9,r13,lo16(_sim_swap_words)
	xor	 r11,r11,r9
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r10]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	st	 r12,r18[r10]
	ld	 r13,r31,92
	or.u	 r12,r0,hi16(_temp_bs)
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r11,r13,16<0>
	or	 r22,r0,4
	extu	 r13,r13,8<16>
	addu	 r24,r12,r11
	xor	 r21,r13,r9
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L2562
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2567
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9398
	extu	 r13,r24,0<24>
@L2567:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2562
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2562
	extu	 r13,r24,0<24>
@L9398:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2570
	or	 r11,r0,0
@L2572:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2571
	bcnd	 eq0,r11,@L2570
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2570
	st	 r10,r12[r25]
	align	 4
@L2571:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2572
@L2570:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2576
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9399
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2577
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2577:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9399:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2576
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2576:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L2590
	bb1.n	 gt,r13,@L2599
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L2621
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L2583
	ld.bu	 r13,r10,8
@L8845:
	br.n	 @L2562
	st.b	 r13,r31,104
	align	 4
@L2583:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2585
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2588
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2588:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L2562
	st.b	 r13,r31,104
	align	 4
@L2585:
	br.n	 @L8845
	or	 r13,r0,0
	align	 4
@L2590:
	bcnd	 eq0,r10,@L2592
	ld.hu	 r13,r10,8
@L8846:
	br.n	 @L2562
	st.h	 r13,r31,104
	align	 4
@L2592:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2594
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2597
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2597:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2562
	st.h	 r13,r31,104
	align	 4
@L2594:
	br.n	 @L8846
	or	 r13,r0,0
	align	 4
@L2599:
	bcnd	 eq0,r10,@L2601
	ld	 r13,r10,8
@L8847:
	br.n	 @L2562
	st	 r13,r31,104
	align	 4
@L2601:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2603
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2606
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2606:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L2562
	st	 r13,r31,104
	align	 4
@L2603:
	br.n	 @L8847
	or	 r13,r0,0
	align	 4
@L2621:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2562:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r13,r31,104
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L2560
	st	 r13,r15[r21]
	align	 4
@L2559:
	or.u	 r12,r0,hi16(_sim_swap_words)
	ld	 r13,r31,92
	or.u	 r11,r0,hi16(_temp_bs)
	ld	 r10,r12,lo16(_sim_swap_words)
	extu	 r12,r13,8<16>
	ld	 r11,r11,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	xor	 r25,r12,r10
	addu	 r24,r11,r13
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r13,r31,104
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r25]
@L2560:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2687
	or.u	 r12,r0,hi16(_sim_swap_words)
	ld.bu	 r13,r31,93
	ld	 r9,r12,lo16(_sim_swap_words)
	addu	 r13,r13,1
	xor	 r13,r13,r9
	or	 r12,r0,1
	extu	 r10,r13,0<5>
	mask	 r13,r13,31
	ld	 r11,r18[r10]
	mak	 r12,r12,r13
	or	 r11,r11,r12
	st	 r11,r18[r10]
	or.u	 r13,r0,hi16(_temp_bs)
	ld	 r12,r31,92
	ld	 r13,r13,lo16(_temp_bs)
	ext	 r11,r12,16<0>
	or	 r22,r0,4
	extu	 r12,r12,8<16>
	addu	 r13,r13,r11
	addu	 r12,r12,1
	addu	 r24,r13,4
	xor	 r21,r12,r9
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L2690
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2695
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9400
	extu	 r13,r24,0<24>
@L2695:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2690
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2690
	extu	 r13,r24,0<24>
@L9400:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2698
	or	 r11,r0,0
@L2700:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2699
	bcnd	 eq0,r11,@L2698
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2698
	st	 r10,r12[r25]
	align	 4
@L2699:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2700
@L2698:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2704
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9401
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2705
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2705:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9401:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2704
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2704:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L2718
	bb1.n	 gt,r13,@L2727
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L2749
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L2711
	ld.bu	 r13,r10,8
@L8848:
	br.n	 @L2690
	st.b	 r13,r31,104
	align	 4
@L2711:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2713
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2716
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2716:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L2690
	st.b	 r13,r31,104
	align	 4
@L2713:
	br.n	 @L8848
	or	 r13,r0,0
	align	 4
@L2718:
	bcnd	 eq0,r10,@L2720
	ld.hu	 r13,r10,8
@L8849:
	br.n	 @L2690
	st.h	 r13,r31,104
	align	 4
@L2720:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2722
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2725
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2725:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2690
	st.h	 r13,r31,104
	align	 4
@L2722:
	br.n	 @L8849
	or	 r13,r0,0
	align	 4
@L2727:
	bcnd	 eq0,r10,@L2729
	ld	 r13,r10,8
@L8850:
	br.n	 @L2690
	st	 r13,r31,104
	align	 4
@L2729:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2731
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2734
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2734:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L2690
	st	 r13,r31,104
	align	 4
@L2731:
	br.n	 @L8850
	or	 r13,r0,0
	align	 4
@L2749:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2690:
	or.u	 r14,r0,hi16(_spec_regs_R)
	ld	 r13,r31,104
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L2688
	st	 r13,r14[r21]
	align	 4
@L2687:
	ld	 r12,r31,92
	or.u	 r13,r0,hi16(_temp_bs)
	ld	 r10,r13,lo16(_temp_bs)
	extu	 r11,r12,8<16>
	or.u	 r13,r0,hi16(_sim_swap_words)
	ext	 r12,r12,16<0>
	ld	 r13,r13,lo16(_sim_swap_words)
	addu	 r11,r11,1
	addu	 r10,r10,r12
	xor	 r25,r11,r13
	addu	 r24,r10,4
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r13,r31,104
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r25]
@L2688:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2815
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L2815:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L2821:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	extu	 r13,r12,8<16>
	addu	 r13,r13,32
	st	 r0,r31,172
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r9,@L2822
	st	 r13,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L2822:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L2824
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L2826
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9402
	addu	 r13,r13,r12
	align	 4
@L2826:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9402:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L2825
	st	 r13,r15[r9]
	align	 4
@L2824:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L2828
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9403
	addu	 r13,r13,r12
	align	 4
@L2828:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9403:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L2825:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2830
	or.u	 r10,r0,hi16(_use_spec_F)
	ld.hu	 r11,r31,92
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r12,r11,30
	extu	 r11,r11,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r11]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r10[r11]
	ld	 r11,r31,92
	extu	 r13,r11,0<29>
	extu	 r8,r11,0<24>
	ld	 r10,r18[r13]
	mask	 r13,r8,31
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r20,r12,lo16(_spec_regs_F)
	extu	 r21,r11,8<16>
	and	 r10,r10,r9
	bcnd.n	 eq0,r10,@L2832
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9404
	addu	 r24,r13,r3
	align	 4
@L2832:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r13,r3
@L9404:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2899
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L2900
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2840
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9405
	extu	 r13,r24,0<24>
@L2840:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2900
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2900
	extu	 r13,r24,0<24>
@L9405:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2843
	or	 r11,r0,0
@L2845:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2844
	bcnd	 eq0,r11,@L2843
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2843
	st	 r10,r12[r25]
	align	 4
@L2844:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2845
@L2843:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2849
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9406
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2850
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2850:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9406:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2849
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2849:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L2863
	bb1.n	 gt,r13,@L2872
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L2894
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L9315
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2858
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2861
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2861:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L2900
	st.b	 r13,r31,104
	align	 4
@L2858:
	br.n	 @L8860
	or	 r13,r0,0
	align	 4
@L2863:
	bcnd.n	 ne0,r10,@L9316
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2867
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2870
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2870:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2900
	st.h	 r13,r31,104
	align	 4
@L2867:
	br.n	 @L8861
	or	 r13,r0,0
	align	 4
@L2872:
	bcnd.n	 ne0,r10,@L9317
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2876
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2879
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2879:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L2900
	st	 r13,r31,104
	align	 4
@L2876:
	br.n	 @L8862
	or	 r13,r0,0
	align	 4
@L2894:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2830:
	ld	 r11,r31,92
	extu	 r13,r11,0<29>
	extu	 r8,r11,0<24>
	ld	 r10,r18[r13]
	mask	 r9,r8,31
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_regs_F)
	mak	 r13,r13,r9
	or	 r20,r12,lo16(_regs_F)
	extu	 r21,r11,8<16>
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L2897
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9407
	addu	 r24,r13,r3
	align	 4
@L2897:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	addu	 r24,r13,r3
@L9407:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2899
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L2900
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2905
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9408
	extu	 r13,r24,0<24>
@L2905:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L2900
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L2900
	extu	 r13,r24,0<24>
@L9408:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2908
	or	 r11,r0,0
@L2910:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2909
	bcnd	 eq0,r11,@L2908
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2908
	st	 r10,r12[r25]
	align	 4
@L2909:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2910
@L2908:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L2914
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9409
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L2915
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L2915:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9409:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L2914
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L2914:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L2928
	bb1.n	 gt,r13,@L2937
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L2959
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L2921
	or.u	 r13,r0,hi16(_mem_table)
@L9315:
	ld.bu	 r13,r10,8
@L8860:
	br.n	 @L2900
	st.b	 r13,r31,104
	align	 4
@L2921:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2923
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2926
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2926:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L2900
	st.b	 r13,r31,104
	align	 4
@L2923:
	br.n	 @L8860
	or	 r13,r0,0
	align	 4
@L2928:
	bcnd.n	 eq0,r10,@L2930
	or.u	 r13,r0,hi16(_mem_table)
@L9316:
	ld.hu	 r13,r10,8
@L8861:
	br.n	 @L2900
	st.h	 r13,r31,104
	align	 4
@L2930:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2932
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2935
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2935:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L2900
	st.h	 r13,r31,104
	align	 4
@L2932:
	br.n	 @L8861
	or	 r13,r0,0
	align	 4
@L2937:
	bcnd.n	 eq0,r10,@L2939
	or.u	 r13,r0,hi16(_mem_table)
@L9317:
	ld	 r13,r10,8
@L8862:
	br.n	 @L2900
	st	 r13,r31,104
	align	 4
@L2939:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L2941
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L2944
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L2944:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L2900
	st	 r13,r31,104
	align	 4
@L2941:
	br.n	 @L8862
	or	 r13,r0,0
	align	 4
@L2959:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2899:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L2900:
	ld	 r13,r31,104
	st	 r13,r20[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2962
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L2962:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L2968:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	extu	 r13,r12,8<16>
	addu	 r13,r13,32
	st	 r0,r31,172
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r9,@L2969
	st	 r13,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L2969:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,180
	or	 r11,r0,r12
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L2971
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L2973
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9410
	addu	 r13,r13,r12
	align	 4
@L2973:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9410:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L2972
	st	 r13,r15[r9]
	align	 4
@L2971:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L2975
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9411
	addu	 r13,r13,r12
	align	 4
@L2975:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9411:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L2972:
	ld	 r11,r31,92
	extu	 r7,r11,0<16>
	bb0.n	 (31-31),r7,@L2978
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8665
	or.u	 r2,r0,hi16(@LC543)
	or	 r4,r0,336
	or.u	 r3,r0,hi16(@LC544)
	or.u	 r5,r0,hi16(@LC545)
	or	 r2,r2,lo16(@LC543)
	or	 r3,r3,lo16(@LC544)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC545)
	align	 4
@L2978:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L2981
	extu	 r12,r11,0<29>
@L8665:
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r8,r13,lo16(_sim_swap_words)
	or.u	 r10,r0,hi16(_use_spec_F)
	xor	 r12,r12,r8
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,0<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r11,r31,92
	extu	 r13,r11,0<29>
	extu	 r12,r11,8<16>
	extu	 r7,r11,0<24>
	ext	 r3,r11,16<0>
	ld	 r10,r18[r13]
	mask	 r13,r7,31
	xor	 r21,r12,r8
	mak	 r9,r9,r13
	or.u	 r12,r0,hi16(_spec_regs_F)
	and	 r10,r10,r9
	bcnd.n	 eq0,r10,@L2983
	or	 r20,r12,lo16(_spec_regs_F)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r7]
	br.n	 @L9412
	addu	 r24,r13,r3
	align	 4
@L2983:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r7]
	addu	 r24,r13,r3
@L9412:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3050
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3051
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L2991
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9413
	extu	 r13,r24,0<24>
@L2991:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3051
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3051
	extu	 r13,r24,0<24>
@L9413:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L2994
	or	 r11,r0,0
@L2996:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L2995
	bcnd	 eq0,r11,@L2994
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L2994
	st	 r10,r12[r25]
	align	 4
@L2995:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L2996
@L2994:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L3000
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9414
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3001
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3001:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9414:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3000
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3000:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L3014
	bb1.n	 gt,r13,@L3023
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3045
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L9318
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3009
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3012
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3012:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L3051
	st.b	 r13,r31,104
	align	 4
@L3009:
	br.n	 @L8872
	or	 r13,r0,0
	align	 4
@L3014:
	bcnd.n	 ne0,r10,@L9319
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3018
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3021
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3021:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L3051
	st.h	 r13,r31,104
	align	 4
@L3018:
	br.n	 @L8873
	or	 r13,r0,0
	align	 4
@L3023:
	bcnd.n	 ne0,r10,@L9320
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3027
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3030
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3030:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L3051
	st	 r13,r31,104
	align	 4
@L3027:
	br.n	 @L8874
	or	 r13,r0,0
	align	 4
@L3045:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L2981:
	or.u	 r13,r0,hi16(_sim_swap_words)
	extu	 r8,r11,0<24>
	ld	 r9,r13,lo16(_sim_swap_words)
	ext	 r3,r11,16<0>
	ld	 r10,r18[r12]
	mask	 r11,r8,31
	or	 r13,r0,1
	mask	 r12,r7,255
	mak	 r13,r13,r11
	xor	 r21,r12,r9
	or.u	 r12,r0,hi16(_regs_F)
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L3048
	or	 r20,r12,lo16(_regs_F)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9415
	addu	 r24,r13,r3
	align	 4
@L3048:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r13,r3
@L9415:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3050
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3051
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L3056
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9416
	extu	 r13,r24,0<24>
@L3056:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3051
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3051
	extu	 r13,r24,0<24>
@L9416:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3059
	or	 r11,r0,0
@L3061:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3060
	bcnd	 eq0,r11,@L3059
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3059
	st	 r10,r12[r25]
	align	 4
@L3060:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3061
@L3059:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L3065
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9417
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3066
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3066:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9417:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3065
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3065:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L3079
	bb1.n	 gt,r13,@L3088
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3110
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L3072
	or.u	 r13,r0,hi16(_mem_table)
@L9318:
	ld.bu	 r13,r10,8
@L8872:
	br.n	 @L3051
	st.b	 r13,r31,104
	align	 4
@L3072:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3074
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3077
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3077:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L3051
	st.b	 r13,r31,104
	align	 4
@L3074:
	br.n	 @L8872
	or	 r13,r0,0
	align	 4
@L3079:
	bcnd.n	 eq0,r10,@L3081
	or.u	 r13,r0,hi16(_mem_table)
@L9319:
	ld.hu	 r13,r10,8
@L8873:
	br.n	 @L3051
	st.h	 r13,r31,104
	align	 4
@L3081:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3083
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3086
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3086:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L3051
	st.h	 r13,r31,104
	align	 4
@L3083:
	br.n	 @L8873
	or	 r13,r0,0
	align	 4
@L3088:
	bcnd.n	 eq0,r10,@L3090
	or.u	 r13,r0,hi16(_mem_table)
@L9320:
	ld	 r13,r10,8
@L8874:
	br.n	 @L3051
	st	 r13,r31,104
	align	 4
@L3090:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3092
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3095
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3095:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L3051
	st	 r13,r31,104
	align	 4
@L3092:
	br.n	 @L8874
	or	 r13,r0,0
	align	 4
@L3110:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3050:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3051:
	ld	 r13,r31,104
	st	 r13,r20[r21]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3113
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld.bu	 r12,r31,93
	or.u	 r11,r0,hi16(_use_spec_F)
	ld	 r8,r13,lo16(_sim_swap_words)
	addu	 r12,r12,1
	or	 r10,r0,1
	xor	 r12,r12,r8
	or	 r11,r11,lo16(_use_spec_F)
	extu	 r9,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r11[r9]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r11[r9]
	ld	 r13,r31,92
	extu	 r11,r13,8<16>
	extu	 r12,r13,0<29>
	extu	 r9,r13,0<24>
	ext	 r7,r13,16<0>
	ld	 r12,r18[r12]
	mask	 r13,r9,31
	addu	 r11,r11,1
	mak	 r10,r10,r13
	xor	 r21,r11,r8
	or.u	 r13,r0,hi16(_spec_regs_F)
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L3115
	or	 r20,r13,lo16(_spec_regs_F)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9418
	addu	 r13,r13,r7
	align	 4
@L3115:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r7
@L9418:
	addu	 r24,r13,4
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3182
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3183
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L3123
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9419
	extu	 r13,r24,0<24>
@L3123:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3183
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3183
	extu	 r13,r24,0<24>
@L9419:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3126
	or	 r11,r0,0
@L3128:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3127
	bcnd	 eq0,r11,@L3126
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3126
	st	 r10,r12[r25]
	align	 4
@L3127:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3128
@L3126:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L3132
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9420
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3133
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3133:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9420:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3132
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3132:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L3146
	bb1.n	 gt,r13,@L3155
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3177
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L9321
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3141
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3144
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3144:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L3183
	st.b	 r13,r31,104
	align	 4
@L3141:
	br.n	 @L8880
	or	 r13,r0,0
	align	 4
@L3146:
	bcnd.n	 ne0,r10,@L9322
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3150
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3153
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3153:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L3183
	st.h	 r13,r31,104
	align	 4
@L3150:
	br.n	 @L8881
	or	 r13,r0,0
	align	 4
@L3155:
	bcnd.n	 ne0,r10,@L9323
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3159
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3162
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3162:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L3183
	st	 r13,r31,104
	align	 4
@L3159:
	br.n	 @L8882
	or	 r13,r0,0
	align	 4
@L3177:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3113:
	ld	 r9,r31,92
	extu	 r12,r9,0<29>
	ld	 r11,r13,lo16(_sim_swap_words)
	extu	 r13,r9,8<16>
	extu	 r8,r9,0<24>
	addu	 r13,r13,1
	ext	 r9,r9,16<0>
	xor	 r21,r13,r11
	mask	 r11,r8,31
	or	 r13,r0,1
	ld	 r10,r18[r12]
	mak	 r13,r13,r11
	or.u	 r12,r0,hi16(_regs_F)
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L3180
	or	 r20,r12,lo16(_regs_F)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9421
	addu	 r13,r13,r9
	align	 4
@L3180:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	addu	 r13,r13,r9
@L9421:
	addu	 r24,r13,4
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3182
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3183
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L3188
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9422
	extu	 r13,r24,0<24>
@L3188:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3183
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3183
	extu	 r13,r24,0<24>
@L9422:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3191
	or	 r11,r0,0
@L3193:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3192
	bcnd	 eq0,r11,@L3191
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3191
	st	 r10,r12[r25]
	align	 4
@L3192:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3193
@L3191:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L3197
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9423
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3198
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3198:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9423:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3197
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3197:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L3211
	bb1.n	 gt,r13,@L3220
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3242
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L3204
	or.u	 r13,r0,hi16(_mem_table)
@L9321:
	ld.bu	 r13,r10,8
@L8880:
	br.n	 @L3183
	st.b	 r13,r31,104
	align	 4
@L3204:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3206
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3209
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3209:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L3183
	st.b	 r13,r31,104
	align	 4
@L3206:
	br.n	 @L8880
	or	 r13,r0,0
	align	 4
@L3211:
	bcnd.n	 eq0,r10,@L3213
	or.u	 r13,r0,hi16(_mem_table)
@L9322:
	ld.hu	 r13,r10,8
@L8881:
	br.n	 @L3183
	st.h	 r13,r31,104
	align	 4
@L3213:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3215
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3218
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3218:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L3183
	st.h	 r13,r31,104
	align	 4
@L3215:
	br.n	 @L8881
	or	 r13,r0,0
	align	 4
@L3220:
	bcnd.n	 eq0,r10,@L3222
	or.u	 r13,r0,hi16(_mem_table)
@L9323:
	ld	 r13,r10,8
@L8882:
	br.n	 @L3183
	st	 r13,r31,104
	align	 4
@L3222:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3224
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3227
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3227:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L3183
	st	 r13,r31,104
	align	 4
@L3224:
	br.n	 @L8882
	or	 r13,r0,0
	align	 4
@L3242:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3182:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3183:
	ld	 r13,r31,104
	st	 r13,r20[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3245
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L3245:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L3251:
	ld	 r12,r31,92
	or	 r13,r0,1
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	st	 r0,r31,180
	extu	 r17,r12,0<24>
	st	 r0,r31,188
	extu	 r12,r12,0<29>
	st	 r15,r31,164
	mask	 r11,r17,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_temp_bs)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3252
	or	 r11,r11,lo16(_temp_bs)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9424
	st	 r13,r0,r11
	align	 4
@L3252:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	st	 r13,r0,r11
@L9424:
	or.u	 r13,r0,hi16(_temp_bs)
	ld.h	 r12,r31,94
	ld	 r13,r13,lo16(_temp_bs)
	or.u	 r11,r0,hi16(_ss_lr_temp)
	or.u	 r14,r0,hi16(_spec_mode)
	addu	 r13,r13,r12
	ld	 r12,r14,lo16(_spec_mode)
	or	 r21,r11,lo16(_ss_lr_temp)
	bcnd.n	 eq0,r12,@L3254
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L3260
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9425
	extu	 r13,r24,0<24>
@L3260:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3255
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3255
	extu	 r13,r24,0<24>
@L9425:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3263
	or	 r11,r0,0
@L3265:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3264
	bcnd	 eq0,r11,@L3263
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3263
	st	 r10,r12[r25]
	align	 4
@L3264:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3265
@L3263:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L3269
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9426
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3270
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3270:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9426:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3269
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3269:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L3283
	bb1.n	 gt,r13,@L3292
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3314
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L3276
	ld.bu	 r13,r10,8
@L8885:
	br.n	 @L3255
	st.b	 r13,r31,104
	align	 4
@L3276:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3278
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3281
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3281:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L3255
	st.b	 r13,r31,104
	align	 4
@L3278:
	br.n	 @L8885
	or	 r13,r0,0
	align	 4
@L3283:
	bcnd	 eq0,r10,@L3285
	ld.hu	 r13,r10,8
@L8886:
	br.n	 @L3255
	st.h	 r13,r31,104
	align	 4
@L3285:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3287
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3290
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3290:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L3255
	st.h	 r13,r31,104
	align	 4
@L3287:
	br.n	 @L8886
	or	 r13,r0,0
	align	 4
@L3292:
	bcnd	 eq0,r10,@L3294
	ld	 r13,r10,8
@L8887:
	br.n	 @L3255
	st	 r13,r31,104
	align	 4
@L3294:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3296
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3299
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3299:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L3255
	st	 r13,r31,104
	align	 4
@L3296:
	br.n	 @L8887
	or	 r13,r0,0
	align	 4
@L3314:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3254:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3255:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r31,104
	ld	 r12,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L3317
	st	 r13,r0,r21
	ld.hu	 r12,r31,92
	or	 r9,r0,1
	extu	 r11,r12,3<5>
	mask	 r12,r12,31
	ld	 r13,r18[r11]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	or.u	 r10,r0,hi16(_ss_lr_temp)
	or.u	 r12,r0,hi16(_temp_bs)
	extu	 r11,r13,0<16>
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	mask	 r6,r11,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks)
	extu	 r8,r6,0<5>
	ld	 r7,r13[r12]
	mask	 r11,r11,31
	ld	 r13,r18[r8]
	mak	 r9,r9,r11
	ld	 r12,r10,lo16(_ss_lr_temp)
	and	 r13,r13,r9
	bcnd.n	 eq0,r13,@L3319
	and.c	 r12,r12,r7
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r6]
	br.n	 @L9427
	and	 r13,r13,r7
	align	 4
@L3319:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r6]
	and	 r13,r13,r7
@L9427:
	or	 r13,r13,r12
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r13,r14[r6]
	align	 4
@L3317:
	ld	 r13,r31,92
	or.u	 r12,r0,hi16(_temp_bs)
	extu	 r10,r13,0<16>
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	mask	 r8,r10,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks)
	extu	 r11,r8,0<5>
	ld	 r9,r13[r12]
	mask	 r10,r10,31
	ld	 r11,r18[r11]
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	ld	 r12,r12,lo16(_ss_lr_temp)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3321
	and.c	 r12,r12,r9
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9428
	and	 r13,r13,r9
	align	 4
@L3321:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	and	 r13,r13,r9
@L9428:
	br.n	 @L7269
	or	 r13,r13,r12
	align	 4
@L3323:
	ld	 r12,r31,92
	or	 r13,r0,1
	st	 r0,r31,172
	extu	 r14,r12,8<16>
	st	 r0,r31,180
	extu	 r17,r12,0<24>
	st	 r0,r31,188
	extu	 r12,r12,0<29>
	st	 r14,r31,164
	mask	 r11,r17,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_temp_bs)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3324
	or	 r11,r11,lo16(_temp_bs)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9429
	st	 r13,r0,r11
	align	 4
@L3324:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	st	 r13,r0,r11
@L9429:
	or.u	 r13,r0,hi16(_temp_bs)
	ld.h	 r12,r31,94
	ld	 r13,r13,lo16(_temp_bs)
	or.u	 r11,r0,hi16(_ss_lr_temp)
	or.u	 r15,r0,hi16(_spec_mode)
	addu	 r13,r13,r12
	ld	 r12,r15,lo16(_spec_mode)
	or	 r21,r11,lo16(_ss_lr_temp)
	bcnd.n	 eq0,r12,@L3326
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L3332
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9430
	extu	 r13,r24,0<24>
@L3332:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3327
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3327
	extu	 r13,r24,0<24>
@L9430:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3335
	or	 r11,r0,0
@L3337:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3336
	bcnd	 eq0,r11,@L3335
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3335
	st	 r10,r12[r25]
	align	 4
@L3336:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3337
@L3335:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L3341
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9431
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3342
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3342:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9431:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3341
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3341:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L3355
	bb1.n	 gt,r13,@L3364
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3386
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L3348
	ld.bu	 r13,r10,8
@L8890:
	br.n	 @L3327
	st.b	 r13,r31,104
	align	 4
@L3348:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3350
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3353
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3353:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L3327
	st.b	 r13,r31,104
	align	 4
@L3350:
	br.n	 @L8890
	or	 r13,r0,0
	align	 4
@L3355:
	bcnd	 eq0,r10,@L3357
	ld.hu	 r13,r10,8
@L8891:
	br.n	 @L3327
	st.h	 r13,r31,104
	align	 4
@L3357:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3359
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3362
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3362:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L3327
	st.h	 r13,r31,104
	align	 4
@L3359:
	br.n	 @L8891
	or	 r13,r0,0
	align	 4
@L3364:
	bcnd	 eq0,r10,@L3366
	ld	 r13,r10,8
@L8892:
	br.n	 @L3327
	st	 r13,r31,104
	align	 4
@L3366:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L3368
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L3371
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L3371:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L3327
	st	 r13,r31,104
	align	 4
@L3368:
	br.n	 @L8892
	or	 r13,r0,0
	align	 4
@L3386:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3326:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3327:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r31,104
	ld	 r12,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L3389
	st	 r13,r0,r21
	ld.hu	 r12,r31,92
	or	 r8,r0,1
	extu	 r11,r12,3<5>
	mask	 r12,r12,31
	ld	 r13,r18[r11]
	mak	 r12,r8,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	or.u	 r9,r0,hi16(_ss_lr_temp)
	or.u	 r12,r0,hi16(_temp_bs)
	extu	 r11,r13,0<16>
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	mask	 r6,r11,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks+4)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks+4)
	extu	 r7,r6,0<5>
	ld	 r10,r13[r12]
	mask	 r11,r11,31
	ld	 r13,r18[r7]
	mak	 r8,r8,r11
	ld	 r12,r9,lo16(_ss_lr_temp)
	xor.c	 r11,r10,r0
	and	 r13,r13,r8
	bcnd.n	 eq0,r13,@L3391
	and	 r12,r12,r10
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r6]
	br.n	 @L9432
	and	 r13,r13,r11
	align	 4
@L3391:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r6]
	and	 r13,r13,r11
@L9432:
	or	 r12,r13,r12
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r12,r15[r6]
	align	 4
@L3389:
	ld	 r13,r31,92
	or.u	 r12,r0,hi16(_temp_bs)
	extu	 r10,r13,0<16>
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	mask	 r8,r10,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks+4)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks+4)
	extu	 r11,r8,0<5>
	ld	 r9,r13[r12]
	mask	 r10,r10,31
	ld	 r11,r18[r11]
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	ld	 r12,r12,lo16(_ss_lr_temp)
	xor.c	 r10,r9,r0
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3393
	and	 r12,r12,r9
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9433
	and	 r13,r13,r10
	align	 4
@L3393:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	and	 r13,r13,r10
@L9433:
	or	 r12,r13,r12
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	br.n	 @L1739
	st	 r12,r14[r8]
	align	 4
@L3395:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L3396
	ld.bu	 r15,r31,92
	br.n	 @L3397
	st	 r15,r31,164
	align	 4
@L3396:
	st	 r0,r31,164
@L3397:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	st	 r0,r31,172
	extu	 r14,r11,8<16>
	st	 r0,r31,188
	extu	 r17,r11,0<24>
	bcnd.n	 eq0,r13,@L3398
	st	 r14,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L3400
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9434
	addu	 r13,r13,r12
	align	 4
@L3400:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9434:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L3399
	st	 r13,r14[r9]
	align	 4
@L3398:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3402
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9435
	addu	 r13,r13,r12
	align	 4
@L3402:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9435:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L3399:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3404
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9436
	addu	 r24,r13,r3
	align	 4
@L3404:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9436:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3406
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9437
	or	 r12,r0,r13
	align	 4
@L3406:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	or	 r12,r0,r13
@L9437:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3408
	st.b	 r12,r31,97
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3409
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3409
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3417
	or	 r11,r0,0
@L3419:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3418
	bcnd	 eq0,r11,@L3417
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3417
	st	 r10,r12[r25]
	align	 4
@L3418:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3419
@L3417:
	bcnd.n	 ne0,r10,@L9438
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9439
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3424
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3424:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9439:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3423
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3423:
	cmp	 r13,r22,2
@L9438:
	bb0	 ne,r13,@L3438
	bb0.n	 gt,r13,@L3429
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L3447
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L3456
	or.u	 r2,r0,hi16(@LC509)
	br	 @L3468
	align	 4
@L3429:
	ld.bu	 r13,r31,97
	br.n	 @L3409
	st.b	 r13,r10,8
	align	 4
@L3438:
	ld.hu	 r13,r31,97
	br.n	 @L3409
	st.h	 r13,r10,8
	align	 4
@L3447:
	ld	 r13,r31,97
	br.n	 @L3409
	st	 r13,r10,8
	align	 4
@L3456:
	ld	 r13,r31,97
	st	 r13,r10,8
	ld	 r13,r31,101
	br.n	 @L3409
	st	 r13,r10,12
	align	 4
@L3468:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3408:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,97
	bsr.n	 _mem_access
	or	 r5,r0,1
@L3409:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3471
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L3471:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L3477:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L3478
	ld.bu	 r14,r31,92
	br.n	 @L3479
	st	 r14,r31,164
	align	 4
@L3478:
	st	 r0,r31,164
@L3479:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,172
	extu	 r15,r11,8<16>
	st	 r0,r31,188
	extu	 r17,r11,0<24>
	bcnd.n	 eq0,r13,@L3480
	st	 r15,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L3482
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9440
	addu	 r13,r13,r12
	align	 4
@L3482:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9440:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L3481
	st	 r13,r15[r9]
	align	 4
@L3480:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+20)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+20)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3484
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9441
	addu	 r13,r13,r12
	align	 4
@L3484:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9441:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L3481:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3486
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9442
	addu	 r24,r13,r3
	align	 4
@L3486:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r24,r13,r3
@L9442:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3488
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9443
	or	 r12,r0,r13
	align	 4
@L3488:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	or	 r12,r0,r13
@L9443:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3490
	st.h	 r12,r31,100
	bb1.n	 (31-31),r24,@L3491
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3491
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3491
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3499
	or	 r11,r0,0
@L3501:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3500
	bcnd	 eq0,r11,@L3499
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3499
	st	 r10,r12[r25]
	align	 4
@L3500:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3501
@L3499:
	bcnd.n	 ne0,r10,@L3520
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9444
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3506
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3506:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9444:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3520
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3520:
	ld.hu	 r13,r31,100
	br.n	 @L3491
	st.h	 r13,r10,8
	align	 4
@L3490:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,100
	bsr.n	 _mem_access
	or	 r5,r0,2
@L3491:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3553
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L3553:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L3559:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L3560
	ld.bu	 r15,r31,92
	br.n	 @L3561
	st	 r15,r31,164
	align	 4
@L3560:
	st	 r0,r31,164
@L3561:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	st	 r0,r31,172
	extu	 r14,r11,8<16>
	st	 r0,r31,188
	extu	 r17,r11,0<24>
	bcnd.n	 eq0,r13,@L3562
	st	 r14,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L3564
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9445
	addu	 r13,r13,r12
	align	 4
@L3564:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9445:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L3563
	st	 r13,r14[r9]
	align	 4
@L3562:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3566
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9446
	addu	 r13,r13,r12
	align	 4
@L3566:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9446:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L3563:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3568
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9447
	addu	 r24,r13,r3
	align	 4
@L3568:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9447:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3570
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r11,r14[r10]
	br.n	 @L9448
	or.u	 r14,r0,hi16(_spec_mode)
	align	 4
@L3570:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r11,r15[r10]
	or.u	 r14,r0,hi16(_spec_mode)
@L9448:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3572
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3573
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3573
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3573
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3581
	or	 r11,r0,0
@L3583:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3582
	bcnd	 eq0,r11,@L3581
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3581
	st	 r10,r12[r25]
	align	 4
@L3582:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3583
@L3581:
	bcnd.n	 ne0,r10,@L9449
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9450
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3588
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3588:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9450:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3587
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3587:
	cmp	 r13,r22,2
@L9449:
	bb0	 ne,r13,@L3602
	bb1.n	 gt,r13,@L3611
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3632
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L3573
	st.b	 r13,r10,8
	align	 4
@L3602:
	ld.hu	 r13,r31,104
	br.n	 @L3573
	st.h	 r13,r10,8
	align	 4
@L3611:
	ld	 r13,r31,104
	br.n	 @L3573
	st	 r13,r10,8
	align	 4
@L3632:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3572:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3573:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3635
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L3635:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L3641:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L3642
	ld.bu	 r14,r31,92
	br.n	 @L3643
	st	 r14,r31,164
	align	 4
@L3642:
	st	 r0,r31,164
@L3643:
	ld	 r11,r31,92
	st	 r0,r31,172
	or.u	 r15,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r13,r11,0<16>
	ld	 r12,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	mask	 r13,r13,254
	bcnd.n	 eq0,r12,@L3644
	st	 r13,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L3646
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9451
	addu	 r13,r13,r12
	align	 4
@L3646:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9451:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L3645
	st	 r13,r14[r9]
	align	 4
@L3644:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3648
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9452
	addu	 r13,r13,r12
	align	 4
@L3648:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9452:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L3645:
	ld.hu	 r13,r31,92
	bb0.n	 (31-31),r13,@L3651
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L3651
	or.u	 r2,r0,hi16(@LC546)
	or	 r4,r0,378
	or.u	 r3,r0,hi16(@LC547)
	or.u	 r5,r0,hi16(@LC548)
	or	 r2,r2,lo16(@LC546)
	or	 r3,r3,lo16(@LC547)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC548)
	align	 4
@L3651:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3654
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9453
	addu	 r24,r13,r3
	align	 4
@L3654:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r24,r13,r3
@L9453:
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r13,r13,lo16(_sim_swap_words)
	xor	 r10,r12,r13
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3656
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r10]
	br.n	 @L9454
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L3656:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r10]
	or.u	 r15,r0,hi16(_spec_mode)
@L9454:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3658
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3659
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3659
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3659
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3667
	or	 r11,r0,0
@L3669:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3668
	bcnd	 eq0,r11,@L3667
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3667
	st	 r10,r12[r25]
	align	 4
@L3668:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3669
@L3667:
	bcnd.n	 ne0,r10,@L9455
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9456
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3674
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3674:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9456:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3673
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3673:
	cmp	 r13,r22,2
@L9455:
	bb0	 ne,r13,@L3688
	bb1.n	 gt,r13,@L3697
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3718
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L3659
	st.b	 r13,r10,8
	align	 4
@L3688:
	ld.hu	 r13,r31,104
	br.n	 @L3659
	st.h	 r13,r10,8
	align	 4
@L3697:
	ld	 r13,r31,104
	br.n	 @L3659
	st	 r13,r10,8
	align	 4
@L3718:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3658:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3659:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3721
	ext	 r11,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9457
	addu	 r13,r13,r11
	align	 4
@L3721:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L9457:
	addu	 r24,r13,4
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r13,r13,lo16(_sim_swap_words)
	addu	 r12,r12,1
	xor	 r10,r12,r13
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3723
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r11,r14[r10]
	br.n	 @L9458
	or.u	 r14,r0,hi16(_spec_mode)
	align	 4
@L3723:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r11,r15[r10]
	or.u	 r14,r0,hi16(_spec_mode)
@L9458:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3725
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3726
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3726
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3726
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3734
	or	 r11,r0,0
@L3736:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3735
	bcnd	 eq0,r11,@L3734
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3734
	st	 r10,r12[r25]
	align	 4
@L3735:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3736
@L3734:
	bcnd.n	 ne0,r10,@L9459
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9460
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3741
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3741:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9460:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3740
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3740:
	cmp	 r13,r22,2
@L9459:
	bb0	 ne,r13,@L3755
	bb1.n	 gt,r13,@L3764
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3785
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L3726
	st.b	 r13,r10,8
	align	 4
@L3755:
	ld.hu	 r13,r31,104
	br.n	 @L3726
	st.h	 r13,r10,8
	align	 4
@L3764:
	ld	 r13,r31,104
	br.n	 @L3726
	st	 r13,r10,8
	align	 4
@L3785:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3725:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3726:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3788
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L3788:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L3794:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L3795
	ld.bu	 r14,r31,92
	br.n	 @L3796
	st	 r14,r31,164
	align	 4
@L3795:
	st	 r0,r31,164
@L3796:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	st	 r0,r31,172
	st	 r0,r31,180
	st	 r0,r31,188
	bcnd.n	 eq0,r13,@L3797
	extu	 r17,r11,0<24>
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L3799
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9461
	addu	 r13,r13,r12
	align	 4
@L3799:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9461:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L3798
	st	 r13,r14[r9]
	align	 4
@L3797:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3801
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9462
	addu	 r13,r13,r12
	align	 4
@L3801:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9462:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L3798:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3803
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9463
	addu	 r24,r13,r3
	align	 4
@L3803:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9463:
	or.u	 r14,r0,hi16(_use_spec_R)
	ld	 r13,r14,lo16(_use_spec_R)
	bb0.n	 (31-31),r13,@L3805
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r12,r15,lo16(_spec_regs_R)
	br.n	 @L9464
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L3805:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r12,r14,lo16(_regs_R)
	or.u	 r15,r0,hi16(_spec_mode)
@L9464:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3807
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3808
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3808
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3808
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3816
	or	 r11,r0,0
@L3818:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3817
	bcnd	 eq0,r11,@L3816
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3816
	st	 r10,r12[r25]
	align	 4
@L3817:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3818
@L3816:
	bcnd.n	 ne0,r10,@L9465
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9466
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3823
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3823:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9466:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3822
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3822:
	cmp	 r13,r22,2
@L9465:
	bb0	 ne,r13,@L3837
	bb1.n	 gt,r13,@L3846
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3867
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L3808
	st.b	 r13,r10,8
	align	 4
@L3837:
	ld.hu	 r13,r31,104
	br.n	 @L3808
	st.h	 r13,r10,8
	align	 4
@L3846:
	ld	 r13,r31,104
	br.n	 @L3808
	st	 r13,r10,8
	align	 4
@L3867:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3807:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3808:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3870
	ext	 r11,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9467
	addu	 r13,r13,r11
	align	 4
@L3870:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L9467:
	addu	 r24,r13,4
	or.u	 r14,r0,hi16(_use_spec_R)
	ld	 r13,r14,lo16(_use_spec_R)
	bb0.n	 (31-31),r13,@L3872
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r12,r15,lo16(_spec_regs_R)
	br.n	 @L9468
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L3872:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r12,r14,lo16(_regs_R)
	or.u	 r15,r0,hi16(_spec_mode)
@L9468:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3874
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3875
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3875
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3875
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3883
	or	 r11,r0,0
@L3885:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3884
	bcnd	 eq0,r11,@L3883
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3883
	st	 r10,r12[r25]
	align	 4
@L3884:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3885
@L3883:
	bcnd.n	 ne0,r10,@L9469
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9470
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3890
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3890:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9470:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3889
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3889:
	cmp	 r13,r22,2
@L9469:
	bb0	 ne,r13,@L3904
	bb1.n	 gt,r13,@L3913
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L3934
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L3875
	st.b	 r13,r10,8
	align	 4
@L3904:
	ld.hu	 r13,r31,104
	br.n	 @L3875
	st.h	 r13,r10,8
	align	 4
@L3913:
	ld	 r13,r31,104
	br.n	 @L3875
	st	 r13,r10,8
	align	 4
@L3934:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3874:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3875:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3937
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L3937:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L3943:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L3944
	ld.bu	 r15,r31,92
	br.n	 @L3945
	st	 r15,r31,164
	align	 4
@L3944:
	st	 r0,r31,164
@L3945:
	ld	 r11,r31,92
	st	 r0,r31,172
	or.u	 r14,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r13,r11,8<16>
	ld	 r12,r14,lo16(_spec_mode)
	addu	 r13,r13,32
	extu	 r17,r11,0<24>
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r12,@L3946
	st	 r13,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L3948
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9471
	addu	 r13,r13,r12
	align	 4
@L3948:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9471:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L3947
	st	 r13,r15[r9]
	align	 4
@L3946:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L3950
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9472
	addu	 r13,r13,r12
	align	 4
@L3950:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9472:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L3947:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3952
	ext	 r3,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9473
	addu	 r24,r13,r3
	align	 4
@L3952:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r24,r13,r3
@L9473:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L3954
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L8927
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L3954:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L8927:
	ld	 r12,r13[r12]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L3956
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L3957
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L3957
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L3957
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L3965
	or	 r11,r0,0
@L3967:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L3966
	bcnd	 eq0,r11,@L3965
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L3965
	st	 r10,r12[r25]
	align	 4
@L3966:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L3967
@L3965:
	bcnd.n	 ne0,r10,@L9474
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9475
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L3972
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L3972:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9475:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L3971
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L3971:
	cmp	 r13,r22,2
@L9474:
	bb0	 ne,r13,@L3986
	bb1.n	 gt,r13,@L3995
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L4016
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L3957
	st.b	 r13,r10,8
	align	 4
@L3986:
	ld.hu	 r13,r31,104
	br.n	 @L3957
	st.h	 r13,r10,8
	align	 4
@L3995:
	ld	 r13,r31,104
	br.n	 @L3957
	st	 r13,r10,8
	align	 4
@L4016:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L3956:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L3957:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4019
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L4019:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L4025:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L4026
	ld.bu	 r15,r31,92
	br.n	 @L4027
	st	 r15,r31,164
	align	 4
@L4026:
	st	 r0,r31,164
@L4027:
	ld	 r11,r31,92
	st	 r0,r31,172
	or.u	 r14,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r13,r11,8<16>
	ld	 r12,r14,lo16(_spec_mode)
	addu	 r13,r13,32
	extu	 r17,r11,0<24>
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r12,@L4028
	st	 r13,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L4030
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9476
	addu	 r13,r13,r12
	align	 4
@L4030:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9476:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L4029
	st	 r13,r15[r9]
	align	 4
@L4028:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L4032
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9477
	addu	 r13,r13,r12
	align	 4
@L4032:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9477:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L4029:
	ld.hu	 r13,r31,92
	bb0.n	 (31-31),r13,@L4035
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L4035
	or.u	 r2,r0,hi16(@LC549)
	or	 r4,r0,398
	or.u	 r3,r0,hi16(@LC550)
	or.u	 r5,r0,hi16(@LC551)
	or	 r2,r2,lo16(@LC549)
	or	 r3,r3,lo16(@LC550)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC551)
	align	 4
@L4035:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4038
	ext	 r3,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9478
	addu	 r24,r13,r3
	align	 4
@L4038:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L9478:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4040
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L8933
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L4040:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L8933:
	ld	 r12,r13[r12]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4042
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L4043
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4043
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4043
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4051
	or	 r11,r0,0
@L4053:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4052
	bcnd	 eq0,r11,@L4051
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4051
	st	 r10,r12[r25]
	align	 4
@L4052:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4053
@L4051:
	bcnd.n	 ne0,r10,@L9479
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9480
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4058
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4058:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9480:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4057
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4057:
	cmp	 r13,r22,2
@L9479:
	bb0	 ne,r13,@L4072
	bb1.n	 gt,r13,@L4081
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L4102
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L4043
	st.b	 r13,r10,8
	align	 4
@L4072:
	ld.hu	 r13,r31,104
	br.n	 @L4043
	st.h	 r13,r10,8
	align	 4
@L4081:
	ld	 r13,r31,104
	br.n	 @L4043
	st	 r13,r10,8
	align	 4
@L4102:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4042:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L4043:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4105
	ext	 r11,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9481
	addu	 r13,r13,r11
	align	 4
@L4105:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r11
@L9481:
	addu	 r24,r13,4
	ld.bu	 r9,r31,93
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	addu	 r11,r9,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r11,0<5>
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4107
	or.u	 r13,r0,hi16(_spec_regs_F+4)
	br.n	 @L8935
	or	 r13,r13,lo16(_spec_regs_F+4)
	align	 4
@L4107:
	or.u	 r13,r0,hi16(_regs_F+4)
	or	 r13,r13,lo16(_regs_F+4)
@L8935:
	ld	 r11,r13[r9]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4109
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L4110
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4110
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4110
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4118
	or	 r11,r0,0
@L4120:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4119
	bcnd	 eq0,r11,@L4118
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4118
	st	 r10,r12[r25]
	align	 4
@L4119:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4120
@L4118:
	bcnd.n	 ne0,r10,@L9482
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9483
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4125
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4125:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9483:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4124
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4124:
	cmp	 r13,r22,2
@L9482:
	bb0	 ne,r13,@L4139
	bb1.n	 gt,r13,@L4148
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L4169
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L4110
	st.b	 r13,r10,8
	align	 4
@L4139:
	ld.hu	 r13,r31,104
	br.n	 @L4110
	st.h	 r13,r10,8
	align	 4
@L4148:
	ld	 r13,r31,104
	br.n	 @L4110
	st	 r13,r10,8
	align	 4
@L4169:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4109:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L4110:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4172
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L4172:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L4178:
	ld	 r9,r31,92
	st	 r0,r31,164
	extu	 r15,r9,8<16>
	st	 r0,r31,172
	extu	 r13,r9,0<29>
	st	 r0,r31,188
	extu	 r17,r9,0<24>
	st	 r15,r31,180
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	or	 r21,r12,lo16(_ss_lr_temp)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L4179
	ext	 r9,r9,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9484
	addu	 r13,r13,r9
	align	 4
@L4179:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r9
@L9484:
	and	 r24,r13,0xfffc
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L4181
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L4182
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4187
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9485
	extu	 r13,r24,0<24>
@L4187:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4182
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4182
	extu	 r13,r24,0<24>
@L9485:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4190
	or	 r11,r0,0
@L4192:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4191
	bcnd	 eq0,r11,@L4190
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4190
	st	 r10,r12[r25]
	align	 4
@L4191:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4192
@L4190:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4196
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9486
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4197
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4197:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9486:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4196
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4196:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L4210
	bb1.n	 gt,r13,@L4219
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L4241
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L4203
	ld.bu	 r13,r10,8
@L8939:
	br.n	 @L4182
	st.b	 r13,r31,104
	align	 4
@L4203:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4205
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4208
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4208:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L4182
	st.b	 r13,r31,104
	align	 4
@L4205:
	br.n	 @L8939
	or	 r13,r0,0
	align	 4
@L4210:
	bcnd	 eq0,r10,@L4212
	ld.hu	 r13,r10,8
@L8940:
	br.n	 @L4182
	st.h	 r13,r31,104
	align	 4
@L4212:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4214
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4217
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4217:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4182
	st.h	 r13,r31,104
	align	 4
@L4214:
	br.n	 @L8940
	or	 r13,r0,0
	align	 4
@L4219:
	bcnd	 eq0,r10,@L4221
	ld	 r13,r10,8
@L8941:
	br.n	 @L4182
	st	 r13,r31,104
	align	 4
@L4221:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4223
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4226
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4226:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L4182
	st	 r13,r31,104
	align	 4
@L4223:
	br.n	 @L8941
	or	 r13,r0,0
	align	 4
@L4241:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4181:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L4182:
	ld	 r13,r31,92
	ld	 r12,r31,104
	st	 r12,r0,r21
	or	 r11,r0,1
	ext	 r7,r13,16<0>
	or.u	 r12,r0,hi16(_ss_lr_temp)
	extu	 r8,r13,0<24>
	or	 r6,r12,lo16(_ss_lr_temp)
	extu	 r13,r13,0<29>
	mask	 r10,r8,31
	ld	 r13,r18[r13]
	mak	 r11,r11,r10
	or.u	 r12,r0,hi16(_ss_lr_masks)
	and	 r13,r13,r11
	bcnd.n	 eq0,r13,@L4244
	or	 r9,r12,lo16(_ss_lr_masks)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9487
	addu	 r13,r13,r7
	align	 4
@L4244:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	addu	 r13,r13,r7
@L9487:
	mak	 r13,r13,2<2>
	addu	 r9,r13,r9
	ld	 r13,r31,92
	ext	 r7,r13,16<0>
	ld	 r9,r0,r9
	extu	 r8,r13,0<24>
	or	 r12,r0,1
	extu	 r13,r13,0<29>
	xor.c	 r9,r9,r0
	ld	 r11,r18[r13]
	mask	 r10,r8,31
	or.u	 r13,r0,hi16(_ss_lr_temp)
	mak	 r12,r12,r10
	or	 r10,r13,lo16(_ss_lr_temp)
	or.u	 r13,r0,hi16(_ss_lr_masks)
	and	 r11,r11,r12
	bcnd.n	 eq0,r11,@L4246
	or	 r12,r13,lo16(_ss_lr_masks)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9488
	addu	 r13,r13,r7
	align	 4
@L4246:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	addu	 r13,r13,r7
@L9488:
	mak	 r13,r13,2<2>
	addu	 r7,r13,r12
	ld.hu	 r11,r31,92
	or	 r13,r0,1
	mask	 r8,r11,255
	ld	 r10,r0,r10
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r11,r0,r7
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4248
	and	 r10,r10,r11
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9489
	and	 r13,r13,r9
	align	 4
@L4248:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	and	 r13,r13,r9
@L9489:
	or	 r10,r13,r10
	ld	 r11,r31,92
	or	 r13,r0,1
	extu	 r9,r11,0<24>
	st	 r10,r0,r6
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4250
	ext	 r11,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9490
	addu	 r13,r13,r11
	align	 4
@L4250:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r11
@L9490:
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ss_lr_temp)
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r13,lo16(_ss_lr_temp)
	ld	 r12,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L4389
	st	 r13,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L1739
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L1739
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L1739
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4261
	or	 r11,r0,0
@L4263:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4262
	bcnd	 eq0,r11,@L4261
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4261
	st	 r10,r12[r25]
	align	 4
@L4262:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4263
@L4261:
	bcnd.n	 ne0,r10,@L9491
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9492
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4268
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4268:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9492:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4267
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4267:
	cmp	 r13,r22,2
@L9491:
	bb0	 ne,r13,@L4419
	bb1.n	 gt,r13,@L4428
	cmp	 r13,r22,1
	bb0.n	 ne,r13,@L8946
	or.u	 r2,r0,hi16(@LC509)
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4315:
	ld	 r9,r31,92
	st	 r0,r31,164
	extu	 r14,r9,8<16>
	st	 r0,r31,172
	extu	 r13,r9,0<29>
	st	 r0,r31,188
	extu	 r17,r9,0<24>
	st	 r14,r31,180
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	or	 r21,r12,lo16(_ss_lr_temp)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L4316
	ext	 r9,r9,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9493
	addu	 r13,r13,r9
	align	 4
@L4316:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r9
@L9493:
	and	 r24,r13,0xfffc
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4318
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L4319
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4324
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9494
	extu	 r13,r24,0<24>
@L4324:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4319
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4319
	extu	 r13,r24,0<24>
@L9494:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4327
	or	 r11,r0,0
@L4329:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4328
	bcnd	 eq0,r11,@L4327
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4327
	st	 r10,r12[r25]
	align	 4
@L4328:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4329
@L4327:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4333
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9495
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4334
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4334:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9495:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4333
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4333:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L4347
	bb1.n	 gt,r13,@L4356
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L4378
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L4340
	ld.bu	 r13,r10,8
@L8948:
	br.n	 @L4319
	st.b	 r13,r31,104
	align	 4
@L4340:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4342
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4345
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4345:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L4319
	st.b	 r13,r31,104
	align	 4
@L4342:
	br.n	 @L8948
	or	 r13,r0,0
	align	 4
@L4347:
	bcnd	 eq0,r10,@L4349
	ld.hu	 r13,r10,8
@L8949:
	br.n	 @L4319
	st.h	 r13,r31,104
	align	 4
@L4349:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4351
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4354
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4354:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4319
	st.h	 r13,r31,104
	align	 4
@L4351:
	br.n	 @L8949
	or	 r13,r0,0
	align	 4
@L4356:
	bcnd	 eq0,r10,@L4358
	ld	 r13,r10,8
@L8950:
	br.n	 @L4319
	st	 r13,r31,104
	align	 4
@L4358:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4360
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4363
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4363:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L4319
	st	 r13,r31,104
	align	 4
@L4360:
	br.n	 @L8950
	or	 r13,r0,0
	align	 4
@L4378:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4318:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L4319:
	ld	 r13,r31,92
	ld	 r12,r31,104
	st	 r12,r0,r21
	or	 r11,r0,1
	ext	 r8,r13,16<0>
	or.u	 r12,r0,hi16(_ss_lr_temp)
	extu	 r9,r13,0<24>
	or	 r5,r12,lo16(_ss_lr_temp)
	extu	 r13,r13,0<29>
	mask	 r10,r9,31
	ld	 r13,r18[r13]
	mak	 r11,r11,r10
	or.u	 r12,r0,hi16(_ss_lr_masks)
	and	 r13,r13,r11
	bcnd.n	 eq0,r13,@L4381
	or	 r12,r12,lo16(_ss_lr_masks)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9496
	addu	 r13,r13,r8
	align	 4
@L4381:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r8
@L9496:
	mask	 r13,r13,3
	addu	 r13,r13,1
	lda	 r11,r12[r13]
	ld	 r13,r31,92
	extu	 r12,r13,0<29>
	extu	 r9,r13,0<24>
	ld	 r7,r0,r11
	ext	 r8,r13,16<0>
	or	 r13,r0,1
	ld	 r10,r18[r12]
	mask	 r11,r9,31
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r11
	or	 r6,r12,lo16(_ss_lr_temp)
	or.u	 r12,r0,hi16(_ss_lr_masks)
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L4383
	or	 r12,r12,lo16(_ss_lr_masks)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9497
	addu	 r13,r13,r8
	align	 4
@L4383:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r8
@L9497:
	mask	 r13,r13,3
	addu	 r13,r13,1
	lda	 r10,r12[r13]
	ld.hu	 r11,r31,92
	or	 r13,r0,1
	mask	 r9,r11,255
	ld	 r10,r0,r10
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r11,r0,r6
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4385
	and.c	 r11,r11,r10
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9498
	and	 r13,r13,r7
	align	 4
@L4385:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	and	 r13,r13,r7
@L9498:
	or	 r13,r13,r11
	ld	 r11,r31,92
	st	 r13,r0,r5
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4387
	ext	 r11,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9499
	addu	 r13,r13,r11
	align	 4
@L4387:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L9499:
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ss_lr_temp)
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r13,lo16(_ss_lr_temp)
	ld	 r12,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L4389
	st	 r13,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L1739
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L1739
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L1739
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4398
	or	 r11,r0,0
@L4400:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4399
	bcnd	 eq0,r11,@L4398
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4398
	st	 r10,r12[r25]
	align	 4
@L4399:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4400
@L4398:
	bcnd.n	 ne0,r10,@L9500
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9501
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4405
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4405:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9501:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4404
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4404:
	cmp	 r13,r22,2
@L9500:
	bb0	 ne,r13,@L4419
	bb1.n	 gt,r13,@L4428
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L4449
	or.u	 r2,r0,hi16(@LC509)
@L8946:
	ld.bu	 r13,r31,104
	br.n	 @L1739
	st.b	 r13,r10,8
	align	 4
@L4419:
	ld.hu	 r13,r31,104
	br.n	 @L1739
	st.h	 r13,r10,8
	align	 4
@L4428:
	ld	 r13,r31,104
	br.n	 @L1739
	st	 r13,r10,8
	align	 4
@L4449:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4389:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	or	 r5,r0,4
	bsr	 _mem_access
	br	 @L1739
	align	 4
@L4452:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L4453
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L4453:
	or.u	 r15,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L4455
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L4457
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9502
	addu	 r13,r13,r12
	align	 4
@L4457:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9502:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L4456
	st	 r13,r15[r9]
	align	 4
@L4455:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L4459
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9503
	addu	 r13,r13,r12
	align	 4
@L4459:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9503:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L4456:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4461
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L4463
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9504
	extu	 r11,r11,0<8>
	align	 4
@L4463:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9504:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4465
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9505
	addu	 r24,r3,r13
	align	 4
@L4465:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9505:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4467
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4473
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9506
	extu	 r13,r24,0<24>
@L4473:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4468
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4468
	extu	 r13,r24,0<24>
@L9506:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4476
	or	 r11,r0,0
@L4478:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4477
	bcnd	 eq0,r11,@L4476
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4476
	st	 r10,r12[r25]
	align	 4
@L4477:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4478
@L4476:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4482
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9507
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4483
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4483:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9507:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4482
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4482:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L4496
	bb0.n	 gt,r13,@L4487
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L4505
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L4514
	or.u	 r2,r0,hi16(@LC509)
	br	 @L4527
	align	 4
@L4487:
	bcnd	 eq0,r10,@L4489
	ld.bu	 r13,r10,8
@L8958:
	br.n	 @L4468
	st.b	 r13,r31,96
	align	 4
@L4489:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4491
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4494
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4494:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L4468
	st.b	 r13,r31,96
	align	 4
@L4491:
	br.n	 @L8958
	or	 r13,r0,0
	align	 4
@L4496:
	bcnd	 eq0,r10,@L4498
	ld.hu	 r13,r10,8
@L8959:
	br.n	 @L4468
	st.h	 r13,r31,96
	align	 4
@L4498:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4500
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4503
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4503:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4468
	st.h	 r13,r31,96
	align	 4
@L4500:
	br.n	 @L8959
	or	 r13,r0,0
	align	 4
@L4505:
	bcnd	 eq0,r10,@L4507
	ld	 r13,r10,8
@L8960:
	br.n	 @L4468
	st	 r13,r31,96
	align	 4
@L4507:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4509
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4512
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4512:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L4468
	st	 r13,r31,96
	align	 4
@L4509:
	br.n	 @L8960
	or	 r13,r0,0
	align	 4
@L4514:
	bcnd	 eq0,r10,@L4516
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,96
	br.n	 @L4468
	st	 r12,r31,100
	align	 4
@L4516:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4518
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4521
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4521:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9508
	addu	 r23,r24,4
	align	 4
@L4518:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9508:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L4522
	st	 r12,r31,96
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L4525
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L4525:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L4468
	st	 r13,r31,100
	align	 4
@L4522:
	or	 r13,r0,0
	br.n	 @L4468
	st	 r13,r31,100
	align	 4
@L4527:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4467:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,96
	bsr.n	 _mem_access
	or	 r5,r0,1
@L4468:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld.b	 r13,r31,96
	br.n	 @L8961
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L4461:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4530
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9509
	extu	 r11,r11,0<8>
	align	 4
@L4530:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9509:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4532
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9510
	addu	 r24,r3,r13
	align	 4
@L4532:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9510:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4534
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4540
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9511
	extu	 r13,r24,0<24>
@L4540:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4535
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4535
	extu	 r13,r24,0<24>
@L9511:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4543
	or	 r11,r0,0
@L4545:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4544
	bcnd	 eq0,r11,@L4543
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4543
	st	 r10,r12[r25]
	align	 4
@L4544:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4545
@L4543:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4549
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9512
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4550
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4550:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9512:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4549
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4549:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L4563
	bb0.n	 gt,r13,@L4554
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L4572
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L4581
	or.u	 r2,r0,hi16(@LC509)
	br	 @L4594
	align	 4
@L4554:
	bcnd	 eq0,r10,@L4556
	ld.bu	 r13,r10,8
@L8963:
	br.n	 @L4535
	st.b	 r13,r31,96
	align	 4
@L4556:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4558
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4561
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4561:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L4535
	st.b	 r13,r31,96
	align	 4
@L4558:
	br.n	 @L8963
	or	 r13,r0,0
	align	 4
@L4563:
	bcnd	 eq0,r10,@L4565
	ld.hu	 r13,r10,8
@L8964:
	br.n	 @L4535
	st.h	 r13,r31,96
	align	 4
@L4565:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4567
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4570
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4570:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4535
	st.h	 r13,r31,96
	align	 4
@L4567:
	br.n	 @L8964
	or	 r13,r0,0
	align	 4
@L4572:
	bcnd	 eq0,r10,@L4574
	ld	 r13,r10,8
@L8965:
	br.n	 @L4535
	st	 r13,r31,96
	align	 4
@L4574:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4576
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4579
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4579:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L4535
	st	 r13,r31,96
	align	 4
@L4576:
	br.n	 @L8965
	or	 r13,r0,0
	align	 4
@L4581:
	bcnd	 eq0,r10,@L4583
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,96
	br.n	 @L4535
	st	 r12,r31,100
	align	 4
@L4583:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4585
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4588
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4588:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9513
	addu	 r23,r24,4
	align	 4
@L4585:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9513:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L4589
	st	 r12,r31,96
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L4592
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L4592:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L4535
	st	 r13,r31,100
	align	 4
@L4589:
	or	 r13,r0,0
	br.n	 @L4535
	st	 r13,r31,100
	align	 4
@L4594:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4534:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,96
	bsr.n	 _mem_access
	or	 r5,r0,1
@L4535:
	or.u	 r15,r0,hi16(_regs_R)
	ld.b	 r13,r31,96
	or	 r15,r15,lo16(_regs_R)
@L8961:
	st	 r13,r15[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4597
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L4597:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L4603:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L4604
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L4604:
	or.u	 r15,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L4606
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L4608
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9514
	addu	 r13,r13,r12
	align	 4
@L4608:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9514:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L4607
	st	 r13,r15[r9]
	align	 4
@L4606:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L4610
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9515
	addu	 r13,r13,r12
	align	 4
@L4610:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9515:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L4607:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4612
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L4614
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9516
	extu	 r11,r11,0<8>
	align	 4
@L4614:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9516:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4616
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9517
	addu	 r24,r3,r13
	align	 4
@L4616:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9517:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4618
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4624
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9518
	extu	 r13,r24,0<24>
@L4624:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4619
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4619
	extu	 r13,r24,0<24>
@L9518:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4627
	or	 r11,r0,0
@L4629:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4628
	bcnd	 eq0,r11,@L4627
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4627
	st	 r10,r12[r25]
	align	 4
@L4628:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4629
@L4627:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4633
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9519
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4634
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4634:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9519:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4633
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4633:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L4647
	bb0.n	 gt,r13,@L4638
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L4656
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L4665
	or.u	 r2,r0,hi16(@LC509)
	br	 @L4678
	align	 4
@L4638:
	bcnd	 eq0,r10,@L4640
	ld.bu	 r13,r10,8
@L8971:
	br.n	 @L4619
	st.b	 r13,r31,97
	align	 4
@L4640:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4642
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4645
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4645:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L4619
	st.b	 r13,r31,97
	align	 4
@L4642:
	br.n	 @L8971
	or	 r13,r0,0
	align	 4
@L4647:
	bcnd	 eq0,r10,@L4649
	ld.hu	 r13,r10,8
@L8972:
	br.n	 @L4619
	st.h	 r13,r31,97
	align	 4
@L4649:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4651
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4654
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4654:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4619
	st.h	 r13,r31,97
	align	 4
@L4651:
	br.n	 @L8972
	or	 r13,r0,0
	align	 4
@L4656:
	bcnd	 eq0,r10,@L4658
	ld	 r13,r10,8
@L8973:
	br.n	 @L4619
	st	 r13,r31,97
	align	 4
@L4658:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4660
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4663
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4663:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L4619
	st	 r13,r31,97
	align	 4
@L4660:
	br.n	 @L8973
	or	 r13,r0,0
	align	 4
@L4665:
	bcnd	 eq0,r10,@L4667
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,97
	br.n	 @L4619
	st	 r12,r31,101
	align	 4
@L4667:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4669
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4672
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4672:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9520
	addu	 r23,r24,4
	align	 4
@L4669:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9520:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L4673
	st	 r12,r31,97
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L4676
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L4676:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L4619
	st	 r13,r31,101
	align	 4
@L4673:
	or	 r13,r0,0
	br.n	 @L4619
	st	 r13,r31,101
	align	 4
@L4678:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4618:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,97
	bsr.n	 _mem_access
	or	 r5,r0,1
@L4619:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld.bu	 r13,r31,97
	br.n	 @L8974
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L4612:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4681
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9521
	extu	 r11,r11,0<8>
	align	 4
@L4681:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9521:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4683
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9522
	addu	 r24,r3,r13
	align	 4
@L4683:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9522:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4685
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4691
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9523
	extu	 r13,r24,0<24>
@L4691:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4686
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4686
	extu	 r13,r24,0<24>
@L9523:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4694
	or	 r11,r0,0
@L4696:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4695
	bcnd	 eq0,r11,@L4694
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4694
	st	 r10,r12[r25]
	align	 4
@L4695:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4696
@L4694:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4700
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9524
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4701
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4701:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9524:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4700
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4700:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L4714
	bb0.n	 gt,r13,@L4705
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L4723
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L4732
	or.u	 r2,r0,hi16(@LC509)
	br	 @L4745
	align	 4
@L4705:
	bcnd	 eq0,r10,@L4707
	ld.bu	 r13,r10,8
@L8976:
	br.n	 @L4686
	st.b	 r13,r31,97
	align	 4
@L4707:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4709
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4712
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4712:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L4686
	st.b	 r13,r31,97
	align	 4
@L4709:
	br.n	 @L8976
	or	 r13,r0,0
	align	 4
@L4714:
	bcnd	 eq0,r10,@L4716
	ld.hu	 r13,r10,8
@L8977:
	br.n	 @L4686
	st.h	 r13,r31,97
	align	 4
@L4716:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4718
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4721
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4721:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4686
	st.h	 r13,r31,97
	align	 4
@L4718:
	br.n	 @L8977
	or	 r13,r0,0
	align	 4
@L4723:
	bcnd	 eq0,r10,@L4725
	ld	 r13,r10,8
@L8978:
	br.n	 @L4686
	st	 r13,r31,97
	align	 4
@L4725:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4727
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4730
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4730:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L4686
	st	 r13,r31,97
	align	 4
@L4727:
	br.n	 @L8978
	or	 r13,r0,0
	align	 4
@L4732:
	bcnd	 eq0,r10,@L4734
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,97
	br.n	 @L4686
	st	 r12,r31,101
	align	 4
@L4734:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4736
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4739
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4739:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L9525
	addu	 r23,r24,4
	align	 4
@L4736:
	or	 r12,r0,0
	addu	 r23,r24,4
@L9525:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r23,15<16>
	or	 r22,r13,lo16(_mem_table)
	ld	 r13,r22[r25]
	bcnd.n	 eq0,r13,@L4740
	st	 r12,r31,97
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r22[r25]
	bcnd	 ne0,r13,@L4743
	bsr	 _mem_newblock
	st	 r2,r22[r25]
@L4743:
	ld	 r12,r22[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L4686
	st	 r13,r31,101
	align	 4
@L4740:
	or	 r13,r0,0
	br.n	 @L4686
	st	 r13,r31,101
	align	 4
@L4745:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L4685:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,97
	bsr.n	 _mem_access
	or	 r5,r0,1
@L4686:
	or.u	 r15,r0,hi16(_regs_R)
	ld.bu	 r13,r31,97
	or	 r15,r15,lo16(_regs_R)
@L8974:
	st	 r13,r15[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4748
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L4748:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L4754:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L4755
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L4755:
	or.u	 r15,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L4757
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L4759
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9526
	addu	 r13,r13,r12
	align	 4
@L4759:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9526:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L4758
	st	 r13,r15[r9]
	align	 4
@L4757:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+20)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+20)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L4761
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9527
	addu	 r13,r13,r12
	align	 4
@L4761:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9527:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L4758:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4763
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L4765
	extu	 r22,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9528
	extu	 r11,r11,0<8>
	align	 4
@L4765:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9528:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4767
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9529
	addu	 r24,r3,r13
	align	 4
@L4767:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9529:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4769
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L4770
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4775
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9530
	extu	 r13,r24,0<24>
@L4775:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4770
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4770
	extu	 r13,r24,0<24>
@L9530:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4778
	or	 r11,r0,0
@L4780:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4779
	bcnd	 eq0,r11,@L4778
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4778
	st	 r10,r12[r25]
	align	 4
@L4779:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4780
@L4778:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4798
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9531
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4785
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4785:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9531:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4798
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4798:
	bcnd	 eq0,r10,@L4800
	ld.hu	 r13,r10,8
@L8984:
	br.n	 @L4770
	st.h	 r13,r31,98
	align	 4
@L4800:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4802
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4805
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4805:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4770
	st.h	 r13,r31,98
	align	 4
@L4802:
	br.n	 @L8984
	or	 r13,r0,0
	align	 4
@L4769:
	or	 r3,r0,r24
	addu	 r4,r31,98
	bsr.n	 _mem_access
	or	 r5,r0,2
@L4770:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld.h	 r13,r31,98
	br.n	 @L8985
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L4763:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4832
	extu	 r22,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9532
	extu	 r11,r11,0<8>
	align	 4
@L4832:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9532:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4834
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9533
	addu	 r24,r3,r13
	align	 4
@L4834:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9533:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4836
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L4837
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4842
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9534
	extu	 r13,r24,0<24>
@L4842:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4837
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4837
	extu	 r13,r24,0<24>
@L9534:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4845
	or	 r11,r0,0
@L4847:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4846
	bcnd	 eq0,r11,@L4845
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4845
	st	 r10,r12[r25]
	align	 4
@L4846:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4847
@L4845:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4865
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9535
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4852
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4852:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9535:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4865
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4865:
	bcnd	 eq0,r10,@L4867
	ld.hu	 r13,r10,8
@L8987:
	br.n	 @L4837
	st.h	 r13,r31,98
	align	 4
@L4867:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4869
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4872
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4872:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4837
	st.h	 r13,r31,98
	align	 4
@L4869:
	br.n	 @L8987
	or	 r13,r0,0
	align	 4
@L4836:
	or	 r3,r0,r24
	addu	 r4,r31,98
	bsr.n	 _mem_access
	or	 r5,r0,2
@L4837:
	or.u	 r15,r0,hi16(_regs_R)
	ld.h	 r13,r31,98
	or	 r15,r15,lo16(_regs_R)
@L8985:
	st	 r13,r15[r22]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4899
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L4899:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L4905:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L4906
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L4906:
	or.u	 r15,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L4908
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L4910
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9536
	addu	 r13,r13,r12
	align	 4
@L4910:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9536:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L4909
	st	 r13,r15[r9]
	align	 4
@L4908:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+20)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+20)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L4912
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9537
	addu	 r13,r13,r12
	align	 4
@L4912:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9537:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L4909:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4914
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L4916
	extu	 r22,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9538
	extu	 r11,r11,0<8>
	align	 4
@L4916:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9538:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4918
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9539
	addu	 r24,r3,r13
	align	 4
@L4918:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9539:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4920
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L4921
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4926
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9540
	extu	 r13,r24,0<24>
@L4926:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4921
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4921
	extu	 r13,r24,0<24>
@L9540:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4929
	or	 r11,r0,0
@L4931:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4930
	bcnd	 eq0,r11,@L4929
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4929
	st	 r10,r12[r25]
	align	 4
@L4930:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4931
@L4929:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L4949
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9541
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L4936
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L4936:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9541:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L4949
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L4949:
	bcnd	 eq0,r10,@L4951
	ld.hu	 r13,r10,8
@L8993:
	br.n	 @L4921
	st.h	 r13,r31,100
	align	 4
@L4951:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L4953
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L4956
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L4956:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4921
	st.h	 r13,r31,100
	align	 4
@L4953:
	br.n	 @L8993
	or	 r13,r0,0
	align	 4
@L4920:
	or	 r3,r0,r24
	addu	 r4,r31,100
	bsr.n	 _mem_access
	or	 r5,r0,2
@L4921:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld.hu	 r13,r31,100
	br.n	 @L8994
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L4914:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4983
	extu	 r22,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9542
	extu	 r11,r11,0<8>
	align	 4
@L4983:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9542:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L4985
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9543
	addu	 r24,r3,r13
	align	 4
@L4985:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9543:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L4987
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L4988
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L4993
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9544
	extu	 r13,r24,0<24>
@L4993:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L4988
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L4988
	extu	 r13,r24,0<24>
@L9544:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L4996
	or	 r11,r0,0
@L4998:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L4997
	bcnd	 eq0,r11,@L4996
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L4996
	st	 r10,r12[r25]
	align	 4
@L4997:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L4998
@L4996:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5016
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9545
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5003
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5003:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9545:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5016
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5016:
	bcnd	 eq0,r10,@L5018
	ld.hu	 r13,r10,8
@L8996:
	br.n	 @L4988
	st.h	 r13,r31,100
	align	 4
@L5018:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5020
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5023
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5023:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L4988
	st.h	 r13,r31,100
	align	 4
@L5020:
	br.n	 @L8996
	or	 r13,r0,0
	align	 4
@L4987:
	or	 r3,r0,r24
	addu	 r4,r31,100
	bsr.n	 _mem_access
	or	 r5,r0,2
@L4988:
	or.u	 r15,r0,hi16(_regs_R)
	ld.hu	 r13,r31,100
	or	 r15,r15,lo16(_regs_R)
@L8994:
	st	 r13,r15[r22]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5050
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L5050:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L5056:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L5057
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L5057:
	or.u	 r15,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L5059
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L5061
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9546
	addu	 r13,r13,r12
	align	 4
@L5061:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9546:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L5060
	st	 r13,r15[r9]
	align	 4
@L5059:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5063
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9547
	addu	 r13,r13,r12
	align	 4
@L5063:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9547:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L5060:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5065
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L5067
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9548
	extu	 r11,r11,0<8>
	align	 4
@L5067:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9548:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5069
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9549
	addu	 r24,r3,r13
	align	 4
@L5069:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9549:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5071
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5072
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5077
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9550
	extu	 r13,r24,0<24>
@L5077:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5072
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5072
	extu	 r13,r24,0<24>
@L9550:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5080
	or	 r11,r0,0
@L5082:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5081
	bcnd	 eq0,r11,@L5080
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5080
	st	 r10,r12[r25]
	align	 4
@L5081:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5082
@L5080:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5086
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9551
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5087
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5087:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9551:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5086
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5086:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5100
	bb1.n	 gt,r13,@L5109
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5131
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L5093
	ld.bu	 r13,r10,8
@L9002:
	br.n	 @L5072
	st.b	 r13,r31,104
	align	 4
@L5093:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5095
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5098
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5098:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5072
	st.b	 r13,r31,104
	align	 4
@L5095:
	br.n	 @L9002
	or	 r13,r0,0
	align	 4
@L5100:
	bcnd	 eq0,r10,@L5102
	ld.hu	 r13,r10,8
@L9003:
	br.n	 @L5072
	st.h	 r13,r31,104
	align	 4
@L5102:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5104
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5107
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5107:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5072
	st.h	 r13,r31,104
	align	 4
@L5104:
	br.n	 @L9003
	or	 r13,r0,0
	align	 4
@L5109:
	bcnd	 eq0,r10,@L5111
	ld	 r13,r10,8
@L9004:
	br.n	 @L5072
	st	 r13,r31,104
	align	 4
@L5111:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5113
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5116
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5116:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5072
	st	 r13,r31,104
	align	 4
@L5113:
	br.n	 @L9004
	or	 r13,r0,0
	align	 4
@L5131:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5071:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L5072:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r13,r31,104
	br.n	 @L9005
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L5065:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5134
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br.n	 @L9552
	extu	 r11,r11,0<8>
	align	 4
@L5134:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9552:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5136
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9553
	addu	 r24,r3,r13
	align	 4
@L5136:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9553:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5138
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5139
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5144
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9554
	extu	 r13,r24,0<24>
@L5144:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5139
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5139
	extu	 r13,r24,0<24>
@L9554:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5147
	or	 r11,r0,0
@L5149:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5148
	bcnd	 eq0,r11,@L5147
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5147
	st	 r10,r12[r25]
	align	 4
@L5148:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5149
@L5147:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5153
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9555
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5154
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5154:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9555:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5153
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5153:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5167
	bb1.n	 gt,r13,@L5176
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5198
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L5160
	ld.bu	 r13,r10,8
@L9007:
	br.n	 @L5139
	st.b	 r13,r31,104
	align	 4
@L5160:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5162
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5165
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5165:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5139
	st.b	 r13,r31,104
	align	 4
@L5162:
	br.n	 @L9007
	or	 r13,r0,0
	align	 4
@L5167:
	bcnd	 eq0,r10,@L5169
	ld.hu	 r13,r10,8
@L9008:
	br.n	 @L5139
	st.h	 r13,r31,104
	align	 4
@L5169:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5171
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5174
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5174:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5139
	st.h	 r13,r31,104
	align	 4
@L5171:
	br.n	 @L9008
	or	 r13,r0,0
	align	 4
@L5176:
	bcnd	 eq0,r10,@L5178
	ld	 r13,r10,8
@L9009:
	br.n	 @L5139
	st	 r13,r31,104
	align	 4
@L5178:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5180
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5183
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5183:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5139
	st	 r13,r31,104
	align	 4
@L5180:
	br.n	 @L9009
	or	 r13,r0,0
	align	 4
@L5198:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5138:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L5139:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r13,r31,104
	or	 r15,r15,lo16(_regs_R)
@L9005:
	st	 r13,r15[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5201
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L5201:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L5207:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	extu	 r13,r12,0<16>
	st	 r0,r31,172
	mask	 r13,r13,254
	bcnd.n	 eq0,r9,@L5208
	st	 r13,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L5208:
	or.u	 r14,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r15,r11,8<8>
	bcnd.n	 eq0,r13,@L5210
	st	 r15,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L5212
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9556
	addu	 r13,r13,r12
	align	 4
@L5212:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9556:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L5211
	st	 r13,r14[r9]
	align	 4
@L5210:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5214
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9557
	addu	 r13,r13,r12
	align	 4
@L5214:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9557:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L5211:
	ld.hu	 r13,r31,92
	bb0.n	 (31-31),r13,@L5217
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L5217
	or.u	 r2,r0,hi16(@LC552)
	or	 r4,r0,454
	or.u	 r3,r0,hi16(@LC553)
	or.u	 r5,r0,hi16(@LC554)
	or	 r2,r2,lo16(@LC552)
	or	 r3,r3,lo16(@LC553)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC554)
	align	 4
@L5217:
	ld	 r13,r31,92
	extu	 r10,r13,0<24>
	or	 r12,r0,1
	extu	 r13,r13,0<29>
	mask	 r11,r10,31
	ld	 r13,r18[r13]
	mak	 r12,r12,r11
	or.u	 r11,r0,hi16(_temp_bs)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L5220
	or	 r9,r11,lo16(_temp_bs)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r10]
	br	 @L5221
	align	 4
@L5220:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r10]
@L5221:
	ld	 r12,r31,92
	extu	 r12,r12,0<8>
	st	 r11,r0,r9
	mask	 r10,r12,255
	or	 r13,r0,1
	extu	 r11,r10,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r11]
	mak	 r13,r13,r12
	or.u	 r12,r0,hi16(_temp_rd)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5222
	or	 r12,r12,lo16(_temp_rd)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r10]
	br.n	 @L9558
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L5222:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r10]
	or.u	 r15,r0,hi16(_spec_mode)
@L9558:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5224
	st	 r11,r0,r12
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld.bu	 r11,r31,93
	ld	 r9,r13,lo16(_sim_swap_words)
	xor	 r11,r11,r9
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r10]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	or.u	 r13,r0,hi16(_temp_bs)
	st	 r12,r18[r10]
	or.u	 r12,r0,hi16(_temp_rd)
	ld	 r11,r13,lo16(_temp_bs)
	ld	 r13,r12,lo16(_temp_rd)
	ld.bu	 r12,r31,93
	or	 r22,r0,4
	addu	 r24,r11,r13
	xor	 r21,r12,r9
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5227
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5232
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9559
	extu	 r13,r24,0<24>
@L5232:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5227
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5227
	extu	 r13,r24,0<24>
@L9559:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5235
	or	 r11,r0,0
@L5237:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5236
	bcnd	 eq0,r11,@L5235
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5235
	st	 r10,r12[r25]
	align	 4
@L5236:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5237
@L5235:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5241
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9560
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5242
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5242:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9560:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5241
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5241:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5255
	bb1.n	 gt,r13,@L5264
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5286
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L5248
	ld.bu	 r13,r10,8
@L9014:
	br.n	 @L5227
	st.b	 r13,r31,104
	align	 4
@L5248:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5250
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5253
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5253:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5227
	st.b	 r13,r31,104
	align	 4
@L5250:
	br.n	 @L9014
	or	 r13,r0,0
	align	 4
@L5255:
	bcnd	 eq0,r10,@L5257
	ld.hu	 r13,r10,8
@L9015:
	br.n	 @L5227
	st.h	 r13,r31,104
	align	 4
@L5257:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5259
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5262
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5262:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5227
	st.h	 r13,r31,104
	align	 4
@L5259:
	br.n	 @L9015
	or	 r13,r0,0
	align	 4
@L5264:
	bcnd	 eq0,r10,@L5266
	ld	 r13,r10,8
@L9016:
	br.n	 @L5227
	st	 r13,r31,104
	align	 4
@L5266:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5268
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5271
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5271:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5227
	st	 r13,r31,104
	align	 4
@L5268:
	br.n	 @L9016
	or	 r13,r0,0
	align	 4
@L5286:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5227:
	or.u	 r14,r0,hi16(_spec_regs_R)
	ld	 r13,r31,104
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L5225
	st	 r13,r14[r21]
	align	 4
@L5224:
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld.bu	 r10,r31,93
	or.u	 r12,r0,hi16(_temp_bs)
	ld	 r11,r13,lo16(_sim_swap_words)
	or.u	 r13,r0,hi16(_temp_rd)
	ld	 r12,r12,lo16(_temp_bs)
	ld	 r13,r13,lo16(_temp_rd)
	xor	 r25,r10,r11
	addu	 r24,r12,r13
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r13,r31,104
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r25]
@L5225:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5352
	or.u	 r12,r0,hi16(_sim_swap_words)
	ld.bu	 r13,r31,93
	ld	 r9,r12,lo16(_sim_swap_words)
	addu	 r13,r13,1
	xor	 r13,r13,r9
	or	 r12,r0,1
	extu	 r10,r13,0<5>
	mask	 r13,r13,31
	ld	 r11,r18[r10]
	mak	 r12,r12,r13
	or.u	 r13,r0,hi16(_temp_bs)
	or	 r11,r11,r12
	ld	 r12,r13,lo16(_temp_bs)
	st	 r11,r18[r10]
	or.u	 r13,r0,hi16(_temp_rd)
	ld	 r11,r13,lo16(_temp_rd)
	ld.bu	 r13,r31,93
	or	 r22,r0,4
	addu	 r12,r12,r11
	addu	 r13,r13,1
	addu	 r24,r12,4
	xor	 r21,r13,r9
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5355
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5360
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9561
	extu	 r13,r24,0<24>
@L5360:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5355
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5355
	extu	 r13,r24,0<24>
@L9561:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5363
	or	 r11,r0,0
@L5365:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5364
	bcnd	 eq0,r11,@L5363
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5363
	st	 r10,r12[r25]
	align	 4
@L5364:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5365
@L5363:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5369
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9562
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5370
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5370:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9562:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5369
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5369:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5383
	bb1.n	 gt,r13,@L5392
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5414
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L5376
	ld.bu	 r13,r10,8
@L9017:
	br.n	 @L5355
	st.b	 r13,r31,104
	align	 4
@L5376:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5378
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5381
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5381:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5355
	st.b	 r13,r31,104
	align	 4
@L5378:
	br.n	 @L9017
	or	 r13,r0,0
	align	 4
@L5383:
	bcnd	 eq0,r10,@L5385
	ld.hu	 r13,r10,8
@L9018:
	br.n	 @L5355
	st.h	 r13,r31,104
	align	 4
@L5385:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5387
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5390
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5390:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5355
	st.h	 r13,r31,104
	align	 4
@L5387:
	br.n	 @L9018
	or	 r13,r0,0
	align	 4
@L5392:
	bcnd	 eq0,r10,@L5394
	ld	 r13,r10,8
@L9019:
	br.n	 @L5355
	st	 r13,r31,104
	align	 4
@L5394:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5396
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5399
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5399:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5355
	st	 r13,r31,104
	align	 4
@L5396:
	br.n	 @L9019
	or	 r13,r0,0
	align	 4
@L5414:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5355:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r13,r31,104
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L5353
	st	 r13,r15[r21]
	align	 4
@L5352:
	ld.bu	 r11,r31,93
	or.u	 r13,r0,hi16(_temp_bs)
	ld	 r12,r13,lo16(_temp_bs)
	or.u	 r13,r0,hi16(_temp_rd)
	ld	 r10,r13,lo16(_temp_rd)
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r13,r13,lo16(_sim_swap_words)
	addu	 r11,r11,1
	addu	 r12,r12,r10
	xor	 r25,r11,r13
	addu	 r24,r12,4
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r13,r31,104
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r25]
@L5353:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5480
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L5480:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L5486:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	extu	 r13,r12,8<16>
	addu	 r13,r13,32
	st	 r0,r31,172
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r9,@L5487
	st	 r13,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L5487:
	or.u	 r15,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L5489
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L5491
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9563
	addu	 r13,r13,r12
	align	 4
@L5491:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9563:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L5490
	st	 r13,r15[r9]
	align	 4
@L5489:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5493
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9564
	addu	 r13,r13,r12
	align	 4
@L5493:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9564:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L5490:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5495
	or.u	 r10,r0,hi16(_use_spec_F)
	ld.hu	 r11,r31,92
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r12,r11,30
	extu	 r11,r11,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r11]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r10[r11]
	ld	 r10,r31,92
	extu	 r13,r10,0<29>
	extu	 r8,r10,0<24>
	ld	 r11,r18[r13]
	mask	 r13,r8,31
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r20,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L5497
	extu	 r21,r10,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r8]
	br	 @L5498
	align	 4
@L5497:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r8]
@L5498:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5499
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9565
	addu	 r24,r3,r13
	align	 4
@L5499:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L9565:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5568
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5569
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5507
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9566
	extu	 r13,r24,0<24>
@L5507:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5569
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5569
	extu	 r13,r24,0<24>
@L9566:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5510
	or	 r11,r0,0
@L5512:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5511
	bcnd	 eq0,r11,@L5510
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5510
	st	 r10,r12[r25]
	align	 4
@L5511:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5512
@L5510:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5516
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9567
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5517
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5517:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9567:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5516
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5516:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5530
	bb1.n	 gt,r13,@L5539
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5561
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L9324
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5525
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5528
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5528:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5569
	st.b	 r13,r31,104
	align	 4
@L5525:
	br.n	 @L9029
	or	 r13,r0,0
	align	 4
@L5530:
	bcnd.n	 ne0,r10,@L9325
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5534
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5537
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5537:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5569
	st.h	 r13,r31,104
	align	 4
@L5534:
	br.n	 @L9030
	or	 r13,r0,0
	align	 4
@L5539:
	bcnd.n	 ne0,r10,@L9326
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5543
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5546
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5546:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5569
	st	 r13,r31,104
	align	 4
@L5543:
	br.n	 @L9031
	or	 r13,r0,0
	align	 4
@L5561:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5495:
	ld	 r9,r31,92
	extu	 r13,r9,0<29>
	extu	 r8,r9,0<24>
	ld	 r11,r18[r13]
	mask	 r10,r8,31
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_regs_F)
	mak	 r13,r13,r10
	or	 r20,r12,lo16(_regs_F)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5564
	extu	 r21,r9,8<16>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r3,r15[r8]
	br	 @L5565
	align	 4
@L5564:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r3,r14[r8]
@L5565:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5566
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9568
	addu	 r24,r3,r13
	align	 4
@L5566:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	addu	 r24,r3,r13
@L9568:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5568
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5569
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5574
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9569
	extu	 r13,r24,0<24>
@L5574:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5569
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5569
	extu	 r13,r24,0<24>
@L9569:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5577
	or	 r11,r0,0
@L5579:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5578
	bcnd	 eq0,r11,@L5577
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5577
	st	 r10,r12[r25]
	align	 4
@L5578:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5579
@L5577:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5583
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9570
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5584
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5584:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9570:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5583
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5583:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5597
	bb1.n	 gt,r13,@L5606
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5628
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L5590
	or.u	 r13,r0,hi16(_mem_table)
@L9324:
	ld.bu	 r13,r10,8
@L9029:
	br.n	 @L5569
	st.b	 r13,r31,104
	align	 4
@L5590:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5592
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5595
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5595:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5569
	st.b	 r13,r31,104
	align	 4
@L5592:
	br.n	 @L9029
	or	 r13,r0,0
	align	 4
@L5597:
	bcnd.n	 eq0,r10,@L5599
	or.u	 r13,r0,hi16(_mem_table)
@L9325:
	ld.hu	 r13,r10,8
@L9030:
	br.n	 @L5569
	st.h	 r13,r31,104
	align	 4
@L5599:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5601
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5604
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5604:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5569
	st.h	 r13,r31,104
	align	 4
@L5601:
	br.n	 @L9030
	or	 r13,r0,0
	align	 4
@L5606:
	bcnd.n	 eq0,r10,@L5608
	or.u	 r13,r0,hi16(_mem_table)
@L9326:
	ld	 r13,r10,8
@L9031:
	br.n	 @L5569
	st	 r13,r31,104
	align	 4
@L5608:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5610
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5613
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5613:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5569
	st	 r13,r31,104
	align	 4
@L5610:
	br.n	 @L9031
	or	 r13,r0,0
	align	 4
@L5628:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5568:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L5569:
	ld	 r13,r31,104
	st	 r13,r20[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5631
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L5631:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L5637:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	extu	 r13,r12,8<16>
	addu	 r13,r13,32
	st	 r0,r31,172
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r9,@L5638
	st	 r13,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L5638:
	or.u	 r14,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r15,r11,8<8>
	bcnd.n	 eq0,r13,@L5640
	st	 r15,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L5642
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9571
	addu	 r13,r13,r12
	align	 4
@L5642:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9571:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L5641
	st	 r13,r14[r9]
	align	 4
@L5640:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5644
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9572
	addu	 r13,r13,r12
	align	 4
@L5644:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9572:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L5641:
	ld	 r10,r31,92
	extu	 r7,r10,0<16>
	bb0.n	 (31-31),r7,@L5647
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8666
	or.u	 r2,r0,hi16(@LC555)
	or	 r4,r0,469
	or.u	 r3,r0,hi16(@LC556)
	or.u	 r5,r0,hi16(@LC557)
	or	 r2,r2,lo16(@LC555)
	or	 r3,r3,lo16(@LC556)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC557)
	align	 4
@L5647:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5650
	or.u	 r13,r0,hi16(_sim_swap_words)
@L8666:
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r8,r13,lo16(_sim_swap_words)
	or.u	 r11,r0,hi16(_use_spec_F)
	or	 r10,r0,1
	xor	 r12,r12,r8
	or	 r11,r11,lo16(_use_spec_F)
	extu	 r9,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r11[r9]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r11[r9]
	ld	 r12,r31,92
	extu	 r11,r12,8<16>
	extu	 r9,r12,0<24>
	extu	 r12,r12,0<29>
	mask	 r13,r9,31
	mak	 r10,r10,r13
	ld	 r13,r18[r12]
	xor	 r21,r11,r8
	or.u	 r12,r0,hi16(_spec_regs_F)
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L5652
	or	 r20,r12,lo16(_spec_regs_F)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r9]
	br	 @L5653
	align	 4
@L5652:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r9]
@L5653:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5654
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9573
	addu	 r24,r3,r13
	align	 4
@L5654:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9573:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5723
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5724
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5662
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9574
	extu	 r13,r24,0<24>
@L5662:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5724
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5724
	extu	 r13,r24,0<24>
@L9574:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5665
	or	 r11,r0,0
@L5667:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5666
	bcnd	 eq0,r11,@L5665
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5665
	st	 r10,r12[r25]
	align	 4
@L5666:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5667
@L5665:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5671
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9575
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5672
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5672:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9575:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5671
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5671:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5685
	bb1.n	 gt,r13,@L5694
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5716
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L9327
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5680
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5683
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5683:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5724
	st.b	 r13,r31,104
	align	 4
@L5680:
	br.n	 @L9041
	or	 r13,r0,0
	align	 4
@L5685:
	bcnd.n	 ne0,r10,@L9328
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5689
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5692
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5692:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5724
	st.h	 r13,r31,104
	align	 4
@L5689:
	br.n	 @L9042
	or	 r13,r0,0
	align	 4
@L5694:
	bcnd.n	 ne0,r10,@L9329
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5698
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5701
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5701:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5724
	st	 r13,r31,104
	align	 4
@L5698:
	br.n	 @L9043
	or	 r13,r0,0
	align	 4
@L5716:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5650:
	extu	 r12,r10,0<29>
	ld	 r9,r13,lo16(_sim_swap_words)
	extu	 r8,r10,0<24>
	ld	 r11,r18[r12]
	mask	 r10,r8,31
	or	 r13,r0,1
	mask	 r12,r7,255
	mak	 r13,r13,r10
	xor	 r21,r12,r9
	or.u	 r12,r0,hi16(_regs_F)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5719
	or	 r20,r12,lo16(_regs_F)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r3,r15[r8]
	br	 @L5720
	align	 4
@L5719:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r3,r14[r8]
@L5720:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5721
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r8]
	br.n	 @L9576
	addu	 r24,r3,r13
	align	 4
@L5721:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r8]
	addu	 r24,r3,r13
@L9576:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5723
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5724
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5729
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9577
	extu	 r13,r24,0<24>
@L5729:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5724
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5724
	extu	 r13,r24,0<24>
@L9577:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5732
	or	 r11,r0,0
@L5734:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5733
	bcnd	 eq0,r11,@L5732
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5732
	st	 r10,r12[r25]
	align	 4
@L5733:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5734
@L5732:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5738
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9578
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5739
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5739:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9578:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5738
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5738:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5752
	bb1.n	 gt,r13,@L5761
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5783
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L5745
	or.u	 r13,r0,hi16(_mem_table)
@L9327:
	ld.bu	 r13,r10,8
@L9041:
	br.n	 @L5724
	st.b	 r13,r31,104
	align	 4
@L5745:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5747
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5750
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5750:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5724
	st.b	 r13,r31,104
	align	 4
@L5747:
	br.n	 @L9041
	or	 r13,r0,0
	align	 4
@L5752:
	bcnd.n	 eq0,r10,@L5754
	or.u	 r13,r0,hi16(_mem_table)
@L9328:
	ld.hu	 r13,r10,8
@L9042:
	br.n	 @L5724
	st.h	 r13,r31,104
	align	 4
@L5754:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5756
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5759
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5759:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5724
	st.h	 r13,r31,104
	align	 4
@L5756:
	br.n	 @L9042
	or	 r13,r0,0
	align	 4
@L5761:
	bcnd.n	 eq0,r10,@L5763
	or.u	 r13,r0,hi16(_mem_table)
@L9329:
	ld	 r13,r10,8
@L9043:
	br.n	 @L5724
	st	 r13,r31,104
	align	 4
@L5763:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5765
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5768
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5768:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5724
	st	 r13,r31,104
	align	 4
@L5765:
	br.n	 @L9043
	or	 r13,r0,0
	align	 4
@L5783:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5723:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L5724:
	ld	 r13,r31,104
	st	 r13,r20[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5786
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld.bu	 r12,r31,93
	or.u	 r11,r0,hi16(_use_spec_F)
	ld	 r8,r13,lo16(_sim_swap_words)
	addu	 r12,r12,1
	or	 r10,r0,1
	xor	 r12,r12,r8
	or	 r11,r11,lo16(_use_spec_F)
	extu	 r9,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r11[r9]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r11[r9]
	ld	 r12,r31,92
	extu	 r11,r12,8<16>
	extu	 r9,r12,0<24>
	extu	 r12,r12,0<29>
	mask	 r13,r9,31
	addu	 r11,r11,1
	mak	 r10,r10,r13
	ld	 r13,r18[r12]
	xor	 r21,r11,r8
	or.u	 r12,r0,hi16(_spec_regs_F)
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L5788
	or	 r20,r12,lo16(_spec_regs_F)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L5789
	align	 4
@L5788:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L5789:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5790
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9579
	addu	 r13,r9,r13
	align	 4
@L5790:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	addu	 r13,r9,r13
@L9579:
	addu	 r24,r13,4
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5859
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5860
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5798
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9580
	extu	 r13,r24,0<24>
@L5798:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5860
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5860
	extu	 r13,r24,0<24>
@L9580:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5801
	or	 r11,r0,0
@L5803:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5802
	bcnd	 eq0,r11,@L5801
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5801
	st	 r10,r12[r25]
	align	 4
@L5802:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5803
@L5801:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5807
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9581
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5808
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5808:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9581:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5807
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5807:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5821
	bb1.n	 gt,r13,@L5830
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5852
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L9330
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5816
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5819
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5819:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5860
	st.b	 r13,r31,104
	align	 4
@L5816:
	br.n	 @L9049
	or	 r13,r0,0
	align	 4
@L5821:
	bcnd.n	 ne0,r10,@L9331
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5825
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5828
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5828:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5860
	st.h	 r13,r31,104
	align	 4
@L5825:
	br.n	 @L9050
	or	 r13,r0,0
	align	 4
@L5830:
	bcnd.n	 ne0,r10,@L9332
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5834
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5837
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5837:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5860
	st	 r13,r31,104
	align	 4
@L5834:
	br.n	 @L9051
	or	 r13,r0,0
	align	 4
@L5852:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5786:
	ld	 r13,r31,92
	or.u	 r12,r0,hi16(_sim_swap_words)
	extu	 r11,r13,0<29>
	ld	 r9,r12,lo16(_sim_swap_words)
	extu	 r8,r13,0<24>
	or	 r12,r0,1
	extu	 r13,r13,8<16>
	ld	 r10,r18[r11]
	mask	 r11,r8,31
	addu	 r13,r13,1
	mak	 r12,r12,r11
	xor	 r21,r13,r9
	or.u	 r13,r0,hi16(_regs_F)
	and	 r10,r10,r12
	bcnd.n	 eq0,r10,@L5855
	or	 r20,r13,lo16(_regs_F)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r8]
	br	 @L5856
	align	 4
@L5855:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r8]
@L5856:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5857
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9582
	addu	 r13,r9,r13
	align	 4
@L5857:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	addu	 r13,r9,r13
@L9582:
	addu	 r24,r13,4
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5859
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L5860
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L5865
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9583
	extu	 r13,r24,0<24>
@L5865:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5860
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5860
	extu	 r13,r24,0<24>
@L9583:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5868
	or	 r11,r0,0
@L5870:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5869
	bcnd	 eq0,r11,@L5868
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5868
	st	 r10,r12[r25]
	align	 4
@L5869:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5870
@L5868:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L5874
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9584
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5875
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5875:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9584:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5874
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5874:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L5888
	bb1.n	 gt,r13,@L5897
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L5919
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L5881
	or.u	 r13,r0,hi16(_mem_table)
@L9330:
	ld.bu	 r13,r10,8
@L9049:
	br.n	 @L5860
	st.b	 r13,r31,104
	align	 4
@L5881:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5883
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5886
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5886:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L5860
	st.b	 r13,r31,104
	align	 4
@L5883:
	br.n	 @L9049
	or	 r13,r0,0
	align	 4
@L5888:
	bcnd.n	 eq0,r10,@L5890
	or.u	 r13,r0,hi16(_mem_table)
@L9331:
	ld.hu	 r13,r10,8
@L9050:
	br.n	 @L5860
	st.h	 r13,r31,104
	align	 4
@L5890:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5892
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5895
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5895:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L5860
	st.h	 r13,r31,104
	align	 4
@L5892:
	br.n	 @L9050
	or	 r13,r0,0
	align	 4
@L5897:
	bcnd.n	 eq0,r10,@L5899
	or.u	 r13,r0,hi16(_mem_table)
@L9332:
	ld	 r13,r10,8
@L9051:
	br.n	 @L5860
	st	 r13,r31,104
	align	 4
@L5899:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L5901
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L5904
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L5904:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L5860
	st	 r13,r31,104
	align	 4
@L5901:
	br.n	 @L9051
	or	 r13,r0,0
	align	 4
@L5919:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5859:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L5860:
	ld	 r13,r31,104
	st	 r13,r20[r21]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5922
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L5922:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L5928:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L5929
	ld.bu	 r14,r31,92
	br.n	 @L5930
	st	 r14,r31,164
	align	 4
@L5929:
	st	 r0,r31,164
@L5930:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r15,r11,8<16>
	st	 r0,r31,172
	extu	 r17,r11,0<24>
	st	 r15,r31,180
	extu	 r15,r11,8<8>
	bcnd.n	 eq0,r13,@L5931
	st	 r15,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L5933
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9585
	addu	 r13,r13,r12
	align	 4
@L5933:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9585:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L5932
	st	 r13,r14[r9]
	align	 4
@L5931:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L5935
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9586
	addu	 r13,r13,r12
	align	 4
@L5935:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9586:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L5932:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5937
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r10]
	br	 @L5938
	align	 4
@L5937:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r10]
@L5938:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5939
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9587
	addu	 r24,r3,r13
	align	 4
@L5939:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9587:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L5941
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9588
	or	 r12,r0,r13
	align	 4
@L5941:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	or	 r12,r0,r13
@L9588:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L5943
	st.b	 r12,r31,97
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L5944
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L5944
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L5952
	or	 r11,r0,0
@L5954:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L5953
	bcnd	 eq0,r11,@L5952
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L5952
	st	 r10,r12[r25]
	align	 4
@L5953:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L5954
@L5952:
	bcnd.n	 ne0,r10,@L9589
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9590
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L5959
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L5959:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9590:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L5958
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L5958:
	cmp	 r13,r22,2
@L9589:
	bb0	 ne,r13,@L5973
	bb0.n	 gt,r13,@L5964
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L5982
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L5991
	or.u	 r2,r0,hi16(@LC509)
	br	 @L6003
	align	 4
@L5964:
	ld.bu	 r13,r31,97
	br.n	 @L5944
	st.b	 r13,r10,8
	align	 4
@L5973:
	ld.hu	 r13,r31,97
	br.n	 @L5944
	st.h	 r13,r10,8
	align	 4
@L5982:
	ld	 r13,r31,97
	br.n	 @L5944
	st	 r13,r10,8
	align	 4
@L5991:
	ld	 r13,r31,97
	st	 r13,r10,8
	ld	 r13,r31,101
	br.n	 @L5944
	st	 r13,r10,12
	align	 4
@L6003:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L5943:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,97
	bsr.n	 _mem_access
	or	 r5,r0,1
@L5944:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6006
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L6006:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L6012:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L6013
	ld.bu	 r14,r31,92
	br.n	 @L6014
	st	 r14,r31,164
	align	 4
@L6013:
	st	 r0,r31,164
@L6014:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r15,r11,8<16>
	st	 r0,r31,172
	extu	 r17,r11,0<24>
	st	 r15,r31,180
	extu	 r15,r11,8<8>
	bcnd.n	 eq0,r13,@L6015
	st	 r15,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6017
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9591
	addu	 r13,r13,r12
	align	 4
@L6017:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9591:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L6016
	st	 r13,r14[r9]
	align	 4
@L6015:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+20)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+20)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6019
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9592
	addu	 r13,r13,r12
	align	 4
@L6019:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9592:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L6016:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6021
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r10]
	br	 @L6022
	align	 4
@L6021:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r10]
@L6022:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6023
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9593
	addu	 r24,r3,r13
	align	 4
@L6023:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9593:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6025
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9594
	or	 r12,r0,r13
	align	 4
@L6025:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	or	 r12,r0,r13
@L9594:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6027
	st.h	 r12,r31,100
	bb1.n	 (31-31),r24,@L6028
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6028
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6028
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6036
	or	 r11,r0,0
@L6038:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6037
	bcnd	 eq0,r11,@L6036
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6036
	st	 r10,r12[r25]
	align	 4
@L6037:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6038
@L6036:
	bcnd.n	 ne0,r10,@L6057
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9595
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6043
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6043:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9595:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6057
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6057:
	ld.hu	 r13,r31,100
	br.n	 @L6028
	st.h	 r13,r10,8
	align	 4
@L6027:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,100
	bsr.n	 _mem_access
	or	 r5,r0,2
@L6028:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6090
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L6090:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L6096:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L6097
	ld.bu	 r14,r31,92
	br.n	 @L6098
	st	 r14,r31,164
	align	 4
@L6097:
	st	 r0,r31,164
@L6098:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r15,r11,8<16>
	st	 r0,r31,172
	extu	 r17,r11,0<24>
	st	 r15,r31,180
	extu	 r15,r11,8<8>
	bcnd.n	 eq0,r13,@L6099
	st	 r15,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6101
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9596
	addu	 r13,r13,r12
	align	 4
@L6101:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9596:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L6100
	st	 r13,r14[r9]
	align	 4
@L6099:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6103
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9597
	addu	 r13,r13,r12
	align	 4
@L6103:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9597:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L6100:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6105
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r10]
	br	 @L6106
	align	 4
@L6105:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r10]
@L6106:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6107
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9598
	addu	 r24,r3,r13
	align	 4
@L6107:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9598:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6109
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r11,r14[r10]
	br.n	 @L9599
	or.u	 r14,r0,hi16(_spec_mode)
	align	 4
@L6109:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r11,r15[r10]
	or.u	 r14,r0,hi16(_spec_mode)
@L9599:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6111
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6112
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6112
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6112
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6120
	or	 r11,r0,0
@L6122:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6121
	bcnd	 eq0,r11,@L6120
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6120
	st	 r10,r12[r25]
	align	 4
@L6121:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6122
@L6120:
	bcnd.n	 ne0,r10,@L9600
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9601
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6127
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6127:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9601:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6126
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6126:
	cmp	 r13,r22,2
@L9600:
	bb0	 ne,r13,@L6141
	bb1.n	 gt,r13,@L6150
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6171
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6112
	st.b	 r13,r10,8
	align	 4
@L6141:
	ld.hu	 r13,r31,104
	br.n	 @L6112
	st.h	 r13,r10,8
	align	 4
@L6150:
	ld	 r13,r31,104
	br.n	 @L6112
	st	 r13,r10,8
	align	 4
@L6171:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6111:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6112:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6174
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L6174:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L6180:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L6181
	ld.bu	 r14,r31,92
	br.n	 @L6182
	st	 r14,r31,164
	align	 4
@L6181:
	st	 r0,r31,164
@L6182:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r12,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	extu	 r13,r11,0<16>
	st	 r0,r31,172
	extu	 r14,r11,8<8>
	st	 r14,r31,188
	mask	 r13,r13,254
	bcnd.n	 eq0,r12,@L6183
	st	 r13,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6185
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9602
	addu	 r13,r13,r12
	align	 4
@L6185:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9602:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L6184
	st	 r13,r15[r9]
	align	 4
@L6183:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6187
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9603
	addu	 r13,r13,r12
	align	 4
@L6187:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9603:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L6184:
	ld.hu	 r13,r31,92
	bb0.n	 (31-31),r13,@L6190
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L6190
	or.u	 r2,r0,hi16(@LC558)
	or	 r4,r0,493
	or.u	 r3,r0,hi16(@LC559)
	or.u	 r5,r0,hi16(@LC560)
	or	 r2,r2,lo16(@LC558)
	or	 r3,r3,lo16(@LC559)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC560)
	align	 4
@L6190:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6193
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r10]
	br	 @L6194
	align	 4
@L6193:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r10]
@L6194:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6195
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9604
	addu	 r24,r3,r13
	align	 4
@L6195:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9604:
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r13,r13,lo16(_sim_swap_words)
	xor	 r10,r12,r13
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6197
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r11,r14[r10]
	br.n	 @L9605
	or.u	 r14,r0,hi16(_spec_mode)
	align	 4
@L6197:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r11,r15[r10]
	or.u	 r14,r0,hi16(_spec_mode)
@L9605:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6199
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6200
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6200
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6200
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6208
	or	 r11,r0,0
@L6210:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6209
	bcnd	 eq0,r11,@L6208
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6208
	st	 r10,r12[r25]
	align	 4
@L6209:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6210
@L6208:
	bcnd.n	 ne0,r10,@L9606
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9607
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6215
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6215:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9607:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6214
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6214:
	cmp	 r13,r22,2
@L9606:
	bb0	 ne,r13,@L6229
	bb1.n	 gt,r13,@L6238
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6259
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6200
	st.b	 r13,r10,8
	align	 4
@L6229:
	ld.hu	 r13,r31,104
	br.n	 @L6200
	st.h	 r13,r10,8
	align	 4
@L6238:
	ld	 r13,r31,104
	br.n	 @L6200
	st	 r13,r10,8
	align	 4
@L6259:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6199:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6200:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6262
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r10]
	br	 @L6263
	align	 4
@L6262:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r10]
@L6263:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6264
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9608
	addu	 r13,r9,r13
	align	 4
@L6264:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	addu	 r13,r9,r13
@L9608:
	addu	 r24,r13,4
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r13,r13,lo16(_sim_swap_words)
	addu	 r12,r12,1
	xor	 r10,r12,r13
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6266
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r10]
	br.n	 @L9609
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L6266:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r10]
	or.u	 r15,r0,hi16(_spec_mode)
@L9609:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6268
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6269
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6269
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6269
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6277
	or	 r11,r0,0
@L6279:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6278
	bcnd	 eq0,r11,@L6277
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6277
	st	 r10,r12[r25]
	align	 4
@L6278:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6279
@L6277:
	bcnd.n	 ne0,r10,@L9610
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9611
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6284
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6284:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9611:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6283
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6283:
	cmp	 r13,r22,2
@L9610:
	bb0	 ne,r13,@L6298
	bb1.n	 gt,r13,@L6307
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6328
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6269
	st.b	 r13,r10,8
	align	 4
@L6298:
	ld.hu	 r13,r31,104
	br.n	 @L6269
	st.h	 r13,r10,8
	align	 4
@L6307:
	ld	 r13,r31,104
	br.n	 @L6269
	st	 r13,r10,8
	align	 4
@L6328:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6268:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6269:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6331
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L6331:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L6337:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L6338
	ld.bu	 r15,r31,92
	br.n	 @L6339
	st	 r15,r31,164
	align	 4
@L6338:
	st	 r0,r31,164
@L6339:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,172
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r15,r11,8<8>
	bcnd.n	 eq0,r13,@L6340
	st	 r15,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6342
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9612
	addu	 r13,r13,r12
	align	 4
@L6342:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9612:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L6341
	st	 r13,r14[r9]
	align	 4
@L6340:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6344
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9613
	addu	 r13,r13,r12
	align	 4
@L6344:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9613:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L6341:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6346
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r10]
	br	 @L6347
	align	 4
@L6346:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r10]
@L6347:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6348
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9614
	addu	 r24,r3,r13
	align	 4
@L6348:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9614:
	or.u	 r14,r0,hi16(_use_spec_R)
	ld	 r13,r14,lo16(_use_spec_R)
	bb0.n	 (31-31),r13,@L6350
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r12,r15,lo16(_spec_regs_R)
	br.n	 @L9615
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L6350:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r12,r14,lo16(_regs_R)
	or.u	 r15,r0,hi16(_spec_mode)
@L9615:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6352
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6353
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6353
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6353
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6361
	or	 r11,r0,0
@L6363:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6362
	bcnd	 eq0,r11,@L6361
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6361
	st	 r10,r12[r25]
	align	 4
@L6362:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6363
@L6361:
	bcnd.n	 ne0,r10,@L9616
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9617
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6368
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6368:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9617:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6367
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6367:
	cmp	 r13,r22,2
@L9616:
	bb0	 ne,r13,@L6382
	bb1.n	 gt,r13,@L6391
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6412
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6353
	st.b	 r13,r10,8
	align	 4
@L6382:
	ld.hu	 r13,r31,104
	br.n	 @L6353
	st.h	 r13,r10,8
	align	 4
@L6391:
	ld	 r13,r31,104
	br.n	 @L6353
	st	 r13,r10,8
	align	 4
@L6412:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6352:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6353:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6415
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r10]
	br	 @L6416
	align	 4
@L6415:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r10]
@L6416:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6417
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9618
	addu	 r13,r9,r13
	align	 4
@L6417:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	addu	 r13,r9,r13
@L9618:
	addu	 r24,r13,4
	or.u	 r14,r0,hi16(_use_spec_R)
	ld	 r13,r14,lo16(_use_spec_R)
	bb0.n	 (31-31),r13,@L6419
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r12,r15,lo16(_spec_regs_R)
	br.n	 @L9619
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L6419:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r12,r14,lo16(_regs_R)
	or.u	 r15,r0,hi16(_spec_mode)
@L9619:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6421
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6422
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6422
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6422
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6430
	or	 r11,r0,0
@L6432:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6431
	bcnd	 eq0,r11,@L6430
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6430
	st	 r10,r12[r25]
	align	 4
@L6431:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6432
@L6430:
	bcnd.n	 ne0,r10,@L9620
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9621
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6437
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6437:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9621:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6436
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6436:
	cmp	 r13,r22,2
@L9620:
	bb0	 ne,r13,@L6451
	bb1.n	 gt,r13,@L6460
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6481
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6422
	st.b	 r13,r10,8
	align	 4
@L6451:
	ld.hu	 r13,r31,104
	br.n	 @L6422
	st.h	 r13,r10,8
	align	 4
@L6460:
	ld	 r13,r31,104
	br.n	 @L6422
	st	 r13,r10,8
	align	 4
@L6481:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6421:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6422:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6484
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L6484:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L6490:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L6491
	ld.bu	 r15,r31,92
	br.n	 @L6492
	st	 r15,r31,164
	align	 4
@L6491:
	st	 r0,r31,164
@L6492:
	ld	 r11,r31,92
	or.u	 r14,r0,hi16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,172
	extu	 r13,r11,8<16>
	ld	 r12,r14,lo16(_spec_mode)
	extu	 r15,r11,8<8>
	addu	 r13,r13,32
	st	 r15,r31,188
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r12,@L6493
	st	 r13,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6495
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9622
	addu	 r13,r13,r12
	align	 4
@L6495:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9622:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L6494
	st	 r13,r14[r9]
	align	 4
@L6493:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6497
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9623
	addu	 r13,r13,r12
	align	 4
@L6497:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9623:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L6494:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6499
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r10]
	br	 @L6500
	align	 4
@L6499:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r10]
@L6500:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6501
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9624
	addu	 r24,r3,r13
	align	 4
@L6501:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9624:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6503
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9086
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L6503:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9086:
	ld	 r12,r13[r12]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6505
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6506
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6506
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6506
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6514
	or	 r11,r0,0
@L6516:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6515
	bcnd	 eq0,r11,@L6514
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6514
	st	 r10,r12[r25]
	align	 4
@L6515:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6516
@L6514:
	bcnd.n	 ne0,r10,@L9625
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9626
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6521
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6521:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9626:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6520
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6520:
	cmp	 r13,r22,2
@L9625:
	bb0	 ne,r13,@L6535
	bb1.n	 gt,r13,@L6544
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6565
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6506
	st.b	 r13,r10,8
	align	 4
@L6535:
	ld.hu	 r13,r31,104
	br.n	 @L6506
	st.h	 r13,r10,8
	align	 4
@L6544:
	ld	 r13,r31,104
	br.n	 @L6506
	st	 r13,r10,8
	align	 4
@L6565:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6505:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6506:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6568
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L6568:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L6574:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L6575
	ld.bu	 r14,r31,92
	br.n	 @L6576
	st	 r14,r31,164
	align	 4
@L6575:
	st	 r0,r31,164
@L6576:
	ld	 r11,r31,92
	or.u	 r15,r0,hi16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,172
	extu	 r13,r11,8<16>
	ld	 r12,r15,lo16(_spec_mode)
	extu	 r14,r11,8<8>
	addu	 r13,r13,32
	st	 r14,r31,188
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r12,@L6577
	st	 r13,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6579
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9627
	addu	 r13,r13,r12
	align	 4
@L6579:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9627:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L6578
	st	 r13,r15[r9]
	align	 4
@L6577:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+140)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+140)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6581
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9628
	addu	 r13,r13,r12
	align	 4
@L6581:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9628:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L6578:
	ld.hu	 r13,r31,92
	bb0.n	 (31-31),r13,@L6584
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L6584
	or.u	 r2,r0,hi16(@LC561)
	or	 r4,r0,514
	or.u	 r3,r0,hi16(@LC562)
	or.u	 r5,r0,hi16(@LC563)
	or	 r2,r2,lo16(@LC561)
	or	 r3,r3,lo16(@LC562)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC563)
	align	 4
@L6584:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6587
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r3,r14[r10]
	br	 @L6588
	align	 4
@L6587:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r3,r15[r10]
@L6588:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6589
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9629
	addu	 r24,r3,r13
	align	 4
@L6589:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r24,r3,r13
@L9629:
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r13,r13,lo16(_sim_swap_words)
	xor	 r9,r12,r13
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6591
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9092
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L6591:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9092:
	ld	 r10,r13[r9]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6593
	st	 r10,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6594
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6594
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6594
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6602
	or	 r11,r0,0
@L6604:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6603
	bcnd	 eq0,r11,@L6602
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6602
	st	 r10,r12[r25]
	align	 4
@L6603:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6604
@L6602:
	bcnd.n	 ne0,r10,@L9630
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9631
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6609
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6609:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9631:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6608
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6608:
	cmp	 r13,r22,2
@L9630:
	bb0	 ne,r13,@L6623
	bb1.n	 gt,r13,@L6632
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6653
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6594
	st.b	 r13,r10,8
	align	 4
@L6623:
	ld.hu	 r13,r31,104
	br.n	 @L6594
	st.h	 r13,r10,8
	align	 4
@L6632:
	ld	 r13,r31,104
	br.n	 @L6594
	st	 r13,r10,8
	align	 4
@L6653:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6593:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6594:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6656
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r10]
	br	 @L6657
	align	 4
@L6656:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r10]
@L6657:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6658
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9632
	addu	 r13,r9,r13
	align	 4
@L6658:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	addu	 r13,r9,r13
@L9632:
	addu	 r24,r13,4
	ld.bu	 r12,r31,93
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r13,r13,lo16(_sim_swap_words)
	addu	 r12,r12,1
	xor	 r9,r12,r13
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6660
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9094
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L6660:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9094:
	ld	 r10,r13[r9]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6662
	st	 r10,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6663
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6663
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6663
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6671
	or	 r11,r0,0
@L6673:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6672
	bcnd	 eq0,r11,@L6671
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6671
	st	 r10,r12[r25]
	align	 4
@L6672:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6673
@L6671:
	bcnd.n	 ne0,r10,@L9633
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9634
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6678
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6678:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9634:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6677
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6677:
	cmp	 r13,r22,2
@L9633:
	bb0	 ne,r13,@L6692
	bb1.n	 gt,r13,@L6701
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6722
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6663
	st.b	 r13,r10,8
	align	 4
@L6692:
	ld.hu	 r13,r31,104
	br.n	 @L6663
	st.h	 r13,r10,8
	align	 4
@L6701:
	ld	 r13,r31,104
	br.n	 @L6663
	st	 r13,r10,8
	align	 4
@L6722:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6662:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6663:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6725
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L6725:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L6731:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	extu	 r13,r12,8<16>
	addu	 r13,r13,32
	st	 r0,r31,172
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r9,@L6732
	st	 r13,r31,164
	extu	 r15,r12,0<24>
	st	 r15,r31,172
@L6732:
	or.u	 r14,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r15,r11,8<8>
	bcnd.n	 eq0,r13,@L6734
	st	 r15,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6736
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9635
	addu	 r13,r13,r12
	align	 4
@L6736:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9635:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L6735
	st	 r13,r14[r9]
	align	 4
@L6734:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6738
	or	 r10,r0,r17
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9636
	addu	 r13,r13,r12
	align	 4
@L6738:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	addu	 r13,r13,r12
@L9636:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r13,r15[r10]
@L6735:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6740
	or.u	 r10,r0,hi16(_use_spec_F)
	ld.hu	 r11,r31,92
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r12,r11,30
	extu	 r11,r11,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r11]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r10[r11]
	ld	 r10,r31,92
	extu	 r13,r10,0<29>
	extu	 r8,r10,0<24>
	ld	 r11,r18[r13]
	mask	 r13,r8,31
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r20,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L6742
	extu	 r21,r10,8<16>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r10,r15[r8]
	br	 @L6743
	align	 4
@L6742:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r10,r14[r8]
@L6743:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6744
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9637
	addu	 r13,r10,r13
	align	 4
@L6744:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r10,r13
@L9637:
	addu	 r24,r13,4
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6813
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6814
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L6752
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9638
	extu	 r13,r24,0<24>
@L6752:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6814
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6814
	extu	 r13,r24,0<24>
@L9638:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6755
	or	 r11,r0,0
@L6757:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6756
	bcnd	 eq0,r11,@L6755
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6755
	st	 r10,r12[r25]
	align	 4
@L6756:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6757
@L6755:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L6761
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9639
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6762
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6762:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9639:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6761
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6761:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L6775
	bb1.n	 gt,r13,@L6784
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6806
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L9333
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L6770
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L6773
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L6773:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L6814
	st.b	 r13,r31,104
	align	 4
@L6770:
	br.n	 @L9104
	or	 r13,r0,0
	align	 4
@L6775:
	bcnd.n	 ne0,r10,@L9334
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L6779
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L6782
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L6782:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L6814
	st.h	 r13,r31,104
	align	 4
@L6779:
	br.n	 @L9105
	or	 r13,r0,0
	align	 4
@L6784:
	bcnd.n	 ne0,r10,@L9335
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L6788
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L6791
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L6791:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L6814
	st	 r13,r31,104
	align	 4
@L6788:
	br.n	 @L9106
	or	 r13,r0,0
	align	 4
@L6806:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6740:
	ld	 r9,r31,92
	extu	 r13,r9,0<29>
	extu	 r8,r9,0<24>
	ld	 r11,r18[r13]
	mask	 r10,r8,31
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_regs_F)
	mak	 r13,r13,r10
	or	 r20,r12,lo16(_regs_F)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6809
	extu	 r21,r9,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r8]
	br	 @L6810
	align	 4
@L6809:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r8]
@L6810:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r8,r11,255
	or	 r13,r0,1
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6811
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r8]
	br.n	 @L9640
	addu	 r13,r9,r13
	align	 4
@L6811:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r8]
	addu	 r13,r9,r13
@L9640:
	addu	 r24,r13,4
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6813
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6814
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L6819
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9641
	extu	 r13,r24,0<24>
@L6819:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6814
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6814
	extu	 r13,r24,0<24>
@L9641:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6822
	or	 r11,r0,0
@L6824:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6823
	bcnd	 eq0,r11,@L6822
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6822
	st	 r10,r12[r25]
	align	 4
@L6823:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6824
@L6822:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L6828
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9642
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6829
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6829:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9642:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6828
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6828:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L6842
	bb1.n	 gt,r13,@L6851
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6873
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L6835
	or.u	 r13,r0,hi16(_mem_table)
@L9333:
	ld.bu	 r13,r10,8
@L9104:
	br.n	 @L6814
	st.b	 r13,r31,104
	align	 4
@L6835:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L6837
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L6840
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L6840:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L6814
	st.b	 r13,r31,104
	align	 4
@L6837:
	br.n	 @L9104
	or	 r13,r0,0
	align	 4
@L6842:
	bcnd.n	 eq0,r10,@L6844
	or.u	 r13,r0,hi16(_mem_table)
@L9334:
	ld.hu	 r13,r10,8
@L9105:
	br.n	 @L6814
	st.h	 r13,r31,104
	align	 4
@L6844:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L6846
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L6849
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L6849:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L6814
	st.h	 r13,r31,104
	align	 4
@L6846:
	br.n	 @L9105
	or	 r13,r0,0
	align	 4
@L6851:
	bcnd.n	 eq0,r10,@L6853
	or.u	 r13,r0,hi16(_mem_table)
@L9335:
	ld	 r13,r10,8
@L9106:
	br.n	 @L6814
	st	 r13,r31,104
	align	 4
@L6853:
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L6855
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L6858
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L6858:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L6814
	st	 r13,r31,104
	align	 4
@L6855:
	br.n	 @L9106
	or	 r13,r0,0
	align	 4
@L6873:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6813:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6814:
	ld	 r13,r31,104
	st	 r13,r20[r21]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6876
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6878
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9370
	addu	 r13,r13,r12
	align	 4
@L6878:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L9370:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r13,r14[r9]
	align	 4
@L6876:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L2101
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7837
	addu	 r13,r13,r11
	align	 4
@L6882:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L6883
	ld.bu	 r14,r31,92
	br.n	 @L6884
	st	 r14,r31,164
	align	 4
@L6883:
	st	 r0,r31,164
@L6884:
	ld	 r11,r31,92
	or.u	 r15,r0,hi16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,172
	extu	 r13,r11,8<16>
	ld	 r12,r15,lo16(_spec_mode)
	extu	 r14,r11,8<8>
	addu	 r13,r13,32
	st	 r14,r31,188
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r12,@L6885
	st	 r13,r31,180
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6887
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9643
	addu	 r13,r13,r12
	align	 4
@L6887:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9643:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L6886
	st	 r13,r15[r9]
	align	 4
@L6885:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6889
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9644
	addu	 r13,r13,r12
	align	 4
@L6889:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9644:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L6886:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6891
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r10]
	br	 @L6892
	align	 4
@L6891:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r10]
@L6892:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6893
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9645
	addu	 r13,r9,r13
	align	 4
@L6893:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	addu	 r13,r9,r13
@L9645:
	addu	 r24,r13,4
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6895
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9111
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L6895:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9111:
	ld	 r12,r13[r12]
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6897
	st	 r12,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6898
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6898
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6898
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6906
	or	 r11,r0,0
@L6908:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6907
	bcnd	 eq0,r11,@L6906
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6906
	st	 r10,r12[r25]
	align	 4
@L6907:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6908
@L6906:
	bcnd.n	 ne0,r10,@L9646
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9647
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6913
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6913:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9647:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6912
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6912:
	cmp	 r13,r22,2
@L9646:
	bb0	 ne,r13,@L6927
	bb1.n	 gt,r13,@L6936
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L6957
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L6898
	st.b	 r13,r10,8
	align	 4
@L6927:
	ld.hu	 r13,r31,104
	br.n	 @L6898
	st.h	 r13,r10,8
	align	 4
@L6936:
	ld	 r13,r31,104
	br.n	 @L6898
	st	 r13,r10,8
	align	 4
@L6957:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6897:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6898:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6960
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L6960:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L6966:
	ld	 r12,r31,92
	ld.bu	 r9,r31,90
	st	 r0,r31,172
	extu	 r15,r12,8<16>
	bcnd.n	 eq0,r9,@L6967
	st	 r15,r31,164
	extu	 r14,r12,0<24>
	st	 r14,r31,172
@L6967:
	or.u	 r15,r0,hi16(_spec_mode)
	or	 r11,r0,r12
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r11,0<24>
	st	 r0,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L6969
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L6971
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9648
	addu	 r13,r13,r12
	align	 4
@L6971:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9648:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L6970
	st	 r13,r15[r9]
	align	 4
@L6969:
	extu	 r13,r12,0<29>
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L6973
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9649
	addu	 r13,r13,r12
	align	 4
@L6973:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9649:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L6970:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6975
	or	 r10,r0,1
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L6977
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r9]
	br.n	 @L9650
	extu	 r11,r11,0<8>
	align	 4
@L6977:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9650:
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L6979
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9651
	addu	 r13,r9,r13
	align	 4
@L6979:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	addu	 r13,r9,r13
@L9651:
	addu	 r24,r13,4
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L6981
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L6982
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L6987
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9652
	extu	 r13,r24,0<24>
@L6987:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L6982
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L6982
	extu	 r13,r24,0<24>
@L9652:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L6990
	or	 r11,r0,0
@L6992:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L6991
	bcnd	 eq0,r11,@L6990
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L6990
	st	 r10,r12[r25]
	align	 4
@L6991:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L6992
@L6990:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L6996
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9653
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L6997
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L6997:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9653:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L6996
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L6996:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L7010
	bb1.n	 gt,r13,@L7019
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L7041
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L7003
	ld.bu	 r13,r10,8
@L9117:
	br.n	 @L6982
	st.b	 r13,r31,104
	align	 4
@L7003:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L7005
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L7008
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L7008:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L6982
	st.b	 r13,r31,104
	align	 4
@L7005:
	br.n	 @L9117
	or	 r13,r0,0
	align	 4
@L7010:
	bcnd	 eq0,r10,@L7012
	ld.hu	 r13,r10,8
@L9118:
	br.n	 @L6982
	st.h	 r13,r31,104
	align	 4
@L7012:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L7014
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L7017
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L7017:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L6982
	st.h	 r13,r31,104
	align	 4
@L7014:
	br.n	 @L9118
	or	 r13,r0,0
	align	 4
@L7019:
	bcnd	 eq0,r10,@L7021
	ld	 r13,r10,8
@L9119:
	br.n	 @L6982
	st	 r13,r31,104
	align	 4
@L7021:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L7023
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L7026
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L7026:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L6982
	st	 r13,r31,104
	align	 4
@L7023:
	br.n	 @L9119
	or	 r13,r0,0
	align	 4
@L7041:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L6981:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L6982:
	or.u	 r15,r0,hi16(_spec_regs_R)
	ld	 r13,r31,104
	br.n	 @L9120
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L6975:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7044
	extu	 r21,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r10,r14[r9]
	br.n	 @L9654
	extu	 r11,r11,0<8>
	align	 4
@L7044:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r10,r15[r9]
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
@L9654:
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7046
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9655
	addu	 r13,r10,r13
	align	 4
@L7046:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r10,r13
@L9655:
	addu	 r24,r13,4
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7048
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L7049
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L7054
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L9656
	extu	 r13,r24,0<24>
@L7054:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L7049
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L7049
	extu	 r13,r24,0<24>
@L9656:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L7057
	or	 r11,r0,0
@L7059:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L7058
	bcnd	 eq0,r11,@L7057
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L7057
	st	 r10,r12[r25]
	align	 4
@L7058:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L7059
@L7057:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L7063
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9657
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L7064
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L7064:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9657:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L7063
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L7063:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L7077
	bb1.n	 gt,r13,@L7086
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L7108
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L7070
	ld.bu	 r13,r10,8
@L9122:
	br.n	 @L7049
	st.b	 r13,r31,104
	align	 4
@L7070:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L7072
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L7075
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L7075:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L7049
	st.b	 r13,r31,104
	align	 4
@L7072:
	br.n	 @L9122
	or	 r13,r0,0
	align	 4
@L7077:
	bcnd	 eq0,r10,@L7079
	ld.hu	 r13,r10,8
@L9123:
	br.n	 @L7049
	st.h	 r13,r31,104
	align	 4
@L7079:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L7081
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L7084
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L7084:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L7049
	st.h	 r13,r31,104
	align	 4
@L7081:
	br.n	 @L9123
	or	 r13,r0,0
	align	 4
@L7086:
	bcnd	 eq0,r10,@L7088
	ld	 r13,r10,8
@L9124:
	br.n	 @L7049
	st	 r13,r31,104
	align	 4
@L7088:
	or.u	 r13,r0,hi16(_mem_table)
	extu	 r25,r24,15<16>
	or	 r23,r13,lo16(_mem_table)
	ld	 r13,r23[r25]
	bcnd.n	 eq0,r13,@L7090
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r23[r25]
	bcnd	 ne0,r13,@L7093
	bsr	 _mem_newblock
	st	 r2,r23[r25]
@L7093:
	ld	 r12,r23[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L7049
	st	 r13,r31,104
	align	 4
@L7090:
	br.n	 @L9124
	or	 r13,r0,0
	align	 4
@L7108:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L7048:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L7049:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r13,r31,104
	or	 r15,r15,lo16(_regs_R)
@L9120:
	st	 r13,r15[r21]
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7111
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L7111:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L7117:
	ld.bu	 r13,r31,90
	bcnd	 eq0,r13,@L7118
	ld.bu	 r15,r31,92
	br.n	 @L7119
	st	 r15,r31,164
	align	 4
@L7118:
	st	 r0,r31,164
@L7119:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r11,8<16>
	st	 r0,r31,172
	extu	 r17,r11,0<24>
	st	 r14,r31,180
	extu	 r14,r11,8<8>
	bcnd.n	 eq0,r13,@L7120
	st	 r14,r31,188
	mask	 r12,r17,31
	extu	 r11,r11,0<29>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7122
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9658
	addu	 r13,r13,r12
	align	 4
@L7122:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9658:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L7121
	st	 r13,r15[r9]
	align	 4
@L7120:
	extu	 r13,r11,0<29>
	ld.bu	 r9,r31,90
	or.u	 r12,r0,hi16(_ss_fore_tab+60)
	mask	 r10,r17,31
	ld	 r11,r18[r13]
	or	 r13,r0,1
	or	 r12,r12,lo16(_ss_fore_tab+60)
	mak	 r13,r13,r10
	ld	 r12,r12[r9]
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L7124
	or	 r10,r0,r17
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r17]
	br.n	 @L9659
	addu	 r13,r13,r12
	align	 4
@L7124:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r17]
	addu	 r13,r13,r12
@L9659:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r10]
@L7121:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7126
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r10]
	br	 @L7127
	align	 4
@L7126:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r10]
@L7127:
	ld	 r11,r31,92
	extu	 r11,r11,0<8>
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7128
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9660
	addu	 r13,r9,r13
	align	 4
@L7128:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	addu	 r13,r9,r13
@L9660:
	addu	 r24,r13,4
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7130
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r10]
	br.n	 @L9661
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L7130:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r10]
	or.u	 r15,r0,hi16(_spec_mode)
@L9661:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7132
	st	 r11,r31,104
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L7133
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L7133
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L7133
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L7141
	or	 r11,r0,0
@L7143:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L7142
	bcnd	 eq0,r11,@L7141
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L7141
	st	 r10,r12[r25]
	align	 4
@L7142:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L7143
@L7141:
	bcnd.n	 ne0,r10,@L9662
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L9663
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L7148
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L7148:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L9663:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L7147
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L7147:
	cmp	 r13,r22,2
@L9662:
	bb0	 ne,r13,@L7162
	bb1.n	 gt,r13,@L7171
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L7192
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,104
	br.n	 @L7133
	st.b	 r13,r10,8
	align	 4
@L7162:
	ld.hu	 r13,r31,104
	br.n	 @L7133
	st.h	 r13,r10,8
	align	 4
@L7171:
	ld	 r13,r31,104
	br.n	 @L7133
	st	 r13,r10,8
	align	 4
@L7192:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L7132:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,104
	bsr.n	 _mem_access
	or	 r5,r0,4
@L7133:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7195
	or	 r10,r0,1
	ld	 r12,r31,92
	extu	 r11,r12,0<29>
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,90
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd.n	 eq0,r13,@L7197
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9358
	addu	 r13,r13,r12
	align	 4
@L7197:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r13,r13,r12
@L9358:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r13,r15[r9]
	align	 4
@L7195:
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	or	 r11,r0,1
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,90
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd.n	 eq0,r12,@L7199
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9359
	addu	 r13,r13,r11
	align	 4
@L7199:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L9359:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	br.n	 @L1739
	st	 r13,r14[r9]
	align	 4
@L7201:
	ld	 r11,r31,92
	extu	 r15,r11,0<24>
	extu	 r12,r11,0<29>
	or	 r13,r0,1
	extu	 r14,r11,8<8>
	st	 r0,r31,172
	extu	 r17,r11,8<16>
	st	 r15,r31,180
	mask	 r10,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	st	 r0,r31,188
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7204
	st	 r14,r31,164
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r15]
	bcnd	 le0,r13,@L7203
	br	 @L7205
	align	 4
@L7204:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r15]
	bcnd	 le0,r13,@L7203
@L7205:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7206
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	bcnd	 le0,r13,@L7203
	br	 @L7207
	align	 4
@L7206:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	bcnd	 le0,r13,@L7203
@L7207:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7208
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r12,r15[r10]
	br.n	 @L9664
	set	 r13,r0,31<0>
	align	 4
@L7208:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r12,r14[r10]
	set	 r13,r0,31<0>
@L9664:
	subu	 r9,r13,r12
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7210
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb0.n	 ge,r13,@L9665
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L7203
	align	 4
@L7210:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
	bb1.n	 ge,r13,@L7203
	or.u	 r15,r0,hi16(_spec_mode)
@L9665:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7203
	or.u	 r2,r0,hi16(@LC564)
	or	 r4,r0,553
	or.u	 r3,r0,hi16(@LC565)
	or.u	 r5,r0,hi16(@LC566)
	or	 r2,r2,lo16(@LC564)
	or	 r3,r3,lo16(@LC565)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC566)
	align	 4
@L7203:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7216
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd	 lt0,r13,@L7217
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L9666
	align	 4
@L7216:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd	 ge0,r13,@L7215
@L7217:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7218
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd	 lt0,r13,@L7219
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L9666
	align	 4
@L7218:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd	 ge0,r13,@L7215
@L7219:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7220
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	or.u	 r12,r0,0x8000
	or	 r12,r12,0x1
	ld	 r13,r14[r10]
	br.n	 @L9667
	subu	 r9,r12,r13
	align	 4
@L7220:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	or.u	 r12,r0,0x8000
	or	 r12,r12,0x1
	ld	 r13,r15[r10]
	subu	 r9,r12,r13
@L9667:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7222
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
	bb1.n	 gt,r13,@L9668
	or.u	 r14,r0,hi16(_spec_mode)
	br.n	 @L9666
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L7222:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb0.n	 gt,r13,@L7215
	or.u	 r14,r0,hi16(_spec_mode)
@L9668:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8667
	or.u	 r2,r0,hi16(@LC567)
	or	 r4,r0,553
	or.u	 r3,r0,hi16(@LC568)
	or.u	 r5,r0,hi16(@LC569)
	or	 r2,r2,lo16(@LC567)
	or	 r3,r3,lo16(@LC568)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC569)
	align	 4
@L7215:
	or.u	 r15,r0,hi16(_spec_mode)
@L9666:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7226
	or	 r13,r0,1
@L8667:
	ld	 r13,r31,92
	extu	 r13,r13,0<8>
	mask	 r11,r13,31
	extu	 r13,r13,3<5>
	or	 r10,r0,1
	ld	 r12,r18[r13]
	mak	 r11,r10,r11
	or	 r12,r12,r11
	st	 r12,r18[r13]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7228
	extu	 r8,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r9]
	br	 @L7229
	align	 4
@L7228:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r9]
@L7229:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7230
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7307
	addu	 r11,r9,r13
	align	 4
@L7230:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7307
	addu	 r11,r9,r13
	align	 4
@L7226:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7232
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r10,r15[r9]
	br	 @L7233
	align	 4
@L7232:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r10,r14[r9]
@L7233:
	ld.hu	 r11,r31,92
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7234
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7770
	addu	 r10,r10,r13
	align	 4
@L7234:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7770
	addu	 r10,r10,r13
	align	 4
@L7236:
	ld	 r11,r31,92
	extu	 r14,r11,0<24>
	or	 r13,r0,1
	extu	 r12,r11,0<29>
	st	 r0,r31,172
	extu	 r11,r11,8<16>
	st	 r14,r31,180
	mask	 r10,r14,31
	ld	 r12,r18[r12]
	or	 r17,r0,0
	mak	 r13,r13,r10
	st	 r0,r31,188
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7239
	st	 r11,r31,164
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r14]
	bcnd	 le0,r13,@L7238
	br	 @L7240
	align	 4
@L7239:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r14]
	bcnd	 le0,r13,@L7238
@L7240:
	ld	 r12,r31,92
	ext	 r9,r12,16<0>
	bcnd.n	 le0,r9,@L9776
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7241
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r12,r14[r10]
	set	 r13,r0,31<0>
	subu	 r13,r13,r12
	cmp	 r13,r13,r9
	bb0.n	 ge,r13,@L9669
	or.u	 r14,r0,hi16(_spec_mode)
	br	 @L7238
	align	 4
@L7241:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r12,r15[r10]
	set	 r13,r0,31<0>
	subu	 r13,r13,r12
	cmp	 r13,r13,r9
	bb1.n	 ge,r13,@L7238
	or.u	 r14,r0,hi16(_spec_mode)
@L9669:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7238
	or.u	 r2,r0,hi16(@LC570)
	or	 r4,r0,558
	or.u	 r3,r0,hi16(@LC571)
	or.u	 r5,r0,hi16(@LC572)
	or	 r2,r2,lo16(@LC570)
	or	 r3,r3,lo16(@LC571)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC572)
	align	 4
@L7238:
	ld	 r12,r31,92
@L9776:
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7247
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	bcnd	 lt0,r13,@L7248
	or.u	 r14,r0,hi16(_spec_mode)
	br	 @L9670
	align	 4
@L7247:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	bcnd	 ge0,r13,@L7246
@L7248:
	ld	 r12,r31,92
	ext	 r9,r12,16<0>
	bcnd.n	 ge0,r9,@L7246
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7249
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	or.u	 r13,r0,0x8000
	or	 r13,r13,0x1
	ld	 r12,r15[r10]
	subu	 r13,r13,r12
	cmp	 r13,r13,r9
	bb1.n	 gt,r13,@L9671
	or.u	 r15,r0,hi16(_spec_mode)
	br.n	 @L9670
	or.u	 r14,r0,hi16(_spec_mode)
	align	 4
@L7249:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	or.u	 r13,r0,0x8000
	or	 r13,r13,0x1
	ld	 r12,r14[r10]
	subu	 r13,r13,r12
	cmp	 r13,r13,r9
	bb0.n	 gt,r13,@L7246
	or.u	 r15,r0,hi16(_spec_mode)
@L9671:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8668
	or.u	 r2,r0,hi16(@LC573)
	or	 r4,r0,558
	or.u	 r3,r0,hi16(@LC574)
	or.u	 r5,r0,hi16(@LC575)
	or	 r2,r2,lo16(@LC573)
	or	 r3,r3,lo16(@LC574)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC575)
	align	 4
@L7246:
	or.u	 r14,r0,hi16(_spec_mode)
@L9670:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7253
	or	 r13,r0,1
@L8668:
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r12,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7273
	ext	 r11,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9672
	addu	 r11,r13,r11
	align	 4
@L7253:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r10,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7257
	ext	 r11,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9673
	addu	 r11,r13,r11
	align	 4
@L7257:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	addu	 r11,r13,r11
@L9673:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	br.n	 @L1739
	st	 r11,r14[r10]
	align	 4
@L7259:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r15,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r15,r31,180
	mask	 r15,r11,255
	bcnd.n	 eq0,r13,@L7260
	st	 r15,r31,164
	mask	 r12,r11,31
	extu	 r11,r15,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7262
	extu	 r8,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r9]
	br	 @L7263
	align	 4
@L7262:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r9]
@L7263:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7230
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7307
	addu	 r11,r9,r13
	align	 4
@L7260:
	ld	 r15,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7266
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9138
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7266:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9138:
	ld	 r9,r14[r15]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7268
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7269
	addu	 r13,r9,r13
	align	 4
@L7268:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	addu	 r13,r9,r13
@L7269:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	br.n	 @L1739
	st	 r13,r15[r8]
	align	 4
@L7270:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r10,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r12,r10,0<16>
	st	 r0,r31,188
	extu	 r15,r10,0<24>
	st	 r15,r31,180
	mask	 r14,r12,255
	bcnd.n	 eq0,r13,@L7271
	st	 r14,r31,164
	mask	 r12,r12,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r12,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7273
	ext	 r11,r11,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9672
	addu	 r11,r13,r11
	align	 4
@L7273:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	addu	 r11,r13,r11
@L9672:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r11,r15[r12]
	align	 4
@L7271:
	ld	 r14,r31,180
	or	 r13,r0,1
	extu	 r12,r10,0<29>
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7275
	ext	 r11,r10,16<0>
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9140
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7275:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9140:
	ld	 r13,r15[r14]
	addu	 r12,r13,r11
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	br.n	 @L1739
	st	 r12,r14[r9]
	align	 4
@L7277:
	ld	 r11,r31,92
	extu	 r15,r11,0<24>
	extu	 r12,r11,0<29>
	or	 r13,r0,1
	extu	 r14,r11,8<8>
	st	 r0,r31,172
	extu	 r17,r11,8<16>
	st	 r15,r31,180
	mask	 r10,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	st	 r0,r31,188
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7280
	st	 r14,r31,164
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r15]
	bcnd	 le0,r13,@L7279
	br	 @L7281
	align	 4
@L7280:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r15]
	bcnd	 le0,r13,@L7279
@L7281:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7282
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	bcnd	 lt0,r13,@L7283
	br	 @L7279
	align	 4
@L7282:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	bcnd	 ge0,r13,@L7279
@L7283:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7284
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r12,r15[r10]
	br.n	 @L9674
	set	 r13,r0,31<0>
	align	 4
@L7284:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r12,r14[r10]
	set	 r13,r0,31<0>
@L9674:
	addu	 r9,r12,r13
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7286
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb0.n	 ge,r13,@L9675
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L7279
	align	 4
@L7286:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
	bb1.n	 ge,r13,@L7279
	or.u	 r15,r0,hi16(_spec_mode)
@L9675:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7279
	or.u	 r2,r0,hi16(@LC576)
	or	 r4,r0,574
	or.u	 r3,r0,hi16(@LC577)
	or.u	 r5,r0,hi16(@LC578)
	or	 r2,r2,lo16(@LC576)
	or	 r3,r3,lo16(@LC577)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC578)
	align	 4
@L7279:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7292
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd	 lt0,r13,@L7293
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L9676
	align	 4
@L7292:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd	 ge0,r13,@L7291
@L7293:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7294
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd	 gt0,r13,@L7295
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L9676
	align	 4
@L7294:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd	 le0,r13,@L7291
@L7295:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7296
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	or.u	 r12,r0,0x8000
	or	 r12,r12,0x1
	ld	 r13,r14[r10]
	br.n	 @L9677
	addu	 r9,r13,r12
	align	 4
@L7296:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	or.u	 r12,r0,0x8000
	or	 r12,r12,0x1
	ld	 r13,r15[r10]
	addu	 r9,r13,r12
@L9677:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7298
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
	bb1.n	 gt,r13,@L9678
	or.u	 r14,r0,hi16(_spec_mode)
	br.n	 @L9676
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L7298:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb0.n	 gt,r13,@L7291
	or.u	 r14,r0,hi16(_spec_mode)
@L9678:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8669
	or.u	 r2,r0,hi16(@LC579)
	or	 r4,r0,574
	or.u	 r3,r0,hi16(@LC580)
	or.u	 r5,r0,hi16(@LC581)
	or	 r2,r2,lo16(@LC579)
	or	 r3,r3,lo16(@LC580)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC581)
	align	 4
@L7291:
	or.u	 r15,r0,hi16(_spec_mode)
@L9676:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7302
	or	 r13,r0,1
@L8669:
	ld	 r13,r31,92
	extu	 r13,r13,0<8>
	mask	 r11,r13,31
	extu	 r13,r13,3<5>
	or	 r10,r0,1
	ld	 r12,r18[r13]
	mak	 r11,r10,r11
	or	 r12,r12,r11
	st	 r12,r18[r13]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7304
	extu	 r8,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r9]
	br	 @L7305
	align	 4
@L7304:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r9]
@L7305:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7306
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7307
	subu	 r11,r9,r13
	align	 4
@L7306:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	subu	 r11,r9,r13
@L7307:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r11,r14[r8]
	align	 4
@L7302:
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7308
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r10,r15[r9]
	br	 @L7309
	align	 4
@L7308:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r10,r14[r9]
@L7309:
	ld.hu	 r11,r31,92
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7310
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7770
	subu	 r10,r10,r13
	align	 4
@L7310:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7770
	subu	 r10,r10,r13
	align	 4
@L7312:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r14,r31,180
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7313
	st	 r14,r31,164
	mask	 r12,r11,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7315
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L7316
	align	 4
@L7315:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L7316:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7317
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7683
	subu	 r11,r9,r13
	align	 4
@L7317:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7683
	subu	 r11,r9,r13
	align	 4
@L7313:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7319
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9146
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7319:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9146:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7321
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7830
	subu	 r13,r9,r13
	align	 4
@L7321:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7830
	subu	 r13,r9,r13
	align	 4
@L7323:
	ld	 r13,r31,92
	or	 r15,r0,64
	st	 r15,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,88
	ld	 r11,r15,lo16(_spec_mode)
	or	 r14,r0,65
	st	 r14,r31,172
	st	 r0,r31,188
	st	 r0,r31,228
	st	 r0,r31,236
	st	 r12,r31,112
	extu	 r14,r13,8<16>
	st	 r13,r31,116
	extu	 r17,r13,0<24>
	bcnd.n	 eq0,r11,@L7325
	st	 r14,r31,180
	or.u	 r13,r0,hi16(_use_spec_HI)
	or	 r12,r0,1
	st	 r12,r13,lo16(_use_spec_HI)
	or.u	 r13,r0,hi16(_spec_regs_HI)
	br.n	 @L9777
	st	 r0,r13,lo16(_spec_regs_HI)
	align	 4
@L7325:
	or.u	 r13,r0,hi16(_regs_HI)
	st	 r0,r13,lo16(_regs_HI)
	or.u	 r15,r0,hi16(_spec_mode)
@L9777:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7327
	or.u	 r13,r0,hi16(_use_spec_LO)
	or	 r12,r0,1
	st	 r12,r13,lo16(_use_spec_LO)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	br.n	 @L7328
	st	 r0,r13,lo16(_spec_regs_LO)
	align	 4
@L7327:
	or.u	 r13,r0,hi16(_regs_LO)
	st	 r0,r13,lo16(_regs_LO)
@L7328:
	ld	 r12,r31,116
	extu	 r2,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r2,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7329
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r16,r14[r2]
	br	 @L7330
	align	 4
@L7329:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r16,r15[r2]
@L7330:
	ld.hu	 r11,r31,116
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7331
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r25,r14[r10]
	br	 @L7332
	align	 4
@L7331:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r25,r15[r10]
@L7332:
	bcnd.n	 ge0,r16,@L7333
	xor.c	 r13,r16,r0
	or	 r14,r0,1
	st	 r14,r31,236
	addu	 r16,r13,1
@L7333:
	bcnd.n	 ge0,r25,@L7334
	xor.c	 r13,r25,r0
	or	 r15,r0,1
	st	 r15,r31,228
	addu	 r25,r13,1
@L7334:
	bcnd.n	 ge0,r16,@L9679
	or	 r19,r0,0
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7336
	or.u	 r13,r0,hi16(_use_spec_LO)
	or	 r12,r0,1
	st	 r12,r13,lo16(_use_spec_LO)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	br.n	 @L9679
	st	 r25,r13,lo16(_spec_regs_LO)
	align	 4
@L7336:
	or.u	 r13,r0,hi16(_regs_LO)
	st	 r25,r13,lo16(_regs_LO)
	or	 r19,r0,0
@L9679:
	or.u	 r20,r0,hi16(_use_spec_HI)
	or.u	 r21,r0,hi16(_use_spec_LO)
	or.u	 r22,r0,hi16(_regs_LO)
	or.u	 r23,r0,hi16(_spec_regs_HI)
	or.u	 r15,r0,hi16(_spec_mode)
@L9689:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7342
	or	 r14,r0,1
	ld	 r13,r23,lo16(_spec_regs_HI)
	st	 r14,r20,lo16(_use_spec_HI)
	mak	 r13,r13,0<1>
	br.n	 @L7343
	st	 r13,r23,lo16(_spec_regs_HI)
	align	 4
@L7342:
	ld	 r13,r20,lo16(_use_spec_HI)
	bcnd.n	 eq0,r13,@L7346
	or.u	 r15,r0,hi16(_regs_HI)
	ld	 r13,r23,lo16(_spec_regs_HI)
	br.n	 @L9680
	mak	 r13,r13,0<1>
	align	 4
@L7346:
	ld	 r13,r15,lo16(_regs_HI)
	mak	 r13,r13,0<1>
@L9680:
	or.u	 r14,r0,hi16(_regs_HI)
	or	 r14,r14,lo16(_regs_HI)
	st	 r13,r0,r14
@L7343:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7348
	or	 r14,r0,1
	ld	 r13,r21,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L7350
	st	 r14,r20,lo16(_use_spec_HI)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r2,r13,lo16(_spec_regs_LO)
	br.n	 @L9681
	or	 r3,r0,31
	align	 4
@L7350:
	ld	 r2,r22,lo16(_regs_LO)
	or	 r3,r0,31
@L9681:
	bsr.n	 _extractl
	or	 r4,r0,1
	ld	 r13,r20,lo16(_use_spec_HI)
	bcnd.n	 ne0,r13,@L7352
	or.u	 r15,r0,hi16(_regs_HI)
	ld	 r13,r15,lo16(_regs_HI)
	br.n	 @L9682
	addu	 r2,r13,r2
	align	 4
@L7352:
	ld	 r13,r23,lo16(_spec_regs_HI)
	addu	 r2,r13,r2
@L9682:
	or.u	 r14,r0,hi16(_spec_regs_HI)
	br.n	 @L9150
	or	 r14,r14,lo16(_spec_regs_HI)
	align	 4
@L7348:
	ld	 r13,r21,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L7354
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r2,r13,lo16(_spec_regs_LO)
	br.n	 @L9683
	or	 r3,r0,31
	align	 4
@L7354:
	ld	 r2,r22,lo16(_regs_LO)
	or	 r3,r0,31
@L9683:
	bsr.n	 _extractl
	or	 r4,r0,1
	ld	 r13,r20,lo16(_use_spec_HI)
	bcnd.n	 ne0,r13,@L7356
	or.u	 r15,r0,hi16(_regs_HI)
	ld	 r13,r15,lo16(_regs_HI)
	br.n	 @L9684
	addu	 r2,r13,r2
	align	 4
@L7356:
	ld	 r13,r23,lo16(_spec_regs_HI)
	addu	 r2,r13,r2
@L9684:
	or.u	 r14,r0,hi16(_regs_HI)
	or	 r14,r14,lo16(_regs_HI)
@L9150:
	st	 r2,r0,r14
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7358
	or.u	 r12,r0,hi16(_spec_regs_LO)
	ld	 r13,r12,lo16(_spec_regs_LO)
	or	 r14,r0,1
	st	 r14,r21,lo16(_use_spec_LO)
	mak	 r13,r13,0<1>
	br.n	 @L7359
	st	 r13,r12,lo16(_spec_regs_LO)
	align	 4
@L7358:
	ld	 r13,r21,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L7362
	or	 r12,r22,lo16(_regs_LO)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	br.n	 @L9685
	mak	 r13,r13,0<1>
	align	 4
@L7362:
	ld	 r13,r22,lo16(_regs_LO)
	mak	 r13,r13,0<1>
@L9685:
	st	 r13,r0,r12
@L7359:
	or	 r2,r0,r16
	or	 r3,r0,30
	or	 r4,r0,1
	bsr.n	 _extractl
	subu	 r3,r3,r19
	cmp	 r2,r2,1
	bb1	 ne,r2,@L7340
	ld	 r13,r21,lo16(_use_spec_LO)
	bcnd.n	 ne0,r13,@L7366
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r22,lo16(_regs_LO)
	xor.c	 r13,r13,r0
	cmp	 r13,r13,r25
	bb1.n	 lo,r13,@L9686
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L7365
	align	 4
@L7366:
	ld	 r13,r13,lo16(_spec_regs_LO)
	xor.c	 r13,r13,r0
	cmp	 r13,r13,r25
	bb0.n	 lo,r13,@L7365
	or.u	 r15,r0,hi16(_spec_mode)
@L9686:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7368
	or	 r14,r0,1
	ld	 r13,r23,lo16(_spec_regs_HI)
	st	 r14,r20,lo16(_use_spec_HI)
	addu	 r13,r13,1
	br.n	 @L7365
	st	 r13,r23,lo16(_spec_regs_HI)
	align	 4
@L7368:
	ld	 r13,r20,lo16(_use_spec_HI)
	bcnd.n	 ne0,r13,@L7372
	or.u	 r15,r0,hi16(_regs_HI)
	ld	 r13,r15,lo16(_regs_HI)
	br.n	 @L9687
	addu	 r13,r13,1
	align	 4
@L7372:
	ld	 r13,r23,lo16(_spec_regs_HI)
	addu	 r13,r13,1
@L9687:
	or.u	 r14,r0,hi16(_regs_HI)
	or	 r14,r14,lo16(_regs_HI)
	st	 r13,r0,r14
@L7365:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7374
	or.u	 r12,r0,hi16(_spec_regs_LO)
	ld	 r13,r12,lo16(_spec_regs_LO)
	or	 r14,r0,1
	st	 r14,r21,lo16(_use_spec_LO)
	addu	 r13,r13,r25
	br.n	 @L7340
	st	 r13,r12,lo16(_spec_regs_LO)
	align	 4
@L7374:
	ld	 r13,r21,lo16(_use_spec_LO)
	bcnd.n	 ne0,r13,@L7378
	or	 r12,r22,lo16(_regs_LO)
	ld	 r13,r22,lo16(_regs_LO)
	br.n	 @L9688
	addu	 r13,r13,r25
	align	 4
@L7378:
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	addu	 r13,r13,r25
@L9688:
	st	 r13,r0,r12
@L7340:
	addu	 r19,r19,1
	cmp	 r13,r19,30
	bb1.n	 le,r13,@L9689
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r15,r31,236
	ld	 r14,r31,228
	cmp	 r13,r15,r14
	bb0.n	 ne,r13,@L1739
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7382
	or.u	 r10,r0,hi16(_spec_regs_LO)
	ld	 r12,r10,lo16(_spec_regs_LO)
	or.u	 r11,r0,hi16(_use_spec_LO)
	or	 r13,r0,1
	st	 r13,r11,lo16(_use_spec_LO)
	xor.c	 r12,r12,r0
	br.n	 @L7383
	st	 r12,r10,lo16(_spec_regs_LO)
	align	 4
@L7382:
	or.u	 r13,r0,hi16(_use_spec_LO)
	or.u	 r12,r0,hi16(_regs_LO)
	ld	 r13,r13,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L7386
	or	 r11,r12,lo16(_regs_LO)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	br.n	 @L9690
	xor.c	 r13,r13,r0
	align	 4
@L7386:
	ld	 r13,r12,lo16(_regs_LO)
	xor.c	 r13,r13,r0
@L9690:
	st	 r13,r0,r11
@L7383:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7388
	or.u	 r10,r0,hi16(_spec_regs_HI)
	ld	 r12,r10,lo16(_spec_regs_HI)
	or.u	 r11,r0,hi16(_use_spec_HI)
	or	 r13,r0,1
	st	 r13,r11,lo16(_use_spec_HI)
	xor.c	 r12,r12,r0
	br.n	 @L7389
	st	 r12,r10,lo16(_spec_regs_HI)
	align	 4
@L7388:
	or.u	 r13,r0,hi16(_use_spec_HI)
	or.u	 r12,r0,hi16(_regs_HI)
	ld	 r13,r13,lo16(_use_spec_HI)
	bcnd.n	 eq0,r13,@L7392
	or	 r11,r12,lo16(_regs_HI)
	or.u	 r13,r0,hi16(_spec_regs_HI)
	ld	 r13,r13,lo16(_spec_regs_HI)
	br.n	 @L9691
	xor.c	 r13,r13,r0
	align	 4
@L7392:
	ld	 r13,r12,lo16(_regs_HI)
	xor.c	 r13,r13,r0
@L9691:
	st	 r13,r0,r11
@L7389:
	or.u	 r13,r0,hi16(_use_spec_LO)
	ld	 r13,r13,lo16(_use_spec_LO)
	bcnd.n	 ne0,r13,@L7395
	or.u	 r13,r0,hi16(_spec_regs_LO)
	or.u	 r13,r0,hi16(_regs_LO)
	ld	 r13,r13,lo16(_regs_LO)
	addu	 r13,r13,1
	bcnd.n	 eq0,r13,@L9692
	or.u	 r15,r0,hi16(_spec_mode)
	br.n	 @L9693
	or.u	 r14,r0,hi16(_spec_mode)
	align	 4
@L7395:
	ld	 r13,r13,lo16(_spec_regs_LO)
	addu	 r13,r13,1
	bcnd.n	 ne0,r13,@L9693
	or.u	 r14,r0,hi16(_spec_mode)
	or.u	 r15,r0,hi16(_spec_mode)
@L9692:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7397
	or.u	 r10,r0,hi16(_spec_regs_HI)
	ld	 r12,r10,lo16(_spec_regs_HI)
	or.u	 r11,r0,hi16(_use_spec_HI)
	or	 r13,r0,1
	st	 r13,r11,lo16(_use_spec_HI)
	addu	 r12,r12,1
	br.n	 @L7394
	st	 r12,r10,lo16(_spec_regs_HI)
	align	 4
@L7397:
	or.u	 r13,r0,hi16(_use_spec_HI)
	or.u	 r12,r0,hi16(_regs_HI)
	ld	 r13,r13,lo16(_use_spec_HI)
	bcnd.n	 ne0,r13,@L7401
	or	 r11,r12,lo16(_regs_HI)
	ld	 r13,r12,lo16(_regs_HI)
	br.n	 @L9694
	addu	 r13,r13,1
	align	 4
@L7401:
	or.u	 r13,r0,hi16(_spec_regs_HI)
	ld	 r13,r13,lo16(_spec_regs_HI)
	addu	 r13,r13,1
@L9694:
	st	 r13,r0,r11
@L7394:
	or.u	 r14,r0,hi16(_spec_mode)
@L9693:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7403
	or.u	 r10,r0,hi16(_spec_regs_LO)
	ld	 r12,r10,lo16(_spec_regs_LO)
	or.u	 r11,r0,hi16(_use_spec_LO)
	or	 r13,r0,1
	st	 r13,r11,lo16(_use_spec_LO)
	addu	 r12,r12,1
	br.n	 @L1739
	st	 r12,r10,lo16(_spec_regs_LO)
	align	 4
@L7403:
	or.u	 r13,r0,hi16(_use_spec_LO)
	or.u	 r12,r0,hi16(_regs_LO)
	ld	 r13,r13,lo16(_use_spec_LO)
	bcnd.n	 ne0,r13,@L7407
	or	 r11,r12,lo16(_regs_LO)
	ld	 r13,r12,lo16(_regs_LO)
	br.n	 @L9695
	addu	 r13,r13,1
	align	 4
@L7407:
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	addu	 r13,r13,1
@L9695:
	br.n	 @L1739
	st	 r13,r0,r11
	align	 4
@L7409:
	ld	 r13,r31,92
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r12,r31,88
	ld	 r11,r14,lo16(_spec_mode)
	or	 r15,r0,64
	st	 r15,r31,164
	or	 r15,r0,65
	st	 r15,r31,172
	st	 r0,r31,188
	st	 r12,r31,112
	extu	 r14,r13,8<16>
	st	 r13,r31,116
	extu	 r17,r13,0<24>
	bcnd.n	 eq0,r11,@L7411
	st	 r14,r31,180
	or.u	 r13,r0,hi16(_use_spec_HI)
	or	 r12,r0,1
	st	 r12,r13,lo16(_use_spec_HI)
	or.u	 r13,r0,hi16(_spec_regs_HI)
	br.n	 @L7412
	st	 r0,r13,lo16(_spec_regs_HI)
	align	 4
@L7411:
	or.u	 r13,r0,hi16(_regs_HI)
	st	 r0,r13,lo16(_regs_HI)
@L7412:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7413
	or.u	 r13,r0,hi16(_use_spec_LO)
	or	 r12,r0,1
	st	 r12,r13,lo16(_use_spec_LO)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	br.n	 @L7414
	st	 r0,r13,lo16(_spec_regs_LO)
	align	 4
@L7413:
	or.u	 r13,r0,hi16(_regs_LO)
	st	 r0,r13,lo16(_regs_LO)
@L7414:
	ld	 r12,r31,116
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7416
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 lt0,r13,@L9696
	or.u	 r14,r0,hi16(_spec_mode)
	br.n	 @L9697
	or	 r20,r0,0
	align	 4
@L7416:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 ge0,r13,@L9697
	or	 r20,r0,0
	or.u	 r14,r0,hi16(_spec_mode)
@L9696:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7418
	or	 r10,r0,1
	ld.hu	 r13,r31,116
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r10,r13
	or.u	 r11,r0,hi16(_use_spec_LO)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7420
	st	 r10,r11,lo16(_use_spec_LO)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r12,r15[r9]
	br.n	 @L9698
	or.u	 r15,r0,hi16(_spec_regs_LO)
	align	 4
@L7420:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r12,r14[r9]
	or.u	 r15,r0,hi16(_spec_regs_LO)
@L9698:
	or	 r15,r15,lo16(_spec_regs_LO)
	br.n	 @L7415
	st	 r12,r0,r15
	align	 4
@L7418:
	ld.hu	 r12,r31,116
	mask	 r10,r12,255
	or	 r13,r0,1
	extu	 r11,r10,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r11]
	mak	 r13,r13,r12
	or.u	 r12,r0,hi16(_regs_LO)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L7422
	or	 r12,r12,lo16(_regs_LO)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r11,r14[r10]
	br.n	 @L7415
	st	 r11,r0,r12
	align	 4
@L7422:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r11,r15[r10]
	st	 r11,r0,r12
@L7415:
	or	 r20,r0,0
@L9697:
	or.u	 r21,r0,hi16(_use_spec_HI)
	or	 r25,r0,1
	or.u	 r22,r0,hi16(_use_spec_LO)
	or.u	 r19,r0,hi16(_regs_HI)
	or.u	 r23,r0,hi16(_spec_regs_HI)
	or	 r16,r19,lo16(_regs_HI)
	or.u	 r14,r0,hi16(_spec_mode)
@L9712:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7428
	ld	 r13,r23,lo16(_spec_regs_HI)
	st	 r25,r21,lo16(_use_spec_HI)
	mak	 r13,r13,0<1>
	br.n	 @L7429
	st	 r13,r23,lo16(_spec_regs_HI)
	align	 4
@L7428:
	ld	 r13,r21,lo16(_use_spec_HI)
	bcnd	 eq0,r13,@L7432
	ld	 r13,r23,lo16(_spec_regs_HI)
	br.n	 @L9699
	mak	 r13,r13,0<1>
	align	 4
@L7432:
	ld	 r13,r19,lo16(_regs_HI)
	mak	 r13,r13,0<1>
@L9699:
	st	 r13,r0,r16
@L7429:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7434
	ld	 r13,r22,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L7436
	st	 r25,r21,lo16(_use_spec_HI)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r2,r13,lo16(_spec_regs_LO)
	br.n	 @L9700
	or	 r3,r0,31
	align	 4
@L7436:
	or.u	 r13,r0,hi16(_regs_LO)
	ld	 r2,r13,lo16(_regs_LO)
	or	 r3,r0,31
@L9700:
	bsr.n	 _extractl
	or	 r4,r0,1
	ld	 r13,r21,lo16(_use_spec_HI)
	bcnd	 ne0,r13,@L7438
	ld	 r13,r19,lo16(_regs_HI)
	br.n	 @L9701
	addu	 r2,r13,r2
	align	 4
@L7438:
	ld	 r13,r23,lo16(_spec_regs_HI)
	addu	 r2,r13,r2
@L9701:
	or.u	 r14,r0,hi16(_spec_regs_HI)
	or	 r14,r14,lo16(_spec_regs_HI)
	br.n	 @L7435
	st	 r2,r0,r14
	align	 4
@L7434:
	ld	 r13,r22,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L7440
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r2,r13,lo16(_spec_regs_LO)
	br.n	 @L9702
	or	 r3,r0,31
	align	 4
@L7440:
	or.u	 r13,r0,hi16(_regs_LO)
	ld	 r2,r13,lo16(_regs_LO)
	or	 r3,r0,31
@L9702:
	bsr.n	 _extractl
	or	 r4,r0,1
	ld	 r13,r21,lo16(_use_spec_HI)
	bcnd	 ne0,r13,@L7442
	ld	 r13,r19,lo16(_regs_HI)
	br.n	 @L9703
	addu	 r2,r13,r2
	align	 4
@L7442:
	ld	 r13,r23,lo16(_spec_regs_HI)
	addu	 r2,r13,r2
@L9703:
	st	 r2,r0,r16
@L7435:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7444
	or.u	 r12,r0,hi16(_spec_regs_LO)
	ld	 r13,r12,lo16(_spec_regs_LO)
	st	 r25,r22,lo16(_use_spec_LO)
	mak	 r13,r13,0<1>
	br.n	 @L7445
	st	 r13,r12,lo16(_spec_regs_LO)
	align	 4
@L7444:
	ld	 r13,r22,lo16(_use_spec_LO)
	or.u	 r12,r0,hi16(_regs_LO)
	bcnd.n	 eq0,r13,@L7448
	or	 r11,r12,lo16(_regs_LO)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	br.n	 @L9704
	mak	 r13,r13,0<1>
	align	 4
@L7448:
	ld	 r13,r12,lo16(_regs_LO)
	mak	 r13,r13,0<1>
@L9704:
	st	 r13,r0,r11
@L7445:
	ld	 r13,r31,116
	extu	 r2,r13,0<24>
	extu	 r13,r13,0<29>
	mask	 r12,r2,31
	ld	 r13,r18[r13]
	mak	 r12,r25,r12
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L7451
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r2,r14[r2]
	br.n	 @L9705
	or	 r3,r0,30
	align	 4
@L7451:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r2,r15[r2]
	or	 r3,r0,30
@L9705:
	or	 r4,r0,1
	bsr.n	 _extractl
	subu	 r3,r3,r20
	cmp	 r2,r2,1
	bb1	 ne,r2,@L7426
	ld	 r13,r22,lo16(_use_spec_LO)
	bcnd.n	 ne0,r13,@L7454
	or.u	 r13,r0,hi16(_spec_regs_LO)
	or.u	 r13,r0,hi16(_regs_LO)
	ld	 r13,r13,lo16(_regs_LO)
	br.n	 @L9706
	xor.c	 r10,r13,r0
	align	 4
@L7454:
	ld	 r13,r13,lo16(_spec_regs_LO)
	xor.c	 r10,r13,r0
@L9706:
	ld.hu	 r13,r31,116
	mask	 r11,r13,255
	extu	 r12,r11,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r25,r13
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7456
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r11]
	cmp	 r13,r10,r13
	bb1.n	 lo,r13,@L9707
	or.u	 r14,r0,hi16(_spec_mode)
	br.n	 @L9708
	or.u	 r15,r0,hi16(_spec_mode)
	align	 4
@L7456:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r11]
	cmp	 r13,r10,r13
	bb0.n	 lo,r13,@L7453
	or.u	 r14,r0,hi16(_spec_mode)
@L9707:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7458
	ld	 r13,r23,lo16(_spec_regs_HI)
	st	 r25,r21,lo16(_use_spec_HI)
	addu	 r13,r13,1
	br.n	 @L7453
	st	 r13,r23,lo16(_spec_regs_HI)
	align	 4
@L7458:
	ld	 r13,r21,lo16(_use_spec_HI)
	bcnd	 ne0,r13,@L7462
	ld	 r13,r19,lo16(_regs_HI)
	br.n	 @L9709
	addu	 r13,r13,1
	align	 4
@L7462:
	ld	 r13,r23,lo16(_spec_regs_HI)
	addu	 r13,r13,1
@L9709:
	st	 r13,r0,r16
@L7453:
	or.u	 r15,r0,hi16(_spec_mode)
@L9708:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7464
	or.u	 r11,r0,hi16(_spec_regs_LO)
	ld.hu	 r13,r31,116
	mask	 r10,r13,255
	extu	 r12,r10,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r25,r13
	st	 r25,r22,lo16(_use_spec_LO)
	and	 r12,r12,r13
	ld	 r11,r11,lo16(_spec_regs_LO)
	bcnd.n	 eq0,r12,@L7468
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9710
	addu	 r13,r11,r13
	align	 4
@L7468:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	addu	 r13,r11,r13
@L9710:
	or.u	 r14,r0,hi16(_spec_regs_LO)
	or	 r14,r14,lo16(_spec_regs_LO)
	br.n	 @L7426
	st	 r13,r0,r14
	align	 4
@L7464:
	ld	 r13,r22,lo16(_use_spec_LO)
	or.u	 r12,r0,hi16(_regs_LO)
	bcnd.n	 eq0,r13,@L7470
	or	 r9,r12,lo16(_regs_LO)
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r10,r13,lo16(_spec_regs_LO)
	br	 @L7471
	align	 4
@L7470:
	ld	 r10,r12,lo16(_regs_LO)
@L7471:
	ld.hu	 r13,r31,116
	mask	 r11,r13,255
	extu	 r12,r11,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r25,r13
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7472
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r11]
	br.n	 @L9711
	addu	 r13,r10,r13
	align	 4
@L7472:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r11]
	addu	 r13,r10,r13
@L9711:
	st	 r13,r0,r9
@L7426:
	addu	 r20,r20,1
	cmp	 r13,r20,30
	bb1.n	 le,r13,@L9712
	or.u	 r14,r0,hi16(_spec_mode)
	br.n	 @L9347
	or.u	 r15,r0,hi16(_ss_op2flags)
	align	 4
@L7475:
	ld	 r12,r31,92
	or	 r15,r0,64
	st	 r15,r31,164
	or	 r14,r0,65
	st	 r14,r31,172
	or.u	 r15,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r11,r12,0<16>
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r17,r12,0<24>
	mask	 r14,r11,255
	bcnd.n	 ne0,r13,@L8670
	st	 r14,r31,180
	extu	 r12,r14,0<5>
	mask	 r11,r11,31
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7480
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9167
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7480:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9167:
	ld	 r13,r15[r14]
	bcnd.n	 ne0,r13,@L7477
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8670
	or.u	 r2,r0,hi16(@LC582)
	or	 r4,r0,596
	or.u	 r3,r0,hi16(@LC583)
	or.u	 r5,r0,hi16(@LC584)
	or	 r2,r2,lo16(@LC582)
	or	 r3,r3,lo16(@LC583)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC584)
	align	 4
@L7477:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7484
	or	 r13,r0,1
@L8670:
	ld.hu	 r13,r31,92
	mask	 r9,r13,255
	or	 r10,r0,1
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r10,r13
	or.u	 r11,r0,hi16(_use_spec_LO)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7488
	st	 r10,r11,lo16(_use_spec_LO)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	bcnd.n	 ne0,r13,@L7489
	or	 r11,r0,0
	br	 @L7487
	align	 4
@L7488:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	bcnd.n	 eq0,r13,@L7486
	or	 r11,r0,0
@L7489:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7490
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r10,r14[r10]
	br	 @L7491
	align	 4
@L7490:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r10,r15[r10]
@L7491:
	ld.hu	 r11,r31,92
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7492
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r12,r14[r9]
	bcnd.n	 le0,r12,@L9310
	subu	 r13,r0,r12
	bcnd.n	 lt0,r10,@L9168
	subu	 r11,r0,r10
	br	 @L7502
	align	 4
@L7492:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r12,r15[r9]
	bcnd.n	 gt0,r12,@L7500
	subu	 r13,r0,r12
@L9310:
	bcnd.n	 ge0,r10,@L7501
	subu	 r11,r0,r10
	divu	 r11,r11,r13
	bcnd.n	 ne0,r12,@L9713
	or.u	 r14,r0,hi16(_spec_regs_LO)
@L7503:
	tb0	 0,r0,503
	br.n	 @L9713
	or.u	 r14,r0,hi16(_spec_regs_LO)
	align	 4
@L7501:
	divu	 r11,r10,r13
	bcnd	 eq0,r12,@L7503
	subu	 r11,r0,r11
	br.n	 @L9713
	or.u	 r14,r0,hi16(_spec_regs_LO)
	align	 4
@L7500:
	bcnd.n	 ge0,r10,@L7502
	subu	 r11,r0,r10
@L9168:
	divu	 r11,r11,r12
	br.n	 @L7487
	subu	 r11,r0,r11
	align	 4
@L7502:
	divu	 r11,r10,r12
	br.n	 @L9713
	or.u	 r14,r0,hi16(_spec_regs_LO)
	align	 4
@L7486:
@L7487:
	or.u	 r14,r0,hi16(_spec_regs_LO)
@L9713:
	or	 r14,r14,lo16(_spec_regs_LO)
	br.n	 @L7485
	st	 r11,r0,r14
	align	 4
@L7484:
	ld.hu	 r12,r31,92
	mask	 r10,r12,255
	extu	 r11,r10,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r11]
	mak	 r13,r13,r12
	or.u	 r12,r0,hi16(_regs_LO)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L7506
	or	 r8,r12,lo16(_regs_LO)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 ne0,r13,@L7507
	or	 r11,r0,0
	br	 @L7505
	align	 4
@L7506:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 eq0,r13,@L7504
	or	 r11,r0,0
@L7507:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7508
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r10,r15[r10]
	br	 @L7509
	align	 4
@L7508:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r10,r14[r10]
@L7509:
	ld.hu	 r11,r31,92
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7510
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r12,r15[r9]
	bcnd.n	 le0,r12,@L9311
	subu	 r13,r0,r12
	bcnd.n	 lt0,r10,@L9169
	subu	 r11,r0,r10
	br	 @L7520
	align	 4
@L7510:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r12,r14[r9]
	bcnd.n	 gt0,r12,@L7518
	subu	 r13,r0,r12
@L9311:
	bcnd.n	 ge0,r10,@L7519
	subu	 r11,r0,r10
	divu	 r11,r11,r13
	bcnd	 ne0,r12,@L7505
@L7521:
	tb0	 0,r0,503
	br.n	 @L7485
	st	 r11,r0,r8
	align	 4
@L7519:
	divu	 r11,r10,r13
	bcnd	 eq0,r12,@L7521
	subu	 r11,r0,r11
	br.n	 @L7485
	st	 r11,r0,r8
	align	 4
@L7518:
	bcnd.n	 ge0,r10,@L7520
	subu	 r11,r0,r10
@L9169:
	divu	 r11,r11,r12
	br.n	 @L7505
	subu	 r11,r0,r11
	align	 4
@L7520:
	divu	 r11,r10,r12
	br.n	 @L7485
	st	 r11,r0,r8
	align	 4
@L7504:
@L7505:
	st	 r11,r0,r8
@L7485:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7522
	or.u	 r13,r0,hi16(_use_spec_HI)
	ld.hu	 r12,r31,92
	or	 r11,r0,1
	mask	 r10,r12,255
	st	 r11,r13,lo16(_use_spec_HI)
	extu	 r13,r10,0<5>
	mask	 r12,r12,31
	ld	 r13,r18[r13]
	mak	 r11,r11,r12
	or.u	 r12,r0,hi16(_spec_regs_HI)
	and	 r13,r13,r11
	bcnd.n	 eq0,r13,@L7526
	or	 r7,r12,lo16(_spec_regs_HI)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 ne0,r13,@L7527
	or	 r11,r0,0
	br	 @L9714
	align	 4
@L7526:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 eq0,r13,@L9714
	or	 r11,r0,0
@L7527:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7528
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r8,r14[r10]
	br	 @L7529
	align	 4
@L7528:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r8,r15[r10]
@L7529:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7530
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r10]
	or	 r11,r0,r10
	bcnd.n	 gt0,r9,@L7533
	subu	 r10,r0,r9
	bcnd.n	 ge0,r8,@L7534
	subu	 r13,r0,r8
	divu	 r10,r13,r10
	bcnd.n	 ne0,r9,@L9715
	or.u	 r15,r0,hi16(_spec_regs_R)
@L7536:
	tb0	 0,r0,503
	br.n	 @L9715
	or.u	 r15,r0,hi16(_spec_regs_R)
	align	 4
@L7534:
	divu	 r10,r8,r10
	bcnd	 eq0,r9,@L7536
	subu	 r10,r0,r10
	br.n	 @L9715
	or.u	 r15,r0,hi16(_spec_regs_R)
	align	 4
@L7533:
	bcnd.n	 ge0,r8,@L7535
	subu	 r13,r0,r8
	divu	 r10,r13,r9
	br.n	 @L7532
	subu	 r10,r0,r10
	align	 4
@L7535:
	divu	 r10,r8,r9
@L7532:
	or.u	 r15,r0,hi16(_spec_regs_R)
@L9715:
	br.n	 @L9171
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7530:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r11,r0,r10
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r11]
	bcnd.n	 le0,r9,@L9338
	subu	 r10,r0,r9
	bcnd.n	 lt0,r8,@L9337
	subu	 r13,r0,r8
	br	 @L7558
	align	 4
@L7522:
	ld.hu	 r12,r31,92
	mask	 r10,r12,255
	or	 r13,r0,1
	extu	 r11,r10,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r11]
	mak	 r13,r13,r12
	or.u	 r12,r0,hi16(_regs_HI)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L7544
	or	 r7,r12,lo16(_regs_HI)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 ne0,r13,@L7545
	or	 r11,r0,0
	br	 @L9714
	align	 4
@L7544:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 eq0,r13,@L7542
	or	 r11,r0,0
@L7545:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7546
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r8,r14[r10]
	br	 @L7547
	align	 4
@L7546:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r8,r15[r10]
@L7547:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7548
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r10]
	or	 r11,r0,r10
	bcnd.n	 gt0,r9,@L7551
	subu	 r10,r0,r9
	bcnd.n	 ge0,r8,@L7552
	subu	 r13,r0,r8
	divu	 r10,r13,r10
	bcnd.n	 ne0,r9,@L9716
	or.u	 r15,r0,hi16(_spec_regs_R)
@L7554:
	tb0	 0,r0,503
	br.n	 @L9716
	or.u	 r15,r0,hi16(_spec_regs_R)
	align	 4
@L7552:
	divu	 r10,r8,r10
	bcnd	 eq0,r9,@L7554
	subu	 r10,r0,r10
	br.n	 @L9716
	or.u	 r15,r0,hi16(_spec_regs_R)
	align	 4
@L7551:
	bcnd.n	 ge0,r8,@L7553
	subu	 r13,r0,r8
	divu	 r10,r13,r9
	br.n	 @L7550
	subu	 r10,r0,r10
	align	 4
@L7553:
	divu	 r10,r8,r9
@L7550:
	or.u	 r15,r0,hi16(_spec_regs_R)
@L9716:
	br.n	 @L9171
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7548:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r11,r0,r10
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r11]
	bcnd.n	 gt0,r9,@L7556
	subu	 r10,r0,r9
@L9338:
	bcnd.n	 ge0,r8,@L7557
	subu	 r13,r0,r8
	divu	 r10,r13,r10
	bcnd.n	 ne0,r9,@L9717
	or.u	 r15,r0,hi16(_regs_R)
@L7559:
	tb0	 0,r0,503
	br.n	 @L9717
	or.u	 r15,r0,hi16(_regs_R)
	align	 4
@L7557:
	divu	 r10,r8,r10
	bcnd	 eq0,r9,@L7559
	subu	 r10,r0,r10
	br.n	 @L9717
	or.u	 r15,r0,hi16(_regs_R)
	align	 4
@L7556:
	bcnd.n	 ge0,r8,@L7558
	subu	 r13,r0,r8
@L9337:
	divu	 r10,r13,r9
	br.n	 @L7555
	subu	 r10,r0,r10
	align	 4
@L7558:
	divu	 r10,r8,r9
@L7555:
	or.u	 r15,r0,hi16(_regs_R)
@L9717:
	or	 r15,r15,lo16(_regs_R)
@L9171:
	ld	 r13,r15[r11]
	mul	 r13,r10,r13
	subu	 r11,r8,r13
@L7542:
@L9714:
	br.n	 @L1739
	st	 r11,r0,r7
	align	 4
@L7560:
	ld	 r12,r31,92
	or	 r14,r0,64
	st	 r14,r31,164
	or	 r15,r0,65
	st	 r15,r31,172
	or.u	 r14,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r11,r12,0<16>
	ld	 r13,r14,lo16(_spec_mode)
	extu	 r17,r12,0<24>
	mask	 r15,r11,255
	bcnd.n	 ne0,r13,@L8671
	st	 r15,r31,180
	extu	 r12,r15,0<5>
	mask	 r11,r11,31
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7565
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9172
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7565:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9172:
	ld	 r13,r14[r15]
	bcnd.n	 ne0,r13,@L7562
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8671
	or.u	 r2,r0,hi16(@LC585)
	or	 r4,r0,603
	or.u	 r3,r0,hi16(@LC586)
	or.u	 r5,r0,hi16(@LC587)
	or	 r2,r2,lo16(@LC585)
	or	 r3,r3,lo16(@LC586)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC587)
	align	 4
@L7562:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7569
	or	 r13,r0,1
@L8671:
	ld.hu	 r13,r31,92
	mask	 r9,r13,255
	or	 r10,r0,1
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r10,r13
	or.u	 r11,r0,hi16(_use_spec_LO)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7573
	st	 r10,r11,lo16(_use_spec_LO)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	bcnd.n	 ne0,r13,@L7574
	or	 r11,r0,0
	br	 @L7572
	align	 4
@L7573:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	bcnd.n	 eq0,r13,@L7571
	or	 r11,r0,0
@L7574:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7575
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r10]
	br	 @L7576
	align	 4
@L7575:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r10]
@L7576:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7577
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br	 @L9173
	align	 4
@L7577:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
@L9173:
	divu	 r11,r9,r13
	bcnd	 ne0,r13,@L7572
	tb0	 0,r0,503
	br.n	 @L9718
	or.u	 r15,r0,hi16(_spec_regs_LO)
	align	 4
@L7571:
@L7572:
	or.u	 r15,r0,hi16(_spec_regs_LO)
@L9718:
	or	 r15,r15,lo16(_spec_regs_LO)
	br.n	 @L7570
	st	 r11,r0,r15
	align	 4
@L7569:
	ld.hu	 r12,r31,92
	mask	 r10,r12,255
	extu	 r11,r10,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r11]
	mak	 r13,r13,r12
	or.u	 r12,r0,hi16(_regs_LO)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L7583
	or	 r8,r12,lo16(_regs_LO)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 ne0,r13,@L7584
	or	 r11,r0,0
	br	 @L7582
	align	 4
@L7583:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 eq0,r13,@L7581
	or	 r11,r0,0
@L7584:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7585
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r9,r14[r10]
	br	 @L7586
	align	 4
@L7585:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r9,r15[r10]
@L7586:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7587
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br	 @L9174
	align	 4
@L7587:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
@L9174:
	divu	 r11,r9,r13
	bcnd	 ne0,r13,@L7582
	tb0	 0,r0,503
	br.n	 @L7570
	st	 r11,r0,r8
	align	 4
@L7581:
@L7582:
	st	 r11,r0,r8
@L7570:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7591
	or.u	 r13,r0,hi16(_use_spec_HI)
	ld.hu	 r12,r31,92
	or	 r11,r0,1
	mask	 r10,r12,255
	st	 r11,r13,lo16(_use_spec_HI)
	extu	 r13,r10,0<5>
	mask	 r12,r12,31
	ld	 r13,r18[r13]
	mak	 r11,r11,r12
	or.u	 r12,r0,hi16(_spec_regs_HI)
	and	 r13,r13,r11
	bcnd.n	 eq0,r13,@L7595
	or	 r8,r12,lo16(_spec_regs_HI)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 ne0,r13,@L7596
	or	 r11,r0,0
	br	 @L9719
	align	 4
@L7595:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 eq0,r13,@L9719
	or	 r11,r0,0
@L7596:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7597
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r10]
	br	 @L7598
	align	 4
@L7597:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r10]
@L7598:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7609
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r10]
	br	 @L9313
	align	 4
@L7591:
	ld.hu	 r12,r31,92
	mask	 r10,r12,255
	or	 r13,r0,1
	extu	 r11,r10,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r11]
	mak	 r13,r13,r12
	or.u	 r12,r0,hi16(_regs_HI)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L7605
	or	 r8,r12,lo16(_regs_HI)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 ne0,r13,@L7606
	or	 r11,r0,0
	br	 @L9719
	align	 4
@L7605:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 eq0,r13,@L7603
	or	 r11,r0,0
@L7606:
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7607
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r10]
	br	 @L7608
	align	 4
@L7607:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r10]
@L7608:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7609
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r10]
	br	 @L9313
	align	 4
@L7609:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r10]
@L9313:
	divu	 r13,r9,r11
	bcnd	 ne0,r11,@L7612
	tb0	 0,r0,503
@L7612:
	mul	 r13,r13,r11
	subu	 r11,r9,r13
@L7603:
@L9719:
	br.n	 @L1739
	st	 r11,r0,r8
	align	 4
@L7613:
	ld	 r13,r31,92
	or.u	 r14,r0,hi16(_spec_mode)
	extu	 r11,r13,0<8>
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,172
	or	 r15,r0,64
	st	 r15,r31,180
	or	 r17,r0,0
	st	 r0,r31,188
	mask	 r15,r11,255
	bcnd.n	 eq0,r13,@L7614
	st	 r15,r31,164
	extu	 r10,r15,0<5>
	mask	 r11,r11,31
	or	 r13,r0,1
	ld	 r12,r18[r10]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	or.u	 r13,r0,hi16(_use_spec_HI)
	st	 r12,r18[r10]
	ld	 r13,r13,lo16(_use_spec_HI)
	ld.bu	 r12,r31,94
	bcnd.n	 eq0,r13,@L7616
	or.u	 r13,r0,hi16(_spec_regs_HI)
	ld	 r13,r13,lo16(_spec_regs_HI)
	br.n	 @L9720
	or.u	 r14,r0,hi16(_spec_regs_R)
	align	 4
@L7616:
	or.u	 r13,r0,hi16(_regs_HI)
	ld	 r13,r13,lo16(_regs_HI)
	br.n	 @L9720
	or.u	 r14,r0,hi16(_spec_regs_R)
	align	 4
@L7614:
	or.u	 r13,r0,hi16(_use_spec_HI)
	ld	 r13,r13,lo16(_use_spec_HI)
	ld	 r12,r31,164
	bcnd.n	 eq0,r13,@L7618
	or.u	 r13,r0,hi16(_spec_regs_HI)
	ld	 r13,r13,lo16(_spec_regs_HI)
	br.n	 @L9721
	or.u	 r15,r0,hi16(_regs_R)
	align	 4
@L7618:
	or.u	 r13,r0,hi16(_regs_HI)
	ld	 r13,r13,lo16(_regs_HI)
	or.u	 r15,r0,hi16(_regs_R)
@L9721:
	or	 r15,r15,lo16(_regs_R)
	br.n	 @L1739
	st	 r13,r15[r12]
	align	 4
@L7620:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r11,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	or	 r14,r0,64
	st	 r14,r31,164
	st	 r0,r31,172
	or	 r17,r0,0
	st	 r0,r31,188
	extu	 r14,r11,0<24>
	bcnd.n	 eq0,r13,@L7621
	st	 r14,r31,180
	or.u	 r13,r0,hi16(_use_spec_HI)
	or	 r12,r0,1
	st	 r12,r13,lo16(_use_spec_HI)
	extu	 r13,r11,0<29>
	mask	 r11,r14,31
	ld	 r13,r18[r13]
	mak	 r12,r12,r11
	or.u	 r11,r0,hi16(_spec_regs_HI)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L7639
	or	 r11,r11,lo16(_spec_regs_HI)
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9176
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7621:
	ld	 r14,r31,180
	extu	 r12,r11,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_HI)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7639
	or	 r11,r11,lo16(_regs_HI)
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9176
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7627:
	ld	 r13,r31,92
	or.u	 r15,r0,hi16(_spec_mode)
	extu	 r11,r13,0<8>
	ld	 r13,r15,lo16(_spec_mode)
	st	 r0,r31,172
	or	 r14,r0,65
	st	 r14,r31,180
	or	 r17,r0,0
	st	 r0,r31,188
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7628
	st	 r14,r31,164
	extu	 r10,r14,0<5>
	mask	 r11,r11,31
	or	 r13,r0,1
	ld	 r12,r18[r10]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	or.u	 r13,r0,hi16(_use_spec_LO)
	st	 r12,r18[r10]
	ld	 r13,r13,lo16(_use_spec_LO)
	ld.bu	 r12,r31,94
	bcnd.n	 eq0,r13,@L7630
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	br.n	 @L9722
	or.u	 r15,r0,hi16(_spec_regs_R)
	align	 4
@L7630:
	or.u	 r13,r0,hi16(_regs_LO)
	ld	 r13,r13,lo16(_regs_LO)
	or.u	 r15,r0,hi16(_spec_regs_R)
@L9722:
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r13,r15[r12]
	align	 4
@L7628:
	or.u	 r13,r0,hi16(_use_spec_LO)
	ld	 r13,r13,lo16(_use_spec_LO)
	ld	 r12,r31,164
	bcnd.n	 eq0,r13,@L7632
	or.u	 r13,r0,hi16(_spec_regs_LO)
	ld	 r13,r13,lo16(_spec_regs_LO)
	br.n	 @L9723
	or.u	 r14,r0,hi16(_regs_R)
	align	 4
@L7632:
	or.u	 r13,r0,hi16(_regs_LO)
	ld	 r13,r13,lo16(_regs_LO)
	or.u	 r14,r0,hi16(_regs_R)
@L9723:
	or	 r14,r14,lo16(_regs_R)
	br.n	 @L1739
	st	 r13,r14[r12]
	align	 4
@L7634:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r14,lo16(_spec_mode)
	or	 r15,r0,65
	st	 r15,r31,164
	st	 r0,r31,172
	or	 r17,r0,0
	st	 r0,r31,188
	extu	 r15,r12,0<24>
	bcnd.n	 eq0,r13,@L7635
	st	 r15,r31,180
	extu	 r12,r12,0<29>
	or	 r10,r0,1
	mask	 r13,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r10,r13
	or.u	 r11,r0,hi16(_use_spec_LO)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7637
	st	 r10,r11,lo16(_use_spec_LO)
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9178
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7637:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9178:
	ld	 r13,r14[r15]
	or.u	 r15,r0,hi16(_spec_regs_LO)
	or	 r15,r15,lo16(_spec_regs_LO)
	br.n	 @L1739
	st	 r13,r0,r15
	align	 4
@L7635:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_LO)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7639
	or	 r11,r11,lo16(_regs_LO)
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9176
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7639:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9176:
	ld	 r13,r15[r14]
	br.n	 @L1739
	st	 r13,r0,r11
	align	 4
@L7641:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r14,r31,180
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7642
	st	 r14,r31,164
	mask	 r12,r11,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7644
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L7645
	align	 4
@L7644:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L7645:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7646
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7683
	and	 r11,r9,r13
	align	 4
@L7646:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7683
	and	 r11,r9,r13
	align	 4
@L7642:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7648
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9180
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7648:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9180:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7650
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7830
	and	 r13,r9,r13
	align	 4
@L7650:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7830
	and	 r13,r9,r13
	align	 4
@L7652:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r10,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r12,r10,0<16>
	st	 r0,r31,188
	extu	 r14,r10,0<24>
	st	 r14,r31,180
	mask	 r15,r12,255
	bcnd.n	 eq0,r13,@L7653
	st	 r15,r31,164
	mask	 r12,r12,31
	extu	 r11,r15,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	mask	 r12,r11,65535
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7655
	extu	 r10,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7768
	and	 r11,r13,r12
	align	 4
@L7655:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7768
	and	 r11,r13,r12
	align	 4
@L7653:
	ld	 r15,r31,180
	extu	 r12,r10,0<29>
	or	 r13,r0,1
	mask	 r11,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7657
	mask	 r11,r10,65535
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9183
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7657:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9183:
	ld	 r13,r14[r15]
	br.n	 @L7694
	and	 r12,r13,r11
	align	 4
@L7659:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r14,r31,180
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7660
	st	 r14,r31,164
	mask	 r12,r11,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7662
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L7663
	align	 4
@L7662:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L7663:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7664
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7683
	or	 r11,r9,r13
	align	 4
@L7664:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7683
	or	 r11,r9,r13
	align	 4
@L7660:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7666
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9185
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7666:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9185:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7668
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7830
	or	 r13,r9,r13
	align	 4
@L7668:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7830
	or	 r13,r9,r13
	align	 4
@L7670:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r10,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r12,r10,0<16>
	st	 r0,r31,188
	extu	 r14,r10,0<24>
	st	 r14,r31,180
	mask	 r15,r12,255
	bcnd.n	 eq0,r13,@L7671
	st	 r15,r31,164
	mask	 r12,r12,31
	extu	 r11,r15,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	mask	 r12,r11,65535
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7673
	extu	 r10,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7768
	or	 r11,r13,r12
	align	 4
@L7673:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7768
	or	 r11,r13,r12
	align	 4
@L7671:
	ld	 r15,r31,180
	extu	 r12,r10,0<29>
	or	 r13,r0,1
	mask	 r11,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7675
	mask	 r11,r10,65535
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9188
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7675:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9188:
	ld	 r13,r14[r15]
	br.n	 @L7694
	or	 r12,r13,r11
	align	 4
@L7677:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r14,r31,180
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7678
	st	 r14,r31,164
	mask	 r12,r11,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7680
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L7681
	align	 4
@L7680:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L7681:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7682
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7683
	xor	 r11,r9,r13
	align	 4
@L7682:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	xor	 r11,r9,r13
@L7683:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r11,r15[r8]
	align	 4
@L7678:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7684
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9190
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7684:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9190:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7686
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L7830
	xor	 r13,r9,r13
	align	 4
@L7686:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L7830
	xor	 r13,r9,r13
	align	 4
@L7688:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r10,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r12,r10,0<16>
	st	 r0,r31,188
	extu	 r14,r10,0<24>
	st	 r14,r31,180
	mask	 r15,r12,255
	bcnd.n	 eq0,r13,@L7689
	st	 r15,r31,164
	mask	 r12,r12,31
	extu	 r11,r15,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	mask	 r12,r11,65535
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7691
	extu	 r10,r11,8<16>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7768
	xor	 r11,r13,r12
	align	 4
@L7691:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7768
	xor	 r11,r13,r12
	align	 4
@L7689:
	ld	 r15,r31,180
	extu	 r12,r10,0<29>
	or	 r13,r0,1
	mask	 r11,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7693
	mask	 r11,r10,65535
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9193
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7693:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9193:
	ld	 r13,r14[r15]
	xor	 r12,r13,r11
@L7694:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	br.n	 @L1739
	st	 r12,r15[r9]
	align	 4
@L7695:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r14,r31,180
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7696
	st	 r14,r31,164
	mask	 r12,r11,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7698
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L7699
	align	 4
@L7698:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L7699:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7700
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9724
	or	 r13,r9,r13
	align	 4
@L7700:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	or	 r13,r9,r13
@L9724:
	br.n	 @L7826
	xor.c	 r13,r13,r0
	align	 4
@L7696:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7702
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9195
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7702:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9195:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7704
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9725
	or	 r13,r9,r13
	align	 4
@L7704:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	or	 r13,r9,r13
@L9725:
	br.n	 @L7830
	xor.c	 r13,r13,r0
	align	 4
@L7706:
	ld	 r10,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	or.u	 r15,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r12,r10,0<8>
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r11,r10,0<16>
	mask	 r14,r12,255
	st	 r14,r31,164
	mask	 r15,r11,255
	bcnd.n	 eq0,r13,@L7707
	st	 r15,r31,180
	mask	 r12,r12,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r13,r11,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r10,r10,r13
	mask	 r8,r11,255
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L7709
	extu	 r10,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7768
	mak	 r11,r13,r8
	align	 4
@L7709:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7768
	mak	 r11,r13,r8
	align	 4
@L7707:
	ld	 r15,r31,180
	mask	 r11,r11,31
	extu	 r12,r15,0<5>
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7711
	mask	 r11,r10,255
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9198
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7711:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9198:
	ld	 r13,r14[r15]
	br.n	 @L7837
	mak	 r13,r13,r11
	align	 4
@L7713:
	ld	 r13,r31,92
	or.u	 r14,r0,hi16(_spec_mode)
	st	 r0,r31,172
	extu	 r17,r13,0<24>
	st	 r0,r31,188
	extu	 r10,r13,0<8>
	ld	 r12,r14,lo16(_spec_mode)
	extu	 r11,r13,0<16>
	mask	 r15,r10,255
	st	 r15,r31,164
	mask	 r14,r11,255
	bcnd.n	 eq0,r12,@L7714
	st	 r14,r31,180
	mask	 r12,r10,31
	extu	 r10,r15,0<5>
	or	 r11,r0,1
	ld	 r13,r18[r10]
	mak	 r12,r11,r12
	or	 r13,r13,r12
	st	 r13,r18[r10]
	ld	 r10,r31,92
	extu	 r13,r10,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r11,r11,r13
	and	 r12,r12,r11
	bcnd.n	 eq0,r12,@L7716
	extu	 r10,r10,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r8,r15[r9]
	br	 @L7717
	align	 4
@L7716:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r8,r14[r9]
@L7717:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7718
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9726
	mask	 r13,r13,31
	align	 4
@L7718:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	mask	 r13,r13,31
@L9726:
	br.n	 @L7737
	mak	 r12,r8,r13
	align	 4
@L7714:
	ld	 r14,r31,180
	mask	 r11,r11,31
	extu	 r12,r14,0<5>
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7720
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9200
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7720:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9200:
	ld	 r9,r15[r14]
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7722
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9727
	mask	 r13,r13,31
	align	 4
@L7722:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	mask	 r13,r13,31
@L9727:
	br.n	 @L7830
	mak	 r13,r9,r13
	align	 4
@L7724:
	ld	 r10,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	or.u	 r15,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r12,r10,0<8>
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r11,r10,0<16>
	mask	 r14,r12,255
	st	 r14,r31,164
	mask	 r15,r11,255
	bcnd.n	 eq0,r13,@L7725
	st	 r15,r31,180
	mask	 r12,r12,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r13,r11,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r10,r10,r13
	mask	 r8,r11,255
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L7727
	extu	 r10,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7768
	extu	 r11,r13,r8
	align	 4
@L7727:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7768
	extu	 r11,r13,r8
	align	 4
@L7725:
	ld	 r15,r31,180
	mask	 r11,r11,31
	extu	 r12,r15,0<5>
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7729
	mask	 r11,r10,255
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9203
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7729:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9203:
	ld	 r13,r14[r15]
	br.n	 @L7837
	extu	 r13,r13,r11
	align	 4
@L7731:
	ld	 r13,r31,92
	or.u	 r14,r0,hi16(_spec_mode)
	st	 r0,r31,172
	extu	 r17,r13,0<24>
	st	 r0,r31,188
	extu	 r10,r13,0<8>
	ld	 r12,r14,lo16(_spec_mode)
	extu	 r11,r13,0<16>
	mask	 r15,r10,255
	st	 r15,r31,164
	mask	 r14,r11,255
	bcnd.n	 eq0,r12,@L7732
	st	 r14,r31,180
	mask	 r12,r10,31
	extu	 r10,r15,0<5>
	or	 r11,r0,1
	ld	 r13,r18[r10]
	mak	 r12,r11,r12
	or	 r13,r13,r12
	st	 r13,r18[r10]
	ld	 r10,r31,92
	extu	 r13,r10,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r11,r11,r13
	and	 r12,r12,r11
	bcnd.n	 eq0,r12,@L7734
	extu	 r10,r10,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r8,r15[r9]
	br	 @L7735
	align	 4
@L7734:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r8,r14[r9]
@L7735:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7736
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9728
	mask	 r13,r13,31
	align	 4
@L7736:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	mask	 r13,r13,31
@L9728:
	extu	 r12,r8,r13
@L7737:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r12,r15[r10]
	align	 4
@L7732:
	ld	 r14,r31,180
	mask	 r11,r11,31
	extu	 r12,r14,0<5>
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7738
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9205
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7738:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9205:
	ld	 r9,r15[r14]
	ld	 r12,r31,92
	extu	 r10,r12,0<24>
	or	 r13,r0,1
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7740
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9729
	mask	 r13,r13,31
	align	 4
@L7740:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	mask	 r13,r13,31
@L9729:
	br.n	 @L7830
	extu	 r13,r9,r13
	align	 4
@L7742:
	ld	 r10,r31,92
	ld	 r13,r31,88
	st	 r13,r31,112
	or	 r13,r0,1
	st	 r0,r31,172
	extu	 r11,r10,0<16>
	st	 r10,r31,116
	extu	 r10,r10,8<8>
	mask	 r15,r11,255
	st	 r15,r31,180
	extu	 r12,r15,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	or	 r17,r0,0
	mak	 r13,r13,r11
	st	 r0,r31,188
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7745
	st	 r10,r31,164
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r15]
	bcnd.n	 lt0,r13,@L9730
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L9731
	align	 4
@L7745:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r15]
	bcnd.n	 ge0,r13,@L9731
	or.u	 r15,r0,hi16(_spec_mode)
@L9730:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7747
	or	 r10,r0,1
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,31
	extu	 r13,r13,3<5>
	ld	 r12,r18[r13]
	mak	 r11,r10,r11
	or	 r12,r12,r11
	st	 r12,r18[r13]
	ld	 r11,r31,116
	extu	 r13,r11,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r10,r10,r13
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L7749
	extu	 r11,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r12,r14[r9]
	br.n	 @L9732
	or.u	 r14,r0,hi16(_spec_regs_R)
	align	 4
@L7749:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r12,r15[r9]
	or.u	 r14,r0,hi16(_spec_regs_R)
@L9732:
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L7748
	st	 r12,r14[r11]
	align	 4
@L7747:
	ld	 r10,r31,116
	extu	 r11,r10,0<16>
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7751
	extu	 r10,r10,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r9]
	br.n	 @L9733
	or.u	 r15,r0,hi16(_regs_R)
	align	 4
@L7751:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r9]
	or.u	 r15,r0,hi16(_regs_R)
@L9733:
	or	 r15,r15,lo16(_regs_R)
	st	 r11,r15[r10]
@L7748:
	ld	 r12,r31,116
	mask	 r13,r12,255
	bcnd.n	 eq0,r13,@L1739
	or	 r10,r0,0
	or.u	 r14,r0,hi16(_spec_mode)
	or	 r9,r0,1
	ld	 r8,r14,lo16(_spec_mode)
@L7756:
	bcnd.n	 eq0,r8,@L7757
	extu	 r13,r12,0<8>
	extu	 r11,r13,3<5>
	mask	 r13,r13,31
	ld	 r12,r18[r11]
	mak	 r13,r9,r13
	or	 r12,r12,r13
	st	 r12,r18[r11]
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,255
	extu	 r12,r11,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r9,r13
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7759
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r11]
	br.n	 @L9734
	ext	 r13,r13,0<1>
	align	 4
@L7759:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r11]
	ext	 r13,r13,0<1>
@L9734:
	or.u	 r13,r13,0x8000
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L7755
	st	 r13,r15[r11]
	align	 4
@L7757:
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,255
	extu	 r12,r11,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r9,r13
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7761
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r11]
	br.n	 @L9735
	ext	 r13,r13,0<1>
	align	 4
@L7761:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r11]
	ext	 r13,r13,0<1>
@L9735:
	or.u	 r13,r13,0x8000
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r11]
@L7755:
	ld	 r12,r31,116
	addu	 r10,r10,1
	mask	 r13,r12,255
	cmp	 r13,r10,r13
	bb1	 lo,r13,@L7756
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L9731:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7765
	or	 r10,r0,1
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,31
	extu	 r13,r13,3<5>
	ld	 r12,r18[r13]
	mak	 r11,r10,r11
	or	 r12,r12,r11
	st	 r12,r18[r13]
	ld	 r11,r31,116
	extu	 r13,r11,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r10,r10,r13
	mask	 r8,r11,255
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L7767
	extu	 r10,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L7768
	ext	 r11,r13,r8
	align	 4
@L7767:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	ext	 r11,r13,r8
@L7768:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r11,r14[r10]
	align	 4
@L7765:
	ld	 r10,r31,116
	extu	 r12,r10,0<16>
	or	 r13,r0,1
	mask	 r9,r12,255
	mask	 r12,r12,31
	extu	 r11,r9,0<5>
	mak	 r13,r13,r12
	ld	 r11,r18[r11]
	extu	 r8,r10,8<8>
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L7769
	mask	 r12,r10,255
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L7770
	ext	 r10,r13,r12
	align	 4
@L7769:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	ext	 r10,r13,r12
@L7770:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	br.n	 @L1739
	st	 r10,r15[r8]
	align	 4
@L7771:
	ld	 r11,r31,92
	ld	 r13,r31,88
	or	 r12,r0,1
	st	 r0,r31,172
	extu	 r17,r11,0<24>
	st	 r13,r31,112
	extu	 r13,r11,0<29>
	st	 r11,r31,116
	extu	 r14,r11,8<8>
	ld	 r13,r18[r13]
	extu	 r11,r11,8<16>
	mask	 r10,r17,31
	st	 r0,r31,188
	mak	 r12,r12,r10
	st	 r14,r31,164
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L7773
	st	 r11,r31,180
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r17]
	br.n	 @L9736
	mask	 r8,r13,31
	align	 4
@L7773:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r17]
	mask	 r8,r13,31
@L9736:
	ld.hu	 r11,r31,116
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7776
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	bcnd.n	 lt0,r13,@L9737
	or.u	 r15,r0,hi16(_spec_mode)
	br	 @L9738
	align	 4
@L7776:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	bcnd.n	 ge0,r13,@L9738
	or.u	 r15,r0,hi16(_spec_mode)
@L9737:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7778
	or	 r10,r0,1
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,31
	extu	 r13,r13,3<5>
	ld	 r12,r18[r13]
	mak	 r11,r10,r11
	or	 r12,r12,r11
	st	 r12,r18[r13]
	ld	 r11,r31,116
	extu	 r13,r11,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r10,r10,r13
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L7780
	extu	 r11,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r12,r14[r9]
	br.n	 @L9739
	or.u	 r14,r0,hi16(_spec_regs_R)
	align	 4
@L7780:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r12,r15[r9]
	or.u	 r14,r0,hi16(_spec_regs_R)
@L9739:
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L7779
	st	 r12,r14[r11]
	align	 4
@L7778:
	ld	 r10,r31,116
	extu	 r11,r10,0<16>
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7782
	extu	 r10,r10,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r11,r15[r9]
	br.n	 @L9740
	or.u	 r15,r0,hi16(_regs_R)
	align	 4
@L7782:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r11,r14[r9]
	or.u	 r15,r0,hi16(_regs_R)
@L9740:
	or	 r15,r15,lo16(_regs_R)
	st	 r11,r15[r10]
@L7779:
	bcnd.n	 eq0,r8,@L1739
	or	 r10,r0,0
	or.u	 r14,r0,hi16(_spec_mode)
	or	 r9,r0,1
	ld	 r7,r14,lo16(_spec_mode)
@L7787:
	bcnd	 eq0,r7,@L7788
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	extu	 r11,r13,3<5>
	mask	 r13,r13,31
	ld	 r12,r18[r11]
	mak	 r13,r9,r13
	or	 r12,r12,r13
	st	 r12,r18[r11]
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,255
	extu	 r12,r11,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r9,r13
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7790
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r11]
	br.n	 @L9741
	ext	 r13,r13,0<1>
	align	 4
@L7790:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r11]
	ext	 r13,r13,0<1>
@L9741:
	or.u	 r13,r13,0x8000
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L7786
	st	 r13,r15[r11]
	align	 4
@L7788:
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,255
	extu	 r12,r11,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r13,r9,r13
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7792
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r11]
	br.n	 @L9742
	ext	 r13,r13,0<1>
	align	 4
@L7792:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r11]
	ext	 r13,r13,0<1>
@L9742:
	or.u	 r13,r13,0x8000
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	st	 r13,r14[r11]
@L7786:
	addu	 r10,r10,1
	cmp	 r13,r10,r8
	bb1	 lo,r13,@L7787
	or.u	 r15,r0,hi16(_ss_op2flags)
	br	 @L9347
	align	 4
@L9738:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L7796
	or	 r10,r0,1
	ld	 r13,r31,116
	extu	 r13,r13,0<8>
	mask	 r11,r13,31
	extu	 r13,r13,3<5>
	ld	 r12,r18[r13]
	mak	 r11,r10,r11
	or	 r12,r12,r11
	st	 r12,r18[r13]
	ld	 r11,r31,116
	extu	 r13,r11,0<16>
	mask	 r9,r13,255
	extu	 r12,r9,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r10,r10,r13
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L7798
	extu	 r11,r11,8<8>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9743
	ext	 r13,r13,r8
	align	 4
@L7798:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	ext	 r13,r13,r8
@L9743:
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r13,r14[r11]
	align	 4
@L7796:
	ld	 r10,r31,116
	extu	 r11,r10,0<16>
	mask	 r9,r11,255
	or	 r13,r0,1
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7800
	extu	 r10,r10,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r9]
	br.n	 @L9744
	ext	 r13,r13,r8
	align	 4
@L7800:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r9]
	ext	 r13,r13,r8
@L9744:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	br.n	 @L1739
	st	 r13,r15[r10]
	align	 4
@L7802:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r14,r31,180
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7803
	st	 r14,r31,164
	mask	 r12,r11,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7805
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L7806
	align	 4
@L7805:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L7806:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7807
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9745
	cmp	 r13,r9,r13
	align	 4
@L7807:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
@L9745:
	br.n	 @L7826
	extu	 r13,r13,1<lt>
	align	 4
@L7803:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7809
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9217
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7809:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9217:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7811
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9746
	cmp	 r13,r9,r13
	align	 4
@L7811:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
@L9746:
	br.n	 @L7830
	extu	 r13,r13,1<lt>
	align	 4
@L7813:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r10,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r12,r10,0<16>
	st	 r0,r31,188
	extu	 r14,r10,0<24>
	st	 r14,r31,180
	mask	 r15,r12,255
	bcnd.n	 eq0,r13,@L7814
	st	 r15,r31,164
	mask	 r12,r12,31
	extu	 r11,r15,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r12,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7816
	ext	 r11,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9747
	cmp	 r13,r13,r11
	align	 4
@L7816:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	cmp	 r13,r13,r11
@L9747:
	br.n	 @L7835
	extu	 r13,r13,1<lt>
	align	 4
@L7814:
	ld	 r15,r31,180
	or	 r13,r0,1
	extu	 r12,r10,0<29>
	mask	 r11,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7818
	ext	 r11,r10,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9220
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7818:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9220:
	ld	 r13,r14[r15]
	cmp	 r13,r13,r11
	br.n	 @L7837
	extu	 r13,r13,1<lt>
	align	 4
@L7820:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	extu	 r14,r12,0<24>
	st	 r0,r31,172
	extu	 r11,r12,0<8>
	st	 r0,r31,188
	extu	 r17,r12,8<16>
	st	 r14,r31,180
	mask	 r14,r11,255
	bcnd.n	 eq0,r13,@L7821
	st	 r14,r31,164
	mask	 r12,r11,31
	extu	 r11,r14,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7823
	extu	 r8,r11,8<8>
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r9,r15[r9]
	br	 @L7824
	align	 4
@L7823:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r9,r14[r9]
@L7824:
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7825
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r13,r15[r10]
	br.n	 @L9748
	cmp	 r13,r9,r13
	align	 4
@L7825:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r13,r14[r10]
	cmp	 r13,r9,r13
@L9748:
	extu	 r13,r13,1<lo>
@L7826:
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r13,r15[r8]
	align	 4
@L7821:
	ld	 r14,r31,180
	extu	 r12,r12,0<29>
	or	 r13,r0,1
	mask	 r11,r14,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r8,r31,164
	bcnd.n	 eq0,r12,@L7827
	or.u	 r15,r0,hi16(_spec_regs_R)
	br.n	 @L9222
	or	 r15,r15,lo16(_spec_regs_R)
	align	 4
@L7827:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,180
	or	 r15,r15,lo16(_regs_R)
@L9222:
	ld	 r9,r15[r14]
	ld.hu	 r11,r31,92
	mask	 r10,r11,255
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7829
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r10]
	br.n	 @L9749
	cmp	 r13,r9,r13
	align	 4
@L7829:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
@L9749:
	extu	 r13,r13,1<lo>
@L7830:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	br.n	 @L1739
	st	 r13,r14[r8]
	align	 4
@L7831:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r10,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r12,r10,0<16>
	st	 r0,r31,188
	extu	 r14,r10,0<24>
	st	 r14,r31,180
	mask	 r15,r12,255
	bcnd.n	 eq0,r13,@L7832
	st	 r15,r31,164
	mask	 r12,r12,31
	extu	 r11,r15,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r11,r31,92
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r12,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L7834
	ext	 r11,r11,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r13,r14[r9]
	br.n	 @L9750
	cmp	 r13,r13,r11
	align	 4
@L7834:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r13,r15[r9]
	cmp	 r13,r13,r11
@L9750:
	extu	 r13,r13,1<lo>
@L7835:
	or.u	 r14,r0,hi16(_spec_regs_R)
@L9720:
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r13,r14[r12]
	align	 4
@L7832:
	ld	 r15,r31,180
	or	 r13,r0,1
	extu	 r12,r10,0<29>
	mask	 r11,r15,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r9,r31,164
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7836
	ext	 r11,r10,16<0>
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9225
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L7836:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9225:
	ld	 r13,r14[r15]
	cmp	 r13,r13,r11
	extu	 r13,r13,1<lo>
@L7837:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	br.n	 @L1739
	st	 r13,r15[r9]
	align	 4
@L7838:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L7840
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7840
	or.u	 r2,r0,hi16(@LC588)
	or	 r4,r0,726
	or.u	 r3,r0,hi16(@LC589)
	or.u	 r5,r0,hi16(@LC590)
	or	 r2,r2,lo16(@LC588)
	or	 r3,r3,lo16(@LC589)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC590)
	align	 4
@L7840:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L7844
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7844
	or.u	 r2,r0,hi16(@LC591)
	or	 r4,r0,726
	or.u	 r3,r0,hi16(@LC592)
	or.u	 r5,r0,hi16(@LC593)
	or	 r2,r2,lo16(@LC591)
	or	 r3,r3,lo16(@LC592)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC593)
	align	 4
@L7844:
	ld	 r10,r31,92
	bb0.n	 (31-15),r10,@L7848
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8672
	or.u	 r2,r0,hi16(@LC594)
	or	 r4,r0,726
	or.u	 r3,r0,hi16(@LC595)
	or.u	 r5,r0,hi16(@LC596)
	or	 r2,r2,lo16(@LC594)
	or	 r3,r3,lo16(@LC595)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC596)
	align	 4
@L7848:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7851
	extu	 r9,r10,0<24>
@L8672:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r5,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L7853
	extu	 r6,r8,8<8>
	ld	 r8,r5[r7]
	br	 @L7854
	align	 4
@L7853:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r8,r13[r7]
@L7854:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7855
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9226
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7855:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9226:
	ld	 r13,r13[r12]
	fadd.sss r8,r8,r13
	br.n	 @L1739
	st	 r8,r5[r6]
	align	 4
@L7851:
	extu	 r6,r10,8<8>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7857
	or	 r7,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r8,r13[r9]
	br	 @L7858
	align	 4
@L7857:
	ld	 r8,r7[r9]
@L7858:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7859
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9227
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7859:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9227:
	ld	 r13,r13[r12]
	fadd.sss r10,r8,r13
	br.n	 @L1739
	st	 r10,r7[r6]
	align	 4
@L7861:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L7863
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7863
	or.u	 r2,r0,hi16(@LC597)
	or	 r4,r0,733
	or.u	 r3,r0,hi16(@LC598)
	or.u	 r5,r0,hi16(@LC599)
	or	 r2,r2,lo16(@LC597)
	or	 r3,r3,lo16(@LC598)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC599)
	align	 4
@L7863:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L7867
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7867
	or.u	 r2,r0,hi16(@LC600)
	or	 r4,r0,733
	or.u	 r3,r0,hi16(@LC601)
	or.u	 r5,r0,hi16(@LC602)
	or	 r2,r2,lo16(@LC600)
	or	 r3,r3,lo16(@LC601)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC602)
	align	 4
@L7867:
	ld	 r9,r31,92
	bb0.n	 (31-15),r9,@L7871
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8673
	or.u	 r2,r0,hi16(@LC603)
	or	 r4,r0,733
	or.u	 r3,r0,hi16(@LC604)
	or.u	 r5,r0,hi16(@LC605)
	or	 r2,r2,lo16(@LC603)
	or	 r3,r3,lo16(@LC604)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC605)
	align	 4
@L7871:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7874
	extu	 r5,r9,7<9>
@L8673:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r5,r8,7<9>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L7876
	or	 r4,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r6,r4[r13]
	br	 @L7877
	align	 4
@L7876:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r6,r13[r12]
@L7877:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7878
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9228
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7878:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9228:
	ld.d	 r12,r13[r12]
	fadd.ddd r12,r6,r12
	br.n	 @L1739
	st.d	 r12,r4[r5]
	align	 4
@L7874:
	extu	 r11,r9,0<24>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7880
	or	 r8,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r6,r13[r12]
	br	 @L7881
	align	 4
@L7880:
	extu	 r13,r9,0<25>
	ld.d	 r6,r8[r13]
@L7881:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7882
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9229
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7882:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9229:
	ld.d	 r12,r13[r12]
	fadd.ddd r12,r6,r12
	br.n	 @L1739
	st.d	 r12,r8[r5]
	align	 4
@L7884:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L7886
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7886
	or.u	 r2,r0,hi16(@LC606)
	or	 r4,r0,739
	or.u	 r3,r0,hi16(@LC607)
	or.u	 r5,r0,hi16(@LC608)
	or	 r2,r2,lo16(@LC606)
	or	 r3,r3,lo16(@LC607)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC608)
	align	 4
@L7886:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L7890
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7890
	or.u	 r2,r0,hi16(@LC609)
	or	 r4,r0,739
	or.u	 r3,r0,hi16(@LC610)
	or.u	 r5,r0,hi16(@LC611)
	or	 r2,r2,lo16(@LC609)
	or	 r3,r3,lo16(@LC610)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC611)
	align	 4
@L7890:
	ld	 r10,r31,92
	bb0.n	 (31-15),r10,@L7894
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8674
	or.u	 r2,r0,hi16(@LC612)
	or	 r4,r0,739
	or.u	 r3,r0,hi16(@LC613)
	or.u	 r5,r0,hi16(@LC614)
	or	 r2,r2,lo16(@LC612)
	or	 r3,r3,lo16(@LC613)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC614)
	align	 4
@L7894:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7897
	extu	 r9,r10,0<24>
@L8674:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r5,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L7899
	extu	 r6,r8,8<8>
	ld	 r8,r5[r7]
	br	 @L7900
	align	 4
@L7899:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r8,r13[r7]
@L7900:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7901
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9230
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7901:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9230:
	ld	 r13,r13[r12]
	fsub.sss r8,r8,r13
	br.n	 @L1739
	st	 r8,r5[r6]
	align	 4
@L7897:
	extu	 r6,r10,8<8>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7903
	or	 r7,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r8,r13[r9]
	br	 @L7904
	align	 4
@L7903:
	ld	 r8,r7[r9]
@L7904:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7905
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9231
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7905:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9231:
	ld	 r13,r13[r12]
	fsub.sss r10,r8,r13
	br.n	 @L1739
	st	 r10,r7[r6]
	align	 4
@L7907:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L7909
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7909
	or.u	 r2,r0,hi16(@LC615)
	or	 r4,r0,745
	or.u	 r3,r0,hi16(@LC616)
	or.u	 r5,r0,hi16(@LC617)
	or	 r2,r2,lo16(@LC615)
	or	 r3,r3,lo16(@LC616)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC617)
	align	 4
@L7909:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L7913
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7913
	or.u	 r2,r0,hi16(@LC618)
	or	 r4,r0,745
	or.u	 r3,r0,hi16(@LC619)
	or.u	 r5,r0,hi16(@LC620)
	or	 r2,r2,lo16(@LC618)
	or	 r3,r3,lo16(@LC619)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC620)
	align	 4
@L7913:
	ld	 r9,r31,92
	bb0.n	 (31-15),r9,@L7917
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8675
	or.u	 r2,r0,hi16(@LC621)
	or	 r4,r0,745
	or.u	 r3,r0,hi16(@LC622)
	or.u	 r5,r0,hi16(@LC623)
	or	 r2,r2,lo16(@LC621)
	or	 r3,r3,lo16(@LC622)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC623)
	align	 4
@L7917:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7920
	extu	 r5,r9,7<9>
@L8675:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r5,r8,7<9>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L7922
	or	 r4,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r6,r4[r13]
	br	 @L7923
	align	 4
@L7922:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r6,r13[r12]
@L7923:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7924
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9232
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7924:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9232:
	ld.d	 r12,r13[r12]
	fsub.ddd r12,r6,r12
	br.n	 @L1739
	st.d	 r12,r4[r5]
	align	 4
@L7920:
	extu	 r11,r9,0<24>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7926
	or	 r8,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r6,r13[r12]
	br	 @L7927
	align	 4
@L7926:
	extu	 r13,r9,0<25>
	ld.d	 r6,r8[r13]
@L7927:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7928
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9233
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7928:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9233:
	ld.d	 r12,r13[r12]
	fsub.ddd r12,r6,r12
	br.n	 @L1739
	st.d	 r12,r8[r5]
	align	 4
@L7930:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L7932
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7932
	or.u	 r2,r0,hi16(@LC624)
	or	 r4,r0,751
	or.u	 r3,r0,hi16(@LC625)
	or.u	 r5,r0,hi16(@LC626)
	or	 r2,r2,lo16(@LC624)
	or	 r3,r3,lo16(@LC625)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC626)
	align	 4
@L7932:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L7936
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7936
	or.u	 r2,r0,hi16(@LC627)
	or	 r4,r0,751
	or.u	 r3,r0,hi16(@LC628)
	or.u	 r5,r0,hi16(@LC629)
	or	 r2,r2,lo16(@LC627)
	or	 r3,r3,lo16(@LC628)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC629)
	align	 4
@L7936:
	ld	 r10,r31,92
	bb0.n	 (31-15),r10,@L7940
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8676
	or.u	 r2,r0,hi16(@LC630)
	or	 r4,r0,751
	or.u	 r3,r0,hi16(@LC631)
	or.u	 r5,r0,hi16(@LC632)
	or	 r2,r2,lo16(@LC630)
	or	 r3,r3,lo16(@LC631)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC632)
	align	 4
@L7940:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7943
	extu	 r9,r10,0<24>
@L8676:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r5,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L7945
	extu	 r6,r8,8<8>
	ld	 r8,r5[r7]
	br	 @L7946
	align	 4
@L7945:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r8,r13[r7]
@L7946:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7947
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9234
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7947:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9234:
	ld	 r13,r13[r12]
	fmul.sss r8,r8,r13
	br.n	 @L1739
	st	 r8,r5[r6]
	align	 4
@L7943:
	extu	 r6,r10,8<8>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7949
	or	 r7,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r8,r13[r9]
	br	 @L7950
	align	 4
@L7949:
	ld	 r8,r7[r9]
@L7950:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7951
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9235
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7951:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9235:
	ld	 r13,r13[r12]
	fmul.sss r10,r8,r13
	br.n	 @L1739
	st	 r10,r7[r6]
	align	 4
@L7953:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L7955
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7955
	or.u	 r2,r0,hi16(@LC633)
	or	 r4,r0,757
	or.u	 r3,r0,hi16(@LC634)
	or.u	 r5,r0,hi16(@LC635)
	or	 r2,r2,lo16(@LC633)
	or	 r3,r3,lo16(@LC634)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC635)
	align	 4
@L7955:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L7959
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7959
	or.u	 r2,r0,hi16(@LC636)
	or	 r4,r0,757
	or.u	 r3,r0,hi16(@LC637)
	or.u	 r5,r0,hi16(@LC638)
	or	 r2,r2,lo16(@LC636)
	or	 r3,r3,lo16(@LC637)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC638)
	align	 4
@L7959:
	ld	 r9,r31,92
	bb0.n	 (31-15),r9,@L7963
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8677
	or.u	 r2,r0,hi16(@LC639)
	or	 r4,r0,757
	or.u	 r3,r0,hi16(@LC640)
	or.u	 r5,r0,hi16(@LC641)
	or	 r2,r2,lo16(@LC639)
	or	 r3,r3,lo16(@LC640)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC641)
	align	 4
@L7963:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7966
	extu	 r5,r9,7<9>
@L8677:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r5,r8,7<9>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L7968
	or	 r4,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r6,r4[r13]
	br	 @L7969
	align	 4
@L7968:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r6,r13[r12]
@L7969:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7970
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9236
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7970:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9236:
	ld.d	 r12,r13[r12]
	fmul.ddd r12,r6,r12
	br.n	 @L1739
	st.d	 r12,r4[r5]
	align	 4
@L7966:
	extu	 r11,r9,0<24>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7972
	or	 r8,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r6,r13[r12]
	br	 @L7973
	align	 4
@L7972:
	extu	 r13,r9,0<25>
	ld.d	 r6,r8[r13]
@L7973:
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7974
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9237
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L7974:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9237:
	ld.d	 r12,r13[r12]
	fmul.ddd r12,r6,r12
	br.n	 @L1739
	st.d	 r12,r8[r5]
	align	 4
@L7976:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L7978
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7978
	or.u	 r2,r0,hi16(@LC642)
	or	 r4,r0,763
	or.u	 r3,r0,hi16(@LC643)
	or.u	 r5,r0,hi16(@LC644)
	or	 r2,r2,lo16(@LC642)
	or	 r3,r3,lo16(@LC643)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC644)
	align	 4
@L7978:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L7982
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L7982
	or.u	 r2,r0,hi16(@LC645)
	or	 r4,r0,763
	or.u	 r3,r0,hi16(@LC646)
	or.u	 r5,r0,hi16(@LC647)
	or	 r2,r2,lo16(@LC645)
	or	 r3,r3,lo16(@LC646)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC647)
	align	 4
@L7982:
	ld.hu	 r9,r31,92
	bb0.n	 (31-31),r9,@L7986
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8678
	or.u	 r2,r0,hi16(@LC648)
	or	 r4,r0,763
	or.u	 r3,r0,hi16(@LC649)
	or.u	 r5,r0,hi16(@LC650)
	or	 r2,r2,lo16(@LC648)
	or	 r3,r3,lo16(@LC649)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC650)
	align	 4
@L7986:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8678
	extu	 r10,r9,3<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L7993
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r13,r13[r12]
	fcmp.sss r13,r13,r0
	bb0.n	 ne,r13,@L9751
	or.u	 r14,r0,hi16(_spec_mode)
	br	 @L9752
	align	 4
@L7993:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
	ld	 r13,r13[r12]
	fcmp.sss r13,r13,r0
	bb1.n	 ne,r13,@L9752
	or.u	 r15,r0,hi16(_spec_mode)
	or.u	 r14,r0,hi16(_spec_mode)
@L9751:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8678
	or.u	 r2,r0,hi16(@LC651)
	or	 r4,r0,763
	or.u	 r3,r0,hi16(@LC652)
	or.u	 r5,r0,hi16(@LC653)
	or	 r2,r2,lo16(@LC651)
	or	 r3,r3,lo16(@LC652)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC653)
	align	 4
@L9752:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L7997
	or.u	 r12,r0,hi16(_use_spec_F)
@L8678:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<16>
	extu	 r13,r7,3<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r6,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8001
	extu	 r8,r8,8<8>
	mask	 r13,r7,255
	ld	 r13,r6[r13]
	fcmp.sss r13,r13,r0
	bb0	 ne,r13,@L7999
	br	 @L8002
	align	 4
@L8001:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r7,255
	or	 r13,r13,lo16(_regs_F)
	ld	 r13,r13[r12]
	fcmp.sss r13,r13,r0
	bb0	 ne,r13,@L7999
@L8002:
	ld.bu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,0<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8003
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9238
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8003:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9238:
	ld	 r7,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8005
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9239
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8005:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9239:
	ld	 r13,r13[r12]
	fdiv.sss r10,r7,r13
	br.n	 @L1739
	st	 r10,r6[r8]
	align	 4
@L7999:
	or	 r10,r0,0
	br.n	 @L1739
	st	 r10,r6[r8]
	align	 4
@L7997:
	ld	 r13,r31,92
	extu	 r7,r13,8<8>
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r9,r13,0<16>
	or	 r13,r0,1
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8009
	or	 r6,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r13,r13[r12]
	fcmp.sss r13,r13,r0
	bb0	 ne,r13,@L8007
	br	 @L8010
	align	 4
@L8009:
	mask	 r13,r9,255
	ld	 r13,r6[r13]
	fcmp.sss r13,r13,r0
	bb0	 ne,r13,@L8007
@L8010:
	ld.bu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,0<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8011
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9240
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8011:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9240:
	ld	 r8,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8013
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9241
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8013:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9241:
	ld	 r13,r13[r12]
	fdiv.sss r10,r8,r13
	br.n	 @L1739
	st	 r10,r6[r7]
	align	 4
@L8007:
	or	 r10,r0,0
	br.n	 @L1739
	st	 r10,r6[r7]
	align	 4
@L8015:
	ld	 r12,r31,92
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r11,r12,0<8>
	addu	 r13,r13,32
	extu	 r12,r12,8<16>
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	mask	 r13,r11,255
	addu	 r12,r12,32
	addu	 r13,r13,32
	and	 r17,r12,0xfffe
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r11,@L8017
	st	 r13,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8017
	or.u	 r2,r0,hi16(@LC654)
	or	 r4,r0,769
	or.u	 r3,r0,hi16(@LC655)
	or.u	 r5,r0,hi16(@LC656)
	or	 r2,r2,lo16(@LC654)
	or	 r3,r3,lo16(@LC655)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC656)
	align	 4
@L8017:
	ld.bu	 r13,r31,92
	bb0.n	 (31-31),r13,@L8021
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8021
	or.u	 r2,r0,hi16(@LC657)
	or	 r4,r0,769
	or.u	 r3,r0,hi16(@LC658)
	or.u	 r5,r0,hi16(@LC659)
	or	 r2,r2,lo16(@LC657)
	or	 r3,r3,lo16(@LC658)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC659)
	align	 4
@L8021:
	ld.hu	 r9,r31,92
	bb0.n	 (31-31),r9,@L8025
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8679
	or.u	 r2,r0,hi16(@LC660)
	or	 r4,r0,769
	or.u	 r3,r0,hi16(@LC661)
	or.u	 r5,r0,hi16(@LC662)
	or	 r2,r2,lo16(@LC660)
	or	 r3,r3,lo16(@LC661)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC662)
	align	 4
@L8025:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8679
	extu	 r10,r9,3<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8032
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r12,r13[r12]
	fcmp.sds r13,r12,r0
	bb0.n	 ne,r13,@L9753
	or.u	 r14,r0,hi16(_spec_mode)
	br	 @L9754
	align	 4
@L8032:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r12,r13[r12]
	fcmp.sds r13,r12,r0
	bb1.n	 ne,r13,@L9754
	or.u	 r15,r0,hi16(_spec_mode)
	or.u	 r14,r0,hi16(_spec_mode)
@L9753:
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8679
	or.u	 r2,r0,hi16(@LC663)
	or	 r4,r0,769
	or.u	 r3,r0,hi16(@LC664)
	or.u	 r5,r0,hi16(@LC665)
	or	 r2,r2,lo16(@LC663)
	or	 r3,r3,lo16(@LC664)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC665)
	align	 4
@L9754:
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8036
	or.u	 r12,r0,hi16(_use_spec_F)
@L8679:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<16>
	extu	 r13,r7,3<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r5,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8040
	extu	 r8,r8,7<9>
	extu	 r13,r7,7<1>
	ld.d	 r12,r5[r13]
	fcmp.sds r13,r12,r0
	bb0	 ne,r13,@L8046
	br	 @L8041
	align	 4
@L8040:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r7,7<1>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r12,r13[r12]
	fcmp.sds r13,r12,r0
	bb0	 ne,r13,@L8046
@L8041:
	ld	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r11,r9,0<24>
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r11,0<5>
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8042
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9242
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8042:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9242:
	ld.d	 r6,r13[r12]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8052
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9245
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8036:
	ld	 r13,r31,92
	extu	 r8,r13,7<9>
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r9,r13,0<16>
	or	 r13,r0,1
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8048
	or	 r5,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r12,r13[r12]
	fcmp.sds r13,r12,r0
	bb0	 ne,r13,@L8046
	br	 @L8049
	align	 4
@L8048:
	extu	 r13,r9,7<1>
	ld.d	 r12,r5[r13]
	fcmp.sds r13,r12,r0
	bb0	 ne,r13,@L8046
@L8049:
	ld	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r11,r9,0<24>
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r11,0<5>
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8050
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9244
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8050:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9244:
	ld.d	 r6,r13[r12]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8052
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,7<1>
	br.n	 @L9245
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8052:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9245:
	ld.d	 r12,r13[r12]
	fdiv.ddd r12,r6,r12
	br.n	 @L1739
	st.d	 r12,r5[r8]
	align	 4
@L8046:
	or	 r12,r0,0
	or	 r13,r0,0
	br.n	 @L1739
	st.d	 r12,r5[r8]
	align	 4
@L8054:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8056
	st	 r12,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8056
	or.u	 r2,r0,hi16(@LC666)
	or	 r4,r0,774
	or.u	 r3,r0,hi16(@LC667)
	or.u	 r5,r0,hi16(@LC668)
	or	 r2,r2,lo16(@LC666)
	or	 r3,r3,lo16(@LC667)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC668)
	align	 4
@L8056:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	bb0.n	 (31-31),r9,@L8060
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9755
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC669)
	or	 r4,r0,774
	or.u	 r3,r0,hi16(@LC670)
	or.u	 r5,r0,hi16(@LC671)
	or	 r2,r2,lo16(@LC669)
	or	 r3,r3,lo16(@LC670)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC671)
	align	 4
@L8060:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8063
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9755:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r12,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8065
	extu	 r10,r8,8<8>
	ld	 r8,r12[r7]
	br.n	 @L9756
	and.u	 r13,r8,0x7fff
	align	 4
@L8065:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r8,r13[r7]
	and.u	 r13,r8,0x7fff
@L9756:
	br.n	 @L1739
	st	 r13,r12[r10]
	align	 4
@L8063:
	extu	 r8,r12,8<8>
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8067
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r10,r13[r9]
	br.n	 @L9757
	and.u	 r13,r10,0x7fff
	align	 4
@L8067:
	ld	 r10,r11[r9]
	and.u	 r13,r10,0x7fff
@L9757:
	br.n	 @L1739
	st	 r13,r11[r8]
	align	 4
@L8069:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8071
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8071
	or.u	 r2,r0,hi16(@LC672)
	or	 r4,r0,779
	or.u	 r3,r0,hi16(@LC673)
	or.u	 r5,r0,hi16(@LC674)
	or	 r2,r2,lo16(@LC672)
	or	 r3,r3,lo16(@LC673)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC674)
	align	 4
@L8071:
	ld	 r9,r31,92
	extu	 r11,r9,0<24>
	bb0.n	 (31-31),r11,@L8075
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8681
	or.u	 r2,r0,hi16(@LC675)
	or	 r4,r0,779
	or.u	 r3,r0,hi16(@LC676)
	or.u	 r5,r0,hi16(@LC677)
	or	 r2,r2,lo16(@LC675)
	or	 r3,r3,lo16(@LC676)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC677)
	align	 4
@L8075:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8078
	extu	 r8,r9,7<9>
@L8681:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r10,r8,7<9>
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8080
	or	 r7,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r12,r7[r13]
	br.n	 @L9758
	and.u	 r12,r12,0x7fff
	align	 4
@L8080:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r12,r13[r12]
	and.u	 r12,r12,0x7fff
@L9251:
@L9758:
	br.n	 @L1739
	st.d	 r12,r7[r10]
	align	 4
@L8078:
	extu	 r10,r11,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r11,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8082
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r12,r13[r12]
	br.n	 @L9759
	and.u	 r12,r12,0x7fff
	align	 4
@L8082:
	extu	 r13,r9,0<25>
	ld.d	 r12,r11[r13]
	and.u	 r12,r12,0x7fff
@L9336:
@L9759:
	br.n	 @L1739
	st.d	 r12,r11[r8]
	align	 4
@L8084:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8086
	st	 r12,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8086
	or.u	 r2,r0,hi16(@LC678)
	or	 r4,r0,784
	or.u	 r3,r0,hi16(@LC679)
	or.u	 r5,r0,hi16(@LC680)
	or	 r2,r2,lo16(@LC678)
	or	 r3,r3,lo16(@LC679)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC680)
	align	 4
@L8086:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	bb0.n	 (31-31),r9,@L8090
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9760
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC681)
	or	 r4,r0,784
	or.u	 r3,r0,hi16(@LC682)
	or.u	 r5,r0,hi16(@LC683)
	or	 r2,r2,lo16(@LC681)
	or	 r3,r3,lo16(@LC682)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC683)
	align	 4
@L8090:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8093
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9760:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r12,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8095
	extu	 r10,r8,8<8>
	ld	 r8,r12[r7]
	br.n	 @L1739
	st	 r8,r12[r10]
	align	 4
@L8095:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r8,r13[r7]
	br.n	 @L1739
	st	 r8,r12[r10]
	align	 4
@L8093:
	extu	 r8,r12,8<8>
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8097
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r10,r13[r9]
	br.n	 @L1739
	st	 r10,r11[r8]
	align	 4
@L8097:
	ld	 r10,r11[r9]
	br.n	 @L1739
	st	 r10,r11[r8]
	align	 4
@L8099:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8101
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8101
	or.u	 r2,r0,hi16(@LC684)
	or	 r4,r0,789
	or.u	 r3,r0,hi16(@LC685)
	or.u	 r5,r0,hi16(@LC686)
	or	 r2,r2,lo16(@LC684)
	or	 r3,r3,lo16(@LC685)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC686)
	align	 4
@L8101:
	ld	 r9,r31,92
	extu	 r11,r9,0<24>
	bb0.n	 (31-31),r11,@L8105
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8683
	or.u	 r2,r0,hi16(@LC687)
	or	 r4,r0,789
	or.u	 r3,r0,hi16(@LC688)
	or.u	 r5,r0,hi16(@LC689)
	or	 r2,r2,lo16(@LC687)
	or	 r3,r3,lo16(@LC688)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC689)
	align	 4
@L8105:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8108
	extu	 r8,r9,7<9>
@L8683:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r10,r8,7<9>
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8110
	or	 r7,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r12,r7[r13]
	br.n	 @L1739
	st.d	 r12,r7[r10]
	align	 4
@L8110:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r12,r13[r12]
	br.n	 @L1739
	st.d	 r12,r7[r10]
	align	 4
@L8108:
	extu	 r10,r11,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r11,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8112
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r12,r13[r12]
	br.n	 @L1739
	st.d	 r12,r11[r8]
	align	 4
@L8112:
	extu	 r13,r9,0<25>
	ld.d	 r12,r11[r13]
	br.n	 @L1739
	st.d	 r12,r11[r8]
	align	 4
@L8114:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8116
	st	 r12,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8116
	or.u	 r2,r0,hi16(@LC690)
	or	 r4,r0,794
	or.u	 r3,r0,hi16(@LC691)
	or.u	 r5,r0,hi16(@LC692)
	or	 r2,r2,lo16(@LC690)
	or	 r3,r3,lo16(@LC691)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC692)
	align	 4
@L8116:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	bb0.n	 (31-31),r9,@L8120
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9761
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC693)
	or	 r4,r0,794
	or.u	 r3,r0,hi16(@LC694)
	or.u	 r5,r0,hi16(@LC695)
	or	 r2,r2,lo16(@LC693)
	or	 r3,r3,lo16(@LC694)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC695)
	align	 4
@L8120:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8123
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9761:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r12,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8125
	extu	 r8,r8,8<8>
	ld	 r13,r12[r7]
	br.n	 @L9762
	xor.u	 r13,r13,0x8000
	align	 4
@L8125:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r13,r13[r7]
	xor.u	 r13,r13,0x8000
@L9762:
	br.n	 @L1739
	st	 r13,r12[r8]
	align	 4
@L8123:
	extu	 r8,r12,8<8>
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8127
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r13,r13[r9]
	br.n	 @L9763
	xor.u	 r13,r13,0x8000
	align	 4
@L8127:
	ld	 r13,r11[r9]
	xor.u	 r13,r13,0x8000
@L9763:
	br.n	 @L1739
	st	 r13,r11[r8]
	align	 4
@L8129:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8131
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8131
	or.u	 r2,r0,hi16(@LC696)
	or	 r4,r0,799
	or.u	 r3,r0,hi16(@LC697)
	or.u	 r5,r0,hi16(@LC698)
	or	 r2,r2,lo16(@LC696)
	or	 r3,r3,lo16(@LC697)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC698)
	align	 4
@L8131:
	ld	 r9,r31,92
	extu	 r11,r9,0<24>
	bb0.n	 (31-31),r11,@L8135
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8685
	or.u	 r2,r0,hi16(@LC699)
	or	 r4,r0,799
	or.u	 r3,r0,hi16(@LC700)
	or.u	 r5,r0,hi16(@LC701)
	or	 r2,r2,lo16(@LC699)
	or	 r3,r3,lo16(@LC700)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC701)
	align	 4
@L8135:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8138
	extu	 r8,r9,7<9>
@L8685:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r10,r8,7<9>
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8140
	or	 r7,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r12,r7[r13]
	br.n	 @L9758
	xor.u	 r12,r12,0x8000
	align	 4
@L8140:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r12,r13[r12]
	br.n	 @L9251
	xor.u	 r12,r12,0x8000
	align	 4
@L8138:
	extu	 r10,r11,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r11,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8142
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r12,r13[r12]
	br.n	 @L9759
	xor.u	 r12,r12,0x8000
	align	 4
@L8142:
	extu	 r13,r9,0<25>
	ld.d	 r12,r11[r13]
	br.n	 @L9336
	xor.u	 r12,r12,0x8000
	align	 4
@L8144:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8146
	st	 r12,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8146
	or.u	 r2,r0,hi16(@LC702)
	or	 r4,r0,805
	or.u	 r3,r0,hi16(@LC703)
	or.u	 r5,r0,hi16(@LC704)
	or	 r2,r2,lo16(@LC702)
	or	 r3,r3,lo16(@LC703)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC704)
	align	 4
@L8146:
	ld	 r9,r31,92
	extu	 r11,r9,0<24>
	bb0.n	 (31-31),r11,@L8150
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8686
	or.u	 r2,r0,hi16(@LC705)
	or	 r4,r0,805
	or.u	 r3,r0,hi16(@LC706)
	or.u	 r5,r0,hi16(@LC707)
	or	 r2,r2,lo16(@LC705)
	or	 r3,r3,lo16(@LC706)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC707)
	align	 4
@L8150:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8153
	extu	 r8,r9,8<8>
@L8686:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r10,r8,8<8>
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8155
	or	 r7,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r12,r7[r13]
	br	 @L8156
	align	 4
@L8155:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r12,r13[r12]
@L8156:
	fsub.sds r13,r12,r0
	br.n	 @L1739
	st	 r13,r7[r10]
	align	 4
@L8153:
	extu	 r10,r11,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r11,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8157
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r12,r13[r12]
	br	 @L8158
	align	 4
@L8157:
	extu	 r13,r9,0<25>
	ld.d	 r12,r11[r13]
@L8158:
	fsub.sds r13,r12,r0
	br.n	 @L1739
	st	 r13,r11[r8]
	align	 4
@L8159:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8161
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8161
	or.u	 r2,r0,hi16(@LC708)
	or	 r4,r0,810
	or.u	 r3,r0,hi16(@LC709)
	or.u	 r5,r0,hi16(@LC710)
	or	 r2,r2,lo16(@LC708)
	or	 r3,r3,lo16(@LC709)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC710)
	align	 4
@L8161:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	bb0.n	 (31-31),r9,@L8165
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9764
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC711)
	or	 r4,r0,810
	or.u	 r3,r0,hi16(@LC712)
	or.u	 r5,r0,hi16(@LC713)
	or	 r2,r2,lo16(@LC711)
	or	 r3,r3,lo16(@LC712)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC713)
	align	 4
@L8165:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8168
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9764:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r12,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8170
	extu	 r8,r8,8<8>
	ld	 r13,r12[r7]
	br	 @L9253
	align	 4
@L8170:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r13,r13[r7]
@L9253:
	flt.ss	 r13,r13
	br.n	 @L1739
	st	 r13,r12[r8]
	align	 4
@L8168:
	extu	 r8,r12,8<8>
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8172
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r13,r13[r9]
	br	 @L9254
	align	 4
@L8172:
	ld	 r13,r11[r9]
@L9254:
	flt.ss	 r13,r13
	br.n	 @L1739
	st	 r13,r11[r8]
	align	 4
@L8174:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8176
	st	 r12,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8176
	or.u	 r2,r0,hi16(@LC714)
	or	 r4,r0,815
	or.u	 r3,r0,hi16(@LC715)
	or.u	 r5,r0,hi16(@LC716)
	or	 r2,r2,lo16(@LC714)
	or	 r3,r3,lo16(@LC715)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC716)
	align	 4
@L8176:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	bb0.n	 (31-31),r9,@L8180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9765
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC717)
	or	 r4,r0,815
	or.u	 r3,r0,hi16(@LC718)
	or.u	 r5,r0,hi16(@LC719)
	or	 r2,r2,lo16(@LC717)
	or	 r3,r3,lo16(@LC718)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC719)
	align	 4
@L8180:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8183
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9765:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r6,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8185
	extu	 r10,r8,7<9>
	ld	 r8,r6[r7]
	br	 @L8186
	align	 4
@L8185:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r8,r13[r7]
@L8186:
	fsub.dss r12,r8,r0
	br.n	 @L1739
	st.d	 r12,r6[r10]
	align	 4
@L8183:
	extu	 r8,r12,7<9>
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8187
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r10,r13[r9]
	br	 @L8188
	align	 4
@L8187:
	ld	 r10,r11[r9]
@L8188:
	fsub.dss r12,r10,r0
	br.n	 @L1739
	st.d	 r12,r11[r8]
	align	 4
@L8189:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8191
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8191
	or.u	 r2,r0,hi16(@LC720)
	or	 r4,r0,820
	or.u	 r3,r0,hi16(@LC721)
	or.u	 r5,r0,hi16(@LC722)
	or	 r2,r2,lo16(@LC720)
	or	 r3,r3,lo16(@LC721)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC722)
	align	 4
@L8191:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	bb0.n	 (31-31),r9,@L8195
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9766
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC723)
	or	 r4,r0,820
	or.u	 r3,r0,hi16(@LC724)
	or.u	 r5,r0,hi16(@LC725)
	or	 r2,r2,lo16(@LC723)
	or	 r3,r3,lo16(@LC724)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC725)
	align	 4
@L8195:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8198
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9766:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r12,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8200
	extu	 r8,r8,7<9>
	ld	 r13,r12[r7]
	br	 @L9255
	align	 4
@L8200:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r13,r13[r7]
@L9255:
	flt.ds	 r10,r13
	br.n	 @L1739
	st.d	 r10,r12[r8]
	align	 4
@L8198:
	extu	 r8,r12,7<9>
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8202
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r13,r13[r9]
	br	 @L9256
	align	 4
@L8202:
	ld	 r13,r11[r9]
@L9256:
	flt.ds	 r12,r13
	br.n	 @L1739
	st.d	 r12,r11[r8]
	align	 4
@L8204:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8206
	st	 r12,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8206
	or.u	 r2,r0,hi16(@LC726)
	or	 r4,r0,825
	or.u	 r3,r0,hi16(@LC727)
	or.u	 r5,r0,hi16(@LC728)
	or	 r2,r2,lo16(@LC726)
	or	 r3,r3,lo16(@LC727)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC728)
	align	 4
@L8206:
	ld	 r12,r31,92
	extu	 r9,r12,0<24>
	bb0.n	 (31-31),r9,@L8210
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9767
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC729)
	or	 r4,r0,825
	or.u	 r3,r0,hi16(@LC730)
	or.u	 r5,r0,hi16(@LC731)
	or	 r2,r2,lo16(@LC729)
	or	 r3,r3,lo16(@LC730)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC731)
	align	 4
@L8210:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8213
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9767:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r7,r8,0<24>
	extu	 r13,r7,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r7,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r12,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8215
	extu	 r8,r8,8<8>
	ld	 r13,r12[r7]
	br	 @L9258
	align	 4
@L8215:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r13,r13[r7]
@L9258:
	trnc.ss	 r13,r13
	br.n	 @L1739
	st	 r13,r12[r8]
	align	 4
@L8213:
	extu	 r8,r12,8<8>
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8217
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r13,r13[r9]
	br	 @L9259
	align	 4
@L8217:
	ld	 r13,r11[r9]
@L9259:
	trnc.ss	 r13,r13
	br.n	 @L1739
	st	 r13,r11[r8]
	align	 4
@L8219:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8221
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8221
	or.u	 r2,r0,hi16(@LC732)
	or	 r4,r0,830
	or.u	 r3,r0,hi16(@LC733)
	or.u	 r5,r0,hi16(@LC734)
	or	 r2,r2,lo16(@LC732)
	or	 r3,r3,lo16(@LC733)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC734)
	align	 4
@L8221:
	ld	 r9,r31,92
	extu	 r11,r9,0<24>
	bb0.n	 (31-31),r11,@L8225
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8691
	or.u	 r2,r0,hi16(@LC735)
	or	 r4,r0,830
	or.u	 r3,r0,hi16(@LC736)
	or.u	 r5,r0,hi16(@LC737)
	or	 r2,r2,lo16(@LC735)
	or	 r3,r3,lo16(@LC736)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC737)
	align	 4
@L8225:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8228
	extu	 r8,r9,8<8>
@L8691:
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r12,r8,0<24>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r11,r10[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r10,r8,8<8>
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8230
	or	 r7,r13,lo16(_spec_regs_F)
	extu	 r13,r8,0<25>
	ld.d	 r12,r7[r13]
	br	 @L9260
	align	 4
@L8230:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r8,0<25>
	or	 r13,r13,lo16(_regs_F)
	ld.d	 r12,r13[r12]
@L9260:
	trnc.sd	 r12,r12
	br.n	 @L1739
	st	 r12,r7[r10]
	align	 4
@L8228:
	extu	 r10,r11,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r11,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8232
	or	 r11,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_spec_regs_F)
	ld.d	 r12,r13[r12]
	br	 @L9261
	align	 4
@L8232:
	extu	 r13,r9,0<25>
	ld.d	 r12,r11[r13]
@L9261:
	trnc.sd	 r12,r12
	br.n	 @L1739
	st	 r12,r11[r8]
	align	 4
@L8234:
	ld	 r13,r31,92
	or	 r14,r0,66
	st	 r14,r31,164
	st	 r0,r31,172
	extu	 r11,r13,0<24>
	st	 r0,r31,188
	extu	 r13,r13,8<16>
	addu	 r12,r11,32
	addu	 r13,r13,32
	and	 r12,r12,0xfffe
	and	 r17,r13,0xfffe
	bb0.n	 (31-31),r11,@L8236
	st	 r12,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8236
	or.u	 r2,r0,hi16(@LC738)
	or	 r4,r0,836
	or.u	 r3,r0,hi16(@LC739)
	or.u	 r5,r0,hi16(@LC740)
	or	 r2,r2,lo16(@LC738)
	or	 r3,r3,lo16(@LC739)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC740)
	align	 4
@L8236:
	ld	 r10,r31,92
	bb0.n	 (31-15),r10,@L8240
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8692
	or.u	 r2,r0,hi16(@LC741)
	or	 r4,r0,836
	or.u	 r3,r0,hi16(@LC742)
	or.u	 r5,r0,hi16(@LC743)
	or	 r2,r2,lo16(@LC741)
	or	 r3,r3,lo16(@LC742)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC743)
	align	 4
@L8240:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8243
	extu	 r9,r10,0<24>
@L8692:
	ld.bu	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
	or	 r10,r0,1
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r11,r9,0<5>
	mask	 r12,r9,30
	ld	 r13,r13[r11]
	mak	 r12,r10,r12
	or.u	 r11,r0,hi16(_use_spec_FCC)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8245
	st	 r10,r11,lo16(_use_spec_FCC)
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9262
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8245:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9262:
	ld	 r8,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8247
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9263
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8247:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9263:
	ld	 r13,r13[r12]
	fcmp.sss r13,r8,r13
	br.n	 @L8343
	extu	 r13,r13,1<eq>
	align	 4
@L8243:
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8249
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9264
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8249:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9264:
	ld	 r8,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8251
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9265
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8251:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9265:
	ld	 r13,r13[r12]
	fcmp.sss r13,r8,r13
	br.n	 @L8347
	extu	 r13,r13,1<eq>
	align	 4
@L8253:
	ld	 r13,r31,92
	or	 r14,r0,66
	st	 r14,r31,164
	st	 r0,r31,172
	extu	 r11,r13,0<24>
	st	 r0,r31,188
	extu	 r13,r13,8<16>
	addu	 r12,r11,32
	addu	 r13,r13,32
	and	 r12,r12,0xfffe
	and	 r17,r13,0xfffe
	bb0.n	 (31-31),r11,@L8255
	st	 r12,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8255
	or.u	 r2,r0,hi16(@LC744)
	or	 r4,r0,841
	or.u	 r3,r0,hi16(@LC745)
	or.u	 r5,r0,hi16(@LC746)
	or	 r2,r2,lo16(@LC744)
	or	 r3,r3,lo16(@LC745)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC746)
	align	 4
@L8255:
	ld	 r9,r31,92
	bb0.n	 (31-15),r9,@L8259
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9768
	or.u	 r13,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC747)
	or	 r4,r0,841
	or.u	 r3,r0,hi16(@LC748)
	or.u	 r5,r0,hi16(@LC749)
	or	 r2,r2,lo16(@LC747)
	or	 r3,r3,lo16(@LC748)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC749)
	align	 4
@L8259:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8262
	ld	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
@L9768:
	or	 r10,r0,1
	extu	 r12,r9,0<24>
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r11,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r13[r11]
	mak	 r12,r10,r12
	or.u	 r11,r0,hi16(_use_spec_FCC)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8264
	st	 r10,r11,lo16(_use_spec_FCC)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9266
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8264:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9266:
	ld.d	 r8,r13[r12]
	ld.hu	 r7,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r7,3<5>
	mask	 r11,r7,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8266
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r7,7<1>
	br.n	 @L9267
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8266:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r7,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9267:
	ld.d	 r12,r13[r12]
	fcmp.sdd r13,r8,r12
	br.n	 @L8343
	extu	 r13,r13,1<eq>
	align	 4
@L8262:
	extu	 r11,r9,0<24>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8268
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9268
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8268:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9268:
	ld.d	 r8,r13[r12]
	ld.hu	 r7,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r7,3<5>
	mask	 r11,r7,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8270
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r7,7<1>
	br.n	 @L9269
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8270:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r7,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9269:
	ld.d	 r12,r13[r12]
	fcmp.sdd r13,r8,r12
	br.n	 @L8347
	extu	 r13,r13,1<eq>
	align	 4
@L8272:
	ld	 r13,r31,92
	or	 r14,r0,66
	st	 r14,r31,164
	st	 r0,r31,172
	extu	 r11,r13,0<24>
	st	 r0,r31,188
	extu	 r13,r13,8<16>
	addu	 r12,r11,32
	addu	 r13,r13,32
	and	 r12,r12,0xfffe
	and	 r17,r13,0xfffe
	bb0.n	 (31-31),r11,@L8274
	st	 r12,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8274
	or.u	 r2,r0,hi16(@LC750)
	or	 r4,r0,846
	or.u	 r3,r0,hi16(@LC751)
	or.u	 r5,r0,hi16(@LC752)
	or	 r2,r2,lo16(@LC750)
	or	 r3,r3,lo16(@LC751)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC752)
	align	 4
@L8274:
	ld	 r10,r31,92
	bb0.n	 (31-15),r10,@L8278
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8694
	or.u	 r2,r0,hi16(@LC753)
	or	 r4,r0,846
	or.u	 r3,r0,hi16(@LC754)
	or.u	 r5,r0,hi16(@LC755)
	or	 r2,r2,lo16(@LC753)
	or	 r3,r3,lo16(@LC754)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC755)
	align	 4
@L8278:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8281
	extu	 r9,r10,0<24>
@L8694:
	ld.bu	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
	or	 r10,r0,1
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r11,r9,0<5>
	mask	 r12,r9,30
	ld	 r13,r13[r11]
	mak	 r12,r10,r12
	or.u	 r11,r0,hi16(_use_spec_FCC)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8283
	st	 r10,r11,lo16(_use_spec_FCC)
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9270
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8283:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9270:
	ld	 r8,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8285
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9271
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8285:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9271:
	ld	 r13,r13[r12]
	fcmp.sss r13,r8,r13
	br.n	 @L8343
	extu	 r13,r13,1<lt>
	align	 4
@L8281:
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8287
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9272
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8287:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9272:
	ld	 r8,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8289
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9273
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8289:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9273:
	ld	 r13,r13[r12]
	fcmp.sss r13,r8,r13
	br.n	 @L8347
	extu	 r13,r13,1<lt>
	align	 4
@L8291:
	ld	 r13,r31,92
	or	 r14,r0,66
	st	 r14,r31,164
	st	 r0,r31,172
	extu	 r11,r13,0<24>
	st	 r0,r31,188
	extu	 r13,r13,8<16>
	addu	 r12,r11,32
	addu	 r13,r13,32
	and	 r12,r12,0xfffe
	and	 r17,r13,0xfffe
	bb0.n	 (31-31),r11,@L8293
	st	 r12,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8293
	or.u	 r2,r0,hi16(@LC756)
	or	 r4,r0,851
	or.u	 r3,r0,hi16(@LC757)
	or.u	 r5,r0,hi16(@LC758)
	or	 r2,r2,lo16(@LC756)
	or	 r3,r3,lo16(@LC757)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC758)
	align	 4
@L8293:
	ld	 r9,r31,92
	bb0.n	 (31-15),r9,@L8297
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9769
	or.u	 r13,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC759)
	or	 r4,r0,851
	or.u	 r3,r0,hi16(@LC760)
	or.u	 r5,r0,hi16(@LC761)
	or	 r2,r2,lo16(@LC759)
	or	 r3,r3,lo16(@LC760)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC761)
	align	 4
@L8297:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8300
	ld	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
@L9769:
	or	 r10,r0,1
	extu	 r12,r9,0<24>
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r11,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r13[r11]
	mak	 r12,r10,r12
	or.u	 r11,r0,hi16(_use_spec_FCC)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8302
	st	 r10,r11,lo16(_use_spec_FCC)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9274
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8302:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9274:
	ld.d	 r8,r13[r12]
	ld.hu	 r7,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r7,3<5>
	mask	 r11,r7,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8304
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r7,7<1>
	br.n	 @L9275
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8304:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r7,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9275:
	ld.d	 r12,r13[r12]
	fcmp.sdd r13,r8,r12
	br.n	 @L8343
	extu	 r13,r13,1<lt>
	align	 4
@L8300:
	extu	 r11,r9,0<24>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8306
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9276
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8306:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9276:
	ld.d	 r8,r13[r12]
	ld.hu	 r7,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r7,3<5>
	mask	 r11,r7,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L8308
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r7,7<1>
	br.n	 @L9277
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8308:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r7,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9277:
	ld.d	 r12,r13[r12]
	fcmp.sdd r13,r8,r12
	br.n	 @L8347
	extu	 r13,r13,1<lt>
	align	 4
@L8310:
	ld	 r13,r31,92
	or	 r14,r0,66
	st	 r14,r31,164
	st	 r0,r31,172
	extu	 r11,r13,0<24>
	st	 r0,r31,188
	extu	 r13,r13,8<16>
	addu	 r12,r11,32
	addu	 r13,r13,32
	and	 r12,r12,0xfffe
	and	 r17,r13,0xfffe
	bb0.n	 (31-31),r11,@L8312
	st	 r12,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8312
	or.u	 r2,r0,hi16(@LC762)
	or	 r4,r0,856
	or.u	 r3,r0,hi16(@LC763)
	or.u	 r5,r0,hi16(@LC764)
	or	 r2,r2,lo16(@LC762)
	or	 r3,r3,lo16(@LC763)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC764)
	align	 4
@L8312:
	ld	 r10,r31,92
	bb0.n	 (31-15),r10,@L8316
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8696
	or.u	 r2,r0,hi16(@LC765)
	or	 r4,r0,856
	or.u	 r3,r0,hi16(@LC766)
	or.u	 r5,r0,hi16(@LC767)
	or	 r2,r2,lo16(@LC765)
	or	 r3,r3,lo16(@LC766)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC767)
	align	 4
@L8316:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8319
	extu	 r9,r10,0<24>
@L8696:
	ld.bu	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
	or	 r10,r0,1
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r11,r9,0<5>
	mask	 r12,r9,30
	ld	 r13,r13[r11]
	mak	 r12,r10,r12
	or.u	 r11,r0,hi16(_use_spec_FCC)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8321
	st	 r10,r11,lo16(_use_spec_FCC)
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9278
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8321:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9278:
	ld	 r8,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8323
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9279
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8323:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9279:
	ld	 r13,r13[r12]
	fcmp.sss r13,r8,r13
	br.n	 @L8343
	extu	 r13,r13,1<le>
	align	 4
@L8319:
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r9,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8325
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9281
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8325:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9281:
	ld	 r8,r13[r9]
	ld.hu	 r9,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8327
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L9282
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8327:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L9282:
	ld	 r13,r13[r12]
	fcmp.sss r13,r8,r13
	br.n	 @L8347
	extu	 r13,r13,1<le>
	align	 4
@L8329:
	ld	 r13,r31,92
	or	 r14,r0,66
	st	 r14,r31,164
	st	 r0,r31,172
	extu	 r11,r13,0<24>
	st	 r0,r31,188
	extu	 r13,r13,8<16>
	addu	 r12,r11,32
	addu	 r13,r13,32
	and	 r12,r12,0xfffe
	and	 r17,r13,0xfffe
	bb0.n	 (31-31),r11,@L8331
	st	 r12,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8331
	or.u	 r2,r0,hi16(@LC768)
	or	 r4,r0,861
	or.u	 r3,r0,hi16(@LC769)
	or.u	 r5,r0,hi16(@LC770)
	or	 r2,r2,lo16(@LC768)
	or	 r3,r3,lo16(@LC769)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC770)
	align	 4
@L8331:
	ld	 r9,r31,92
	bb0.n	 (31-15),r9,@L8335
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9770
	or.u	 r13,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC771)
	or	 r4,r0,861
	or.u	 r3,r0,hi16(@LC772)
	or.u	 r5,r0,hi16(@LC773)
	or	 r2,r2,lo16(@LC771)
	or	 r3,r3,lo16(@LC772)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC773)
	align	 4
@L8335:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8338
	ld	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
@L9770:
	or	 r10,r0,1
	extu	 r12,r9,0<24>
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r11,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r13[r11]
	mak	 r12,r10,r12
	or.u	 r11,r0,hi16(_use_spec_FCC)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8340
	st	 r10,r11,lo16(_use_spec_FCC)
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9284
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8340:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9284:
	ld.d	 r8,r13[r12]
	ld.hu	 r7,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r7,3<5>
	mask	 r11,r7,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8342
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r7,7<1>
	br.n	 @L9285
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8342:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r7,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9285:
	ld.d	 r12,r13[r12]
	fcmp.sdd r13,r8,r12
	extu	 r13,r13,1<le>
@L8343:
	or.u	 r14,r0,hi16(_spec_regs_FCC)
	or	 r14,r14,lo16(_spec_regs_FCC)
	br.n	 @L1739
	st	 r13,r0,r14
	align	 4
@L8338:
	extu	 r11,r9,0<24>
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	or	 r12,r12,lo16(_use_spec_F)
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8344
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9286
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8344:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9286:
	ld.d	 r8,r13[r12]
	ld.hu	 r7,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r7,3<5>
	mask	 r11,r7,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8346
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r7,7<1>
	br.n	 @L9287
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8346:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r7,7<1>
	or	 r13,r13,lo16(_regs_F)
@L9287:
	ld.d	 r12,r13[r12]
	fcmp.sdd r13,r8,r12
	extu	 r13,r13,1<le>
@L8347:
	or.u	 r15,r0,hi16(_regs_FCC)
	or	 r15,r15,lo16(_regs_FCC)
	br.n	 @L1739
	st	 r13,r0,r15
	align	 4
@L8348:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8350
	st	 r12,r31,164
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8350
	or.u	 r2,r0,hi16(@LC774)
	or	 r4,r0,867
	or.u	 r3,r0,hi16(@LC775)
	or.u	 r5,r0,hi16(@LC776)
	or	 r2,r2,lo16(@LC774)
	or	 r3,r3,lo16(@LC775)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC776)
	align	 4
@L8350:
	ld	 r12,r31,92
	extu	 r2,r12,0<24>
	bb0.n	 (31-31),r2,@L8354
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L9771
	or.u	 r10,r0,hi16(_use_spec_F)
	or.u	 r2,r0,hi16(@LC777)
	or	 r4,r0,867
	or.u	 r3,r0,hi16(@LC778)
	or.u	 r5,r0,hi16(@LC779)
	or	 r2,r2,lo16(@LC777)
	or	 r3,r3,lo16(@LC778)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC779)
	align	 4
@L8354:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8357
	ld	 r12,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
@L9771:
	extu	 r12,r12,0<8>
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r8,r31,92
	extu	 r2,r8,0<24>
	extu	 r13,r2,0<5>
	ld	 r11,r10[r13]
	mask	 r13,r2,30
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r23,r12,lo16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8359
	extu	 r25,r8,8<8>
	ld	 r2,r23[r2]
	br	 @L8360
	align	 4
@L8359:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
	ld	 r2,r13[r2]
@L8360:
	bsr.n	 _sqrt
	fsub.dss r2,r2,r0
	fsub.sds r13,r2,r0
	br.n	 @L1739
	st	 r13,r23[r25]
	align	 4
@L8357:
	extu	 r23,r12,8<8>
	extu	 r10,r2,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r2,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8361
	or	 r25,r11,lo16(_regs_F)
	or.u	 r13,r0,hi16(_spec_regs_F)
	or	 r13,r13,lo16(_spec_regs_F)
	ld	 r2,r13[r2]
	br	 @L8362
	align	 4
@L8361:
	ld	 r2,r25[r2]
@L8362:
	bsr.n	 _sqrt
	fsub.dss r2,r2,r0
	fsub.sds r13,r2,r0
	br.n	 @L1739
	st	 r13,r25[r23]
	align	 4
@L8363:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<8>
	st	 r0,r31,188
	extu	 r13,r13,0<24>
	mask	 r12,r11,255
	addu	 r13,r13,32
	addu	 r12,r12,32
	and	 r13,r13,0xfffe
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8365
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8365
	or.u	 r2,r0,hi16(@LC780)
	or	 r4,r0,872
	or.u	 r3,r0,hi16(@LC781)
	or.u	 r5,r0,hi16(@LC782)
	or	 r2,r2,lo16(@LC780)
	or	 r3,r3,lo16(@LC781)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC782)
	align	 4
@L8365:
	ld	 r9,r31,92
	extu	 r11,r9,0<24>
	bb0.n	 (31-31),r11,@L8369
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8699
	or.u	 r2,r0,hi16(@LC783)
	or	 r4,r0,872
	or.u	 r3,r0,hi16(@LC784)
	or.u	 r5,r0,hi16(@LC785)
	or	 r2,r2,lo16(@LC783)
	or	 r3,r3,lo16(@LC784)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC785)
	align	 4
@L8369:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8372
	extu	 r10,r11,0<5>
@L8699:
	ld	 r12,r31,92
	or.u	 r9,r0,hi16(_use_spec_F)
	extu	 r12,r12,0<8>
	or	 r9,r9,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r9[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r9[r12]
	ld	 r11,r31,92
	extu	 r12,r11,0<24>
	extu	 r13,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r9[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd	 eq0,r13,@L8374
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r11,0<25>
	br.n	 @L9288
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8374:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r11,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9288:
	bsr.n	 _sqrt
	ld.d	 r2,r13[r12]
	or.u	 r13,r0,hi16(_spec_regs_F)
	ld.bu	 r12,r31,94
	br.n	 @L9289
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8372:
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r11,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8376
	or.u	 r13,r0,hi16(_spec_regs_F)
	extu	 r12,r9,0<25>
	br.n	 @L9290
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8376:
	or.u	 r13,r0,hi16(_regs_F)
	extu	 r12,r9,0<25>
	or	 r13,r13,lo16(_regs_F)
@L9290:
	bsr.n	 _sqrt
	ld.d	 r2,r13[r12]
	or.u	 r13,r0,hi16(_regs_F)
	ld.bu	 r12,r31,94
	or	 r13,r13,lo16(_regs_F)
@L9289:
	extu	 r12,r12,0<1>
	br.n	 @L1739
	st.d	 r2,r13[r12]
	align	 4
@L8378:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	st	 r0,r31,164
	st	 r0,r31,172
	st	 r0,r31,180
	or	 r17,r0,0
	bcnd.n	 eq0,r13,@L8380
	st	 r0,r31,188
	or.u	 r2,r0,hi16(@LC786)
	or	 r4,r0,883
	or.u	 r3,r0,hi16(@LC787)
	or.u	 r5,r0,hi16(@LC788)
	or	 r2,r2,lo16(@LC786)
	or	 r3,r3,lo16(@LC787)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC788)
	align	 4
@L8380:
	ld	 r13,r31,88
	ld	 r12,r31,92
	or.u	 r2,r0,hi16(_mem_access)
	st	 r13,r31,4
	or	 r2,r2,lo16(_mem_access)
	st	 r12,r31,8
	bsr.n	 _ss_syscall
	addu	 r1,r1,@L9780
@L9781:
	align	 4
@L8382:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r31,92
	ld	 r13,r15,lo16(_spec_mode)
	st	 r0,r31,172
	or	 r17,r0,0
	st	 r0,r31,180
	extu	 r10,r12,0<16>
	st	 r0,r31,188
	mask	 r14,r10,255
	bcnd.n	 eq0,r13,@L8383
	st	 r14,r31,164
	mask	 r10,r10,31
	extu	 r11,r14,0<5>
	or	 r13,r0,1
	ld	 r12,r18[r11]
	mak	 r13,r13,r10
	or	 r12,r12,r13
	st	 r12,r18[r11]
	ld	 r13,r31,92
	or.u	 r15,r0,hi16(_spec_regs_R)
	extu	 r12,r13,8<16>
	or	 r15,r15,lo16(_spec_regs_R)
	mak	 r13,r13,0<16>
	br.n	 @L1739
	st	 r13,r15[r12]
	align	 4
@L8383:
	or.u	 r15,r0,hi16(_regs_R)
	ld	 r14,r31,164
	or	 r15,r15,lo16(_regs_R)
	mak	 r13,r12,0<16>
	br.n	 @L1739
	st	 r13,r15[r14]
	align	 4
@L8385:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	or.u	 r14,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r9,r13,0<24>
	ld	 r12,r14,lo16(_spec_mode)
	extu	 r11,r13,0<16>
	addu	 r13,r9,32
	mask	 r15,r11,255
	st	 r15,r31,164
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r12,@L8386
	st	 r13,r31,180
	mask	 r12,r11,31
	extu	 r11,r15,0<5>
	or	 r10,r0,1
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
	extu	 r8,r9,0<24>
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r12,r8,0<5>
	mask	 r11,r8,30
	ld	 r13,r13[r12]
	mak	 r10,r10,r11
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L8388
	extu	 r12,r9,8<16>
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9291
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8388:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9291:
	ld	 r9,r13[r8]
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L1739
	st	 r9,r14[r12]
	align	 4
@L8386:
	extu	 r10,r9,0<5>
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	ld	 r11,r31,164
	bcnd.n	 eq0,r12,@L8390
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9292
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8390:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9292:
	ld	 r10,r13[r9]
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	br.n	 @L1739
	st	 r10,r15[r11]
	align	 4
@L8392:
	ld	 r12,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r13,r12,0<24>
	st	 r0,r31,188
	extu	 r12,r12,0<16>
	addu	 r13,r13,32
	mask	 r14,r12,254
	st	 r14,r31,164
	and	 r13,r13,0xfffe
	bb0.n	 (31-31),r12,@L8394
	st	 r13,r31,180
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8394
	or.u	 r2,r0,hi16(@LC789)
	or	 r4,r0,908
	or.u	 r3,r0,hi16(@LC790)
	or.u	 r5,r0,hi16(@LC791)
	or	 r2,r2,lo16(@LC789)
	or	 r3,r3,lo16(@LC790)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC791)
	align	 4
@L8394:
	ld	 r8,r31,92
	extu	 r9,r8,0<24>
	bb0.n	 (31-31),r9,@L8398
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8700
	or.u	 r2,r0,hi16(@LC792)
	or	 r4,r0,908
	or.u	 r3,r0,hi16(@LC793)
	or.u	 r5,r0,hi16(@LC794)
	or	 r2,r2,lo16(@LC792)
	or	 r3,r3,lo16(@LC793)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC794)
	align	 4
@L8398:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8401
	extu	 r10,r9,0<5>
@L8700:
	ld.hu	 r12,r31,92
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r9,r31,92
	or.u	 r13,r0,hi16(_use_spec_F)
	extu	 r8,r9,0<24>
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r12,r8,0<5>
	mask	 r11,r8,30
	ld	 r13,r13[r12]
	mak	 r10,r10,r11
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L8403
	extu	 r12,r9,8<16>
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9293
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8403:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9293:
	ld	 r9,r13[r8]
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	br.n	 @L8402
	st	 r9,r14[r12]
	align	 4
@L8401:
	or.u	 r12,r0,hi16(_use_spec_F)
	mask	 r11,r9,30
	or	 r12,r12,lo16(_use_spec_F)
	or	 r13,r0,1
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8405
	extu	 r11,r8,8<16>
	or.u	 r13,r0,hi16(_spec_regs_F)
	br.n	 @L9294
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L8405:
	or.u	 r13,r0,hi16(_regs_F)
	or	 r13,r13,lo16(_regs_F)
@L9294:
	ld	 r10,r13[r9]
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	st	 r10,r15[r11]
@L8402:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd	 eq0,r13,@L8407
	or	 r10,r0,1
	ld.bu	 r13,r31,93
	addu	 r13,r13,1
	extu	 r11,r13,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r11]
	mak	 r13,r10,r13
	or	 r12,r12,r13
	st	 r12,r18[r11]
	ld	 r13,r31,92
	extu	 r9,r13,0<24>
	extu	 r8,r13,8<16>
	or.u	 r13,r0,hi16(_use_spec_F)
	addu	 r12,r9,1
	or	 r13,r13,lo16(_use_spec_F)
	extu	 r11,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r13[r11]
	mak	 r10,r10,r12
	or.u	 r12,r0,hi16(_spec_regs_R+4)
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L8409
	or	 r11,r12,lo16(_spec_regs_R+4)
	or.u	 r13,r0,hi16(_spec_regs_F+4)
	br.n	 @L9295
	or	 r13,r13,lo16(_spec_regs_F+4)
	align	 4
@L8409:
	or.u	 r13,r0,hi16(_regs_F+4)
	or	 r13,r13,lo16(_regs_F+4)
@L9295:
	ld	 r12,r13[r9]
	br.n	 @L1739
	st	 r12,r11[r8]
	align	 4
@L8407:
	ld	 r13,r31,92
	or.u	 r12,r0,hi16(_use_spec_F)
	extu	 r9,r13,0<24>
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r8,r13,8<16>
	addu	 r11,r9,1
	or	 r13,r0,1
	extu	 r10,r11,0<5>
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_R+4)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8411
	or	 r10,r11,lo16(_regs_R+4)
	or.u	 r13,r0,hi16(_spec_regs_F+4)
	br.n	 @L9296
	or	 r13,r13,lo16(_spec_regs_F+4)
	align	 4
@L8411:
	or.u	 r13,r0,hi16(_regs_F+4)
	or	 r13,r13,lo16(_regs_F+4)
@L9296:
	ld	 r11,r13[r9]
	br.n	 @L1739
	st	 r11,r10[r8]
	align	 4
@L8414:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	or.u	 r15,r0,hi16(_spec_mode)
	st	 r0,r31,188
	extu	 r10,r13,0<24>
	ld	 r12,r15,lo16(_spec_mode)
	extu	 r11,r13,0<16>
	addu	 r13,r10,32
	mask	 r14,r11,255
	st	 r14,r31,180
	and	 r13,r13,0xfffe
	bcnd.n	 eq0,r12,@L8415
	st	 r13,r31,164
	or.u	 r11,r0,hi16(_use_spec_F)
	mask	 r12,r10,30
	extu	 r10,r10,0<5>
	or	 r11,r11,lo16(_use_spec_F)
	or	 r9,r0,1
	ld	 r13,r11[r10]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r11[r10]
	ld	 r10,r31,92
	extu	 r12,r10,0<16>
	extu	 r10,r10,0<24>
	mask	 r8,r12,255
	extu	 r13,r8,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8417
	or	 r13,r13,lo16(_spec_regs_F)
	or.u	 r15,r0,hi16(_spec_regs_R)
	or	 r15,r15,lo16(_spec_regs_R)
	ld	 r12,r15[r8]
	br.n	 @L1739
	st	 r12,r13[r10]
	align	 4
@L8417:
	or.u	 r14,r0,hi16(_regs_R)
	or	 r14,r14,lo16(_regs_R)
	ld	 r12,r14[r8]
	br.n	 @L1739
	st	 r12,r13[r10]
	align	 4
@L8415:
	ld	 r15,r31,180
	mask	 r11,r11,31
	extu	 r12,r15,0<5>
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8419
	or	 r11,r11,lo16(_regs_F)
	or.u	 r14,r0,hi16(_spec_regs_R)
	br.n	 @L9297
	or	 r14,r14,lo16(_spec_regs_R)
	align	 4
@L8419:
	or.u	 r14,r0,hi16(_regs_R)
	ld	 r15,r31,180
	or	 r14,r14,lo16(_regs_R)
@L9297:
	ld	 r13,r14[r15]
	br.n	 @L1739
	st	 r13,r11[r10]
	align	 4
@L8421:
	ld	 r13,r31,92
	or	 r17,r0,0
	st	 r0,r31,172
	extu	 r11,r13,0<24>
	st	 r0,r31,188
	extu	 r13,r13,0<16>
	addu	 r12,r11,32
	mask	 r13,r13,254
	st	 r13,r31,180
	and	 r12,r12,0xfffe
	bb0.n	 (31-31),r11,@L8423
	st	 r12,r31,164
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8423
	or.u	 r2,r0,hi16(@LC795)
	or	 r4,r0,924
	or.u	 r3,r0,hi16(@LC796)
	or.u	 r5,r0,hi16(@LC797)
	or	 r2,r2,lo16(@LC795)
	or	 r3,r3,lo16(@LC796)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC797)
	align	 4
@L8423:
	ld	 r12,r31,92
	extu	 r11,r12,0<16>
	bb0.n	 (31-31),r11,@L8427
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8701
	or.u	 r2,r0,hi16(@LC798)
	or	 r4,r0,924
	or.u	 r3,r0,hi16(@LC799)
	or.u	 r5,r0,hi16(@LC800)
	or	 r2,r2,lo16(@LC798)
	or	 r3,r3,lo16(@LC799)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC800)
	align	 4
@L8427:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r13,r15,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8430
	extu	 r9,r12,0<24>
@L8701:
	ld.bu	 r11,r31,92
	or.u	 r10,r0,hi16(_use_spec_F)
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r12,r11,30
	extu	 r11,r11,0<5>
	or	 r9,r0,1
	ld	 r13,r10[r11]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r10[r11]
	ld	 r10,r31,92
	extu	 r12,r10,0<16>
	extu	 r10,r10,0<24>
	mask	 r8,r12,255
	extu	 r13,r8,0<5>
	mask	 r12,r12,31
	ld	 r11,r18[r13]
	mak	 r9,r9,r12
	or.u	 r13,r0,hi16(_spec_regs_F)
	and	 r11,r11,r9
	bcnd.n	 eq0,r11,@L8432
	or	 r13,r13,lo16(_spec_regs_F)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r12,r14[r8]
	br.n	 @L8431
	st	 r12,r13[r10]
	align	 4
@L8432:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r12,r15[r8]
	br.n	 @L8431
	st	 r12,r13[r10]
	align	 4
@L8430:
	mask	 r10,r11,255
	mask	 r11,r11,31
	extu	 r12,r10,0<5>
	or	 r13,r0,1
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	or.u	 r11,r0,hi16(_regs_F)
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8434
	or	 r11,r11,lo16(_regs_F)
	or.u	 r14,r0,hi16(_spec_regs_R)
	or	 r14,r14,lo16(_spec_regs_R)
	ld	 r12,r14[r10]
	br.n	 @L8431
	st	 r12,r11[r9]
	align	 4
@L8434:
	or.u	 r15,r0,hi16(_regs_R)
	or	 r15,r15,lo16(_regs_R)
	ld	 r12,r15[r10]
	st	 r12,r11[r9]
@L8431:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L8436
	or.u	 r11,r0,hi16(_use_spec_F)
	ld.bu	 r13,r31,92
	or	 r10,r0,1
	addu	 r13,r13,1
	or	 r11,r11,lo16(_use_spec_F)
	extu	 r9,r13,0<5>
	mask	 r13,r13,30
	ld	 r12,r11[r9]
	mak	 r13,r10,r13
	or	 r12,r12,r13
	st	 r12,r11[r9]
	ld	 r13,r31,92
	extu	 r11,r13,8<16>
	extu	 r9,r13,0<24>
	addu	 r13,r11,1
	extu	 r12,r13,0<5>
	mask	 r13,r13,31
	ld	 r12,r18[r12]
	mak	 r10,r10,r13
	or.u	 r13,r0,hi16(_spec_regs_F+4)
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L8438
	or	 r10,r13,lo16(_spec_regs_F+4)
	or.u	 r13,r0,hi16(_spec_regs_R+4)
	br.n	 @L9298
	or	 r13,r13,lo16(_spec_regs_R+4)
	align	 4
@L8438:
	or.u	 r13,r0,hi16(_regs_R+4)
	or	 r13,r13,lo16(_regs_R+4)
@L9298:
	ld	 r13,r13[r11]
	br.n	 @L1739
	st	 r13,r10[r9]
	align	 4
@L8436:
	ld	 r9,r31,92
	extu	 r8,r9,8<16>
	addu	 r12,r8,1
	extu	 r13,r12,0<5>
	mask	 r12,r12,31
	ld	 r10,r18[r13]
	or	 r13,r0,1
	or.u	 r11,r0,hi16(_regs_F+4)
	mak	 r13,r13,r12
	or	 r11,r11,lo16(_regs_F+4)
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L8440
	extu	 r12,r9,0<24>
	or.u	 r13,r0,hi16(_spec_regs_R+4)
	br.n	 @L9299
	or	 r13,r13,lo16(_spec_regs_R+4)
	align	 4
@L8440:
	or.u	 r13,r0,hi16(_regs_R+4)
	or	 r13,r13,lo16(_regs_R+4)
@L9299:
	ld	 r9,r13[r8]
	br.n	 @L1739
	st	 r9,r11[r12]
	align	 4
@L8443:
	or	 r15,r0,1
	st	 r15,r31,156
@L8778:
	st	 r0,r31,164
	st	 r0,r31,172
	st	 r0,r31,180
	or	 r17,r0,0
	st	 r0,r31,188
@L1739:
	or.u	 r15,r0,hi16(_ss_op2flags)
@L9347:
	ld	 r14,r31,156
	or	 r15,r15,lo16(_ss_op2flags)
	ld	 r13,r15[r14]
	bb0.n	 (31-26),r13,@L8445
	or.u	 r11,r0,hi16(_sim_total_refs)
	or	 r8,r0,0
	or	 r9,r0,1
	ld.d	 r12,r11,lo16(_sim_total_refs)
	or.u	 r14,r0,hi16(_spec_mode)
	addu.co	 r13,r13,r9
	addu.ci	 r12,r12,r8
	ld	 r10,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r10,@L8446
	st.d	 r12,r11,lo16(_sim_total_refs)
	or.u	 r11,r0,hi16(_sim_num_refs)
	ld.d	 r12,r11,lo16(_sim_num_refs)
	addu.co	 r13,r13,r9
	addu.ci	 r12,r12,r8
	st.d	 r12,r11,lo16(_sim_num_refs)
@L8446:
	or.u	 r14,r0,hi16(_ss_op2flags)
	ld	 r15,r31,156
	or	 r14,r14,lo16(_ss_op2flags)
	ld	 r13,r14[r15]
	bb0.n	 (31-24),r13,@L8447
	or	 r15,r0,1
	br.n	 @L8445
	st	 r15,r31,220
	align	 4
@L8447:
	or.u	 r11,r0,hi16(_sim_total_loads)
	ld.d	 r12,r11,lo16(_sim_total_loads)
	addu.co	 r13,r13,r9
	addu.ci	 r12,r12,r8
	bcnd.n	 ne0,r10,@L8445
	st.d	 r12,r11,lo16(_sim_total_loads)
	or.u	 r11,r0,hi16(_sim_num_loads)
	ld.d	 r12,r11,lo16(_sim_num_loads)
	addu.co	 r13,r13,r9
	addu.ci	 r12,r12,r8
	st.d	 r12,r11,lo16(_sim_num_loads)
@L8445:
	or.u	 r12,r0,hi16(_pred_PC)
	ld	 r14,r31,196
	ld	 r13,r12,lo16(_pred_PC)
	cmp	 r13,r13,r14
	bb0.n	 ne,r13,@L8450
	or.u	 r13,r0,hi16(_pred_perfect)
	ld	 r13,r13,lo16(_pred_perfect)
	bcnd.n	 eq0,r13,@L8450
	or.u	 r13,r0,hi16(_fetch_regs_PC)
	st	 r14,r12,lo16(_pred_PC)
	st	 r14,r13,lo16(_fetch_regs_PC)
	or.u	 r13,r0,hi16(_fetch_pred_PC)
	or.u	 r12,r0,hi16(_fetch_num)
	st	 r14,r13,lo16(_fetch_pred_PC)
	or	 r13,r0,1
	st	 r13,r12,lo16(_fetch_num)
	or.u	 r13,r0,hi16(_ruu_decode_width)
	ld	 r12,r13,lo16(_ruu_decode_width)
	or.u	 r13,r0,hi16(_fetch_tail)
	st	 r0,r13,lo16(_fetch_tail)
	or.u	 r13,r0,hi16(_fetch_head)
	subu	 r12,r12,1
	st	 r12,r13,lo16(_fetch_head)
@L8450:
	ld	 r15,r31,156
	cmp	 r13,r15,1
	bb0.n	 ne,r13,@L8451
	or	 r21,r0,0
	ld	 r8,r31,88
	or.u	 r13,r0,hi16(_RUU_tail)
	ld	 r10,r31,92
	or.u	 r12,r0,hi16(_RUU)
	ld	 r11,r13,lo16(_RUU_tail)
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r9,r12,lo16(_RUU)
	or.u	 r6,r0,hi16(_inst_seq)
	ld	 r3,r14,lo16(_spec_mode)
	or.u	 r13,r0,hi16(_regs_PC)
	ld	 r7,r6,lo16(_inst_seq)
	mul	 r11,r11,96
	or.u	 r12,r0,hi16(_pred_PC)
	ld	 r5,r13,lo16(_regs_PC)
	ld	 r4,r12,lo16(_pred_PC)
	addu	 r25,r9,r11
	st	 r8,r0,r25
	st	 r10,r25,4
	st	 r15,r25,8
	st	 r5,r25,12
	ld	 r15,r31,196
	st	 r15,r25,16
	st	 r4,r25,20
	st	 r0,r25,24
	st	 r0,r25,28
	st	 r0,r25,32
	st	 r3,r25,36
	st	 r0,r25,40
	addu	 r12,r7,1
	st	 r12,r25,48
	st	 r0,r25,64
	st	 r0,r25,60
	st	 r0,r25,56
	ld	 r14,r31,212
	st	 r14,r25,52
	or.u	 r14,r0,hi16(_ss_op2flags)
	ld	 r15,r31,156
	or	 r14,r14,lo16(_ss_op2flags)
	ld	 r13,r14[r15]
	bb0.n	 (31-26),r13,@L8452
	st	 r12,r6,lo16(_inst_seq)
	or.u	 r12,r0,hi16(_ptrace_seq)
	or	 r13,r0,52
	st	 r13,r25,8
	or	 r9,r0,1
	st	 r9,r25,28
	ld	 r2,r12,lo16(_ptrace_seq)
	ld	 r11,r31,88
	addu	 r13,r2,1
	st	 r13,r12,lo16(_ptrace_seq)
	or.u	 r13,r0,hi16(_LSQ_tail)
	or.u	 r22,r0,hi16(_ptrace_active)
	ld	 r13,r13,lo16(_LSQ_tail)
	addu	 r8,r7,2
	ld	 r7,r22,lo16(_ptrace_active)
	or.u	 r12,r0,hi16(_LSQ)
	mul	 r13,r13,96
	ld	 r10,r12,lo16(_LSQ)
	ld	 r12,r31,92
	addu	 r23,r10,r13
	st	 r11,r0,r23
	st	 r12,r23,4
	st	 r15,r23,8
	st	 r5,r23,12
	ld	 r15,r31,196
	st	 r8,r6,lo16(_inst_seq)
	st	 r15,r23,16
	st	 r4,r23,20
	st	 r9,r23,24
	st	 r0,r23,28
	st	 r0,r23,32
	st	 r3,r23,36
	st	 r24,r23,40
	st	 r8,r23,48
	st	 r0,r23,64
	st	 r0,r23,60
	st	 r0,r23,56
	bcnd.n	 eq0,r7,@L8454
	st	 r2,r23,52
	ld	 r4,r23,12
	or.u	 r3,r0,hi16(@LC801)
	or	 r5,r0,0
	bsr.n	 ___ptrace_newuop
	or	 r3,r3,lo16(@LC801)
	ld	 r13,r22,lo16(_ptrace_active)
	bcnd	 eq0,r13,@L8454
	ld	 r2,r23,52
	or.u	 r3,r0,hi16(@LC802)
	or	 r4,r0,0
	bsr.n	 ___ptrace_newstage
	or	 r3,r3,lo16(@LC802)
@L8454:
	or	 r13,r0,1
	st	 r13,r25,84
	bcnd.n	 ne0,r17,@L8464
	or	 r8,r0,1
	br.n	 @L8463
	st	 r8,r25,88
	align	 4
@L8464:
	bcnd.n	 ge0,r17,@L8467
	or	 r11,r0,r17
	addu	 r11,r17,31
@L8467:
	ext	 r11,r11,0<5>
	or.u	 r13,r0,hi16(_use_spec_cv)
	mak	 r12,r11,0<5>
	or	 r13,r13,lo16(_use_spec_cv)
	subu	 r12,r17,r12
	ld	 r13,r13[r11]
	mak	 r12,r8,r12
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8465
	or.u	 r13,r0,hi16(_spec_create_vector)
	br.n	 @L9300
	or	 r13,r13,lo16(_spec_create_vector)
	align	 4
@L8465:
	or.u	 r13,r0,hi16(_create_vector)
	or	 r13,r13,lo16(_create_vector)
@L9300:
	lda.d	 r13,r13[r17]
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,112
	st	 r13,r31,116
	ld	 r13,r31,112
	bcnd.n	 ne0,r13,@L8469
	or.u	 r9,r0,hi16(_rslink_free_list)
	or	 r13,r0,1
	br.n	 @L8463
	st	 r13,r25,88
	align	 4
@L8469:
	ld	 r10,r9,lo16(_rslink_free_list)
	bcnd.n	 ne0,r10,@L8470
	st	 r0,r25,88
	or.u	 r2,r0,hi16(@LC520)
	or	 r4,r0,3069
	or.u	 r3,r0,hi16(@LC521)
	or.u	 r5,r0,hi16(@LC522)
	or	 r2,r2,lo16(@LC520)
	or	 r3,r3,lo16(@LC521)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC522)
	align	 4
@L8470:
	ld	 r11,r0,r10
	st	 r0,r0,r10
	st	 r25,r10,4
	ld	 r13,r25,44
	st	 r13,r10,8
	st	 r8,r10,16
	ld	 r12,r31,112
	ld	 r13,r31,116
	lda	 r13,r12[r13]
	ld	 r13,r13,76
	st	 r13,r0,r10
	ld	 r12,r31,112
	ld	 r13,r31,116
	st	 r11,r9,lo16(_rslink_free_list)
	lda	 r13,r12[r13]
	st	 r10,r13,76
@L8463:
	ld	 r14,r31,188
	bcnd.n	 eq0,r14,@L9301
	or	 r8,r0,2
	ld	 r10,r31,188
	bcnd	 ge0,r10,@L8475
	addu	 r10,r10,31
@L8475:
	ld	 r15,r31,188
	ext	 r10,r10,0<5>
	or.u	 r12,r0,hi16(_use_spec_cv)
	or	 r13,r0,1
	mak	 r11,r10,0<5>
	or	 r12,r12,lo16(_use_spec_cv)
	subu	 r11,r15,r11
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8473
	or.u	 r13,r0,hi16(_spec_create_vector)
	or	 r13,r13,lo16(_spec_create_vector)
	br.n	 @L9302
	lda.d	 r13,r13[r15]
	align	 4
@L8473:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r14,r31,188
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r14]
@L9302:
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,112
	st	 r13,r31,116
	ld	 r13,r31,112
	bcnd.n	 ne0,r13,@L8477
	or.u	 r9,r0,hi16(_rslink_free_list)
@L9301:
	or	 r13,r0,1
	br.n	 @L8471
	st	 r13,r25,92
	align	 4
@L8477:
	ld	 r10,r9,lo16(_rslink_free_list)
	bcnd.n	 ne0,r10,@L8478
	st	 r0,r25,92
	or.u	 r2,r0,hi16(@LC520)
	or	 r4,r0,3069
	or.u	 r3,r0,hi16(@LC521)
	or.u	 r5,r0,hi16(@LC522)
	or	 r2,r2,lo16(@LC520)
	or	 r3,r3,lo16(@LC521)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC522)
	align	 4
@L8478:
	ld	 r11,r0,r10
	st	 r0,r0,r10
	st	 r25,r10,4
	ld	 r13,r25,44
	st	 r13,r10,8
	st	 r8,r10,16
	ld	 r12,r31,112
	ld	 r13,r31,116
	lda	 r13,r12[r13]
	ld	 r13,r13,76
	st	 r13,r0,r10
	ld	 r12,r31,112
	ld	 r13,r31,116
	st	 r11,r9,lo16(_rslink_free_list)
	lda	 r13,r12[r13]
	st	 r10,r13,76
@L8471:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r12,r15,lo16(_spec_mode)
	or	 r13,r0,67
	st	 r13,r25,68
	st	 r0,r25,76
	st	 r25,r31,112
	bcnd.n	 eq0,r12,@L8481
	st	 r0,r31,116
	or.u	 r13,r0,hi16(_use_spec_cv)
	or	 r13,r13,lo16(_use_spec_cv)
	or.u	 r12,r0,hi16(_spec_create_vector)
	ld	 r11,r13,8
	or	 r12,r12,lo16(_spec_create_vector)
	st	 r25,r12,536
	st	 r0,r12,540
	or	 r11,r11,8
	br.n	 @L8479
	st	 r11,r13,8
	align	 4
@L8481:
	or.u	 r13,r0,hi16(_create_vector)
	or	 r13,r13,lo16(_create_vector)
	st	 r25,r13,536
	st	 r0,r13,540
@L8479:
	st	 r0,r25,72
	ld	 r14,r31,180
	bcnd.n	 ne0,r14,@L8492
	or	 r10,r0,1
	br.n	 @L8491
	st	 r10,r23,84
	align	 4
@L8492:
	ld	 r11,r31,180
	bcnd	 ge0,r11,@L8495
	addu	 r11,r11,31
@L8495:
	ld	 r15,r31,180
	ext	 r11,r11,0<5>
	or.u	 r13,r0,hi16(_use_spec_cv)
	mak	 r12,r11,0<5>
	or	 r13,r13,lo16(_use_spec_cv)
	subu	 r12,r15,r12
	ld	 r13,r13[r11]
	mak	 r12,r10,r12
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8493
	or.u	 r13,r0,hi16(_spec_create_vector)
	or	 r13,r13,lo16(_spec_create_vector)
	br.n	 @L9303
	lda.d	 r13,r13[r15]
	align	 4
@L8493:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r14,r31,180
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r14]
@L9303:
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,112
	st	 r13,r31,116
	ld	 r13,r31,112
	bcnd.n	 ne0,r13,@L8497
	or.u	 r9,r0,hi16(_rslink_free_list)
	or	 r13,r0,1
	br.n	 @L8491
	st	 r13,r23,84
	align	 4
@L8497:
	ld	 r10,r9,lo16(_rslink_free_list)
	bcnd.n	 ne0,r10,@L8498
	st	 r0,r23,84
	or.u	 r2,r0,hi16(@LC520)
	or	 r4,r0,3069
	or.u	 r3,r0,hi16(@LC521)
	or.u	 r5,r0,hi16(@LC522)
	or	 r2,r2,lo16(@LC520)
	or	 r3,r3,lo16(@LC521)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC522)
	align	 4
@L8498:
	ld	 r11,r0,r10
	st	 r0,r0,r10
	st	 r23,r10,4
	ld	 r13,r23,44
	st	 r13,r10,8
	st	 r0,r10,16
	ld	 r12,r31,112
	ld	 r13,r31,116
	lda	 r13,r12[r13]
	ld	 r13,r13,76
	st	 r13,r0,r10
	ld	 r12,r31,112
	ld	 r13,r31,116
	st	 r11,r9,lo16(_rslink_free_list)
	lda	 r13,r12[r13]
	st	 r10,r13,76
@L8491:
	xor.c	 r13,r21,r0
	mask	 r10,r13,67
	bcnd.n	 ne0,r10,@L8502
	or	 r8,r0,1
	br.n	 @L8501
	st	 r8,r23,88
	align	 4
@L8502:
	bcnd.n	 ge0,r10,@L8505
	or	 r11,r0,r10
	addu	 r11,r10,31
@L8505:
	ext	 r11,r11,0<5>
	or.u	 r13,r0,hi16(_use_spec_cv)
	mak	 r12,r11,0<5>
	or	 r13,r13,lo16(_use_spec_cv)
	subu	 r12,r10,r12
	ld	 r13,r13[r11]
	mak	 r12,r8,r12
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8503
	or.u	 r13,r0,hi16(_spec_create_vector)
	br.n	 @L9304
	or	 r13,r13,lo16(_spec_create_vector)
	align	 4
@L8503:
	or.u	 r13,r0,hi16(_create_vector)
	or	 r13,r13,lo16(_create_vector)
@L9304:
	lda.d	 r13,r13[r10]
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,112
	st	 r13,r31,116
	ld	 r13,r31,112
	bcnd.n	 ne0,r13,@L8507
	or.u	 r9,r0,hi16(_rslink_free_list)
	or	 r13,r0,1
	br.n	 @L8501
	st	 r13,r23,88
	align	 4
@L8507:
	ld	 r10,r9,lo16(_rslink_free_list)
	bcnd.n	 ne0,r10,@L8508
	st	 r0,r23,88
	or.u	 r2,r0,hi16(@LC520)
	or	 r4,r0,3069
	or.u	 r3,r0,hi16(@LC521)
	or.u	 r5,r0,hi16(@LC522)
	or	 r2,r2,lo16(@LC520)
	or	 r3,r3,lo16(@LC521)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC522)
	align	 4
@L8508:
	ld	 r11,r0,r10
	st	 r0,r0,r10
	st	 r23,r10,4
	ld	 r13,r23,44
	st	 r13,r10,8
	st	 r8,r10,16
	ld	 r12,r31,112
	ld	 r13,r31,116
	lda	 r13,r12[r13]
	ld	 r13,r13,76
	st	 r13,r0,r10
	ld	 r12,r31,112
	ld	 r13,r31,116
	st	 r11,r9,lo16(_rslink_free_list)
	lda	 r13,r12[r13]
	st	 r10,r13,76
@L8501:
	or	 r9,r0,1
	st	 r9,r23,92
	ld	 r15,r31,164
	bcnd.n	 ne0,r15,@L9778
	or.u	 r14,r0,hi16(_spec_mode)
	br.n	 @L8517
	st	 r0,r23,68
	align	 4
@L9778:
	ld	 r13,r14,lo16(_spec_mode)
	st	 r15,r23,68
	st	 r0,r23,76
	st	 r23,r31,112
	bcnd.n	 eq0,r13,@L8519
	st	 r0,r31,116
	ld	 r10,r31,164
	bcnd	 ge0,r15,@L8521
	addu	 r10,r15,31
@L8521:
	ext	 r10,r10,0<5>
	or.u	 r11,r0,hi16(_use_spec_cv)
	ld	 r14,r31,164
	mak	 r12,r10,0<5>
	or	 r11,r11,lo16(_use_spec_cv)
	subu	 r12,r14,r12
	ld	 r13,r11[r10]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r11[r10]
	or.u	 r13,r0,hi16(_spec_create_vector)
	ld	 r11,r31,112
	or	 r13,r13,lo16(_spec_create_vector)
	ld	 r12,r31,116
	lda.d	 r13,r13[r14]
	st	 r11,r0,r13
	br.n	 @L8517
	st	 r12,r13,4
	align	 4
@L8519:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r15,r31,164
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r15]
	st	 r23,r0,r13
	st	 r0,r13,4
@L8517:
	ld	 r14,r31,172
	bcnd.n	 ne0,r14,@L8524
	or	 r9,r0,1
	br.n	 @L8523
	st	 r0,r23,72
	align	 4
@L8524:
	or.u	 r15,r0,hi16(_spec_mode)
	ld	 r14,r31,172
	ld	 r13,r15,lo16(_spec_mode)
	st	 r14,r23,72
	st	 r0,r23,80
	st	 r23,r31,112
	bcnd.n	 eq0,r13,@L8525
	st	 r9,r31,116
	ld	 r10,r31,172
	bcnd	 ge0,r14,@L8527
	addu	 r10,r14,31
@L8527:
	ext	 r10,r10,0<5>
	or.u	 r11,r0,hi16(_use_spec_cv)
	ld	 r15,r31,172
	mak	 r12,r10,0<5>
	or	 r11,r11,lo16(_use_spec_cv)
	subu	 r12,r15,r12
	ld	 r13,r11[r10]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r11[r10]
	or.u	 r13,r0,hi16(_spec_create_vector)
	ld	 r11,r31,112
	or	 r13,r13,lo16(_spec_create_vector)
	ld	 r12,r31,116
	lda.d	 r13,r13[r15]
	st	 r11,r0,r13
	br.n	 @L8523
	st	 r12,r13,4
	align	 4
@L8525:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r14,r31,172
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r14]
	st	 r23,r0,r13
	st	 r9,r13,4
@L8523:
	ld	 r15,r31,148
	or.u	 r13,r0,hi16(_RUU_tail)
	or.u	 r12,r0,hi16(_RUU_size)
	ld	 r11,r13,lo16(_RUU_tail)
	ld	 r10,r12,lo16(_RUU_size)
	or	 r8,r13,lo16(_RUU_tail)
	addu	 r15,r15,1
	st	 r15,r31,148
	addu	 r9,r11,1
	or	 r11,r12,lo16(_RUU_size)
	bcnd.n	 gt0,r10,@L8530
	subu	 r13,r0,r10
	bcnd.n	 ge0,r9,@L8531
	subu	 r12,r0,r9
	divu	 r13,r12,r13
	bcnd	 ne0,r10,@L8529
@L8533:
	tb0	 0,r0,503
	br	 @L8529
	align	 4
@L8531:
	divu	 r13,r9,r13
	bcnd	 eq0,r10,@L8533
	subu	 r13,r0,r13
	br	 @L8529
	align	 4
@L8530:
	bcnd.n	 ge0,r9,@L8532
	subu	 r12,r0,r9
	divu	 r13,r12,r10
	br.n	 @L8529
	subu	 r13,r0,r13
	align	 4
@L8532:
	divu	 r13,r9,r10
@L8529:
	ld	 r12,r0,r11
	or.u	 r11,r0,hi16(_RUU_num)
	mul	 r12,r13,r12
	ld	 r13,r11,lo16(_RUU_num)
	addu	 r13,r13,1
	st	 r13,r11,lo16(_RUU_num)
	or.u	 r13,r0,hi16(_LSQ_tail)
	subu	 r12,r9,r12
	st	 r12,r0,r8
	or.u	 r11,r0,hi16(_LSQ_size)
	ld	 r12,r13,lo16(_LSQ_tail)
	or	 r8,r13,lo16(_LSQ_tail)
	ld	 r9,r11,lo16(_LSQ_size)
	or	 r11,r11,lo16(_LSQ_size)
	addu	 r12,r12,1
	bcnd.n	 gt0,r9,@L8535
	subu	 r10,r0,r9
	bcnd.n	 ge0,r12,@L8536
	subu	 r13,r0,r12
	divu	 r10,r13,r10
	bcnd	 ne0,r9,@L8534
@L8538:
	tb0	 0,r0,503
	br	 @L8534
	align	 4
@L8536:
	divu	 r10,r12,r10
	bcnd	 eq0,r9,@L8538
	subu	 r10,r0,r10
	br	 @L8534
	align	 4
@L8535:
	bcnd.n	 ge0,r12,@L8537
	subu	 r13,r0,r12
	divu	 r10,r13,r9
	br.n	 @L8534
	subu	 r10,r0,r10
	align	 4
@L8537:
	divu	 r10,r12,r9
@L8534:
	ld	 r13,r0,r11
	mul	 r13,r10,r13
	subu	 r13,r12,r13
	or.u	 r12,r0,hi16(_LSQ_num)
	st	 r13,r0,r8
	ld	 r13,r12,lo16(_LSQ_num)
	ld	 r11,r25,84
	addu	 r13,r13,1
	bcnd.n	 eq0,r11,@L8539
	st	 r13,r12,lo16(_LSQ_num)
	ld	 r13,r25,88
	bcnd	 eq0,r13,@L8539
	ld	 r13,r25,92
	bcnd	 eq0,r13,@L8539
	ld	 r13,r25,56
	bcnd.n	 eq0,r13,@L8541
	or.u	 r10,r0,hi16(_rslink_free_list)
	or.u	 r2,r0,hi16(@LC367)
	or	 r4,r0,1794
	or.u	 r3,r0,hi16(@LC368)
	or.u	 r5,r0,hi16(@LC369)
	or	 r2,r2,lo16(@LC367)
	or	 r3,r3,lo16(@LC368)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC369)
	align	 4
@L8541:
	ld	 r11,r10,lo16(_rslink_free_list)
	or	 r13,r0,1
	bcnd.n	 ne0,r11,@L8542
	st	 r13,r25,56
	or.u	 r2,r0,hi16(@LC370)
	or	 r4,r0,1798
	or.u	 r3,r0,hi16(@LC371)
	or.u	 r5,r0,hi16(@LC372)
	or	 r2,r2,lo16(@LC370)
	or	 r3,r3,lo16(@LC371)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC372)
	align	 4
@L8542:
	ld	 r12,r0,r11
	st	 r0,r0,r11
	st	 r25,r11,4
	ld	 r13,r25,44
	st	 r13,r11,8
	ld	 r13,r25,48
	or	 r9,r0,r11
	st	 r13,r9,16
	ld	 r13,r25,24
	bcnd.n	 ne0,r13,@L8544
	st	 r12,r10,lo16(_rslink_free_list)
	or.u	 r14,r0,hi16(_ss_op2flags)
	ld	 r13,r25,8
	or	 r14,r14,lo16(_ss_op2flags)
	ld	 r13,r14[r13]
	mask	 r13,r13,4100
	bcnd.n	 eq0,r13,@L8543
	or.u	 r13,r0,hi16(_ready_queue)
@L8544:
	br.n	 @L8545
	or	 r11,r0,0
	align	 4
@L8543:
	ld	 r12,r13,lo16(_ready_queue)
	bcnd.n	 eq0,r12,@L8545
	or	 r11,r0,0
	ld	 r10,r25,48
@L8551:
	ld	 r13,r12,16
	cmp	 r13,r13,r10
	bb0	 lo,r13,@L8545
	or	 r11,r0,r12
	ld	 r12,r0,r11
	bcnd	 ne0,r12,@L8551
@L8545:
	bcnd	 eq0,r11,@L8552
	ld	 r13,r0,r11
	st	 r13,r0,r9
	br.n	 @L8539
	st	 r9,r0,r11
	align	 4
@L8552:
	or.u	 r12,r0,hi16(_ready_queue)
	ld	 r13,r12,lo16(_ready_queue)
	st	 r13,r0,r9
	st	 r9,r12,lo16(_ready_queue)
@L8539:
	or.u	 r12,r0,hi16(_last_op)
	st	 r0,r12,lo16(_last_op)
	or	 r12,r12,lo16(_last_op)
	st	 r23,r12,4
	ld	 r13,r23,44
	st	 r13,r12,8
	or.u	 r14,r0,hi16(_ss_op2flags)
	ld	 r15,r31,156
	or	 r14,r14,lo16(_ss_op2flags)
	ld	 r13,r14[r15]
	mask	 r13,r13,160
	cmp	 r13,r13,160
	bb1	 ne,r13,@L8627
	ld	 r13,r23,84
	bcnd	 eq0,r13,@L8627
	ld	 r13,r23,88
	bcnd	 eq0,r13,@L8627
	ld	 r13,r23,92
	bcnd	 eq0,r13,@L8627
	ld	 r13,r23,56
	bcnd.n	 eq0,r13,@L8556
	or.u	 r10,r0,hi16(_rslink_free_list)
	or.u	 r2,r0,hi16(@LC367)
	or	 r4,r0,1794
	or.u	 r3,r0,hi16(@LC368)
	or.u	 r5,r0,hi16(@LC369)
	or	 r2,r2,lo16(@LC367)
	or	 r3,r3,lo16(@LC368)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC369)
	align	 4
@L8556:
	ld	 r11,r10,lo16(_rslink_free_list)
	or	 r13,r0,1
	bcnd.n	 ne0,r11,@L8557
	st	 r13,r23,56
	or.u	 r2,r0,hi16(@LC370)
	or	 r4,r0,1798
	or.u	 r3,r0,hi16(@LC371)
	or.u	 r5,r0,hi16(@LC372)
	or	 r2,r2,lo16(@LC370)
	or	 r3,r3,lo16(@LC371)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC372)
	align	 4
@L8557:
	ld	 r12,r0,r11
	st	 r0,r0,r11
	st	 r23,r11,4
	ld	 r13,r23,44
	st	 r13,r11,8
	ld	 r13,r23,48
	or	 r9,r0,r11
	st	 r13,r9,16
	ld	 r13,r23,24
	bcnd.n	 ne0,r13,@L8559
	st	 r12,r10,lo16(_rslink_free_list)
	or.u	 r15,r0,hi16(_ss_op2flags)
	ld	 r13,r23,8
	or	 r15,r15,lo16(_ss_op2flags)
	ld	 r13,r15[r13]
	mask	 r13,r13,4100
	bcnd.n	 eq0,r13,@L8558
	or.u	 r13,r0,hi16(_ready_queue)
@L8559:
	br.n	 @L8560
	or	 r11,r0,0
	align	 4
@L8558:
	ld	 r12,r13,lo16(_ready_queue)
	bcnd.n	 eq0,r12,@L8560
	or	 r11,r0,0
	ld	 r10,r23,48
@L8566:
	ld	 r13,r12,16
	cmp	 r13,r13,r10
	bb0	 lo,r13,@L8560
	or	 r11,r0,r12
	ld	 r12,r0,r11
	bcnd	 ne0,r12,@L8566
@L8560:
	bcnd	 eq0,r11,@L8567
	ld	 r13,r0,r11
	st	 r13,r0,r9
	br.n	 @L8627
	st	 r9,r0,r11
	align	 4
@L8567:
	or.u	 r12,r0,hi16(_ready_queue)
	ld	 r13,r12,lo16(_ready_queue)
	st	 r13,r0,r9
	br.n	 @L8627
	st	 r9,r12,lo16(_ready_queue)
	align	 4
@L8452:
	ld	 r14,r31,180
	bcnd.n	 eq0,r14,@L9772
	or	 r13,r0,1
	ld	 r10,r31,180
	bcnd	 ge0,r10,@L8574
	addu	 r10,r10,31
@L8574:
	ld	 r15,r31,180
	ext	 r10,r10,0<5>
	or.u	 r12,r0,hi16(_use_spec_cv)
	or	 r13,r0,1
	mak	 r11,r10,0<5>
	or	 r12,r12,lo16(_use_spec_cv)
	subu	 r11,r15,r11
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8572
	or.u	 r13,r0,hi16(_spec_create_vector)
	or	 r13,r13,lo16(_spec_create_vector)
	br.n	 @L9306
	lda.d	 r13,r13[r15]
	align	 4
@L8572:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r14,r31,180
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r14]
@L9306:
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,112
	st	 r13,r31,116
	ld	 r13,r31,112
	bcnd.n	 ne0,r13,@L8576
	or.u	 r9,r0,hi16(_rslink_free_list)
	or	 r13,r0,1
@L9772:
	br.n	 @L8570
	st	 r13,r25,84
	align	 4
@L8576:
	ld	 r10,r9,lo16(_rslink_free_list)
	bcnd.n	 ne0,r10,@L8577
	st	 r0,r25,84
	or.u	 r2,r0,hi16(@LC520)
	or	 r4,r0,3069
	or.u	 r3,r0,hi16(@LC521)
	or.u	 r5,r0,hi16(@LC522)
	or	 r2,r2,lo16(@LC520)
	or	 r3,r3,lo16(@LC521)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC522)
	align	 4
@L8577:
	ld	 r11,r0,r10
	st	 r0,r0,r10
	st	 r25,r10,4
	ld	 r13,r25,44
	st	 r13,r10,8
	st	 r0,r10,16
	ld	 r12,r31,112
	ld	 r13,r31,116
	lda	 r13,r12[r13]
	ld	 r13,r13,76
	st	 r13,r0,r10
	ld	 r12,r31,112
	ld	 r13,r31,116
	st	 r11,r9,lo16(_rslink_free_list)
	lda	 r13,r12[r13]
	st	 r10,r13,76
@L8570:
	bcnd.n	 ne0,r17,@L8579
	or	 r8,r0,1
	br.n	 @L8578
	st	 r8,r25,88
	align	 4
@L8579:
	bcnd.n	 ge0,r17,@L8582
	or	 r11,r0,r17
	addu	 r11,r17,31
@L8582:
	ext	 r11,r11,0<5>
	or.u	 r13,r0,hi16(_use_spec_cv)
	mak	 r12,r11,0<5>
	or	 r13,r13,lo16(_use_spec_cv)
	subu	 r12,r17,r12
	ld	 r13,r13[r11]
	mak	 r12,r8,r12
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L8580
	or.u	 r13,r0,hi16(_spec_create_vector)
	br.n	 @L9307
	or	 r13,r13,lo16(_spec_create_vector)
	align	 4
@L8580:
	or.u	 r13,r0,hi16(_create_vector)
	or	 r13,r13,lo16(_create_vector)
@L9307:
	lda.d	 r13,r13[r17]
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,112
	st	 r13,r31,116
	ld	 r13,r31,112
	bcnd.n	 ne0,r13,@L8584
	or.u	 r9,r0,hi16(_rslink_free_list)
	or	 r13,r0,1
	br.n	 @L8578
	st	 r13,r25,88
	align	 4
@L8584:
	ld	 r10,r9,lo16(_rslink_free_list)
	bcnd.n	 ne0,r10,@L8585
	st	 r0,r25,88
	or.u	 r2,r0,hi16(@LC520)
	or	 r4,r0,3069
	or.u	 r3,r0,hi16(@LC521)
	or.u	 r5,r0,hi16(@LC522)
	or	 r2,r2,lo16(@LC520)
	or	 r3,r3,lo16(@LC521)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC522)
	align	 4
@L8585:
	ld	 r11,r0,r10
	st	 r0,r0,r10
	st	 r25,r10,4
	ld	 r13,r25,44
	st	 r13,r10,8
	st	 r8,r10,16
	ld	 r12,r31,112
	ld	 r13,r31,116
	lda	 r13,r12[r13]
	ld	 r13,r13,76
	st	 r13,r0,r10
	ld	 r12,r31,112
	ld	 r13,r31,116
	st	 r11,r9,lo16(_rslink_free_list)
	lda	 r13,r12[r13]
	st	 r10,r13,76
@L8578:
	ld	 r15,r31,188
	bcnd.n	 eq0,r15,@L9308
	or	 r8,r0,2
	ld	 r10,r31,188
	bcnd	 ge0,r10,@L8590
	addu	 r10,r10,31
@L8590:
	ld	 r14,r31,188
	ext	 r10,r10,0<5>
	or.u	 r12,r0,hi16(_use_spec_cv)
	or	 r13,r0,1
	mak	 r11,r10,0<5>
	or	 r12,r12,lo16(_use_spec_cv)
	subu	 r11,r14,r11
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L8588
	or.u	 r13,r0,hi16(_spec_create_vector)
	or	 r13,r13,lo16(_spec_create_vector)
	br.n	 @L9309
	lda.d	 r13,r13[r14]
	align	 4
@L8588:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r15,r31,188
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r15]
@L9309:
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,112
	st	 r13,r31,116
	ld	 r13,r31,112
	bcnd.n	 ne0,r13,@L8592
	or.u	 r9,r0,hi16(_rslink_free_list)
@L9308:
	or	 r13,r0,1
	br.n	 @L8586
	st	 r13,r25,92
	align	 4
@L8592:
	ld	 r10,r9,lo16(_rslink_free_list)
	bcnd.n	 ne0,r10,@L8593
	st	 r0,r25,92
	or.u	 r2,r0,hi16(@LC520)
	or	 r4,r0,3069
	or.u	 r3,r0,hi16(@LC521)
	or.u	 r5,r0,hi16(@LC522)
	or	 r2,r2,lo16(@LC520)
	or	 r3,r3,lo16(@LC521)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC522)
	align	 4
@L8593:
	ld	 r11,r0,r10
	st	 r0,r0,r10
	st	 r25,r10,4
	ld	 r13,r25,44
	st	 r13,r10,8
	st	 r8,r10,16
	ld	 r12,r31,112
	ld	 r13,r31,116
	lda	 r13,r12[r13]
	ld	 r13,r13,76
	st	 r13,r0,r10
	ld	 r12,r31,112
	ld	 r13,r31,116
	st	 r11,r9,lo16(_rslink_free_list)
	lda	 r13,r12[r13]
	st	 r10,r13,76
@L8586:
	ld	 r14,r31,164
	bcnd.n	 ne0,r14,@L9779
	or.u	 r15,r0,hi16(_spec_mode)
	br.n	 @L8594
	st	 r0,r25,68
	align	 4
@L9779:
	ld	 r13,r15,lo16(_spec_mode)
	st	 r14,r25,68
	st	 r0,r25,76
	st	 r25,r31,112
	bcnd.n	 eq0,r13,@L8596
	st	 r0,r31,116
	ld	 r9,r31,164
	bcnd	 ge0,r14,@L8598
	addu	 r9,r14,31
@L8598:
	ext	 r9,r9,0<5>
	or.u	 r10,r0,hi16(_use_spec_cv)
	or	 r13,r0,1
	ld	 r15,r31,164
	mak	 r11,r9,0<5>
	or	 r10,r10,lo16(_use_spec_cv)
	subu	 r11,r15,r11
	ld	 r12,r10[r9]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	st	 r12,r10[r9]
	or.u	 r13,r0,hi16(_spec_create_vector)
	ld	 r11,r31,112
	or	 r13,r13,lo16(_spec_create_vector)
	ld	 r12,r31,116
	lda.d	 r13,r13[r15]
	st	 r11,r0,r13
	br.n	 @L8594
	st	 r12,r13,4
	align	 4
@L8596:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r14,r31,164
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r14]
	st	 r25,r0,r13
	st	 r0,r13,4
@L8594:
	ld	 r15,r31,172
	bcnd.n	 ne0,r15,@L8601
	or	 r9,r0,1
	br.n	 @L8600
	st	 r0,r25,72
	align	 4
@L8601:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r15,r31,172
	ld	 r13,r14,lo16(_spec_mode)
	st	 r15,r25,72
	st	 r0,r25,80
	st	 r25,r31,112
	bcnd.n	 eq0,r13,@L8602
	st	 r9,r31,116
	ld	 r10,r31,172
	bcnd	 ge0,r15,@L8604
	addu	 r10,r15,31
@L8604:
	ext	 r10,r10,0<5>
	or.u	 r11,r0,hi16(_use_spec_cv)
	ld	 r14,r31,172
	mak	 r12,r10,0<5>
	or	 r11,r11,lo16(_use_spec_cv)
	subu	 r12,r14,r12
	ld	 r13,r11[r10]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r11[r10]
	or.u	 r13,r0,hi16(_spec_create_vector)
	ld	 r11,r31,112
	or	 r13,r13,lo16(_spec_create_vector)
	ld	 r12,r31,116
	lda.d	 r13,r13[r14]
	st	 r11,r0,r13
	br.n	 @L8600
	st	 r12,r13,4
	align	 4
@L8602:
	or.u	 r13,r0,hi16(_create_vector)
	ld	 r15,r31,172
	or	 r13,r13,lo16(_create_vector)
	lda.d	 r13,r13[r15]
	st	 r25,r0,r13
	st	 r9,r13,4
@L8600:
	ld	 r14,r31,148
	or.u	 r13,r0,hi16(_RUU_tail)
	or.u	 r12,r0,hi16(_RUU_size)
	ld	 r11,r13,lo16(_RUU_tail)
	ld	 r9,r12,lo16(_RUU_size)
	or	 r8,r13,lo16(_RUU_tail)
	or	 r12,r12,lo16(_RUU_size)
	addu	 r14,r14,1
	st	 r14,r31,148
	addu	 r11,r11,1
	bcnd.n	 gt0,r9,@L8607
	subu	 r10,r0,r9
	bcnd.n	 ge0,r11,@L8608
	subu	 r13,r0,r11
	divu	 r10,r13,r10
	bcnd	 ne0,r9,@L8606
@L8610:
	tb0	 0,r0,503
	br	 @L8606
	align	 4
@L8608:
	divu	 r10,r11,r10
	bcnd	 eq0,r9,@L8610
	subu	 r10,r0,r10
	br	 @L8606
	align	 4
@L8607:
	bcnd.n	 ge0,r11,@L8609
	subu	 r13,r0,r11
	divu	 r10,r13,r9
	br.n	 @L8606
	subu	 r10,r0,r10
	align	 4
@L8609:
	divu	 r10,r11,r9
@L8606:
	ld	 r13,r0,r12
	mul	 r13,r10,r13
	or.u	 r12,r0,hi16(_RUU_num)
	subu	 r13,r11,r13
	st	 r13,r0,r8
	ld	 r13,r12,lo16(_RUU_num)
	ld	 r11,r25,84
	addu	 r13,r13,1
	bcnd.n	 eq0,r11,@L8611
	st	 r13,r12,lo16(_RUU_num)
	ld	 r13,r25,88
	bcnd	 eq0,r13,@L8611
	ld	 r13,r25,92
	bcnd	 eq0,r13,@L8611
	ld	 r13,r25,56
	bcnd.n	 eq0,r13,@L8613
	or.u	 r10,r0,hi16(_rslink_free_list)
	or.u	 r2,r0,hi16(@LC367)
	or	 r4,r0,1794
	or.u	 r3,r0,hi16(@LC368)
	or.u	 r5,r0,hi16(@LC369)
	or	 r2,r2,lo16(@LC367)
	or	 r3,r3,lo16(@LC368)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC369)
	align	 4
@L8613:
	ld	 r11,r10,lo16(_rslink_free_list)
	or	 r13,r0,1
	bcnd.n	 ne0,r11,@L8614
	st	 r13,r25,56
	or.u	 r2,r0,hi16(@LC370)
	or	 r4,r0,1798
	or.u	 r3,r0,hi16(@LC371)
	or.u	 r5,r0,hi16(@LC372)
	or	 r2,r2,lo16(@LC370)
	or	 r3,r3,lo16(@LC371)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC372)
	align	 4
@L8614:
	ld	 r12,r0,r11
	st	 r0,r0,r11
	st	 r25,r11,4
	ld	 r13,r25,44
	st	 r13,r11,8
	ld	 r13,r25,48
	or	 r9,r0,r11
	st	 r13,r9,16
	ld	 r13,r25,24
	bcnd.n	 ne0,r13,@L8616
	st	 r12,r10,lo16(_rslink_free_list)
	or.u	 r15,r0,hi16(_ss_op2flags)
	ld	 r13,r25,8
	or	 r15,r15,lo16(_ss_op2flags)
	ld	 r13,r15[r13]
	mask	 r13,r13,4100
	bcnd.n	 eq0,r13,@L8615
	or.u	 r13,r0,hi16(_ready_queue)
@L8616:
	br.n	 @L8617
	or	 r11,r0,0
	align	 4
@L8615:
	ld	 r12,r13,lo16(_ready_queue)
	bcnd.n	 eq0,r12,@L8617
	or	 r11,r0,0
	ld	 r10,r25,48
@L8623:
	ld	 r13,r12,16
	cmp	 r13,r13,r10
	bb0	 lo,r13,@L8617
	or	 r11,r0,r12
	ld	 r12,r0,r11
	bcnd	 ne0,r12,@L8623
@L8617:
	bcnd	 eq0,r11,@L8624
	ld	 r13,r0,r11
	st	 r13,r0,r9
	br.n	 @L8612
	st	 r9,r0,r11
	align	 4
@L8624:
	or.u	 r12,r0,hi16(_ready_queue)
	ld	 r13,r12,lo16(_ready_queue)
	st	 r13,r0,r9
	st	 r9,r12,lo16(_ready_queue)
@L8612:
	or.u	 r3,r0,hi16(_RSLINK_NULL)
	or	 r3,r3,lo16(_RSLINK_NULL)
	or.u	 r2,r0,hi16(_last_op)
	subu	 r3,r3,72
	or	 r2,r2,lo16(_last_op)
	ld	 r5,r3,72
	subu	 r2,r2,72
	bsr.n	 ___movstrSI96x24
	addu	 r1,r1,@L9782
@L9783:
	align	 4
@L8611:
	or.u	 r12,r0,hi16(_last_op)
	st	 r0,r12,lo16(_last_op)
	or	 r12,r12,lo16(_last_op)
	st	 r25,r12,4
	ld	 r13,r25,44
	br.n	 @L8627
	st	 r13,r12,8
	align	 4
@L8451:
	or	 r25,r0,0
@L8627:
	or	 r8,r0,0
	or	 r9,r0,1
	or.u	 r13,r0,hi16(_sim_total_insn)
	ld.d	 r10,r13,lo16(_sim_total_insn)
	or.u	 r15,r0,hi16(_ss_op2flags)
	ld	 r14,r31,156
	or	 r15,r15,lo16(_ss_op2flags)
	addu.co	 r11,r11,r9
	addu.ci	 r10,r10,r8
	ld	 r12,r15[r14]
	bb0.n	 (31-29),r12,@L8628
	st.d	 r10,r13,lo16(_sim_total_insn)
	or.u	 r11,r0,hi16(_sim_total_branches)
	ld.d	 r12,r11,lo16(_sim_total_branches)
	addu.co	 r13,r13,r9
	addu.ci	 r12,r12,r8
	st.d	 r12,r11,lo16(_sim_total_branches)
@L8628:
	or.u	 r14,r0,hi16(_spec_mode)
	ld	 r13,r14,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L8629
	or.u	 r13,r0,hi16(_sim_num_insn)
	ld.d	 r10,r13,lo16(_sim_num_insn)
	or.u	 r14,r0,hi16(_ss_op2flags)
	ld	 r15,r31,156
	or	 r14,r14,lo16(_ss_op2flags)
	addu.co	 r11,r11,r9
	addu.ci	 r10,r10,r8
	ld	 r12,r14[r15]
	bb0.n	 (31-29),r12,@L8630
	st.d	 r10,r13,lo16(_sim_num_insn)
	or.u	 r13,r0,hi16(_sim_num_branches)
	ld.d	 r10,r13,lo16(_sim_num_branches)
	or.u	 r12,r0,hi16(_pred)
	addu.co	 r11,r11,r9
	addu.ci	 r10,r10,r8
	ld	 r2,r12,lo16(_pred)
	bcnd.n	 eq0,r2,@L8630
	st.d	 r10,r13,lo16(_sim_num_branches)
	or.u	 r13,r0,hi16(_pred_PC)
	ld	 r4,r31,196
	ld	 r6,r13,lo16(_pred_PC)
	or.u	 r13,r0,hi16(_regs_PC)
	ld	 r3,r13,lo16(_regs_PC)
	cmp	 r6,r6,r4
	addu	 r5,r3,8
	extu	 r6,r6,1<eq>
	cmp	 r5,r4,r5
	ld	 r7,r31,204
	bsr.n	 _bpred_update
	extu	 r5,r5,1<ne>
@L8630:
	or.u	 r13,r0,hi16(_pred_PC)
	ld	 r15,r31,196
	ld	 r13,r13,lo16(_pred_PC)
	cmp	 r13,r13,r15
	bb0.n	 ne,r13,@L8629
	or	 r13,r0,1
	or.u	 r14,r0,hi16(_spec_mode)
	st	 r13,r25,32
	st	 r13,r14,lo16(_spec_mode)
	or.u	 r13,r0,hi16(_recover_PC)
	st	 r15,r13,lo16(_recover_PC)
@L8629:
	or.u	 r25,r0,hi16(_ptrace_active)
	ld	 r13,r25,lo16(_ptrace_active)
	bcnd	 eq0,r13,@L8633
	ld	 r15,r31,196
	or.u	 r13,r0,hi16(_pred_PC)
	ld	 r4,r13,lo16(_pred_PC)
	ld	 r2,r31,212
	cmp	 r4,r4,r15
	or.u	 r3,r0,hi16(@LC803)
	ext	 r4,r4,1<ne>
	or	 r3,r3,lo16(@LC803)
	bsr.n	 ___ptrace_newstage
	mask	 r4,r4,4
@L8633:
	ld	 r14,r31,156
	cmp	 r13,r14,1
	bb1.n	 ne,r13,@L9773
	or.u	 r13,r0,hi16(_pcstat_nelt)
	ld	 r13,r25,lo16(_ptrace_active)
	bcnd.n	 eq0,r13,@L9773
	or.u	 r13,r0,hi16(_pcstat_nelt)
	bsr.n	 ___ptrace_endinst
	ld	 r2,r31,212
	or.u	 r13,r0,hi16(_pcstat_nelt)
@L9773:
	ld	 r13,r13,lo16(_pcstat_nelt)
	bcnd.n	 le0,r13,@L8639
	or	 r25,r0,0
	or.u	 r13,r0,hi16(_pcstat_stats)
	or.u	 r12,r0,hi16(_pcstat_sdists)
	or	 r21,r13,lo16(_pcstat_stats)
	or.u	 r13,r0,hi16(_pcstat_lastvals)
	or	 r22,r12,lo16(_pcstat_sdists)
	or	 r23,r13,lo16(_pcstat_lastvals)
@L8641:
	ld	 r11,r21[r25]
	ld	 r12,r11,16
	bcnd.n	 ne0,r12,@L8642
	cmp	 r13,r12,1
	ld	 r13,r11,24
	ld	 r13,r0,r13
	st	 r13,r31,244
	ext	 r15,r13,0<31>
	br.n	 @L8643
	st	 r15,r31,240
	align	 4
@L8642:
	bb1.n	 ne,r13,@L8644
	cmp	 r13,r12,2
	ld	 r13,r11,24
	ld	 r13,r0,r13
	st	 r13,r31,244
	st	 r0,r31,240
	ld.d	 r14,r31,240
	br.n	 @L8643
	st.d	 r14,r31,240
	align	 4
@L8644:
	bb1.n	 ne,r13,@L8646
	or.u	 r2,r0,hi16(@LC804)
	ld	 r13,r11,24
	ld.d	 r14,r0,r13
	br.n	 @L8643
	st.d	 r14,r31,240
	align	 4
@L8646:
	or	 r4,r0,3697
	or.u	 r3,r0,hi16(@LC805)
	or.u	 r5,r0,hi16(@LC806)
	or	 r2,r2,lo16(@LC804)
	or	 r3,r3,lo16(@LC805)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC806)
	align	 4
@L8643:
	ld.d	 r12,r23[r25]
	ld	 r15,r31,244
	subu	 r4,r15,r13
	bcnd.n	 eq0,r4,@L9774
	or.u	 r13,r0,hi16(_pcstat_nelt)
	or.u	 r13,r0,hi16(_regs_PC)
	ld	 r2,r22[r25]
	bsr.n	 _stat_add_samples
	ld	 r3,r13,lo16(_regs_PC)
	ld.d	 r14,r31,240
	st.d	 r14,r23[r25]
	or.u	 r13,r0,hi16(_pcstat_nelt)
@L9774:
	ld	 r13,r13,lo16(_pcstat_nelt)
	addu	 r25,r25,1
	cmp	 r13,r25,r13
	bb1	 lt,r13,@L8641
@L8639:
	or.u	 r8,r0,hi16(_fetch_head)
	or.u	 r13,r0,hi16(_ruu_ifq_size)
	ld	 r12,r8,lo16(_fetch_head)
	or.u	 r9,r0,hi16(_fetch_num)
	ld	 r11,r13,lo16(_ruu_ifq_size)
	or.u	 r10,r0,hi16(_dlite_check)
	ld	 r13,r9,lo16(_fetch_num)
	or	 r22,r0,1
	ld	 r10,r10,lo16(_dlite_check)
	addu	 r12,r12,1
	subu	 r11,r11,1
	subu	 r13,r13,1
	st	 r13,r9,lo16(_fetch_num)
	and	 r12,r12,r11
	bcnd.n	 ne0,r10,@L8653
	st	 r12,r8,lo16(_fetch_head)
	or.u	 r13,r0,hi16(_dlite_active)
	ld	 r13,r13,lo16(_dlite_active)
	bcnd.n	 eq0,r13,@L9775
	or.u	 r13,r0,hi16(_ruu_decode_width)
@L8653:
	or.u	 r23,r0,hi16(_pred_PC)
	ld	 r15,r31,220
	ld	 r2,r23,lo16(_pred_PC)
	bcnd.n	 eq0,r15,@L8654
	or	 r3,r0,1
	or	 r3,r0,2
@L8654:
	or.u	 r13,r0,hi16(_sim_num_insn)
	or.u	 r25,r0,hi16(_sim_cycle)
	ld.d	 r6,r13,lo16(_sim_num_insn)
	ld.d	 r8,r25,lo16(_sim_cycle)
	bsr.n	 ___check_break
	or	 r4,r0,r24
	bcnd.n	 eq0,r2,@L9775
	or.u	 r13,r0,hi16(_ruu_decode_width)
	ld	 r3,r23,lo16(_pred_PC)
	or.u	 r13,r0,hi16(_regs_PC)
	ld.d	 r4,r25,lo16(_sim_cycle)
	bsr.n	 _dlite_main
	ld	 r2,r13,lo16(_regs_PC)
	or.u	 r13,r0,hi16(_ruu_decode_width)
@L9775:
	ld	 r14,r31,148
	ld	 r13,r13,lo16(_ruu_decode_width)
	cmp	 r13,r14,r13
	bb0.n	 lt,r13,@L1731
	or.u	 r13,r0,hi16(_RUU_num)
	ld	 r12,r13,lo16(_RUU_num)
	or.u	 r13,r0,hi16(_RUU_size)
	ld	 r13,r13,lo16(_RUU_size)
	cmp	 r12,r12,r13
	bb0.n	 lt,r12,@L1731
	or.u	 r13,r0,hi16(_LSQ_num)
	ld	 r12,r13,lo16(_LSQ_num)
	or.u	 r13,r0,hi16(_LSQ_size)
	ld	 r13,r13,lo16(_LSQ_size)
	cmp	 r12,r12,r13
	bb0.n	 lt,r12,@L1731
	or.u	 r13,r0,hi16(_fetch_num)
@L9339:
	ld	 r13,r13,lo16(_fetch_num)
	bcnd.n	 ne0,r13,@L8656
	or.u	 r13,r0,hi16(_ruu_include_spec)
@L1731:
	bcnd.n	 ne0,r22,@L8657
	or.u	 r13,r0,hi16(_dlite_check)
	ld	 r13,r13,lo16(_dlite_check)
	bcnd.n	 ne0,r13,@L8661
	or.u	 r13,r0,hi16(_dlite_active)
	ld	 r13,r13,lo16(_dlite_active)
	bcnd	 eq0,r13,@L8657
@L8661:
	ld	 r15,r31,220
	bcnd.n	 eq0,r15,@L8662
	or	 r3,r0,1
	or	 r3,r0,2
@L8662:
	or.u	 r13,r0,hi16(_sim_num_insn)
	or.u	 r25,r0,hi16(_sim_cycle)
	ld.d	 r6,r13,lo16(_sim_num_insn)
	or	 r2,r0,0
	ld.d	 r8,r25,lo16(_sim_cycle)
	bsr.n	 ___check_break
	or	 r4,r0,r24
	bcnd.n	 eq0,r2,@L8657
	or.u	 r13,r0,hi16(_regs_PC)
	ld.d	 r4,r25,lo16(_sim_cycle)
	ld	 r2,r13,lo16(_regs_PC)
	bsr.n	 _dlite_main
	or	 r3,r0,0
@L8657:
@Lte17:
	ld	 r1,r31,80
	ld.d	 r24,r31,72
	ld.d	 r22,r31,64
	ld.d	 r20,r31,56
	ld.d	 r18,r31,48
	ld.d	 r16,r31,40
	ld.d	 r14,r31,32
	jmp.n	 r1
	addu	 r31,r31,256
	def	 @L9782,@L8627-@L9783
	def	 @L9780,@L1739-@L9781

data
	align	 8
@LC807:
	string	 "sim-outorder.c\000"
	align	 8
@LC808:
	string	 "fetch_init\000"
	align	 8
@LC809:
	string	 "out of virtual memory\000"
text
	align	 8
_fetch_init:
	or.u	 r13,r0,hi16(_ruu_ifq_size)
	ld	 r2,r13,lo16(_ruu_ifq_size)
	subu	 r31,r31,48
	st	 r1,r31,36
@Ltb18:
	bsr.n	 _calloc
	or	 r3,r0,24
	or.u	 r13,r0,hi16(_fetch_data)
	bcnd.n	 ne0,r2,@L9787
	st	 r2,r13,lo16(_fetch_data)
	or.u	 r2,r0,hi16(@LC807)
	or.u	 r3,r0,hi16(@LC808)
	or.u	 r5,r0,hi16(@LC809)
	or	 r4,r0,3741
	or	 r2,r2,lo16(@LC807)
	or	 r3,r3,lo16(@LC808)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC809)
	align	 4
@L9787:
@Lte18:
	ld	 r1,r31,36
	or.u	 r13,r0,hi16(_fetch_num)
	st	 r0,r13,lo16(_fetch_num)
	or.u	 r13,r0,hi16(_fetch_head)
	st	 r0,r13,lo16(_fetch_head)
	or.u	 r13,r0,hi16(_fetch_tail)
	addu	 r31,r31,48
	jmp.n	 r1
	st	 r0,r13,lo16(_fetch_tail)

data
	align	 8
@LC810:
	string	 "** fetch stage state **\n\000"
	align	 8
@LC811:
	string	 "spec_mode: %s\n\000"
	align	 8
@LC812:
	string	 "t\000"
	align	 8
@LC813:
	string	 "f\000"
	align	 8
@LC814:
	string	 "pred_PC: 0x%08x, recover_PC: 0x%08x\n\000"
	align	 8
@LC815:
	string	 "fetch_regs_PC: 0x%08x, fetch_pred_PC: 0x%08x\n\000"
	align	 8
@LC816:
	string	 "\n\000"
	align	 8
@LC817:
	string	 "** fetch queue contents **\n\000"
	align	 8
@LC818:
	string	 "fetch_num: %d\n\000"
	align	 8
@LC819:
	string	 "fetch_head: %d, fetch_tail: %d\n\000"
	align	 8
@LC820:
	string	 "idx: %2d: inst: `\000"
	align	 8
@LC821:
	string	 "'\n\000"
	align	 8
@LC822:
	string	 "         regs_PC: 0x%08x, pred_PC: 0x%08x\n\000"
text
	align	 8
	global	 _fetch_dump
_fetch_dump:
	subu	 r31,r31,96
	st	 r1,r31,80
	st	 r17,r31,44
	or.u	 r3,r0,hi16(@LC810)
	st.d	 r18,r31,48
	or	 r19,r0,r2
	st.d	 r24,r31,72
	or	 r3,r3,lo16(@LC810)
	st.d	 r22,r31,64
	bsr.n	 _fprintf
	st.d	 r20,r31,56
@Ltb19:
	or.u	 r13,r0,hi16(_spec_mode)
	ld	 r12,r13,lo16(_spec_mode)
	or.u	 r13,r0,hi16(@LC811)
	bcnd.n	 eq0,r12,@L9796
	or	 r3,r13,lo16(@LC811)
	or.u	 r13,r0,hi16(@LC812)
	br.n	 @L9797
	or	 r4,r13,lo16(@LC812)
	align	 4
@L9796:
	or.u	 r13,r0,hi16(@LC813)
	or	 r4,r13,lo16(@LC813)
@L9797:
	bsr.n	 _fprintf
	or	 r2,r0,r19
	or.u	 r13,r0,hi16(_pred_PC)
	or	 r2,r0,r19
	ld	 r4,r13,lo16(_pred_PC)
	or.u	 r13,r0,hi16(_recover_PC)
	or.u	 r3,r0,hi16(@LC814)
	ld	 r5,r13,lo16(_recover_PC)
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC814)
	or.u	 r13,r0,hi16(_fetch_regs_PC)
	or	 r2,r0,r19
	ld	 r4,r13,lo16(_fetch_regs_PC)
	or.u	 r13,r0,hi16(_fetch_pred_PC)
	or.u	 r3,r0,hi16(@LC815)
	ld	 r5,r13,lo16(_fetch_pred_PC)
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC815)
	or.u	 r3,r0,hi16(@LC816)
	or	 r2,r0,r19
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC816)
	or.u	 r3,r0,hi16(@LC817)
	or	 r2,r0,r19
	or	 r3,r3,lo16(@LC817)
	bsr.n	 _fprintf
	or.u	 r24,r0,hi16(_fetch_num)
	ld	 r4,r24,lo16(_fetch_num)
	or.u	 r3,r0,hi16(@LC818)
	or	 r2,r0,r19
	or	 r3,r3,lo16(@LC818)
	bsr.n	 _fprintf
	or.u	 r25,r0,hi16(_fetch_head)
	ld	 r4,r25,lo16(_fetch_head)
	or	 r2,r0,r19
	or.u	 r13,r0,hi16(_fetch_tail)
	or.u	 r3,r0,hi16(@LC819)
	ld	 r5,r13,lo16(_fetch_tail)
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC819)
	ld	 r18,r24,lo16(_fetch_num)
	ld	 r20,r25,lo16(_fetch_head)
	bcnd.n	 eq0,r18,@L9799
	or.u	 r17,r0,hi16(_fetch_data)
	bcnd.n	 le0,r18,@L9804
	mask	 r13,r18,1
	bcnd.n	 eq0,r13,@L9811
	or	 r2,r0,r19
@L9804:
	or	 r2,r0,r19
	or.u	 r3,r0,hi16(@LC820)
	or	 r4,r0,r20
	or	 r3,r3,lo16(@LC820)
	bsr.n	 _fprintf
	mul	 r25,r20,24
	ld	 r13,r17,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r0,r31
	or	 r5,r0,r19
	bsr.n	 _ss_print_insn
	st	 r13,r31,4
	or.u	 r3,r0,hi16(@LC821)
	or	 r2,r0,r19
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC821)
	ld	 r13,r17,lo16(_fetch_data)
	addu	 r13,r13,r25
	or	 r2,r0,r19
	ld	 r4,r13,8
	or.u	 r3,r0,hi16(@LC822)
	ld	 r5,r13,12
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC822)
	or.u	 r13,r0,hi16(_ruu_ifq_size)
	ld	 r13,r13,lo16(_ruu_ifq_size)
	addu	 r12,r20,1
	subu	 r13,r13,1
	subu	 r18,r18,1
	bcnd.n	 eq0,r18,@L9799
	and	 r20,r12,r13
@L9800:
	or	 r2,r0,r19
@L9811:
	or.u	 r22,r0,hi16(@LC820)
	or	 r4,r0,r20
	or	 r3,r22,lo16(@LC820)
	bsr.n	 _fprintf
	mul	 r25,r20,24
	ld	 r13,r17,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r0,r31
	or	 r5,r0,r19
	bsr.n	 _ss_print_insn
	st	 r13,r31,4
	or.u	 r21,r0,hi16(@LC821)
	or	 r2,r0,r19
	bsr.n	 _fprintf
	or	 r3,r21,lo16(@LC821)
	ld	 r13,r17,lo16(_fetch_data)
	addu	 r13,r13,r25
	or	 r2,r0,r19
	ld	 r4,r13,8
	or.u	 r23,r0,hi16(@LC822)
	ld	 r5,r13,12
	or	 r3,r23,lo16(@LC822)
	bsr.n	 _fprintf
	or.u	 r24,r0,hi16(_ruu_ifq_size)
	ld	 r13,r24,lo16(_ruu_ifq_size)
	addu	 r12,r20,1
	subu	 r13,r13,1
	or	 r2,r0,r19
	and	 r20,r12,r13
	or	 r3,r22,lo16(@LC820)
	or	 r4,r0,r20
	bsr.n	 _fprintf
	mul	 r25,r20,24
	ld	 r13,r17,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r0,r31
	or	 r5,r0,r19
	bsr.n	 _ss_print_insn
	st	 r13,r31,4
	or	 r2,r0,r19
	bsr.n	 _fprintf
	or	 r3,r21,lo16(@LC821)
	ld	 r13,r17,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	or	 r2,r0,r19
	ld	 r5,r13,12
	bsr.n	 _fprintf
	or	 r3,r23,lo16(@LC822)
	ld	 r13,r24,lo16(_ruu_ifq_size)
	addu	 r12,r20,1
	subu	 r13,r13,1
	subu	 r18,r18,2
	bcnd.n	 ne0,r18,@L9800
	and	 r20,r12,r13
@L9799:
@Lte19:
	ld	 r1,r31,80
	ld	 r17,r31,44
	ld.d	 r24,r31,72
	ld.d	 r22,r31,64
	ld.d	 r20,r31,56
	ld.d	 r18,r31,48
	jmp.n	 r1
	addu	 r31,r31,96

data
	align	 4
_last_inst_missed:
	word	 0
	align	 4
_last_inst_tmissed:
	word	 0
	align	 8
@LC823:
	string	 "IF\000"
	align	 8
@LC824:
	string	 "help\000"
	align	 8
@LC825:
	string	 "mstate commands:\n\n    mstate help   - show all "
	string	 "machine-specific commands (this list)\n    mstate"
	string	 " stats  - dump all statistical variables\n    mst"
	string	 "ate res    - dump current functional unit resourc"
	string	 "e states\n    mstate ruu    - dump contents of th"
	string	 "e register update unit\n    mstate lsq    - dump "
	string	 "contents of the load/store queue\n    mstate even"
	string	 "tq - dump contents of event queue\n    mstate rea"
	string	 "dyq - dump contents of ready instruction queue\n "
	string	 "   mstate cv     - dump contents of the register "
	string	 "create vector\n    mstate rspec  - dump contents "
	string	 "of speculative regs\n    mstate mspec  - dump con"
	string	 "tents of speculative memory\n    mstate fetch  - "
	string	 "dump contents of fetch stage registers and fetch "
	string	 "queue\n\n\000"
	align	 8
@LC826:
	string	 "stats\000"
	align	 8
@LC827:
	string	 "res\000"
	align	 8
@LC828:
	string	 "ruu\000"
	align	 8
@LC829:
	string	 "lsq\000"
	align	 8
@LC830:
	string	 "eventq\000"
	align	 8
@LC831:
	string	 "readyq\000"
	align	 8
@LC832:
	string	 "cv\000"
	align	 8
@LC833:
	string	 "rspec\000"
	align	 8
@LC834:
	string	 "mspec\000"
	align	 8
@LC835:
	string	 "fetch\000"
	align	 8
@LC836:
	string	 "unknown mstate command\000"
text
	align	 8
_simoo_mstate_obj:
	subu	 r31,r31,112
	st	 r1,r31,96
	st	 r15,r31,52
	st.d	 r24,r31,88
	st.d	 r22,r31,80
	st.d	 r20,r31,72
	or	 r20,r0,r2
	st.d	 r18,r31,64
	or	 r25,r0,r3
	bcnd.n	 eq0,r25,@L9838
	st.d	 r16,r31,56
@Ltb20:
	or.u	 r3,r0,hi16(@LC824)
	or	 r2,r0,r25
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC824)
	bcnd.n	 ne0,r2,@L9837
	or.u	 r3,r0,hi16(@LC826)
@L9838:
	or.u	 r3,r0,hi16(@LC825)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC825)
	bsr.n	 _fprintf
	addu	 r1,r1,@L10089
@L10090:
	align	 4
@L9837:
	or	 r2,r0,r25
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC826)
	bcnd.n	 ne0,r2,@L9840
	or.u	 r3,r0,hi16(@LC827)
	or	 r2,r0,r20
	bsr.n	 _sim_print_stats
	addu	 r1,r1,@L10091
@L10092:
	align	 4
@L9840:
	or	 r2,r0,r25
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC827)
	bcnd.n	 ne0,r2,@L9842
	or.u	 r3,r0,hi16(@LC828)
	or.u	 r13,r0,hi16(_fu_pool)
	ld	 r2,r13,lo16(_fu_pool)
	or	 r3,r0,r20
	bsr.n	 _res_dump
	addu	 r1,r1,@L10093
@L10094:
	align	 4
@L9842:
	or	 r2,r0,r25
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC828)
	bcnd.n	 ne0,r2,@L9844
	or	 r2,r0,r25
	or.u	 r3,r0,hi16(@LC343)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC343)
	bsr.n	 _fprintf
	or.u	 r24,r0,hi16(_RUU_head)
	ld	 r4,r24,lo16(_RUU_head)
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_RUU_tail)
	or.u	 r3,r0,hi16(@LC344)
	ld	 r5,r13,lo16(_RUU_tail)
	or	 r3,r3,lo16(@LC344)
	bsr.n	 _fprintf
	or.u	 r25,r0,hi16(_RUU_num)
	ld	 r4,r25,lo16(_RUU_num)
	or.u	 r3,r0,hi16(@LC345)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC345)
	ld	 r23,r25,lo16(_RUU_num)
	ld	 r24,r24,lo16(_RUU_head)
	bcnd.n	 eq0,r23,@L9839
	or.u	 r13,r0,hi16(@LC322)
	or.u	 r12,r0,hi16(@LC329)
	or	 r18,r13,lo16(@LC322)
	or	 r19,r12,lo16(@LC329)
	or.u	 r13,r0,hi16(@LC333)
	or.u	 r12,r0,hi16(@LC340)
	or	 r21,r13,lo16(@LC333)
	or	 r22,r12,lo16(@LC340)
@L9848:
	or.u	 r16,r0,0xaaaa
	or	 r16,r16,0xaaab
	mul	 r12,r24,96
	or.u	 r13,r0,hi16(_RUU)
	or	 r2,r0,r20
	ld	 r13,r13,lo16(_RUU)
	or.u	 r3,r0,hi16(@LC318)
	mul	 r4,r12,r16
	addu	 r25,r13,r12
	or.u	 r13,r0,hi16(_ss_op2name)
	ld	 r12,r25,8
	or	 r13,r13,lo16(_ss_op2name)
	or	 r3,r3,lo16(@LC318)
	ld	 r5,r13[r12]
	bsr.n	 _fprintf
	ext	 r4,r4,0<5>
	ld	 r4,r25,12
	ld	 r13,r0,r25
	ld	 r12,r25,4
	st	 r13,r0,r31
	or	 r5,r0,r20
	bsr.n	 _ss_print_insn
	st	 r12,r31,4
	or.u	 r3,r0,hi16(@LC320)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC320)
	ld	 r4,r25,12
	or	 r2,r0,r20
	ld	 r5,r25,16
	or.u	 r3,r0,hi16(@LC321)
	ld	 r6,r25,20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC321)
	ld	 r13,r25,24
	bcnd.n	 eq0,r13,@L9852
	or.u	 r13,r0,hi16(@LC323)
	br.n	 @L9853
	or	 r4,r13,lo16(@LC323)
	align	 4
@L9852:
	or.u	 r13,r0,hi16(@LC324)
	or	 r4,r13,lo16(@LC324)
@L9853:
	ld	 r13,r25,28
	bcnd.n	 eq0,r13,@L9854
	or.u	 r13,r0,hi16(@LC325)
	br.n	 @L9855
	or	 r5,r13,lo16(@LC325)
	align	 4
@L9854:
	or.u	 r13,r0,hi16(@LC326)
	or	 r5,r13,lo16(@LC326)
@L9855:
	ld	 r13,r25,32
	bcnd.n	 eq0,r13,@L9856
	or.u	 r13,r0,hi16(@LC327)
	br.n	 @L9857
	or	 r6,r13,lo16(@LC327)
	align	 4
@L9856:
	or.u	 r13,r0,hi16(@LC328)
	or	 r6,r13,lo16(@LC328)
@L9857:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r18
	ld	 r13,r25,36
	bcnd.n	 eq0,r13,@L9858
	or.u	 r13,r0,hi16(@LC330)
	br.n	 @L9859
	or	 r4,r13,lo16(@LC330)
	align	 4
@L9858:
	or.u	 r13,r0,hi16(@LC331)
	or	 r4,r13,lo16(@LC331)
@L9859:
	ld	 r5,r25,40
	or	 r2,r0,r20
	ld	 r6,r25,44
	bsr.n	 _fprintf
	or	 r3,r0,r19
	or	 r2,r0,r20
	ld	 r4,r25,48
	or.u	 r3,r0,hi16(@LC332)
	ld	 r5,r25,52
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC332)
	ld	 r13,r25,56
	bcnd.n	 eq0,r13,@L9860
	or.u	 r13,r0,hi16(@LC334)
	br.n	 @L9861
	or	 r4,r13,lo16(@LC334)
	align	 4
@L9860:
	or.u	 r13,r0,hi16(@LC335)
	or	 r4,r13,lo16(@LC335)
@L9861:
	ld	 r13,r25,60
	bcnd.n	 eq0,r13,@L9862
	or.u	 r13,r0,hi16(@LC336)
	br.n	 @L9863
	or	 r5,r13,lo16(@LC336)
	align	 4
@L9862:
	or.u	 r13,r0,hi16(@LC337)
	or	 r5,r13,lo16(@LC337)
@L9863:
	ld	 r13,r25,64
	bcnd.n	 eq0,r13,@L9864
	or.u	 r13,r0,hi16(@LC338)
	br.n	 @L9865
	or	 r6,r13,lo16(@LC338)
	align	 4
@L9864:
	or.u	 r13,r0,hi16(@LC339)
	or	 r6,r13,lo16(@LC339)
@L9865:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r21
	ld	 r13,r25,84
	bcnd	 eq0,r13,@L9866
	ld	 r13,r25,88
	bcnd	 eq0,r13,@L9866
	ld	 r13,r25,92
	bcnd.n	 eq0,r13,@L9866
	or.u	 r13,r0,hi16(@LC341)
	br.n	 @L9867
	or	 r4,r13,lo16(@LC341)
	align	 4
@L9866:
	or.u	 r13,r0,hi16(@LC342)
	or	 r4,r13,lo16(@LC342)
@L9867:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r22
	or.u	 r13,r0,hi16(_RUU_size)
	ld	 r10,r13,lo16(_RUU_size)
	addu	 r12,r24,1
	or	 r9,r13,lo16(_RUU_size)
	bcnd.n	 gt0,r10,@L9869
	subu	 r11,r0,r10
	bcnd.n	 ge0,r12,@L9870
	subu	 r13,r0,r12
	divu	 r11,r13,r11
	bcnd	 ne0,r10,@L9868
@L9872:
	tb0	 0,r0,503
	br	 @L9868
	align	 4
@L9870:
	divu	 r11,r12,r11
	bcnd	 eq0,r10,@L9872
	subu	 r11,r0,r11
	br	 @L9868
	align	 4
@L9869:
	bcnd.n	 ge0,r12,@L9871
	subu	 r13,r0,r12
	divu	 r11,r13,r10
	br.n	 @L9868
	subu	 r11,r0,r11
	align	 4
@L9871:
	divu	 r11,r12,r10
@L9868:
	ld	 r13,r0,r9
	mul	 r13,r11,r13
	subu	 r23,r23,1
	bcnd.n	 ne0,r23,@L9848
	subu	 r24,r12,r13
	br.n	 @L10034
	or	 r2,r0,0
	align	 4
@L9844:
	or.u	 r3,r0,hi16(@LC829)
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC829)
	bcnd.n	 ne0,r2,@L9875
	or	 r2,r0,r25
	or.u	 r3,r0,hi16(@LC349)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC349)
	bsr.n	 _fprintf
	or.u	 r24,r0,hi16(_LSQ_head)
	ld	 r4,r24,lo16(_LSQ_head)
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_LSQ_tail)
	or.u	 r3,r0,hi16(@LC350)
	ld	 r5,r13,lo16(_LSQ_tail)
	or	 r3,r3,lo16(@LC350)
	bsr.n	 _fprintf
	or.u	 r25,r0,hi16(_LSQ_num)
	ld	 r4,r25,lo16(_LSQ_num)
	or.u	 r3,r0,hi16(@LC351)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC351)
	ld	 r23,r25,lo16(_LSQ_num)
	ld	 r24,r24,lo16(_LSQ_head)
	bcnd.n	 eq0,r23,@L9839
	or.u	 r13,r0,hi16(@LC322)
	or.u	 r12,r0,hi16(@LC329)
	or	 r18,r13,lo16(@LC322)
	or	 r19,r12,lo16(@LC329)
	or.u	 r13,r0,hi16(@LC333)
	or.u	 r12,r0,hi16(@LC340)
	or	 r21,r13,lo16(@LC333)
	or	 r22,r12,lo16(@LC340)
@L9879:
	or.u	 r17,r0,0xaaaa
	or	 r17,r17,0xaaab
	mul	 r12,r24,96
	or.u	 r13,r0,hi16(_LSQ)
	or	 r2,r0,r20
	ld	 r13,r13,lo16(_LSQ)
	or.u	 r3,r0,hi16(@LC318)
	mul	 r4,r12,r17
	addu	 r25,r13,r12
	or.u	 r13,r0,hi16(_ss_op2name)
	ld	 r12,r25,8
	or	 r13,r13,lo16(_ss_op2name)
	or	 r3,r3,lo16(@LC318)
	ld	 r5,r13[r12]
	bsr.n	 _fprintf
	ext	 r4,r4,0<5>
	ld	 r4,r25,12
	ld	 r13,r0,r25
	ld	 r12,r25,4
	st	 r13,r0,r31
	or	 r5,r0,r20
	bsr.n	 _ss_print_insn
	st	 r12,r31,4
	or.u	 r3,r0,hi16(@LC320)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC320)
	ld	 r4,r25,12
	or	 r2,r0,r20
	ld	 r5,r25,16
	or.u	 r3,r0,hi16(@LC321)
	ld	 r6,r25,20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC321)
	ld	 r13,r25,24
	bcnd.n	 eq0,r13,@L9883
	or.u	 r13,r0,hi16(@LC323)
	br.n	 @L9884
	or	 r4,r13,lo16(@LC323)
	align	 4
@L9883:
	or.u	 r13,r0,hi16(@LC324)
	or	 r4,r13,lo16(@LC324)
@L9884:
	ld	 r13,r25,28
	bcnd.n	 eq0,r13,@L9885
	or.u	 r13,r0,hi16(@LC325)
	br.n	 @L9886
	or	 r5,r13,lo16(@LC325)
	align	 4
@L9885:
	or.u	 r13,r0,hi16(@LC326)
	or	 r5,r13,lo16(@LC326)
@L9886:
	ld	 r13,r25,32
	bcnd.n	 eq0,r13,@L9887
	or.u	 r13,r0,hi16(@LC327)
	br.n	 @L9888
	or	 r6,r13,lo16(@LC327)
	align	 4
@L9887:
	or.u	 r13,r0,hi16(@LC328)
	or	 r6,r13,lo16(@LC328)
@L9888:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r18
	ld	 r13,r25,36
	bcnd.n	 eq0,r13,@L9889
	or.u	 r13,r0,hi16(@LC330)
	br.n	 @L9890
	or	 r4,r13,lo16(@LC330)
	align	 4
@L9889:
	or.u	 r13,r0,hi16(@LC331)
	or	 r4,r13,lo16(@LC331)
@L9890:
	ld	 r5,r25,40
	or	 r2,r0,r20
	ld	 r6,r25,44
	bsr.n	 _fprintf
	or	 r3,r0,r19
	or	 r2,r0,r20
	ld	 r4,r25,48
	or.u	 r3,r0,hi16(@LC332)
	ld	 r5,r25,52
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC332)
	ld	 r13,r25,56
	bcnd.n	 eq0,r13,@L9891
	or.u	 r13,r0,hi16(@LC334)
	br.n	 @L9892
	or	 r4,r13,lo16(@LC334)
	align	 4
@L9891:
	or.u	 r13,r0,hi16(@LC335)
	or	 r4,r13,lo16(@LC335)
@L9892:
	ld	 r13,r25,60
	bcnd.n	 eq0,r13,@L9893
	or.u	 r13,r0,hi16(@LC336)
	br.n	 @L9894
	or	 r5,r13,lo16(@LC336)
	align	 4
@L9893:
	or.u	 r13,r0,hi16(@LC337)
	or	 r5,r13,lo16(@LC337)
@L9894:
	ld	 r13,r25,64
	bcnd.n	 eq0,r13,@L9895
	or.u	 r13,r0,hi16(@LC338)
	br.n	 @L9896
	or	 r6,r13,lo16(@LC338)
	align	 4
@L9895:
	or.u	 r13,r0,hi16(@LC339)
	or	 r6,r13,lo16(@LC339)
@L9896:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r21
	ld	 r13,r25,84
	bcnd	 eq0,r13,@L9897
	ld	 r13,r25,88
	bcnd	 eq0,r13,@L9897
	ld	 r13,r25,92
	bcnd.n	 eq0,r13,@L9897
	or.u	 r13,r0,hi16(@LC341)
	br.n	 @L9898
	or	 r4,r13,lo16(@LC341)
	align	 4
@L9897:
	or.u	 r13,r0,hi16(@LC342)
	or	 r4,r13,lo16(@LC342)
@L9898:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r22
	or.u	 r13,r0,hi16(_LSQ_size)
	ld	 r10,r13,lo16(_LSQ_size)
	addu	 r12,r24,1
	or	 r9,r13,lo16(_LSQ_size)
	bcnd.n	 gt0,r10,@L9900
	subu	 r11,r0,r10
	bcnd.n	 ge0,r12,@L9901
	subu	 r13,r0,r12
	divu	 r11,r13,r11
	bcnd	 ne0,r10,@L9899
@L9903:
	tb0	 0,r0,503
	br	 @L9899
	align	 4
@L9901:
	divu	 r11,r12,r11
	bcnd	 eq0,r10,@L9903
	subu	 r11,r0,r11
	br	 @L9899
	align	 4
@L9900:
	bcnd.n	 ge0,r12,@L9902
	subu	 r13,r0,r12
	divu	 r11,r13,r10
	br.n	 @L9899
	subu	 r11,r0,r11
	align	 4
@L9902:
	divu	 r11,r12,r10
@L9899:
	ld	 r13,r0,r9
	mul	 r13,r11,r13
	subu	 r23,r23,1
	bcnd.n	 ne0,r23,@L9879
	subu	 r24,r12,r13
	br.n	 @L10034
	or	 r2,r0,0
	align	 4
@L9875:
	or.u	 r3,r0,hi16(@LC830)
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC830)
	bcnd.n	 ne0,r2,@L9906
	or	 r2,r0,r25
	or.u	 r3,r0,hi16(@LC355)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC355)
	or.u	 r13,r0,hi16(_event_queue)
	ld	 r23,r13,lo16(_event_queue)
	bcnd.n	 eq0,r23,@L9839
	or.u	 r13,r0,hi16(@LC356)
	or.u	 r12,r0,hi16(@LC322)
	or	 r18,r13,lo16(@LC356)
	or	 r19,r12,lo16(@LC322)
	or.u	 r13,r0,hi16(@LC329)
	or.u	 r12,r0,hi16(@LC333)
	or	 r21,r13,lo16(@LC329)
	or	 r22,r12,lo16(@LC333)
@L9911:
	ld	 r11,r23,4
	ld	 r13,r23,8
	ld	 r12,r11,44
	cmp	 r13,r13,r12
	bb1.n	 ne,r13,@L9910
	or	 r25,r0,r11
	ld	 r13,r25,24
	bcnd.n	 ne0,r13,@L9913
	or.u	 r13,r0,hi16(_LSQ)
	or.u	 r13,r0,hi16(_RUU)
	or.u	 r16,r0,0xaaaa
	or	 r16,r16,0xaaab
	ld	 r13,r13,lo16(_RUU)
	subu	 r13,r25,r13
	mul	 r13,r13,r16
	br.n	 @L10081
	ext	 r24,r13,0<5>
	align	 4
@L9913:
	or.u	 r17,r0,0xaaaa
	or	 r17,r17,0xaaab
	ld	 r13,r13,lo16(_LSQ)
	subu	 r13,r25,r13
	mul	 r13,r13,r17
	ext	 r24,r13,0<5>
@L10081:
	bsr.n	 ___floatdidf
	ld.d	 r2,r23,16
	or	 r6,r0,r2
	or	 r7,r0,r3
	or	 r2,r0,r20
	or	 r3,r0,r18
	bsr.n	 _fprintf
	or	 r4,r0,r24
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_ss_op2name)
	ld	 r12,r25,8
	or	 r13,r13,lo16(_ss_op2name)
	or.u	 r3,r0,hi16(@LC319)
	ld	 r4,r13[r12]
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC319)
	ld	 r4,r25,12
	ld	 r13,r0,r25
	ld	 r12,r25,4
	st	 r13,r0,r31
	or	 r5,r0,r20
	bsr.n	 _ss_print_insn
	st	 r12,r31,4
	or.u	 r3,r0,hi16(@LC320)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC320)
	ld	 r4,r25,12
	or	 r2,r0,r20
	ld	 r5,r25,16
	or.u	 r3,r0,hi16(@LC321)
	ld	 r6,r25,20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC321)
	ld	 r13,r25,24
	bcnd.n	 eq0,r13,@L9920
	or.u	 r13,r0,hi16(@LC323)
	br.n	 @L9921
	or	 r4,r13,lo16(@LC323)
	align	 4
@L9920:
	or.u	 r13,r0,hi16(@LC324)
	or	 r4,r13,lo16(@LC324)
@L9921:
	ld	 r13,r25,28
	bcnd.n	 eq0,r13,@L9922
	or.u	 r13,r0,hi16(@LC325)
	br.n	 @L9923
	or	 r5,r13,lo16(@LC325)
	align	 4
@L9922:
	or.u	 r13,r0,hi16(@LC326)
	or	 r5,r13,lo16(@LC326)
@L9923:
	ld	 r13,r25,32
	bcnd.n	 eq0,r13,@L9924
	or.u	 r13,r0,hi16(@LC327)
	br.n	 @L9925
	or	 r6,r13,lo16(@LC327)
	align	 4
@L9924:
	or.u	 r13,r0,hi16(@LC328)
	or	 r6,r13,lo16(@LC328)
@L9925:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r19
	ld	 r13,r25,36
	bcnd.n	 eq0,r13,@L9926
	or.u	 r13,r0,hi16(@LC330)
	br.n	 @L9927
	or	 r4,r13,lo16(@LC330)
	align	 4
@L9926:
	or.u	 r13,r0,hi16(@LC331)
	or	 r4,r13,lo16(@LC331)
@L9927:
	ld	 r5,r25,40
	or	 r2,r0,r20
	ld	 r6,r25,44
	bsr.n	 _fprintf
	or	 r3,r0,r21
	or	 r2,r0,r20
	ld	 r4,r25,48
	or.u	 r3,r0,hi16(@LC332)
	ld	 r5,r25,52
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC332)
	ld	 r13,r25,56
	bcnd.n	 eq0,r13,@L9928
	or.u	 r13,r0,hi16(@LC334)
	br.n	 @L9929
	or	 r4,r13,lo16(@LC334)
	align	 4
@L9928:
	or.u	 r13,r0,hi16(@LC335)
	or	 r4,r13,lo16(@LC335)
@L9929:
	ld	 r13,r25,60
	bcnd.n	 eq0,r13,@L9930
	or.u	 r13,r0,hi16(@LC336)
	br.n	 @L9931
	or	 r5,r13,lo16(@LC336)
	align	 4
@L9930:
	or.u	 r13,r0,hi16(@LC337)
	or	 r5,r13,lo16(@LC337)
@L9931:
	ld	 r13,r25,64
	bcnd.n	 eq0,r13,@L9932
	or.u	 r13,r0,hi16(@LC338)
	br.n	 @L9933
	or	 r6,r13,lo16(@LC338)
	align	 4
@L9932:
	or.u	 r13,r0,hi16(@LC339)
	or	 r6,r13,lo16(@LC339)
@L9933:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r22
	ld	 r12,r25,84
	or.u	 r13,r0,hi16(@LC340)
	bcnd.n	 eq0,r12,@L9934
	or	 r3,r13,lo16(@LC340)
	ld	 r13,r25,88
	bcnd	 eq0,r13,@L9934
	ld	 r13,r25,92
	bcnd.n	 eq0,r13,@L9934
	or.u	 r13,r0,hi16(@LC341)
	br.n	 @L9935
	or	 r4,r13,lo16(@LC341)
	align	 4
@L9934:
	or.u	 r13,r0,hi16(@LC342)
	or	 r4,r13,lo16(@LC342)
@L9935:
	bsr.n	 _fprintf
	or	 r2,r0,r20
@L9910:
	ld	 r23,r0,r23
	bcnd.n	 ne0,r23,@L9911
	or	 r2,r0,0
	br	 @L10034
	align	 4
@L9906:
	or.u	 r3,r0,hi16(@LC831)
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC831)
	bcnd.n	 ne0,r2,@L9938
	or	 r2,r0,r25
	or.u	 r3,r0,hi16(@LC366)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC366)
	or.u	 r13,r0,hi16(_ready_queue)
	ld	 r24,r13,lo16(_ready_queue)
	bcnd.n	 eq0,r24,@L9839
	or.u	 r13,r0,hi16(@LC322)
	or.u	 r12,r0,hi16(@LC329)
	or	 r19,r13,lo16(@LC322)
	or	 r21,r12,lo16(@LC329)
	or.u	 r13,r0,hi16(@LC333)
	or.u	 r12,r0,hi16(@LC340)
	or	 r22,r13,lo16(@LC333)
	or	 r23,r12,lo16(@LC340)
@L9943:
	ld	 r11,r24,4
	ld	 r13,r24,8
	ld	 r12,r11,44
	cmp	 r13,r13,r12
	bb1.n	 ne,r13,@L9942
	or	 r25,r0,r11
	ld	 r13,r25,24
	bcnd.n	 ne0,r13,@L9945
	or.u	 r13,r0,hi16(_LSQ)
	or.u	 r13,r0,hi16(_RUU)
	or.u	 r16,r0,0xaaaa
	or	 r16,r16,0xaaab
	ld	 r13,r13,lo16(_RUU)
	subu	 r13,r25,r13
	mul	 r13,r13,r16
	br.n	 @L10082
	ext	 r4,r13,0<5>
	align	 4
@L9945:
	or.u	 r17,r0,0xaaaa
	or	 r17,r17,0xaaab
	ld	 r13,r13,lo16(_LSQ)
	subu	 r13,r25,r13
	mul	 r13,r13,r17
	ext	 r4,r13,0<5>
@L10082:
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_ss_op2name)
	ld	 r12,r25,8
	or	 r13,r13,lo16(_ss_op2name)
	or.u	 r3,r0,hi16(@LC318)
	ld	 r5,r13[r12]
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC318)
	ld	 r4,r25,12
	ld	 r13,r0,r25
	ld	 r12,r25,4
	st	 r13,r0,r31
	or	 r5,r0,r20
	bsr.n	 _ss_print_insn
	st	 r12,r31,4
	or.u	 r3,r0,hi16(@LC320)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC320)
	ld	 r4,r25,12
	or	 r2,r0,r20
	ld	 r5,r25,16
	or.u	 r3,r0,hi16(@LC321)
	ld	 r6,r25,20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC321)
	ld	 r13,r25,24
	bcnd.n	 eq0,r13,@L9950
	or.u	 r13,r0,hi16(@LC323)
	br.n	 @L9951
	or	 r4,r13,lo16(@LC323)
	align	 4
@L9950:
	or.u	 r13,r0,hi16(@LC324)
	or	 r4,r13,lo16(@LC324)
@L9951:
	ld	 r13,r25,28
	bcnd.n	 eq0,r13,@L9952
	or.u	 r13,r0,hi16(@LC325)
	br.n	 @L9953
	or	 r5,r13,lo16(@LC325)
	align	 4
@L9952:
	or.u	 r13,r0,hi16(@LC326)
	or	 r5,r13,lo16(@LC326)
@L9953:
	ld	 r13,r25,32
	bcnd.n	 eq0,r13,@L9954
	or.u	 r13,r0,hi16(@LC327)
	br.n	 @L9955
	or	 r6,r13,lo16(@LC327)
	align	 4
@L9954:
	or.u	 r13,r0,hi16(@LC328)
	or	 r6,r13,lo16(@LC328)
@L9955:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r19
	ld	 r13,r25,36
	bcnd.n	 eq0,r13,@L9956
	or.u	 r13,r0,hi16(@LC330)
	br.n	 @L9957
	or	 r4,r13,lo16(@LC330)
	align	 4
@L9956:
	or.u	 r13,r0,hi16(@LC331)
	or	 r4,r13,lo16(@LC331)
@L9957:
	ld	 r5,r25,40
	or	 r2,r0,r20
	ld	 r6,r25,44
	bsr.n	 _fprintf
	or	 r3,r0,r21
	or	 r2,r0,r20
	ld	 r4,r25,48
	or.u	 r3,r0,hi16(@LC332)
	ld	 r5,r25,52
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC332)
	ld	 r13,r25,56
	bcnd.n	 eq0,r13,@L9958
	or.u	 r13,r0,hi16(@LC334)
	br.n	 @L9959
	or	 r4,r13,lo16(@LC334)
	align	 4
@L9958:
	or.u	 r13,r0,hi16(@LC335)
	or	 r4,r13,lo16(@LC335)
@L9959:
	ld	 r13,r25,60
	bcnd.n	 eq0,r13,@L9960
	or.u	 r13,r0,hi16(@LC336)
	br.n	 @L9961
	or	 r5,r13,lo16(@LC336)
	align	 4
@L9960:
	or.u	 r13,r0,hi16(@LC337)
	or	 r5,r13,lo16(@LC337)
@L9961:
	ld	 r13,r25,64
	bcnd.n	 eq0,r13,@L9962
	or.u	 r13,r0,hi16(@LC338)
	br.n	 @L9963
	or	 r6,r13,lo16(@LC338)
	align	 4
@L9962:
	or.u	 r13,r0,hi16(@LC339)
	or	 r6,r13,lo16(@LC339)
@L9963:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r22
	ld	 r13,r25,84
	bcnd	 eq0,r13,@L9964
	ld	 r13,r25,88
	bcnd	 eq0,r13,@L9964
	ld	 r13,r25,92
	bcnd.n	 eq0,r13,@L9964
	or.u	 r13,r0,hi16(@LC341)
	br.n	 @L9965
	or	 r4,r13,lo16(@LC341)
	align	 4
@L9964:
	or.u	 r13,r0,hi16(@LC342)
	or	 r4,r13,lo16(@LC342)
@L9965:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r23
@L9942:
	ld	 r24,r0,r24
	bcnd.n	 ne0,r24,@L9943
	or	 r2,r0,0
	br	 @L10034
	align	 4
@L9938:
	or.u	 r3,r0,hi16(@LC832)
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC832)
	bcnd.n	 ne0,r2,@L9968
	or	 r2,r0,r25
	or.u	 r3,r0,hi16(@LC443)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC443)
	or.u	 r13,r0,hi16(_use_spec_cv)
	or	 r25,r0,0
	or.u	 r12,r0,hi16(@LC445)
	or	 r19,r13,lo16(_use_spec_cv)
	or.u	 r13,r0,hi16(_dep_names)
	or	 r21,r12,lo16(@LC445)
	or.u	 r12,r0,hi16(_spec_create_vector)
	or	 r22,r13,lo16(_dep_names)
	or.u	 r13,r0,hi16(_create_vector)
	or	 r24,r12,lo16(_spec_create_vector)
	or	 r23,r13,lo16(_create_vector)
@L9973:
	bcnd.n	 ge0,r25,@L9976
	or	 r12,r0,r25
	addu	 r12,r25,31
@L9976:
	ext	 r12,r12,0<5>
	mak	 r11,r12,0<5>
	or	 r13,r0,1
	subu	 r11,r25,r11
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L9974
	ld	 r13,r0,r24
	ld	 r12,r24,4
	br.n	 @L10083
	st	 r13,r31,104
	align	 4
@L9974:
	ld	 r13,r0,r23
	ld	 r12,r23,4
	st	 r13,r31,104
@L10083:
	st	 r12,r31,108
	ld	 r13,r31,104
	bcnd	 ne0,r13,@L9978
	ld	 r4,r22[r25]
	or.u	 r3,r0,hi16(@LC444)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC444)
	bsr.n	 _fprintf
	addu	 r1,r1,@L10095
@L10096:
	align	 4
@L9978:
	ld	 r13,r13,24
	ld	 r4,r22[r25]
	bcnd.n	 eq0,r13,@L9980
	or.u	 r13,r0,hi16(@LC446)
	br.n	 @L9981
	or	 r5,r13,lo16(@LC446)
	align	 4
@L9980:
	or.u	 r13,r0,hi16(@LC447)
	or	 r5,r13,lo16(@LC447)
@L9981:
	ld	 r12,r31,104
	ld	 r13,r12,24
	bcnd.n	 ne0,r13,@L9982
	or.u	 r13,r0,hi16(_LSQ)
	or.u	 r13,r0,hi16(_RUU)
	or.u	 r16,r0,0xaaaa
	or	 r16,r16,0xaaab
	ld	 r13,r13,lo16(_RUU)
	subu	 r13,r12,r13
	mul	 r13,r13,r16
	br.n	 @L10084
	ext	 r6,r13,0<5>
	align	 4
@L9982:
	or.u	 r17,r0,0xaaaa
	or	 r17,r17,0xaaab
	ld	 r13,r13,lo16(_LSQ)
	subu	 r13,r12,r13
	mul	 r13,r13,r17
	ext	 r6,r13,0<5>
@L10084:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r0,r21
@L9972:
	addu	 r25,r25,1
	addu	 r23,r23,8
	cmp	 r13,r25,69
	bb1.n	 le,r13,@L9973
	addu	 r24,r24,8
	br.n	 @L10034
	or	 r2,r0,0
	align	 4
@L9968:
	or.u	 r3,r0,hi16(@LC833)
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC833)
	bcnd.n	 ne0,r2,@L9986
	or	 r2,r0,r25
	or.u	 r3,r0,hi16(@LC494)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC494)
	or.u	 r13,r0,hi16(_spec_mode)
	ld	 r12,r13,lo16(_spec_mode)
	or.u	 r13,r0,hi16(@LC495)
	bcnd.n	 eq0,r12,@L9988
	or	 r3,r13,lo16(@LC495)
	or.u	 r13,r0,hi16(@LC496)
	br.n	 @L9989
	or	 r4,r13,lo16(@LC496)
	align	 4
@L9988:
	or.u	 r13,r0,hi16(@LC497)
	or	 r4,r13,lo16(@LC497)
@L9989:
	bsr.n	 _fprintf
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_use_spec_R)
	or	 r25,r0,0
	or.u	 r12,r0,hi16(_spec_regs_R)
	or	 r21,r13,lo16(_use_spec_R)
	or.u	 r13,r0,hi16(_dep_names)
	or	 r23,r12,lo16(_spec_regs_R)
	or	 r22,r13,lo16(_dep_names)
@L9993:
	bcnd.n	 ge0,r25,@L10071
	or	 r13,r0,r25
	addu	 r13,r25,31
@L10071:
	ext	 r13,r13,0<5>
	mak	 r12,r13,0<5>
	or	 r24,r0,1
	subu	 r12,r25,r12
	ld	 r13,r21[r13]
	mak	 r12,r24,r12
	and	 r13,r13,r12
	bcnd	 eq0,r13,@L10069
	ld	 r4,r22[r25]
	ld	 r5,r23[r25]
	or.u	 r3,r0,hi16(@LC498)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC498)
	bsr.n	 _fprintf
	or	 r6,r0,r5
@L10069:
	addu	 r11,r25,1
	bcnd.n	 ge0,r11,@L10075
	or	 r13,r0,r11
	addu	 r13,r25,32
@L10075:
	ext	 r13,r13,0<5>
	mak	 r12,r13,0<5>
	subu	 r12,r11,r12
	ld	 r13,r21[r13]
	mak	 r12,r24,r12
	and	 r13,r13,r12
	bcnd	 eq0,r13,@L10073
	ld	 r4,r22[r11]
	ld	 r5,r23[r11]
	or.u	 r3,r0,hi16(@LC498)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC498)
	bsr.n	 _fprintf
	or	 r6,r0,r5
@L10073:
	addu	 r25,r25,2
	cmp	 r13,r25,31
	bb1	 le,r13,@L9993
	or.u	 r13,r0,hi16(_use_spec_F)
	or	 r25,r0,0
	or.u	 r12,r0,hi16(_spec_regs_F)
	or	 r21,r13,lo16(_use_spec_F)
	or.u	 r13,r0,hi16(_dep_names+128)
	or	 r24,r12,lo16(_spec_regs_F)
	or	 r22,r13,lo16(_dep_names+128)
@L10001:
	bcnd.n	 ge0,r25,@L10063
	or	 r13,r0,r25
	addu	 r13,r25,31
@L10063:
	ext	 r13,r13,0<5>
	mak	 r12,r13,0<5>
	or	 r23,r0,1
	subu	 r12,r25,r12
	ld	 r13,r21[r13]
	mak	 r12,r23,r12
	and	 r13,r13,r12
	bcnd	 eq0,r13,@L10061
	ld	 r4,r22[r25]
	ld	 r5,r24[r25]
	ld	 r13,r24[r25]
	or.u	 r3,r0,hi16(@LC499)
	st	 r4,r31,32
	fsub.dss r8,r13,r0
	ext	 r13,r25,0<1>
	or	 r2,r0,r20
	ld.d	 r16,r24[r13]
	or	 r3,r3,lo16(@LC499)
	or	 r6,r0,r5
	bsr.n	 _fprintf
	st.d	 r16,r31,40
@L10061:
	addu	 r11,r25,1
	bcnd.n	 ge0,r11,@L10067
	or	 r13,r0,r11
	addu	 r13,r25,32
@L10067:
	ext	 r13,r13,0<5>
	mak	 r12,r13,0<5>
	subu	 r12,r11,r12
	ld	 r13,r21[r13]
	mak	 r12,r23,r12
	and	 r13,r13,r12
	bcnd	 eq0,r13,@L10065
	ld	 r4,r22[r11]
	ld	 r5,r24[r11]
	ld	 r13,r24[r11]
	or.u	 r3,r0,hi16(@LC499)
	st	 r4,r31,32
	fsub.dss r8,r13,r0
	ext	 r13,r11,0<1>
	or	 r2,r0,r20
	ld.d	 r16,r24[r13]
	or	 r3,r3,lo16(@LC499)
	or	 r6,r0,r5
	bsr.n	 _fprintf
	st.d	 r16,r31,40
@L10065:
	addu	 r25,r25,2
	cmp	 r13,r25,31
	bb1	 le,r13,@L10001
	or.u	 r13,r0,hi16(_use_spec_HI)
	ld	 r13,r13,lo16(_use_spec_HI)
	bcnd.n	 eq0,r13,@L10006
	or.u	 r13,r0,hi16(_spec_regs_HI)
	or.u	 r3,r0,hi16(@LC500)
	ld	 r4,r13,lo16(_spec_regs_HI)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC500)
	bsr.n	 _fprintf
	or	 r5,r0,r4
@L10006:
	or.u	 r13,r0,hi16(_use_spec_LO)
	ld	 r13,r13,lo16(_use_spec_LO)
	bcnd.n	 eq0,r13,@L10007
	or.u	 r13,r0,hi16(_spec_regs_LO)
	or.u	 r3,r0,hi16(@LC501)
	ld	 r4,r13,lo16(_spec_regs_LO)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC501)
	bsr.n	 _fprintf
	or	 r5,r0,r4
@L10007:
	or.u	 r13,r0,hi16(_use_spec_FCC)
	ld	 r13,r13,lo16(_use_spec_FCC)
	bcnd.n	 eq0,r13,@L9839
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	or.u	 r3,r0,hi16(@LC502)
	ld	 r4,r13,lo16(_spec_regs_FCC)
	or	 r3,r3,lo16(@LC502)
	bsr.n	 _fprintf
	addu	 r1,r1,@L10097
@L10098:
	align	 4
@L9986:
	or.u	 r3,r0,hi16(@LC834)
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC834)
	bcnd.n	 ne0,r2,@L10010
	or	 r2,r0,r25
	or.u	 r3,r0,hi16(@LC512)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC512)
	or.u	 r13,r0,hi16(_spec_mode)
	ld	 r12,r13,lo16(_spec_mode)
	or.u	 r13,r0,hi16(@LC513)
	bcnd.n	 eq0,r12,@L10012
	or	 r3,r13,lo16(@LC513)
	or.u	 r13,r0,hi16(@LC514)
	br.n	 @L10013
	or	 r4,r13,lo16(@LC514)
	align	 4
@L10012:
	or.u	 r13,r0,hi16(@LC515)
	or	 r4,r13,lo16(@LC515)
@L10013:
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r24,r0,0
	or.u	 r13,r0,hi16(_store_htable)
	or.u	 r23,r0,hi16(@LC516)
	or	 r22,r13,lo16(_store_htable)
@L10017:
	ld	 r25,r22[r24]
	bcnd.n	 eq0,r25,@L10085
	addu	 r13,r24,1
@L10047:
	ld	 r4,r25,4
	ld.d	 r6,r25,8
	ld	 r8,r25,8
	or	 r2,r0,r20
	ld	 r9,r25,12
	bsr.n	 _fprintf
	or	 r3,r23,lo16(@LC516)
	ld	 r25,r0,r25
	bcnd.n	 ne0,r25,@L10047
	addu	 r13,r24,1
@L10085:
	ld	 r25,r22[r13]
	bcnd.n	 eq0,r25,@L10086
	addu	 r13,r24,2
@L10051:
	ld	 r4,r25,4
	ld.d	 r6,r25,8
	ld	 r8,r25,8
	or	 r2,r0,r20
	ld	 r9,r25,12
	bsr.n	 _fprintf
	or	 r3,r23,lo16(@LC516)
	ld	 r25,r0,r25
	bcnd.n	 ne0,r25,@L10051
	addu	 r13,r24,2
@L10086:
	ld	 r25,r22[r13]
	bcnd.n	 eq0,r25,@L10087
	addu	 r13,r24,3
@L10055:
	ld	 r4,r25,4
	ld.d	 r6,r25,8
	ld	 r8,r25,8
	or	 r2,r0,r20
	ld	 r9,r25,12
	bsr.n	 _fprintf
	or	 r3,r23,lo16(@LC516)
	ld	 r25,r0,r25
	bcnd.n	 ne0,r25,@L10055
	addu	 r13,r24,3
@L10087:
	ld	 r25,r22[r13]
	bcnd	 eq0,r25,@L10057
@L10059:
	ld	 r4,r25,4
	ld.d	 r6,r25,8
	ld	 r8,r25,8
	or	 r2,r0,r20
	ld	 r9,r25,12
	bsr.n	 _fprintf
	or	 r3,r23,lo16(@LC516)
	ld	 r25,r0,r25
	bcnd	 ne0,r25,@L10059
@L10057:
	addu	 r24,r24,4
	cmp	 r13,r24,31
	bb1.n	 le,r13,@L10017
	or	 r2,r0,0
	br	 @L10034
	align	 4
@L10010:
	or.u	 r3,r0,hi16(@LC835)
	bsr.n	 _strcmp
	or	 r3,r3,lo16(@LC835)
	bcnd.n	 ne0,r2,@L10025
	or.u	 r2,r0,hi16(@LC836)
	or.u	 r3,r0,hi16(@LC810)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC810)
	or.u	 r13,r0,hi16(_spec_mode)
	ld	 r12,r13,lo16(_spec_mode)
	or.u	 r13,r0,hi16(@LC811)
	bcnd.n	 eq0,r12,@L10027
	or	 r3,r13,lo16(@LC811)
	or.u	 r13,r0,hi16(@LC812)
	br.n	 @L10028
	or	 r4,r13,lo16(@LC812)
	align	 4
@L10027:
	or.u	 r13,r0,hi16(@LC813)
	or	 r4,r13,lo16(@LC813)
@L10028:
	bsr.n	 _fprintf
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_pred_PC)
	or	 r2,r0,r20
	ld	 r4,r13,lo16(_pred_PC)
	or.u	 r13,r0,hi16(_recover_PC)
	or.u	 r3,r0,hi16(@LC814)
	ld	 r5,r13,lo16(_recover_PC)
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC814)
	or.u	 r13,r0,hi16(_fetch_regs_PC)
	or	 r2,r0,r20
	ld	 r4,r13,lo16(_fetch_regs_PC)
	or.u	 r13,r0,hi16(_fetch_pred_PC)
	or.u	 r3,r0,hi16(@LC815)
	ld	 r5,r13,lo16(_fetch_pred_PC)
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC815)
	or.u	 r3,r0,hi16(@LC816)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC816)
	or.u	 r3,r0,hi16(@LC817)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC817)
	bsr.n	 _fprintf
	or.u	 r24,r0,hi16(_fetch_num)
	ld	 r4,r24,lo16(_fetch_num)
	or.u	 r3,r0,hi16(@LC818)
	or	 r2,r0,r20
	or	 r3,r3,lo16(@LC818)
	bsr.n	 _fprintf
	or.u	 r25,r0,hi16(_fetch_head)
	ld	 r4,r25,lo16(_fetch_head)
	or	 r2,r0,r20
	or.u	 r13,r0,hi16(_fetch_tail)
	or.u	 r3,r0,hi16(@LC819)
	ld	 r5,r13,lo16(_fetch_tail)
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC819)
	ld	 r18,r24,lo16(_fetch_num)
	ld	 r19,r25,lo16(_fetch_head)
	bcnd.n	 eq0,r18,@L9839
	or.u	 r15,r0,hi16(_fetch_data)
	bcnd.n	 le0,r18,@L10037
	mask	 r13,r18,1
	bcnd.n	 eq0,r13,@L10088
	or	 r2,r0,r20
@L10037:
	or	 r2,r0,r20
	or.u	 r3,r0,hi16(@LC820)
	or	 r4,r0,r19
	or	 r3,r3,lo16(@LC820)
	bsr.n	 _fprintf
	mul	 r25,r19,24
	ld	 r13,r15,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r0,r31
	or	 r5,r0,r20
	bsr.n	 _ss_print_insn
	st	 r13,r31,4
	or.u	 r3,r0,hi16(@LC821)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC821)
	ld	 r13,r15,lo16(_fetch_data)
	addu	 r13,r13,r25
	or	 r2,r0,r20
	ld	 r4,r13,8
	or.u	 r3,r0,hi16(@LC822)
	ld	 r5,r13,12
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC822)
	or.u	 r13,r0,hi16(_ruu_ifq_size)
	ld	 r13,r13,lo16(_ruu_ifq_size)
	addu	 r12,r19,1
	subu	 r13,r13,1
	subu	 r18,r18,1
	bcnd.n	 eq0,r18,@L9839
	and	 r19,r12,r13
@L10031:
	or	 r2,r0,r20
@L10088:
	or.u	 r22,r0,hi16(@LC820)
	or	 r4,r0,r19
	or	 r3,r22,lo16(@LC820)
	bsr.n	 _fprintf
	mul	 r25,r19,24
	ld	 r13,r15,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r0,r31
	or	 r5,r0,r20
	bsr.n	 _ss_print_insn
	st	 r13,r31,4
	or.u	 r21,r0,hi16(@LC821)
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r21,lo16(@LC821)
	ld	 r13,r15,lo16(_fetch_data)
	addu	 r13,r13,r25
	or	 r2,r0,r20
	ld	 r4,r13,8
	or.u	 r23,r0,hi16(@LC822)
	ld	 r5,r13,12
	or	 r3,r23,lo16(@LC822)
	bsr.n	 _fprintf
	or.u	 r24,r0,hi16(_ruu_ifq_size)
	ld	 r13,r24,lo16(_ruu_ifq_size)
	addu	 r12,r19,1
	subu	 r13,r13,1
	or	 r2,r0,r20
	and	 r19,r12,r13
	or	 r3,r22,lo16(@LC820)
	or	 r4,r0,r19
	bsr.n	 _fprintf
	mul	 r25,r19,24
	ld	 r13,r15,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r0,r31
	or	 r5,r0,r20
	bsr.n	 _ss_print_insn
	st	 r13,r31,4
	or	 r2,r0,r20
	bsr.n	 _fprintf
	or	 r3,r21,lo16(@LC821)
	ld	 r13,r15,lo16(_fetch_data)
	addu	 r13,r13,r25
	ld	 r4,r13,8
	or	 r2,r0,r20
	ld	 r5,r13,12
	bsr.n	 _fprintf
	or	 r3,r23,lo16(@LC822)
	ld	 r13,r24,lo16(_ruu_ifq_size)
	addu	 r12,r19,1
	subu	 r13,r13,1
	subu	 r18,r18,2
	bcnd.n	 ne0,r18,@L10031
	and	 r19,r12,r13
	br.n	 @L10034
	or	 r2,r0,0
	align	 4
@L10025:
	br.n	 @L10034
	or	 r2,r2,lo16(@LC836)
	align	 4
@L9839:
	or	 r2,r0,0
@L10034:
@Lte20:
	ld	 r1,r31,96
	ld	 r15,r31,52
	ld.d	 r24,r31,88
	ld.d	 r22,r31,80
	ld.d	 r20,r31,72
	ld.d	 r18,r31,64
	ld.d	 r16,r31,56
	jmp.n	 r1
	addu	 r31,r31,112
	def	 @L10097,@L9839-@L10098
	def	 @L10095,@L9972-@L10096
	def	 @L10093,@L9839-@L10094
	def	 @L10091,@L9839-@L10092
	def	 @L10089,@L9839-@L10090

data
	align	 8
@LC837:
	string	 "sim: ** fast forwarding %d insts **\n\000"
	align	 8
@LC838:
	string	 "ss.def\000"
	align	 8
@LC839:
	string	 "sim_main\000"
	align	 8
@LC840:
	string	 "bad jump alignment\000"
	align	 8
@LC841:
	string	 "ss.def\000"
	align	 8
@LC842:
	string	 "sim_main\000"
	align	 8
@LC843:
	string	 "bad jump alignment\000"
	align	 8
@LC844:
	string	 "ss.def\000"
	align	 8
@LC845:
	string	 "sim_main\000"
	align	 8
@LC846:
	string	 "bad INT register alignment\000"
	align	 8
@LC847:
	string	 "ss.def\000"
	align	 8
@LC848:
	string	 "sim_main\000"
	align	 8
@LC849:
	string	 "bad FP register alignment\000"
	align	 8
@LC850:
	string	 "ss.def\000"
	align	 8
@LC851:
	string	 "sim_main\000"
	align	 8
@LC852:
	string	 "bad INT register alignment\000"
	align	 8
@LC853:
	string	 "ss.def\000"
	align	 8
@LC854:
	string	 "sim_main\000"
	align	 8
@LC855:
	string	 "bad FP register alignment\000"
	align	 8
@LC856:
	string	 "ss.def\000"
	align	 8
@LC857:
	string	 "sim_main\000"
	align	 8
@LC858:
	string	 "bad INT register alignment\000"
	align	 8
@LC859:
	string	 "ss.def\000"
	align	 8
@LC860:
	string	 "sim_main\000"
	align	 8
@LC861:
	string	 "bad FP register alignment\000"
	align	 8
@LC862:
	string	 "ss.def\000"
	align	 8
@LC863:
	string	 "sim_main\000"
	align	 8
@LC864:
	string	 "bad INT register alignment\000"
	align	 8
@LC865:
	string	 "ss.def\000"
	align	 8
@LC866:
	string	 "sim_main\000"
	align	 8
@LC867:
	string	 "bad FP register alignment\000"
	align	 8
@LC868:
	string	 "ss.def\000"
	align	 8
@LC869:
	string	 "sim_main\000"
	align	 8
@LC870:
	string	 "+ overflow\000"
	align	 8
@LC871:
	string	 "ss.def\000"
	align	 8
@LC872:
	string	 "sim_main\000"
	align	 8
@LC873:
	string	 "+ underflow\000"
	align	 8
@LC874:
	string	 "ss.def\000"
	align	 8
@LC875:
	string	 "sim_main\000"
	align	 8
@LC876:
	string	 "+ overflow\000"
	align	 8
@LC877:
	string	 "ss.def\000"
	align	 8
@LC878:
	string	 "sim_main\000"
	align	 8
@LC879:
	string	 "+ underflow\000"
	align	 8
@LC880:
	string	 "ss.def\000"
	align	 8
@LC881:
	string	 "sim_main\000"
	align	 8
@LC882:
	string	 "- overflow\000"
	align	 8
@LC883:
	string	 "ss.def\000"
	align	 8
@LC884:
	string	 "sim_main\000"
	align	 8
@LC885:
	string	 "- underflow\000"
	align	 8
@LC886:
	string	 "ss.def\000"
	align	 8
@LC887:
	string	 "sim_main\000"
	align	 8
@LC888:
	string	 "divide by 0\000"
	align	 8
@LC889:
	string	 "ss.def\000"
	align	 8
@LC890:
	string	 "sim_main\000"
	align	 8
@LC891:
	string	 "divide by 0\000"
	align	 8
@LC892:
	string	 "ss.def\000"
	align	 8
@LC893:
	string	 "sim_main\000"
	align	 8
@LC894:
	string	 "bad FP register alignment\000"
	align	 8
@LC895:
	string	 "ss.def\000"
	align	 8
@LC896:
	string	 "sim_main\000"
	align	 8
@LC897:
	string	 "bad FP register alignment\000"
	align	 8
@LC898:
	string	 "ss.def\000"
	align	 8
@LC899:
	string	 "sim_main\000"
	align	 8
@LC900:
	string	 "bad FP register alignment\000"
	align	 8
@LC901:
	string	 "ss.def\000"
	align	 8
@LC902:
	string	 "sim_main\000"
	align	 8
@LC903:
	string	 "bad FP register alignment\000"
	align	 8
@LC904:
	string	 "ss.def\000"
	align	 8
@LC905:
	string	 "sim_main\000"
	align	 8
@LC906:
	string	 "bad FP register alignment\000"
	align	 8
@LC907:
	string	 "ss.def\000"
	align	 8
@LC908:
	string	 "sim_main\000"
	align	 8
@LC909:
	string	 "bad FP register alignment\000"
	align	 8
@LC910:
	string	 "ss.def\000"
	align	 8
@LC911:
	string	 "sim_main\000"
	align	 8
@LC912:
	string	 "bad FP register alignment\000"
	align	 8
@LC913:
	string	 "ss.def\000"
	align	 8
@LC914:
	string	 "sim_main\000"
	align	 8
@LC915:
	string	 "bad FP register alignment\000"
	align	 8
@LC916:
	string	 "ss.def\000"
	align	 8
@LC917:
	string	 "sim_main\000"
	align	 8
@LC918:
	string	 "bad FP register alignment\000"
	align	 8
@LC919:
	string	 "ss.def\000"
	align	 8
@LC920:
	string	 "sim_main\000"
	align	 8
@LC921:
	string	 "bad FP register alignment\000"
	align	 8
@LC922:
	string	 "ss.def\000"
	align	 8
@LC923:
	string	 "sim_main\000"
	align	 8
@LC924:
	string	 "bad FP register alignment\000"
	align	 8
@LC925:
	string	 "ss.def\000"
	align	 8
@LC926:
	string	 "sim_main\000"
	align	 8
@LC927:
	string	 "bad FP register alignment\000"
	align	 8
@LC928:
	string	 "ss.def\000"
	align	 8
@LC929:
	string	 "sim_main\000"
	align	 8
@LC930:
	string	 "bad FP register alignment\000"
	align	 8
@LC931:
	string	 "ss.def\000"
	align	 8
@LC932:
	string	 "sim_main\000"
	align	 8
@LC933:
	string	 "bad FP register alignment\000"
	align	 8
@LC934:
	string	 "ss.def\000"
	align	 8
@LC935:
	string	 "sim_main\000"
	align	 8
@LC936:
	string	 "bad FP register alignment\000"
	align	 8
@LC937:
	string	 "ss.def\000"
	align	 8
@LC938:
	string	 "sim_main\000"
	align	 8
@LC939:
	string	 "bad FP register alignment\000"
	align	 8
@LC940:
	string	 "ss.def\000"
	align	 8
@LC941:
	string	 "sim_main\000"
	align	 8
@LC942:
	string	 "bad FP register alignment\000"
	align	 8
@LC943:
	string	 "ss.def\000"
	align	 8
@LC944:
	string	 "sim_main\000"
	align	 8
@LC945:
	string	 "bad FP register alignment\000"
	align	 8
@LC946:
	string	 "ss.def\000"
	align	 8
@LC947:
	string	 "sim_main\000"
	align	 8
@LC948:
	string	 "bad FP register alignment\000"
	align	 8
@LC949:
	string	 "ss.def\000"
	align	 8
@LC950:
	string	 "sim_main\000"
	align	 8
@LC951:
	string	 "bad FP register alignment\000"
	align	 8
@LC952:
	string	 "ss.def\000"
	align	 8
@LC953:
	string	 "sim_main\000"
	align	 8
@LC954:
	string	 "bad FP register alignment\000"
	align	 8
@LC955:
	string	 "ss.def\000"
	align	 8
@LC956:
	string	 "sim_main\000"
	align	 8
@LC957:
	string	 "divide by 0\000"
	align	 8
@LC958:
	string	 "ss.def\000"
	align	 8
@LC959:
	string	 "sim_main\000"
	align	 8
@LC960:
	string	 "bad FP register alignment\000"
	align	 8
@LC961:
	string	 "ss.def\000"
	align	 8
@LC962:
	string	 "sim_main\000"
	align	 8
@LC963:
	string	 "bad FP register alignment\000"
	align	 8
@LC964:
	string	 "ss.def\000"
	align	 8
@LC965:
	string	 "sim_main\000"
	align	 8
@LC966:
	string	 "bad FP register alignment\000"
	align	 8
@LC967:
	string	 "ss.def\000"
	align	 8
@LC968:
	string	 "sim_main\000"
	align	 8
@LC969:
	string	 "divide by 0\000"
	align	 8
@LC970:
	string	 "ss.def\000"
	align	 8
@LC971:
	string	 "sim_main\000"
	align	 8
@LC972:
	string	 "bad FP register alignment\000"
	align	 8
@LC973:
	string	 "ss.def\000"
	align	 8
@LC974:
	string	 "sim_main\000"
	align	 8
@LC975:
	string	 "bad FP register alignment\000"
	align	 8
@LC976:
	string	 "ss.def\000"
	align	 8
@LC977:
	string	 "sim_main\000"
	align	 8
@LC978:
	string	 "bad FP register alignment\000"
	align	 8
@LC979:
	string	 "ss.def\000"
	align	 8
@LC980:
	string	 "sim_main\000"
	align	 8
@LC981:
	string	 "bad FP register alignment\000"
	align	 8
@LC982:
	string	 "ss.def\000"
	align	 8
@LC983:
	string	 "sim_main\000"
	align	 8
@LC984:
	string	 "bad FP register alignment\000"
	align	 8
@LC985:
	string	 "ss.def\000"
	align	 8
@LC986:
	string	 "sim_main\000"
	align	 8
@LC987:
	string	 "bad FP register alignment\000"
	align	 8
@LC988:
	string	 "ss.def\000"
	align	 8
@LC989:
	string	 "sim_main\000"
	align	 8
@LC990:
	string	 "bad FP register alignment\000"
	align	 8
@LC991:
	string	 "ss.def\000"
	align	 8
@LC992:
	string	 "sim_main\000"
	align	 8
@LC993:
	string	 "bad FP register alignment\000"
	align	 8
@LC994:
	string	 "ss.def\000"
	align	 8
@LC995:
	string	 "sim_main\000"
	align	 8
@LC996:
	string	 "bad FP register alignment\000"
	align	 8
@LC997:
	string	 "ss.def\000"
	align	 8
@LC998:
	string	 "sim_main\000"
	align	 8
@LC999:
	string	 "bad FP register alignment\000"
	align	 8
@LC1000:
	string	 "ss.def\000"
	align	 8
@LC1001:
	string	 "sim_main\000"
	align	 8
@LC1002:
	string	 "bad FP register alignment\000"
	align	 8
@LC1003:
	string	 "ss.def\000"
	align	 8
@LC1004:
	string	 "sim_main\000"
	align	 8
@LC1005:
	string	 "bad FP register alignment\000"
	align	 8
@LC1006:
	string	 "ss.def\000"
	align	 8
@LC1007:
	string	 "sim_main\000"
	align	 8
@LC1008:
	string	 "bad FP register alignment\000"
	align	 8
@LC1009:
	string	 "ss.def\000"
	align	 8
@LC1010:
	string	 "sim_main\000"
	align	 8
@LC1011:
	string	 "bad FP register alignment\000"
	align	 8
@LC1012:
	string	 "ss.def\000"
	align	 8
@LC1013:
	string	 "sim_main\000"
	align	 8
@LC1014:
	string	 "bad FP register alignment\000"
	align	 8
@LC1015:
	string	 "ss.def\000"
	align	 8
@LC1016:
	string	 "sim_main\000"
	align	 8
@LC1017:
	string	 "bad FP register alignment\000"
	align	 8
@LC1018:
	string	 "ss.def\000"
	align	 8
@LC1019:
	string	 "sim_main\000"
	align	 8
@LC1020:
	string	 "bad FP register alignment\000"
	align	 8
@LC1021:
	string	 "ss.def\000"
	align	 8
@LC1022:
	string	 "sim_main\000"
	align	 8
@LC1023:
	string	 "bad FP register alignment\000"
	align	 8
@LC1024:
	string	 "ss.def\000"
	align	 8
@LC1025:
	string	 "sim_main\000"
	align	 8
@LC1026:
	string	 "bad FP register alignment\000"
	align	 8
@LC1027:
	string	 "ss.def\000"
	align	 8
@LC1028:
	string	 "sim_main\000"
	align	 8
@LC1029:
	string	 "bad FP register alignment\000"
	align	 8
@LC1030:
	string	 "ss.def\000"
	align	 8
@LC1031:
	string	 "sim_main\000"
	align	 8
@LC1032:
	string	 "bad FP register alignment\000"
	align	 8
@LC1033:
	string	 "ss.def\000"
	align	 8
@LC1034:
	string	 "sim_main\000"
	align	 8
@LC1035:
	string	 "bad FP register alignment\000"
	align	 8
@LC1036:
	string	 "ss.def\000"
	align	 8
@LC1037:
	string	 "sim_main\000"
	align	 8
@LC1038:
	string	 "bad FP register alignment\000"
	align	 8
@LC1039:
	string	 "ss.def\000"
	align	 8
@LC1040:
	string	 "sim_main\000"
	align	 8
@LC1041:
	string	 "bad FP register alignment\000"
	align	 8
@LC1042:
	string	 "ss.def\000"
	align	 8
@LC1043:
	string	 "sim_main\000"
	align	 8
@LC1044:
	string	 "bad FP register alignment\000"
	align	 8
@LC1045:
	string	 "ss.def\000"
	align	 8
@LC1046:
	string	 "sim_main\000"
	align	 8
@LC1047:
	string	 "bad FP register alignment\000"
	align	 8
@LC1048:
	string	 "ss.def\000"
	align	 8
@LC1049:
	string	 "sim_main\000"
	align	 8
@LC1050:
	string	 "bad FP register alignment\000"
	align	 8
@LC1051:
	string	 "ss.def\000"
	align	 8
@LC1052:
	string	 "sim_main\000"
	align	 8
@LC1053:
	string	 "bad FP register alignment\000"
	align	 8
@LC1054:
	string	 "ss.def\000"
	align	 8
@LC1055:
	string	 "sim_main\000"
	align	 8
@LC1056:
	string	 "bad FP register alignment\000"
	align	 8
@LC1057:
	string	 "ss.def\000"
	align	 8
@LC1058:
	string	 "sim_main\000"
	align	 8
@LC1059:
	string	 "bad FP register alignment\000"
	align	 8
@LC1060:
	string	 "ss.def\000"
	align	 8
@LC1061:
	string	 "sim_main\000"
	align	 8
@LC1062:
	string	 "bad FP register alignment\000"
	align	 8
@LC1063:
	string	 "ss.def\000"
	align	 8
@LC1064:
	string	 "sim_main\000"
	align	 8
@LC1065:
	string	 "bad FP register alignment\000"
	align	 8
@LC1066:
	string	 "ss.def\000"
	align	 8
@LC1067:
	string	 "sim_main\000"
	align	 8
@LC1068:
	string	 "bad FP register alignment\000"
	align	 8
@LC1069:
	string	 "ss.def\000"
	align	 8
@LC1070:
	string	 "sim_main\000"
	align	 8
@LC1071:
	string	 "bad FP register alignment\000"
	align	 8
@LC1072:
	string	 "ss.def\000"
	align	 8
@LC1073:
	string	 "sim_main\000"
	align	 8
@LC1074:
	string	 "bad FP register alignment\000"
	align	 8
@LC1075:
	string	 "ss.def\000"
	align	 8
@LC1076:
	string	 "sim_main\000"
	align	 8
@LC1077:
	string	 "bad FP register alignment\000"
	align	 8
@LC1078:
	string	 "ss.def\000"
	align	 8
@LC1079:
	string	 "sim_main\000"
	align	 8
@LC1080:
	string	 "bad FP register alignment\000"
	align	 8
@LC1081:
	string	 "ss.def\000"
	align	 8
@LC1082:
	string	 "sim_main\000"
	align	 8
@LC1083:
	string	 "bad FP register alignment\000"
	align	 8
@LC1084:
	string	 "ss.def\000"
	align	 8
@LC1085:
	string	 "sim_main\000"
	align	 8
@LC1086:
	string	 "bad FP register alignment\000"
	align	 8
@LC1087:
	string	 "ss.def\000"
	align	 8
@LC1088:
	string	 "sim_main\000"
	align	 8
@LC1089:
	string	 "bad FP register alignment\000"
	align	 8
@LC1090:
	string	 "ss.def\000"
	align	 8
@LC1091:
	string	 "sim_main\000"
	align	 8
@LC1092:
	string	 "speculative syscall\000"
	align	 8
@LC1093:
	string	 "ss.def\000"
	align	 8
@LC1094:
	string	 "sim_main\000"
	align	 8
@LC1095:
	string	 "bad INT register alignment\000"
	align	 8
@LC1096:
	string	 "ss.def\000"
	align	 8
@LC1097:
	string	 "sim_main\000"
	align	 8
@LC1098:
	string	 "bad FP register alignment\000"
	align	 8
@LC1099:
	string	 "ss.def\000"
	align	 8
@LC1100:
	string	 "sim_main\000"
	align	 8
@LC1101:
	string	 "bad FP register alignment\000"
	align	 8
@LC1102:
	string	 "ss.def\000"
	align	 8
@LC1103:
	string	 "sim_main\000"
	align	 8
@LC1104:
	string	 "bad INT register alignment\000"
	align	 8
@LC1105:
	string	 "sim-outorder.c\000"
	align	 8
@LC1106:
	string	 "sim_main\000"
	align	 8
@LC1107:
	string	 "bogus opcode\000"
	align	 8
@LC1108:
	string	 "sim: ** starting performance simulation **\n\000"
	align	 8
@LC1109:
	string	 "sim-outorder.c\000"
	align	 8
@LC1110:
	string	 "sim_main\000"
	align	 8
@LC1111:
	string	 "RUU_num < LSQ_num\000"
	align	 8
@LC1112:
	string	 "sim-outorder.c\000"
	align	 8
@LC1113:
	string	 "sim_main\000"
	align	 8
@LC1114:
	string	 "RUU_head/RUU_tail wedged\000"
	align	 8
@LC1115:
	string	 "sim-outorder.c\000"
	align	 8
@LC1116:
	string	 "sim_main\000"
	align	 8
@LC1117:
	string	 "LSQ_head/LSQ_tail wedged\000"
text
	align	 4
@LC1118:
	word	 @LC823
	align	 8
	global	 _sim_main
_sim_main:
	subu	 r31,r31,512
	st	 r1,r31,96
	st.d	 r24,r31,88
	st.d	 r22,r31,80
	st.d	 r20,r31,72
	st.d	 r18,r31,64
	or	 r2,r0,8
	st.d	 r16,r31,56
	or	 r3,r0,1
	bsr.n	 _signal
	st.d	 r14,r31,48
@Ltb21:
	or.u	 r13,r0,hi16(_ld_prog_entry)
	ld	 r2,r13,lo16(_ld_prog_entry)
	or.u	 r13,r0,hi16(_dlite_check)
	ld	 r13,r13,lo16(_dlite_check)
	or.u	 r25,r0,hi16(_regs_PC)
	bcnd.n	 ne0,r13,@L10103
	st	 r2,r25,lo16(_regs_PC)
	or.u	 r13,r0,hi16(_dlite_active)
	ld	 r13,r13,lo16(_dlite_active)
	bcnd	 eq0,r13,@L10100
@L10103:
	or	 r6,r0,0
	or	 r7,r0,0
	or	 r8,r0,0
	or	 r9,r0,0
	or	 r3,r0,0
	bsr.n	 ___check_break
	or	 r4,r0,0
	bcnd.n	 eq0,r2,@L10100
	or.u	 r13,r0,hi16(_sim_cycle)
	ld	 r2,r25,lo16(_regs_PC)
	ld.d	 r4,r13,lo16(_sim_cycle)
	bsr.n	 _dlite_main
	addu	 r3,r2,8
@L10100:
	or.u	 r25,r0,hi16(_fastfwd_count)
	ld	 r4,r25,lo16(_fastfwd_count)
	bcnd.n	 le0,r4,@L10104
	or.u	 r2,r0,hi16(__IO_stderr_)
	or.u	 r3,r0,hi16(@LC837)
	or	 r2,r2,lo16(__IO_stderr_)
	bsr.n	 _fprintf
	or	 r3,r3,lo16(@LC837)
	or.u	 r13,r0,hi16(_regs_PC)
	ld	 r12,r25,lo16(_fastfwd_count)
	ld	 r13,r13,lo16(_regs_PC)
	st	 r0,r31,420
	addu	 r13,r13,8
	bcnd.n	 le0,r12,@L10104
	st	 r13,r31,436
	or.u	 r13,r0,hi16(_mem_table)
	or.u	 r18,r0,hi16(_spec_regs_R)
	or.u	 r19,r0,hi16(_regs_R)
	or	 r14,r13,lo16(_mem_table)
	or	 r16,r18,lo16(_spec_regs_R)
	or	 r15,r19,lo16(_regs_R)
	or.u	 r13,r0,hi16(_spec_regs_FCC)
@L10108:
	or.u	 r13,r0,hi16(_regs_PC)
	ld	 r12,r13,lo16(_regs_PC)
	or.u	 r18,r0,hi16(_regs_R)
	st	 r0,r18,lo16(_regs_R)
	extu	 r13,r12,15<16>
	ld	 r13,r14[r13]
	mask	 r12,r12,65535
	addu	 r13,r13,r12
	ld	 r12,r0,r13
	ld	 r13,r13,4
	st	 r12,r31,104
	mask	 r12,r12,255
	st	 r12,r31,428
	ld	 r19,r31,428
	or	 r24,r0,0
	st	 r13,r31,108
	or.u	 r13,r0,hi16(@L16746)
	subu	 r11,r19,1
	st	 r0,r31,444
	cmp	 r12,r11,118
	bb0.n	 ls,r12,@L16745
	or	 r13,r13,lo16(@L16746)
	ld	 r13,r13[r11]
	jmp	 r13
	align	 4
@L16746:
	word	 @L10109
	word	 @L10111
	word	 @L10112
	word	 @L10115
	word	 @L10124
	word	 @L10135
	word	 @L10142
	word	 @L10149
	word	 @L10154
	word	 @L10159
	word	 @L10164
	word	 @L10169
	word	 @L10174
	word	 @L10179
	word	 @L10324
	word	 @L10469
	word	 @L10614
	word	 @L10759
	word	 @L10904
	word	 @L11179
	word	 @L11324
	word	 @L11605
	word	 @L11677
	word	 @L11749
	word	 @L11829
	word	 @L11909
	word	 @L11989
	word	 @L12140
	word	 @L12287
	word	 @L12367
	word	 @L12518
	word	 @L12655
	word	 @L12792
	word	 @L12941
	word	 @L13090
	word	 @L13239
	word	 @L13388
	word	 @L13537
	word	 @L13814
	word	 @L13963
	word	 @L14252
	word	 @L14334
	word	 @L14416
	word	 @L14498
	word	 @L14653
	word	 @L14804
	word	 @L14886
	word	 @L15041
	word	 @L15190
	word	 @L15272
	word	 @L15421
	word	 @L15503
	word	 @L15538
	word	 @L15561
	word	 @L15572
	word	 @L15579
	word	 @L15614
	word	 @L15625
	word	 @L15711
	word	 @L15777
	word	 @L15862
	word	 @L15915
	word	 @L15922
	word	 @L15929
	word	 @L15936
	word	 @L15943
	word	 @L15954
	word	 @L15961
	word	 @L15972
	word	 @L15979
	word	 @L15990
	word	 @L15997
	word	 @L16008
	word	 @L16015
	word	 @L16026
	word	 @L16033
	word	 @L16044
	word	 @L16073
	word	 @L16104
	word	 @L16115
	word	 @L16122
	word	 @L16133
	word	 @L16140
	word	 @L16163
	word	 @L16186
	word	 @L16209
	word	 @L16232
	word	 @L16255
	word	 @L16278
	word	 @L16317
	word	 @L16356
	word	 @L16371
	word	 @L16386
	word	 @L16401
	word	 @L16416
	word	 @L16431
	word	 @L16446
	word	 @L16461
	word	 @L16476
	word	 @L16491
	word	 @L16506
	word	 @L16521
	word	 @L16536
	word	 @L16555
	word	 @L16574
	word	 @L16593
	word	 @L16612
	word	 @L16631
	word	 @L16650
	word	 @L16665
	word	 @L16680
	word	 @L10109
	word	 @L16684
	word	 @L16687
	word	 @L16694
	word	 @L10109
	word	 @L16716
	word	 @L16723
	word	 @L10109
	align	 4
@L10111:
	or.u	 r12,r0,hi16(_regs_PC)
	ld	 r13,r31,108
	ld	 r12,r12,lo16(_regs_PC)
	mak	 r13,r13,26<2>
	mask.u	 r12,r12,0xf000
	or	 r12,r12,r13
	br.n	 @L10109
	st	 r12,r31,436
	align	 4
@L10112:
	or.u	 r12,r0,hi16(_regs_PC)
	ld	 r13,r31,108
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r10,r12,lo16(_regs_PC)
	ld	 r11,r18,lo16(_spec_mode)
	mak	 r13,r13,26<2>
	mask.u	 r12,r10,0xf000
	or	 r12,r12,r13
	bcnd.n	 eq0,r11,@L10113
	st	 r12,r31,436
	or.u	 r19,r0,hi16(_use_spec_R)
	ld	 r12,r19,lo16(_use_spec_R)
	addu	 r13,r10,8
	st	 r13,r16,124
	or.u	 r12,r12,0x8000
	br.n	 @L10109
	st	 r12,r19,lo16(_use_spec_R)
	align	 4
@L10113:
	addu	 r13,r10,8
	br.n	 @L10109
	st	 r13,r15,124
	align	 4
@L10115:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10118
	ld	 r13,r16[r10]
	mask	 r13,r13,7
	bcnd.n	 ne0,r13,@L18034
	or.u	 r19,r0,hi16(_spec_mode)
	br	 @L10117
	align	 4
@L10118:
	ld	 r13,r15[r10]
	mask	 r13,r13,7
	bcnd.n	 eq0,r13,@L10117
	or.u	 r19,r0,hi16(_spec_mode)
@L18034:
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L10117
	or.u	 r2,r0,hi16(@LC838)
	or	 r4,r0,235
	or.u	 r3,r0,hi16(@LC839)
	or.u	 r5,r0,hi16(@LC840)
	or	 r2,r2,lo16(@LC838)
	or	 r3,r3,lo16(@LC839)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC840)
	align	 4
@L10117:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r2,r12,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r2,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10133
	br	 @L17495
	align	 4
@L10124:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10127
	ld	 r13,r16[r10]
	mask	 r13,r13,7
	bcnd.n	 ne0,r13,@L18035
	or.u	 r18,r0,hi16(_spec_mode)
	br.n	 @L18036
	or.u	 r19,r0,hi16(_spec_mode)
	align	 4
@L10127:
	ld	 r13,r15[r10]
	mask	 r13,r13,7
	bcnd.n	 eq0,r13,@L10126
	or.u	 r18,r0,hi16(_spec_mode)
@L18035:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L17295
	or.u	 r2,r0,hi16(@LC841)
	or	 r4,r0,240
	or.u	 r3,r0,hi16(@LC842)
	or.u	 r5,r0,hi16(@LC843)
	or	 r2,r2,lo16(@LC841)
	or	 r3,r3,lo16(@LC842)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC843)
	align	 4
@L10126:
	or.u	 r19,r0,hi16(_spec_mode)
@L18036:
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10131
	or.u	 r13,r0,hi16(_regs_PC)
@L17295:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r11,0<8>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r10,r11,3<5>
	mask	 r11,r11,31
	ld	 r12,r18[r10]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	or.u	 r13,r0,hi16(_regs_PC)
	st	 r12,r18[r10]
	ld	 r13,r13,lo16(_regs_PC)
	ld.bu	 r12,r31,110
	addu	 r13,r13,8
	br.n	 @L10132
	st	 r13,r16[r12]
	align	 4
@L10131:
	ld	 r13,r13,lo16(_regs_PC)
	ld.bu	 r12,r31,110
	addu	 r13,r13,8
	st	 r13,r15[r12]
@L10132:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r2,r12,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r2,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10133
@L17495:
	ld	 r2,r16[r2]
	br.n	 @L10109
	st	 r2,r31,436
	align	 4
@L10133:
	ld	 r2,r15[r2]
	br.n	 @L10109
	st	 r2,r31,436
	align	 4
@L10135:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10138
	ld	 r9,r16[r10]
	br	 @L10139
	align	 4
@L10138:
	ld	 r9,r15[r10]
@L10139:
	ld.hu	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	mask	 r10,r11,255
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10140
	ld	 r13,r16[r10]
	cmp	 r13,r9,r13
	bb0.n	 ne,r13,@L10178
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L18037
	align	 4
@L10140:
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb1.n	 ne,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L10178
	align	 4
@L10142:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10145
	ld	 r9,r16[r10]
	br	 @L10146
	align	 4
@L10145:
	ld	 r9,r15[r10]
@L10146:
	ld.hu	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	mask	 r10,r11,255
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10147
	ld	 r13,r16[r10]
	cmp	 r13,r9,r13
	bb1.n	 ne,r13,@L10178
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L18037
	align	 4
@L10147:
	ld	 r13,r15[r10]
	cmp	 r13,r9,r13
	bb0.n	 ne,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L10178
	align	 4
@L10149:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10152
	ld	 r13,r16[r10]
	bcnd.n	 le0,r13,@L10178
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L18037
	align	 4
@L10152:
	ld	 r13,r15[r10]
	bcnd.n	 gt0,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L10178
	align	 4
@L10154:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10157
	ld	 r13,r16[r10]
	bcnd.n	 gt0,r13,@L10178
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L18037
	align	 4
@L10157:
	ld	 r13,r15[r10]
	bcnd.n	 le0,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L10178
	align	 4
@L10159:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10162
	ld	 r13,r16[r10]
	bcnd.n	 lt0,r13,@L10178
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L18037
	align	 4
@L10162:
	ld	 r13,r15[r10]
	bcnd.n	 ge0,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L10178
	align	 4
@L10164:
	ld	 r12,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r10,r12,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,0<29>
	mask	 r11,r10,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L10167
	ld	 r13,r16[r10]
	bcnd.n	 ge0,r13,@L10178
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L18037
	align	 4
@L10167:
	ld	 r13,r15[r10]
	bcnd.n	 lt0,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L10178
	align	 4
@L10169:
	or.u	 r13,r0,hi16(_use_spec_FCC)
	ld	 r13,r13,lo16(_use_spec_FCC)
	bcnd.n	 eq0,r13,@L10172
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	ld	 r13,r13,lo16(_spec_regs_FCC)
	br	 @L17496
	align	 4
@L10172:
	or.u	 r13,r0,hi16(_regs_FCC)
	ld	 r13,r13,lo16(_regs_FCC)
@L17496:
	bcnd.n	 ne0,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L10178
	align	 4
@L10174:
	or.u	 r13,r0,hi16(_use_spec_FCC)
	ld	 r13,r13,lo16(_use_spec_FCC)
	bcnd.n	 eq0,r13,@L10177
	or.u	 r13,r0,hi16(_spec_regs_FCC)
	ld	 r13,r13,lo16(_spec_regs_FCC)
	bcnd.n	 ne0,r13,@L10178
	or.u	 r13,r0,hi16(_ss_op2flags)
	br	 @L18037
	align	 4
@L10177:
	or.u	 r13,r0,hi16(_regs_FCC)
	ld	 r13,r13,lo16(_regs_FCC)
	bcnd.n	 eq0,r13,@L18037
	or.u	 r13,r0,hi16(_ss_op2flags)
@L10178:
	ld.h	 r13,r31,110
	or.u	 r12,r0,hi16(_regs_PC)
	mak	 r13,r13,0<2>
	ld	 r12,r12,lo16(_regs_PC)
	addu	 r13,r13,8
	addu	 r12,r12,r13
	br.n	 @L10109
	st	 r12,r31,436
	align	 4
@L10179:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10180
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L10182
	ld	 r13,r16[r9]
	br.n	 @L18038
	addu	 r13,r13,r12
	align	 4
@L10182:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18038:
	br.n	 @L10181
	st	 r13,r16[r9]
	align	 4
@L10180:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L10184
	ld	 r13,r16[r9]
	br.n	 @L18039
	addu	 r13,r13,r11
	align	 4
@L10184:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18039:
	st	 r13,r15[r9]
@L10181:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10186
	or.u	 r18,r0,hi16(_use_spec_R)
	ld.hu	 r12,r31,108
	or	 r18,r18,lo16(_use_spec_R)
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,108
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r21,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L10188
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18040
	addu	 r24,r13,r3
	align	 4
@L10188:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
	or.u	 r19,r0,hi16(_spec_mode)
@L18040:
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10190
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10196
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18041
	extu	 r13,r24,0<24>
@L10196:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10191
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10191
	extu	 r13,r24,0<24>
@L18041:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10199
	or	 r11,r0,0
@L10201:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10200
	bcnd	 eq0,r11,@L10199
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10199
	st	 r10,r12[r25]
	align	 4
@L10200:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10201
@L10199:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10205
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18042
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10206
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10206:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18042:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10205
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10205:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L10219
	bb0.n	 gt,r13,@L10210
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L10228
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L10237
	or.u	 r2,r0,hi16(@LC509)
	br	 @L10250
	align	 4
@L10210:
	bcnd	 eq0,r10,@L10212
	ld.bu	 r13,r10,8
@L17500:
	br.n	 @L10191
	st.b	 r13,r31,112
	align	 4
@L10212:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10214
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10217
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10217:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L10191
	st.b	 r13,r31,112
	align	 4
@L10214:
	br.n	 @L17500
	or	 r13,r0,0
	align	 4
@L10219:
	bcnd	 eq0,r10,@L10221
	ld.hu	 r13,r10,8
@L17501:
	br.n	 @L10191
	st.h	 r13,r31,112
	align	 4
@L10221:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10223
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10226
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10226:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10191
	st.h	 r13,r31,112
	align	 4
@L10223:
	br.n	 @L17501
	or	 r13,r0,0
	align	 4
@L10228:
	bcnd	 eq0,r10,@L10230
	ld	 r13,r10,8
@L17502:
	br.n	 @L10191
	st	 r13,r31,112
	align	 4
@L10230:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10232
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10235
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10235:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L10191
	st	 r13,r31,112
	align	 4
@L10232:
	br.n	 @L17502
	or	 r13,r0,0
	align	 4
@L10237:
	bcnd	 eq0,r10,@L10239
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,112
	br.n	 @L10191
	st	 r12,r31,116
	align	 4
@L10239:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10241
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10244
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10244:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18043
	addu	 r23,r24,4
	align	 4
@L10241:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18043:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10245
	st	 r12,r31,112
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10248
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10248:
	ld	 r12,r14[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L10191
	st	 r13,r31,116
	align	 4
@L10245:
	or	 r13,r0,0
	br.n	 @L10191
	st	 r13,r31,116
	align	 4
@L10250:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L10190:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,112
	bsr.n	 _mem_access
	or	 r5,r0,1
@L10191:
	ld.b	 r13,r31,112
	br.n	 @L10187
	st	 r13,r16[r21]
	align	 4
@L10186:
	ld	 r11,r31,108
	or	 r13,r0,1
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r21,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L10253
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18044
	addu	 r24,r13,r3
	align	 4
@L10253:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18044:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10255
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10261
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18045
	extu	 r13,r24,0<24>
@L10261:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10256
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10256
	extu	 r13,r24,0<24>
@L18045:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10264
	or	 r11,r0,0
@L10266:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10265
	bcnd	 eq0,r11,@L10264
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10264
	st	 r10,r12[r25]
	align	 4
@L10265:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10266
@L10264:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10270
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18046
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10271
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10271:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18046:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10270
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10270:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L10284
	bb0.n	 gt,r13,@L10275
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L10293
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L10302
	or.u	 r2,r0,hi16(@LC509)
	br	 @L10315
	align	 4
@L10275:
	bcnd	 eq0,r10,@L10277
	ld.bu	 r13,r10,8
@L17504:
	br.n	 @L10256
	st.b	 r13,r31,112
	align	 4
@L10277:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10279
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10282
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10282:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L10256
	st.b	 r13,r31,112
	align	 4
@L10279:
	br.n	 @L17504
	or	 r13,r0,0
	align	 4
@L10284:
	bcnd	 eq0,r10,@L10286
	ld.hu	 r13,r10,8
@L17505:
	br.n	 @L10256
	st.h	 r13,r31,112
	align	 4
@L10286:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10288
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10291
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10291:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10256
	st.h	 r13,r31,112
	align	 4
@L10288:
	br.n	 @L17505
	or	 r13,r0,0
	align	 4
@L10293:
	bcnd	 eq0,r10,@L10295
	ld	 r13,r10,8
@L17506:
	br.n	 @L10256
	st	 r13,r31,112
	align	 4
@L10295:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10297
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10300
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10300:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L10256
	st	 r13,r31,112
	align	 4
@L10297:
	br.n	 @L17506
	or	 r13,r0,0
	align	 4
@L10302:
	bcnd	 eq0,r10,@L10304
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,112
	br.n	 @L10256
	st	 r12,r31,116
	align	 4
@L10304:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10306
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10309
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10309:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18047
	addu	 r23,r24,4
	align	 4
@L10306:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18047:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10310
	st	 r12,r31,112
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10313
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10313:
	ld	 r12,r14[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L10256
	st	 r13,r31,116
	align	 4
@L10310:
	or	 r13,r0,0
	br.n	 @L10256
	st	 r13,r31,116
	align	 4
@L10315:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L10255:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,112
	bsr.n	 _mem_access
	or	 r5,r0,1
@L10256:
	ld.b	 r13,r31,112
	st	 r13,r15[r21]
@L10187:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10318
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L10318:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L10324:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10325
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L10327
	ld	 r13,r16[r9]
	br.n	 @L18050
	addu	 r13,r13,r12
	align	 4
@L10327:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18050:
	br.n	 @L10326
	st	 r13,r16[r9]
	align	 4
@L10325:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L10329
	ld	 r13,r16[r9]
	br.n	 @L18051
	addu	 r13,r13,r11
	align	 4
@L10329:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18051:
	st	 r13,r15[r9]
@L10326:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10331
	or.u	 r19,r0,hi16(_use_spec_R)
	ld.hu	 r12,r31,108
	or	 r19,r19,lo16(_use_spec_R)
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r19[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r19[r12]
	ld	 r11,r31,108
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	extu	 r21,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L10333
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18052
	addu	 r24,r13,r3
	align	 4
@L10333:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
	or.u	 r18,r0,hi16(_spec_mode)
@L18052:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10335
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10341
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18053
	extu	 r13,r24,0<24>
@L10341:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10336
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10336
	extu	 r13,r24,0<24>
@L18053:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10344
	or	 r11,r0,0
@L10346:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10345
	bcnd	 eq0,r11,@L10344
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10344
	st	 r10,r12[r25]
	align	 4
@L10345:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10346
@L10344:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10350
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18054
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10351
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10351:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18054:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10350
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10350:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L10364
	bb0.n	 gt,r13,@L10355
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L10373
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L10382
	or.u	 r2,r0,hi16(@LC509)
	br	 @L10395
	align	 4
@L10355:
	bcnd	 eq0,r10,@L10357
	ld.bu	 r13,r10,8
@L17514:
	br.n	 @L10336
	st.b	 r13,r31,113
	align	 4
@L10357:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10359
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10362
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10362:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L10336
	st.b	 r13,r31,113
	align	 4
@L10359:
	br.n	 @L17514
	or	 r13,r0,0
	align	 4
@L10364:
	bcnd	 eq0,r10,@L10366
	ld.hu	 r13,r10,8
@L17515:
	br.n	 @L10336
	st.h	 r13,r31,113
	align	 4
@L10366:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10368
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10371
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10371:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10336
	st.h	 r13,r31,113
	align	 4
@L10368:
	br.n	 @L17515
	or	 r13,r0,0
	align	 4
@L10373:
	bcnd	 eq0,r10,@L10375
	ld	 r13,r10,8
@L17516:
	br.n	 @L10336
	st	 r13,r31,113
	align	 4
@L10375:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10377
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10380
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10380:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L10336
	st	 r13,r31,113
	align	 4
@L10377:
	br.n	 @L17516
	or	 r13,r0,0
	align	 4
@L10382:
	bcnd	 eq0,r10,@L10384
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,113
	br.n	 @L10336
	st	 r12,r31,117
	align	 4
@L10384:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10386
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10389
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10389:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18055
	addu	 r23,r24,4
	align	 4
@L10386:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18055:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10390
	st	 r12,r31,113
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10393
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10393:
	ld	 r12,r14[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L10336
	st	 r13,r31,117
	align	 4
@L10390:
	or	 r13,r0,0
	br.n	 @L10336
	st	 r13,r31,117
	align	 4
@L10395:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L10335:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,113
	bsr.n	 _mem_access
	or	 r5,r0,1
@L10336:
	ld.bu	 r13,r31,113
	br.n	 @L10332
	st	 r13,r16[r21]
	align	 4
@L10331:
	ld	 r11,r31,108
	or	 r13,r0,1
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	extu	 r21,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L10398
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18056
	addu	 r24,r13,r3
	align	 4
@L10398:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18056:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10400
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10406
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18057
	extu	 r13,r24,0<24>
@L10406:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10401
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10401
	extu	 r13,r24,0<24>
@L18057:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10409
	or	 r11,r0,0
@L10411:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10410
	bcnd	 eq0,r11,@L10409
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10409
	st	 r10,r12[r25]
	align	 4
@L10410:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10411
@L10409:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10415
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18058
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10416
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10416:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18058:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10415
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10415:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L10429
	bb0.n	 gt,r13,@L10420
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L10438
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L10447
	or.u	 r2,r0,hi16(@LC509)
	br	 @L10460
	align	 4
@L10420:
	bcnd	 eq0,r10,@L10422
	ld.bu	 r13,r10,8
@L17518:
	br.n	 @L10401
	st.b	 r13,r31,113
	align	 4
@L10422:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10424
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10427
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10427:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L10401
	st.b	 r13,r31,113
	align	 4
@L10424:
	br.n	 @L17518
	or	 r13,r0,0
	align	 4
@L10429:
	bcnd	 eq0,r10,@L10431
	ld.hu	 r13,r10,8
@L17519:
	br.n	 @L10401
	st.h	 r13,r31,113
	align	 4
@L10431:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10433
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10436
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10436:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10401
	st.h	 r13,r31,113
	align	 4
@L10433:
	br.n	 @L17519
	or	 r13,r0,0
	align	 4
@L10438:
	bcnd	 eq0,r10,@L10440
	ld	 r13,r10,8
@L17520:
	br.n	 @L10401
	st	 r13,r31,113
	align	 4
@L10440:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10442
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10445
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10445:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L10401
	st	 r13,r31,113
	align	 4
@L10442:
	br.n	 @L17520
	or	 r13,r0,0
	align	 4
@L10447:
	bcnd	 eq0,r10,@L10449
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,113
	br.n	 @L10401
	st	 r12,r31,117
	align	 4
@L10449:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10451
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10454
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10454:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18059
	addu	 r23,r24,4
	align	 4
@L10451:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18059:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10455
	st	 r12,r31,113
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10458
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10458:
	ld	 r12,r14[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L10401
	st	 r13,r31,117
	align	 4
@L10455:
	or	 r13,r0,0
	br.n	 @L10401
	st	 r13,r31,117
	align	 4
@L10460:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L10400:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,113
	bsr.n	 _mem_access
	or	 r5,r0,1
@L10401:
	ld.bu	 r13,r31,113
	st	 r13,r15[r21]
@L10332:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10463
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L10463:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L10469:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10470
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L10472
	ld	 r13,r16[r9]
	br.n	 @L18060
	addu	 r13,r13,r12
	align	 4
@L10472:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18060:
	br.n	 @L10471
	st	 r13,r16[r9]
	align	 4
@L10470:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+20)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L10474
	ld	 r13,r16[r9]
	br.n	 @L18061
	addu	 r13,r13,r11
	align	 4
@L10474:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18061:
	st	 r13,r15[r9]
@L10471:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10476
	or.u	 r18,r0,hi16(_use_spec_R)
	ld.hu	 r12,r31,108
	or	 r18,r18,lo16(_use_spec_R)
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,108
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r22,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L10478
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18062
	addu	 r24,r13,r3
	align	 4
@L10478:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
	or.u	 r19,r0,hi16(_spec_mode)
@L18062:
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10480
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L10481
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10486
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18063
	extu	 r13,r24,0<24>
@L10486:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10481
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10481
	extu	 r13,r24,0<24>
@L18063:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10489
	or	 r11,r0,0
@L10491:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10490
	bcnd	 eq0,r11,@L10489
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10489
	st	 r10,r12[r25]
	align	 4
@L10490:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10491
@L10489:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10509
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18064
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10496
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10496:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18064:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10509
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10509:
	bcnd	 eq0,r10,@L10511
	ld.hu	 r13,r10,8
@L17526:
	br.n	 @L10481
	st.h	 r13,r31,114
	align	 4
@L10511:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10513
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10516
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10516:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10481
	st.h	 r13,r31,114
	align	 4
@L10513:
	br.n	 @L17526
	or	 r13,r0,0
	align	 4
@L10480:
	or	 r3,r0,r24
	addu	 r4,r31,114
	bsr.n	 _mem_access
	or	 r5,r0,2
@L10481:
	ld.h	 r13,r31,114
	br.n	 @L10477
	st	 r13,r16[r22]
	align	 4
@L10476:
	ld	 r11,r31,108
	or	 r13,r0,1
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r22,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L10543
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18065
	addu	 r24,r13,r3
	align	 4
@L10543:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18065:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10545
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L10546
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10551
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18066
	extu	 r13,r24,0<24>
@L10551:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10546
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10546
	extu	 r13,r24,0<24>
@L18066:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10554
	or	 r11,r0,0
@L10556:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10555
	bcnd	 eq0,r11,@L10554
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10554
	st	 r10,r12[r25]
	align	 4
@L10555:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10556
@L10554:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10574
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18067
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10561
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10561:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18067:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10574
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10574:
	bcnd	 eq0,r10,@L10576
	ld.hu	 r13,r10,8
@L17528:
	br.n	 @L10546
	st.h	 r13,r31,114
	align	 4
@L10576:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10578
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10581
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10581:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10546
	st.h	 r13,r31,114
	align	 4
@L10578:
	br.n	 @L17528
	or	 r13,r0,0
	align	 4
@L10545:
	or	 r3,r0,r24
	addu	 r4,r31,114
	bsr.n	 _mem_access
	or	 r5,r0,2
@L10546:
	ld.h	 r13,r31,114
	st	 r13,r15[r22]
@L10477:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10608
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L10608:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L10614:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10615
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L10617
	ld	 r13,r16[r9]
	br.n	 @L18068
	addu	 r13,r13,r12
	align	 4
@L10617:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18068:
	br.n	 @L10616
	st	 r13,r16[r9]
	align	 4
@L10615:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+20)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L10619
	ld	 r13,r16[r9]
	br.n	 @L18069
	addu	 r13,r13,r11
	align	 4
@L10619:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18069:
	st	 r13,r15[r9]
@L10616:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10621
	or.u	 r19,r0,hi16(_use_spec_R)
	ld.hu	 r12,r31,108
	or	 r19,r19,lo16(_use_spec_R)
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r19[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r19[r12]
	ld	 r11,r31,108
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	extu	 r22,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L10623
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18070
	addu	 r24,r13,r3
	align	 4
@L10623:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
	or.u	 r18,r0,hi16(_spec_mode)
@L18070:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10625
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L10626
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10631
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18071
	extu	 r13,r24,0<24>
@L10631:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10626
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10626
	extu	 r13,r24,0<24>
@L18071:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10634
	or	 r11,r0,0
@L10636:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10635
	bcnd	 eq0,r11,@L10634
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10634
	st	 r10,r12[r25]
	align	 4
@L10635:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10636
@L10634:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10654
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18072
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10641
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10641:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18072:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10654
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10654:
	bcnd	 eq0,r10,@L10656
	ld.hu	 r13,r10,8
@L17534:
	br.n	 @L10626
	st.h	 r13,r31,116
	align	 4
@L10656:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10658
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10661
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10661:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10626
	st.h	 r13,r31,116
	align	 4
@L10658:
	br.n	 @L17534
	or	 r13,r0,0
	align	 4
@L10625:
	or	 r3,r0,r24
	addu	 r4,r31,116
	bsr.n	 _mem_access
	or	 r5,r0,2
@L10626:
	ld.hu	 r13,r31,116
	br.n	 @L10622
	st	 r13,r16[r22]
	align	 4
@L10621:
	ld	 r11,r31,108
	or	 r13,r0,1
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	extu	 r22,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L10688
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18073
	addu	 r24,r13,r3
	align	 4
@L10688:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18073:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10690
	or	 r2,r0,0
	bb1.n	 (31-31),r24,@L10691
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10696
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18074
	extu	 r13,r24,0<24>
@L10696:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10691
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10691
	extu	 r13,r24,0<24>
@L18074:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10699
	or	 r11,r0,0
@L10701:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10700
	bcnd	 eq0,r11,@L10699
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10699
	st	 r10,r12[r25]
	align	 4
@L10700:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10701
@L10699:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10719
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18075
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10706
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10706:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18075:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10719
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10719:
	bcnd	 eq0,r10,@L10721
	ld.hu	 r13,r10,8
@L17536:
	br.n	 @L10691
	st.h	 r13,r31,116
	align	 4
@L10721:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10723
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10726
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10726:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10691
	st.h	 r13,r31,116
	align	 4
@L10723:
	br.n	 @L17536
	or	 r13,r0,0
	align	 4
@L10690:
	or	 r3,r0,r24
	addu	 r4,r31,116
	bsr.n	 _mem_access
	or	 r5,r0,2
@L10691:
	ld.hu	 r13,r31,116
	st	 r13,r15[r22]
@L10622:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10753
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L10753:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L10759:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10760
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L10762
	ld	 r13,r16[r9]
	br.n	 @L18076
	addu	 r13,r13,r12
	align	 4
@L10762:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18076:
	br.n	 @L10761
	st	 r13,r16[r9]
	align	 4
@L10760:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L10764
	ld	 r13,r16[r9]
	br.n	 @L18077
	addu	 r13,r13,r11
	align	 4
@L10764:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18077:
	st	 r13,r15[r9]
@L10761:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10766
	or.u	 r18,r0,hi16(_use_spec_R)
	ld.hu	 r12,r31,108
	or	 r18,r18,lo16(_use_spec_R)
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,108
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	extu	 r21,r11,8<16>
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L10768
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18078
	addu	 r24,r13,r3
	align	 4
@L10768:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
	or.u	 r19,r0,hi16(_spec_mode)
@L18078:
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10770
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L10771
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10776
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18079
	extu	 r13,r24,0<24>
@L10776:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10771
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10771
	extu	 r13,r24,0<24>
@L18079:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10779
	or	 r11,r0,0
@L10781:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10780
	bcnd	 eq0,r11,@L10779
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10779
	st	 r10,r12[r25]
	align	 4
@L10780:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10781
@L10779:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10785
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18080
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10786
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10786:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18080:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10785
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10785:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L10799
	bb1.n	 gt,r13,@L10808
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L10830
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L10792
	ld.bu	 r13,r10,8
@L17542:
	br.n	 @L10771
	st.b	 r13,r31,120
	align	 4
@L10792:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10794
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10797
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10797:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L10771
	st.b	 r13,r31,120
	align	 4
@L10794:
	br.n	 @L17542
	or	 r13,r0,0
	align	 4
@L10799:
	bcnd	 eq0,r10,@L10801
	ld.hu	 r13,r10,8
@L17543:
	br.n	 @L10771
	st.h	 r13,r31,120
	align	 4
@L10801:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10803
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10806
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10806:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10771
	st.h	 r13,r31,120
	align	 4
@L10803:
	br.n	 @L17543
	or	 r13,r0,0
	align	 4
@L10808:
	bcnd	 eq0,r10,@L10810
	ld	 r13,r10,8
@L17544:
	br.n	 @L10771
	st	 r13,r31,120
	align	 4
@L10810:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10812
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10815
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10815:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L10771
	st	 r13,r31,120
	align	 4
@L10812:
	br.n	 @L17544
	or	 r13,r0,0
	align	 4
@L10830:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L10770:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L10771:
	ld	 r13,r31,120
	br.n	 @L10767
	st	 r13,r16[r21]
	align	 4
@L10766:
	ld	 r11,r31,108
	or	 r13,r0,1
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	extu	 r21,r11,8<16>
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L10833
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18081
	addu	 r24,r13,r3
	align	 4
@L10833:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18081:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10835
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L10836
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10841
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18082
	extu	 r13,r24,0<24>
@L10841:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10836
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10836
	extu	 r13,r24,0<24>
@L18082:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10844
	or	 r11,r0,0
@L10846:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10845
	bcnd	 eq0,r11,@L10844
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10844
	st	 r10,r12[r25]
	align	 4
@L10845:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10846
@L10844:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10850
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18083
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10851
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10851:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18083:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10850
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10850:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L10864
	bb1.n	 gt,r13,@L10873
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L10895
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L10857
	ld.bu	 r13,r10,8
@L17546:
	br.n	 @L10836
	st.b	 r13,r31,120
	align	 4
@L10857:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10859
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10862
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10862:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L10836
	st.b	 r13,r31,120
	align	 4
@L10859:
	br.n	 @L17546
	or	 r13,r0,0
	align	 4
@L10864:
	bcnd	 eq0,r10,@L10866
	ld.hu	 r13,r10,8
@L17547:
	br.n	 @L10836
	st.h	 r13,r31,120
	align	 4
@L10866:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10868
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10871
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10871:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10836
	st.h	 r13,r31,120
	align	 4
@L10868:
	br.n	 @L17547
	or	 r13,r0,0
	align	 4
@L10873:
	bcnd	 eq0,r10,@L10875
	ld	 r13,r10,8
@L17548:
	br.n	 @L10836
	st	 r13,r31,120
	align	 4
@L10875:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10877
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10880
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10880:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L10836
	st	 r13,r31,120
	align	 4
@L10877:
	br.n	 @L17548
	or	 r13,r0,0
	align	 4
@L10895:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L10835:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L10836:
	ld	 r13,r31,120
	st	 r13,r15[r21]
@L10767:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10898
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L10898:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L10904:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10905
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L10907
	ld	 r13,r16[r9]
	br.n	 @L18084
	addu	 r13,r13,r12
	align	 4
@L10907:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18084:
	br.n	 @L10906
	st	 r13,r16[r9]
	align	 4
@L10905:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L10909
	ld	 r13,r16[r9]
	br.n	 @L18085
	addu	 r13,r13,r11
	align	 4
@L10909:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18085:
	st	 r13,r15[r9]
@L10906:
	ld.hu	 r13,r31,108
	bb0.n	 (31-31),r13,@L10912
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L10912
	or.u	 r2,r0,hi16(@LC844)
	or	 r4,r0,323
	or.u	 r3,r0,hi16(@LC845)
	or.u	 r5,r0,hi16(@LC846)
	or	 r2,r2,lo16(@LC844)
	or	 r3,r3,lo16(@LC845)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC846)
	align	 4
@L10912:
	ld	 r13,r31,108
	or	 r12,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r10,r13,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r11,r10,31
	ld	 r13,r19[r13]
	mak	 r12,r12,r11
	or.u	 r11,r0,hi16(_temp_bs)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L10915
	or	 r12,r11,lo16(_temp_bs)
	ld	 r11,r16[r10]
	br.n	 @L18086
	or.u	 r18,r0,hi16(_spec_mode)
	align	 4
@L10915:
	ld	 r11,r15[r10]
	or.u	 r18,r0,hi16(_spec_mode)
@L18086:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L10917
	st	 r11,r0,r12
	ld.bu	 r11,r31,109
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r9,r13,lo16(_sim_swap_words)
	or.u	 r19,r0,hi16(_use_spec_R)
	or	 r13,r0,1
	xor	 r11,r11,r9
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r10,r11,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r10]
	mak	 r13,r13,r11
	or	 r12,r12,r13
	st	 r12,r19[r10]
	ld	 r13,r31,108
	or.u	 r12,r0,hi16(_temp_bs)
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r11,r13,16<0>
	or	 r22,r0,4
	extu	 r13,r13,8<16>
	addu	 r24,r12,r11
	xor	 r21,r13,r9
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L10920
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L10925
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18087
	extu	 r13,r24,0<24>
@L10925:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10920
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10920
	extu	 r13,r24,0<24>
@L18087:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L10928
	or	 r11,r0,0
@L10930:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L10929
	bcnd	 eq0,r11,@L10928
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L10928
	st	 r10,r12[r25]
	align	 4
@L10929:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L10930
@L10928:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L10934
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18088
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L10935
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L10935:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18088:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L10934
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L10934:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L10948
	bb1.n	 gt,r13,@L10957
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L10979
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L10941
	ld.bu	 r13,r10,8
@L17553:
	br.n	 @L10920
	st.b	 r13,r31,120
	align	 4
@L10941:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10943
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10946
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10946:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L10920
	st.b	 r13,r31,120
	align	 4
@L10943:
	br.n	 @L17553
	or	 r13,r0,0
	align	 4
@L10948:
	bcnd	 eq0,r10,@L10950
	ld.hu	 r13,r10,8
@L17554:
	br.n	 @L10920
	st.h	 r13,r31,120
	align	 4
@L10950:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10952
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10955
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10955:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L10920
	st.h	 r13,r31,120
	align	 4
@L10952:
	br.n	 @L17554
	or	 r13,r0,0
	align	 4
@L10957:
	bcnd	 eq0,r10,@L10959
	ld	 r13,r10,8
@L17555:
	br.n	 @L10920
	st	 r13,r31,120
	align	 4
@L10959:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L10961
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L10964
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L10964:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L10920
	st	 r13,r31,120
	align	 4
@L10961:
	br.n	 @L17555
	or	 r13,r0,0
	align	 4
@L10979:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L10920:
	ld	 r13,r31,120
	br.n	 @L10918
	st	 r13,r16[r21]
	align	 4
@L10917:
	or.u	 r12,r0,hi16(_sim_swap_words)
	ld	 r13,r31,108
	or.u	 r11,r0,hi16(_temp_bs)
	ld	 r10,r12,lo16(_sim_swap_words)
	extu	 r12,r13,8<16>
	ld	 r11,r11,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	xor	 r25,r12,r10
	addu	 r24,r11,r13
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
	ld	 r13,r31,120
	st	 r13,r15[r25]
@L10918:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11045
	or.u	 r12,r0,hi16(_sim_swap_words)
	ld.bu	 r13,r31,109
	or.u	 r19,r0,hi16(_use_spec_R)
	ld	 r9,r12,lo16(_sim_swap_words)
	addu	 r13,r13,1
	or	 r12,r0,1
	xor	 r13,r13,r9
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r10,r13,0<5>
	mask	 r13,r13,31
	ld	 r11,r19[r10]
	mak	 r12,r12,r13
	or	 r11,r11,r12
	st	 r11,r19[r10]
	or.u	 r13,r0,hi16(_temp_bs)
	ld	 r12,r31,108
	ld	 r13,r13,lo16(_temp_bs)
	ext	 r11,r12,16<0>
	or	 r22,r0,4
	extu	 r12,r12,8<16>
	addu	 r13,r13,r11
	addu	 r12,r12,1
	addu	 r24,r13,4
	xor	 r21,r12,r9
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11048
	or	 r9,r0,0
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11053
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18089
	extu	 r13,r24,0<24>
@L11053:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11048
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11048
	extu	 r13,r24,0<24>
@L18089:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11056
	or	 r11,r0,0
@L11058:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11057
	bcnd	 eq0,r11,@L11056
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11056
	st	 r10,r12[r25]
	align	 4
@L11057:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11058
@L11056:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11062
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18090
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11063
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11063:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18090:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11062
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11062:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11076
	bb1.n	 gt,r13,@L11085
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11107
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L11069
	ld.bu	 r13,r10,8
@L17556:
	br.n	 @L11048
	st.b	 r13,r31,120
	align	 4
@L11069:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11071
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11074
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11074:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11048
	st.b	 r13,r31,120
	align	 4
@L11071:
	br.n	 @L17556
	or	 r13,r0,0
	align	 4
@L11076:
	bcnd	 eq0,r10,@L11078
	ld.hu	 r13,r10,8
@L17557:
	br.n	 @L11048
	st.h	 r13,r31,120
	align	 4
@L11078:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11080
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11083
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11083:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11048
	st.h	 r13,r31,120
	align	 4
@L11080:
	br.n	 @L17557
	or	 r13,r0,0
	align	 4
@L11085:
	bcnd	 eq0,r10,@L11087
	ld	 r13,r10,8
@L17558:
	br.n	 @L11048
	st	 r13,r31,120
	align	 4
@L11087:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11089
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11092
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11092:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11048
	st	 r13,r31,120
	align	 4
@L11089:
	br.n	 @L17558
	or	 r13,r0,0
	align	 4
@L11107:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11048:
	ld	 r13,r31,120
	br.n	 @L11046
	st	 r13,r16[r21]
	align	 4
@L11045:
	ld	 r12,r31,108
	or.u	 r13,r0,hi16(_temp_bs)
	ld	 r10,r13,lo16(_temp_bs)
	extu	 r11,r12,8<16>
	or.u	 r13,r0,hi16(_sim_swap_words)
	ext	 r12,r12,16<0>
	ld	 r13,r13,lo16(_sim_swap_words)
	addu	 r11,r11,1
	addu	 r10,r10,r12
	xor	 r25,r11,r13
	addu	 r24,r10,4
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
	ld	 r13,r31,120
	st	 r13,r15[r25]
@L11046:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11173
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L11173:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L11179:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11180
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L11182
	ld	 r13,r16[r9]
	br.n	 @L18091
	addu	 r13,r13,r12
	align	 4
@L11182:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18091:
	br.n	 @L11181
	st	 r13,r16[r9]
	align	 4
@L11180:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L11184
	ld	 r13,r16[r9]
	br.n	 @L18092
	addu	 r13,r13,r11
	align	 4
@L11184:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18092:
	st	 r13,r15[r9]
@L11181:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11186
	or.u	 r10,r0,hi16(_use_spec_F)
	ld.hu	 r11,r31,108
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r12,r11,30
	extu	 r11,r11,3<5>
	or	 r9,r0,1
	ld	 r13,r10[r11]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r10[r11]
	ld	 r11,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r11,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r8,r11,0<24>
	ld	 r10,r19[r13]
	mask	 r13,r8,31
	or.u	 r12,r0,hi16(_spec_regs_F)
	mak	 r9,r9,r13
	or	 r20,r12,lo16(_spec_regs_F)
	extu	 r21,r11,8<16>
	and	 r10,r10,r9
	bcnd.n	 eq0,r10,@L11188
	ext	 r3,r11,16<0>
	ld	 r13,r16[r8]
	br.n	 @L18093
	addu	 r24,r13,r3
	align	 4
@L11188:
	ld	 r13,r15[r8]
	addu	 r24,r13,r3
	or.u	 r18,r0,hi16(_spec_mode)
@L18093:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11255
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11256
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11196
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18094
	extu	 r13,r24,0<24>
@L11196:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11256
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11256
	extu	 r13,r24,0<24>
@L18094:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11199
	or	 r11,r0,0
@L11201:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11200
	bcnd	 eq0,r11,@L11199
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11199
	st	 r10,r12[r25]
	align	 4
@L11200:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11201
@L11199:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11205
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18095
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11206
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11206:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18095:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11205
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11205:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11219
	bb1.n	 gt,r13,@L11228
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11250
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L18008
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11214
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11217
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11217:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11256
	st.b	 r13,r31,120
	align	 4
@L11214:
	br.n	 @L17568
	or	 r13,r0,0
	align	 4
@L11219:
	bcnd.n	 ne0,r10,@L18009
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11223
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11226
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11226:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11256
	st.h	 r13,r31,120
	align	 4
@L11223:
	br.n	 @L17569
	or	 r13,r0,0
	align	 4
@L11228:
	bcnd.n	 ne0,r10,@L18010
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11232
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11235
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11235:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11256
	st	 r13,r31,120
	align	 4
@L11232:
	br.n	 @L17570
	or	 r13,r0,0
	align	 4
@L11250:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11186:
	ld	 r11,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r11,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r8,r11,0<24>
	ld	 r10,r19[r13]
	mask	 r9,r8,31
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_regs_F)
	mak	 r13,r13,r9
	or	 r20,r12,lo16(_regs_F)
	extu	 r21,r11,8<16>
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L11253
	ext	 r3,r11,16<0>
	ld	 r13,r16[r8]
	br.n	 @L18096
	addu	 r24,r13,r3
	align	 4
@L11253:
	ld	 r13,r15[r8]
	addu	 r24,r13,r3
@L18096:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11255
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11256
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11261
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18097
	extu	 r13,r24,0<24>
@L11261:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11256
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11256
	extu	 r13,r24,0<24>
@L18097:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11264
	or	 r11,r0,0
@L11266:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11265
	bcnd	 eq0,r11,@L11264
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11264
	st	 r10,r12[r25]
	align	 4
@L11265:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11266
@L11264:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11270
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18098
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11271
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11271:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18098:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11270
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11270:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11284
	bb1.n	 gt,r13,@L11293
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11315
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L11277
	extu	 r25,r24,15<16>
@L18008:
	ld.bu	 r13,r10,8
@L17568:
	br.n	 @L11256
	st.b	 r13,r31,120
	align	 4
@L11277:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11279
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11282
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11282:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11256
	st.b	 r13,r31,120
	align	 4
@L11279:
	br.n	 @L17568
	or	 r13,r0,0
	align	 4
@L11284:
	bcnd.n	 eq0,r10,@L11286
	extu	 r25,r24,15<16>
@L18009:
	ld.hu	 r13,r10,8
@L17569:
	br.n	 @L11256
	st.h	 r13,r31,120
	align	 4
@L11286:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11288
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11291
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11291:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11256
	st.h	 r13,r31,120
	align	 4
@L11288:
	br.n	 @L17569
	or	 r13,r0,0
	align	 4
@L11293:
	bcnd.n	 eq0,r10,@L11295
	extu	 r25,r24,15<16>
@L18010:
	ld	 r13,r10,8
@L17570:
	br.n	 @L11256
	st	 r13,r31,120
	align	 4
@L11295:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11297
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11300
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11300:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11256
	st	 r13,r31,120
	align	 4
@L11297:
	br.n	 @L17570
	or	 r13,r0,0
	align	 4
@L11315:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11255:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L11256:
	ld	 r13,r31,120
	st	 r13,r20[r21]
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11318
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L11318:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L11324:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11325
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L11327
	ld	 r13,r16[r9]
	br.n	 @L18099
	addu	 r13,r13,r12
	align	 4
@L11327:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18099:
	br.n	 @L11326
	st	 r13,r16[r9]
	align	 4
@L11325:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L11329
	ld	 r13,r16[r9]
	br.n	 @L18100
	addu	 r13,r13,r11
	align	 4
@L11329:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18100:
	st	 r13,r15[r9]
@L11326:
	ld	 r11,r31,108
	extu	 r7,r11,0<16>
	bb0.n	 (31-31),r7,@L11332
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L17296
	or.u	 r2,r0,hi16(@LC847)
	or	 r4,r0,336
	or.u	 r3,r0,hi16(@LC848)
	or.u	 r5,r0,hi16(@LC849)
	or	 r2,r2,lo16(@LC847)
	or	 r3,r3,lo16(@LC848)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC849)
	align	 4
@L11332:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11335
	extu	 r12,r11,0<29>
@L17296:
	ld.bu	 r12,r31,109
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld	 r8,r13,lo16(_sim_swap_words)
	or.u	 r10,r0,hi16(_use_spec_F)
	xor	 r12,r12,r8
	or	 r10,r10,lo16(_use_spec_F)
	mask	 r11,r12,30
	extu	 r12,r12,0<5>
	or	 r9,r0,1
	ld	 r13,r10[r12]
	mak	 r11,r9,r11
	or	 r13,r13,r11
	st	 r13,r10[r12]
	ld	 r11,r31,108
	extu	 r13,r11,0<29>
	extu	 r12,r11,8<16>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r7,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	ext	 r3,r11,16<0>
	ld	 r10,r19[r13]
	mask	 r13,r7,31
	xor	 r21,r12,r8
	mak	 r9,r9,r13
	or.u	 r12,r0,hi16(_spec_regs_F)
	and	 r10,r10,r9
	bcnd.n	 eq0,r10,@L11337
	or	 r20,r12,lo16(_spec_regs_F)
	ld	 r13,r16[r7]
	br.n	 @L18101
	addu	 r24,r13,r3
	align	 4
@L11337:
	ld	 r13,r15[r7]
	addu	 r24,r13,r3
@L18101:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11404
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11405
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11345
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18102
	extu	 r13,r24,0<24>
@L11345:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11405
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11405
	extu	 r13,r24,0<24>
@L18102:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11348
	or	 r11,r0,0
@L11350:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11349
	bcnd	 eq0,r11,@L11348
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11348
	st	 r10,r12[r25]
	align	 4
@L11349:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11350
@L11348:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11354
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18103
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11355
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11355:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18103:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11354
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11354:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11368
	bb1.n	 gt,r13,@L11377
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11399
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L18011
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11363
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11366
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11366:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11405
	st.b	 r13,r31,120
	align	 4
@L11363:
	br.n	 @L17580
	or	 r13,r0,0
	align	 4
@L11368:
	bcnd.n	 ne0,r10,@L18012
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11372
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11375
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11375:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11405
	st.h	 r13,r31,120
	align	 4
@L11372:
	br.n	 @L17581
	or	 r13,r0,0
	align	 4
@L11377:
	bcnd.n	 ne0,r10,@L18013
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11381
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11384
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11384:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11405
	st	 r13,r31,120
	align	 4
@L11381:
	br.n	 @L17582
	or	 r13,r0,0
	align	 4
@L11399:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11335:
	extu	 r8,r11,0<24>
	ext	 r3,r11,16<0>
	or.u	 r13,r0,hi16(_sim_swap_words)
	or.u	 r19,r0,hi16(_use_spec_R)
	ld	 r9,r13,lo16(_sim_swap_words)
	or	 r19,r19,lo16(_use_spec_R)
	mask	 r11,r8,31
	or	 r13,r0,1
	ld	 r10,r19[r12]
	mask	 r12,r7,255
	mak	 r13,r13,r11
	xor	 r21,r12,r9
	or.u	 r12,r0,hi16(_regs_F)
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L11402
	or	 r20,r12,lo16(_regs_F)
	ld	 r13,r16[r8]
	br.n	 @L18104
	addu	 r24,r13,r3
	align	 4
@L11402:
	ld	 r13,r15[r8]
	addu	 r24,r13,r3
@L18104:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11404
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11405
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11410
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18105
	extu	 r13,r24,0<24>
@L11410:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11405
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11405
	extu	 r13,r24,0<24>
@L18105:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11413
	or	 r11,r0,0
@L11415:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11414
	bcnd	 eq0,r11,@L11413
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11413
	st	 r10,r12[r25]
	align	 4
@L11414:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11415
@L11413:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11419
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18106
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11420
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11420:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18106:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11419
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11419:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11433
	bb1.n	 gt,r13,@L11442
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11464
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L11426
	extu	 r25,r24,15<16>
@L18011:
	ld.bu	 r13,r10,8
@L17580:
	br.n	 @L11405
	st.b	 r13,r31,120
	align	 4
@L11426:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11428
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11431
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11431:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11405
	st.b	 r13,r31,120
	align	 4
@L11428:
	br.n	 @L17580
	or	 r13,r0,0
	align	 4
@L11433:
	bcnd.n	 eq0,r10,@L11435
	extu	 r25,r24,15<16>
@L18012:
	ld.hu	 r13,r10,8
@L17581:
	br.n	 @L11405
	st.h	 r13,r31,120
	align	 4
@L11435:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11437
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11440
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11440:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11405
	st.h	 r13,r31,120
	align	 4
@L11437:
	br.n	 @L17581
	or	 r13,r0,0
	align	 4
@L11442:
	bcnd.n	 eq0,r10,@L11444
	extu	 r25,r24,15<16>
@L18013:
	ld	 r13,r10,8
@L17582:
	br.n	 @L11405
	st	 r13,r31,120
	align	 4
@L11444:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11446
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11449
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11449:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11405
	st	 r13,r31,120
	align	 4
@L11446:
	br.n	 @L17582
	or	 r13,r0,0
	align	 4
@L11464:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11404:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L11405:
	ld	 r13,r31,120
	st	 r13,r20[r21]
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11467
	or.u	 r13,r0,hi16(_sim_swap_words)
	ld.bu	 r12,r31,109
	or.u	 r11,r0,hi16(_use_spec_F)
	ld	 r8,r13,lo16(_sim_swap_words)
	addu	 r12,r12,1
	or	 r10,r0,1
	xor	 r12,r12,r8
	or	 r11,r11,lo16(_use_spec_F)
	extu	 r9,r12,0<5>
	mask	 r12,r12,30
	ld	 r13,r11[r9]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r11[r9]
	ld	 r13,r31,108
	extu	 r11,r13,8<16>
	extu	 r12,r13,0<29>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r13,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	ext	 r7,r13,16<0>
	ld	 r12,r18[r12]
	mask	 r13,r9,31
	addu	 r11,r11,1
	mak	 r10,r10,r13
	xor	 r21,r11,r8
	or.u	 r13,r0,hi16(_spec_regs_F)
	and	 r12,r12,r10
	bcnd.n	 eq0,r12,@L11469
	or	 r20,r13,lo16(_spec_regs_F)
	ld	 r13,r16[r9]
	br.n	 @L18107
	addu	 r13,r13,r7
	align	 4
@L11469:
	ld	 r13,r15[r9]
	addu	 r13,r13,r7
@L18107:
	addu	 r24,r13,4
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11536
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11537
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11477
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18108
	extu	 r13,r24,0<24>
@L11477:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11537
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11537
	extu	 r13,r24,0<24>
@L18108:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11480
	or	 r11,r0,0
@L11482:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11481
	bcnd	 eq0,r11,@L11480
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11480
	st	 r10,r12[r25]
	align	 4
@L11481:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11482
@L11480:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11486
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18109
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11487
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11487:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18109:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11486
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11486:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11500
	bb1.n	 gt,r13,@L11509
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11531
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 ne0,r10,@L18014
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11495
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11498
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11498:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11537
	st.b	 r13,r31,120
	align	 4
@L11495:
	br.n	 @L17588
	or	 r13,r0,0
	align	 4
@L11500:
	bcnd.n	 ne0,r10,@L18015
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11504
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11507
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11507:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11537
	st.h	 r13,r31,120
	align	 4
@L11504:
	br.n	 @L17589
	or	 r13,r0,0
	align	 4
@L11509:
	bcnd.n	 ne0,r10,@L18016
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11513
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11516
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11516:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11537
	st	 r13,r31,120
	align	 4
@L11513:
	br.n	 @L17590
	or	 r13,r0,0
	align	 4
@L11531:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11467:
	ld	 r9,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r12,r9,0<29>
	ld	 r11,r13,lo16(_sim_swap_words)
	extu	 r13,r9,8<16>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r8,r9,0<24>
	addu	 r13,r13,1
	ext	 r9,r9,16<0>
	xor	 r21,r13,r11
	mask	 r11,r8,31
	or	 r13,r0,1
	ld	 r10,r18[r12]
	mak	 r13,r13,r11
	or.u	 r12,r0,hi16(_regs_F)
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L11534
	or	 r20,r12,lo16(_regs_F)
	ld	 r13,r16[r8]
	br.n	 @L18110
	addu	 r13,r13,r9
	align	 4
@L11534:
	ld	 r13,r15[r8]
	addu	 r13,r13,r9
@L18110:
	addu	 r24,r13,4
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11536
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11537
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11542
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18111
	extu	 r13,r24,0<24>
@L11542:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11537
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11537
	extu	 r13,r24,0<24>
@L18111:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11545
	or	 r11,r0,0
@L11547:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11546
	bcnd	 eq0,r11,@L11545
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11545
	st	 r10,r12[r25]
	align	 4
@L11546:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11547
@L11545:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11551
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18112
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11552
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11552:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18112:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11551
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11551:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11565
	bb1.n	 gt,r13,@L11574
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11596
	or.u	 r2,r0,hi16(@LC509)
	bcnd.n	 eq0,r10,@L11558
	extu	 r25,r24,15<16>
@L18014:
	ld.bu	 r13,r10,8
@L17588:
	br.n	 @L11537
	st.b	 r13,r31,120
	align	 4
@L11558:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11560
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11563
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11563:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11537
	st.b	 r13,r31,120
	align	 4
@L11560:
	br.n	 @L17588
	or	 r13,r0,0
	align	 4
@L11565:
	bcnd.n	 eq0,r10,@L11567
	extu	 r25,r24,15<16>
@L18015:
	ld.hu	 r13,r10,8
@L17589:
	br.n	 @L11537
	st.h	 r13,r31,120
	align	 4
@L11567:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11569
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11572
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11572:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11537
	st.h	 r13,r31,120
	align	 4
@L11569:
	br.n	 @L17589
	or	 r13,r0,0
	align	 4
@L11574:
	bcnd.n	 eq0,r10,@L11576
	extu	 r25,r24,15<16>
@L18016:
	ld	 r13,r10,8
@L17590:
	br.n	 @L11537
	st	 r13,r31,120
	align	 4
@L11576:
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11578
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11581
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11581:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11537
	st	 r13,r31,120
	align	 4
@L11578:
	br.n	 @L17590
	or	 r13,r0,0
	align	 4
@L11596:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11536:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L11537:
	ld	 r13,r31,120
	st	 r13,r20[r21]
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11599
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L11599:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L11605:
	ld	 r13,r31,108
	or	 r12,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r10,r13,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r11,r10,31
	ld	 r13,r19[r13]
	mak	 r12,r12,r11
	or.u	 r11,r0,hi16(_temp_bs)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L11606
	or	 r12,r11,lo16(_temp_bs)
	ld	 r11,r16[r10]
	br.n	 @L18113
	st	 r11,r0,r12
	align	 4
@L11606:
	ld	 r11,r15[r10]
	st	 r11,r0,r12
@L18113:
	or.u	 r13,r0,hi16(_temp_bs)
	ld.h	 r12,r31,110
	ld	 r13,r13,lo16(_temp_bs)
	or.u	 r11,r0,hi16(_ss_lr_temp)
	or.u	 r18,r0,hi16(_spec_mode)
	addu	 r13,r13,r12
	ld	 r12,r18,lo16(_spec_mode)
	or	 r21,r11,lo16(_ss_lr_temp)
	bcnd.n	 eq0,r12,@L11608
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11614
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18114
	extu	 r13,r24,0<24>
@L11614:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11609
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11609
	extu	 r13,r24,0<24>
@L18114:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11617
	or	 r11,r0,0
@L11619:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11618
	bcnd	 eq0,r11,@L11617
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11617
	st	 r10,r12[r25]
	align	 4
@L11618:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11619
@L11617:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11623
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18115
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11624
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11624:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18115:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11623
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11623:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11637
	bb1.n	 gt,r13,@L11646
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11668
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L11630
	ld.bu	 r13,r10,8
@L17593:
	br.n	 @L11609
	st.b	 r13,r31,120
	align	 4
@L11630:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11632
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11635
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11635:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11609
	st.b	 r13,r31,120
	align	 4
@L11632:
	br.n	 @L17593
	or	 r13,r0,0
	align	 4
@L11637:
	bcnd	 eq0,r10,@L11639
	ld.hu	 r13,r10,8
@L17594:
	br.n	 @L11609
	st.h	 r13,r31,120
	align	 4
@L11639:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11641
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11644
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11644:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11609
	st.h	 r13,r31,120
	align	 4
@L11641:
	br.n	 @L17594
	or	 r13,r0,0
	align	 4
@L11646:
	bcnd	 eq0,r10,@L11648
	ld	 r13,r10,8
@L17595:
	br.n	 @L11609
	st	 r13,r31,120
	align	 4
@L11648:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11650
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11653
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11653:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11609
	st	 r13,r31,120
	align	 4
@L11650:
	br.n	 @L17595
	or	 r13,r0,0
	align	 4
@L11668:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11608:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L11609:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r31,120
	ld	 r12,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L11671
	st	 r13,r0,r21
	ld.hu	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	or	 r9,r0,1
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r11,r12,3<5>
	mask	 r12,r12,31
	ld	 r13,r18[r11]
	mak	 r12,r9,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	or.u	 r10,r0,hi16(_ss_lr_temp)
	or.u	 r12,r0,hi16(_temp_bs)
	extu	 r11,r13,0<16>
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	mask	 r6,r11,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks)
	extu	 r8,r6,0<5>
	ld	 r7,r13[r12]
	mask	 r11,r11,31
	ld	 r13,r18[r8]
	mak	 r9,r9,r11
	ld	 r12,r10,lo16(_ss_lr_temp)
	and	 r13,r13,r9
	bcnd.n	 eq0,r13,@L11673
	and.c	 r12,r12,r7
	ld	 r13,r16[r6]
	br.n	 @L18116
	and	 r13,r13,r7
	align	 4
@L11673:
	ld	 r13,r15[r6]
	and	 r13,r13,r7
@L18116:
	or	 r13,r13,r12
	br.n	 @L10109
	st	 r13,r16[r6]
	align	 4
@L11671:
	ld	 r13,r31,108
	or.u	 r12,r0,hi16(_temp_bs)
	or.u	 r19,r0,hi16(_use_spec_R)
	ld	 r12,r12,lo16(_temp_bs)
	extu	 r10,r13,0<16>
	or	 r19,r19,lo16(_use_spec_R)
	ext	 r13,r13,16<0>
	mask	 r8,r10,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks)
	extu	 r11,r8,0<5>
	ld	 r9,r13[r12]
	mask	 r10,r10,31
	ld	 r11,r19[r11]
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	ld	 r12,r12,lo16(_ss_lr_temp)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L11675
	and.c	 r12,r12,r9
	ld	 r13,r16[r8]
	br.n	 @L18117
	and	 r13,r13,r9
	align	 4
@L11675:
	ld	 r13,r15[r8]
	and	 r13,r13,r9
@L18117:
	or	 r13,r13,r12
	br.n	 @L10109
	st	 r13,r15[r8]
	align	 4
@L11677:
	ld	 r13,r31,108
	or	 r12,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r10,r13,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r11,r10,31
	ld	 r13,r18[r13]
	mak	 r12,r12,r11
	or.u	 r11,r0,hi16(_temp_bs)
	and	 r13,r13,r12
	bcnd.n	 eq0,r13,@L11678
	or	 r12,r11,lo16(_temp_bs)
	ld	 r11,r16[r10]
	br.n	 @L18118
	st	 r11,r0,r12
	align	 4
@L11678:
	ld	 r11,r15[r10]
	st	 r11,r0,r12
@L18118:
	or.u	 r13,r0,hi16(_temp_bs)
	ld.h	 r12,r31,110
	ld	 r13,r13,lo16(_temp_bs)
	or.u	 r11,r0,hi16(_ss_lr_temp)
	or.u	 r19,r0,hi16(_spec_mode)
	addu	 r13,r13,r12
	ld	 r12,r19,lo16(_spec_mode)
	or	 r21,r11,lo16(_ss_lr_temp)
	bcnd.n	 eq0,r12,@L11680
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L11686
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18119
	extu	 r13,r24,0<24>
@L11686:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11681
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11681
	extu	 r13,r24,0<24>
@L18119:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11689
	or	 r11,r0,0
@L11691:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11690
	bcnd	 eq0,r11,@L11689
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11689
	st	 r10,r12[r25]
	align	 4
@L11690:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11691
@L11689:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L11695
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18120
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11696
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11696:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18120:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11695
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11695:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L11709
	bb1.n	 gt,r13,@L11718
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11740
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L11702
	ld.bu	 r13,r10,8
@L17598:
	br.n	 @L11681
	st.b	 r13,r31,120
	align	 4
@L11702:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11704
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11707
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11707:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L11681
	st.b	 r13,r31,120
	align	 4
@L11704:
	br.n	 @L17598
	or	 r13,r0,0
	align	 4
@L11709:
	bcnd	 eq0,r10,@L11711
	ld.hu	 r13,r10,8
@L17599:
	br.n	 @L11681
	st.h	 r13,r31,120
	align	 4
@L11711:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11713
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11716
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11716:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L11681
	st.h	 r13,r31,120
	align	 4
@L11713:
	br.n	 @L17599
	or	 r13,r0,0
	align	 4
@L11718:
	bcnd	 eq0,r10,@L11720
	ld	 r13,r10,8
@L17600:
	br.n	 @L11681
	st	 r13,r31,120
	align	 4
@L11720:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L11722
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L11725
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L11725:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L11681
	st	 r13,r31,120
	align	 4
@L11722:
	br.n	 @L17600
	or	 r13,r0,0
	align	 4
@L11740:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11680:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L11681:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r31,120
	ld	 r12,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L11743
	st	 r13,r0,r21
	ld.hu	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	or	 r8,r0,1
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r11,r12,3<5>
	mask	 r12,r12,31
	ld	 r13,r19[r11]
	mak	 r12,r8,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	or.u	 r9,r0,hi16(_ss_lr_temp)
	or.u	 r12,r0,hi16(_temp_bs)
	extu	 r11,r13,0<16>
	ld	 r12,r12,lo16(_temp_bs)
	ext	 r13,r13,16<0>
	mask	 r6,r11,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks+4)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks+4)
	extu	 r7,r6,0<5>
	ld	 r10,r13[r12]
	mask	 r11,r11,31
	ld	 r13,r19[r7]
	mak	 r8,r8,r11
	ld	 r12,r9,lo16(_ss_lr_temp)
	xor.c	 r11,r10,r0
	and	 r13,r13,r8
	bcnd.n	 eq0,r13,@L11745
	and	 r12,r12,r10
	ld	 r13,r16[r6]
	br.n	 @L18121
	and	 r13,r13,r11
	align	 4
@L11745:
	ld	 r13,r15[r6]
	and	 r13,r13,r11
@L18121:
	or	 r12,r13,r12
	br.n	 @L10109
	st	 r12,r16[r6]
	align	 4
@L11743:
	ld	 r13,r31,108
	or.u	 r12,r0,hi16(_temp_bs)
	or.u	 r18,r0,hi16(_use_spec_R)
	ld	 r12,r12,lo16(_temp_bs)
	extu	 r10,r13,0<16>
	or	 r18,r18,lo16(_use_spec_R)
	ext	 r13,r13,16<0>
	mask	 r8,r10,255
	addu	 r12,r12,r13
	or.u	 r13,r0,hi16(_ss_lr_masks+4)
	mask	 r12,r12,3
	or	 r13,r13,lo16(_ss_lr_masks+4)
	extu	 r11,r8,0<5>
	ld	 r9,r13[r12]
	mask	 r10,r10,31
	ld	 r11,r18[r11]
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	ld	 r12,r12,lo16(_ss_lr_temp)
	xor.c	 r10,r9,r0
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L11747
	and	 r12,r12,r9
	ld	 r13,r16[r8]
	br.n	 @L18122
	and	 r13,r13,r10
	align	 4
@L11747:
	ld	 r13,r15[r8]
	and	 r13,r13,r10
@L18122:
	or	 r12,r13,r12
	br.n	 @L10109
	st	 r12,r15[r8]
	align	 4
@L11749:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11750
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L11752
	ld	 r13,r16[r9]
	br.n	 @L18123
	addu	 r13,r13,r12
	align	 4
@L11752:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18123:
	br.n	 @L11751
	st	 r13,r16[r9]
	align	 4
@L11750:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L11754
	ld	 r13,r16[r9]
	br.n	 @L18124
	addu	 r13,r13,r11
	align	 4
@L11754:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18124:
	st	 r13,r15[r9]
@L11751:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L11756
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18125
	addu	 r24,r13,r3
	align	 4
@L11756:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18125:
	ld.hu	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	mask	 r10,r11,255
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L11758
	ld	 r13,r16[r10]
	br.n	 @L18126
	or	 r12,r0,r13
	align	 4
@L11758:
	ld	 r13,r15[r10]
	or	 r12,r0,r13
@L18126:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11760
	st.b	 r12,r31,113
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11761
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11761
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11769
	or	 r11,r0,0
@L11771:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11770
	bcnd	 eq0,r11,@L11769
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11769
	st	 r10,r12[r25]
	align	 4
@L11770:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11771
@L11769:
	bcnd.n	 ne0,r10,@L18127
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18128
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11776
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11776:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18128:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11775
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11775:
	cmp	 r13,r22,2
@L18127:
	bb0	 ne,r13,@L11790
	bb0.n	 gt,r13,@L11781
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L11799
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L11808
	or.u	 r2,r0,hi16(@LC509)
	br	 @L11820
	align	 4
@L11781:
	ld.bu	 r13,r31,113
	br.n	 @L11761
	st.b	 r13,r10,8
	align	 4
@L11790:
	ld.hu	 r13,r31,113
	br.n	 @L11761
	st.h	 r13,r10,8
	align	 4
@L11799:
	ld	 r13,r31,113
	br.n	 @L11761
	st	 r13,r10,8
	align	 4
@L11808:
	ld	 r13,r31,113
	st	 r13,r10,8
	ld	 r13,r31,117
	br.n	 @L11761
	st	 r13,r10,12
	align	 4
@L11820:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11760:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,113
	bsr.n	 _mem_access
	or	 r5,r0,1
@L11761:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11823
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L11823:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L11829:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11830
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L11832
	ld	 r13,r16[r9]
	br.n	 @L18129
	addu	 r13,r13,r12
	align	 4
@L11832:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18129:
	br.n	 @L11831
	st	 r13,r16[r9]
	align	 4
@L11830:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+20)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L11834
	ld	 r13,r16[r9]
	br.n	 @L18130
	addu	 r13,r13,r11
	align	 4
@L11834:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18130:
	st	 r13,r15[r9]
@L11831:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L11836
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18131
	addu	 r24,r13,r3
	align	 4
@L11836:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18131:
	ld.hu	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	mask	 r10,r11,255
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L11838
	ld	 r13,r16[r10]
	br.n	 @L18132
	or	 r12,r0,r13
	align	 4
@L11838:
	ld	 r13,r15[r10]
	or	 r12,r0,r13
@L18132:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11840
	st.h	 r12,r31,116
	bb1.n	 (31-31),r24,@L18133
	or.u	 r18,r0,hi16(_spec_mode)
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L18133
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L18133
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11849
	or	 r11,r0,0
@L11851:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11850
	bcnd	 eq0,r11,@L11849
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11849
	st	 r10,r12[r25]
	align	 4
@L11850:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11851
@L11849:
	bcnd.n	 ne0,r10,@L11870
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18134
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11856
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11856:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18134:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11870
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11870:
	ld.hu	 r13,r31,116
	br.n	 @L11841
	st.h	 r13,r10,8
	align	 4
@L11840:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,116
	bsr.n	 _mem_access
	or	 r5,r0,2
@L11841:
	or.u	 r18,r0,hi16(_spec_mode)
@L18133:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11903
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+20)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+20)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L11903:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+20)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+20)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L11909:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11910
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L11912
	ld	 r13,r16[r9]
	br.n	 @L18135
	addu	 r13,r13,r12
	align	 4
@L11912:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18135:
	br.n	 @L11911
	st	 r13,r16[r9]
	align	 4
@L11910:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L11914
	ld	 r13,r16[r9]
	br.n	 @L18136
	addu	 r13,r13,r11
	align	 4
@L11914:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18136:
	st	 r13,r15[r9]
@L11911:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L11916
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18137
	addu	 r24,r13,r3
	align	 4
@L11916:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18137:
	ld.hu	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	mask	 r10,r11,255
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r10,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L11918
	ld	 r11,r16[r10]
	br.n	 @L18138
	or.u	 r18,r0,hi16(_spec_mode)
	align	 4
@L11918:
	ld	 r11,r15[r10]
	or.u	 r18,r0,hi16(_spec_mode)
@L18138:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11920
	st	 r11,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L11921
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L11921
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L11921
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L11929
	or	 r11,r0,0
@L11931:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L11930
	bcnd	 eq0,r11,@L11929
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L11929
	st	 r10,r12[r25]
	align	 4
@L11930:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L11931
@L11929:
	bcnd.n	 ne0,r10,@L18139
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18140
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L11936
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L11936:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18140:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L11935
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L11935:
	cmp	 r13,r22,2
@L18139:
	bb0	 ne,r13,@L11950
	bb1.n	 gt,r13,@L11959
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L11980
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L11921
	st.b	 r13,r10,8
	align	 4
@L11950:
	ld.hu	 r13,r31,120
	br.n	 @L11921
	st.h	 r13,r10,8
	align	 4
@L11959:
	ld	 r13,r31,120
	br.n	 @L11921
	st	 r13,r10,8
	align	 4
@L11980:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L11920:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L11921:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11983
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L11983:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L11989:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L11990
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L11992
	ld	 r13,r16[r9]
	br.n	 @L18141
	addu	 r13,r13,r12
	align	 4
@L11992:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18141:
	br.n	 @L11991
	st	 r13,r16[r9]
	align	 4
@L11990:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L11994
	ld	 r13,r16[r9]
	br.n	 @L18142
	addu	 r13,r13,r11
	align	 4
@L11994:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18142:
	st	 r13,r15[r9]
@L11991:
	ld.hu	 r13,r31,108
	bb0.n	 (31-31),r13,@L11997
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L11997
	or.u	 r2,r0,hi16(@LC850)
	or	 r4,r0,378
	or.u	 r3,r0,hi16(@LC851)
	or.u	 r5,r0,hi16(@LC852)
	or	 r2,r2,lo16(@LC850)
	or	 r3,r3,lo16(@LC851)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC852)
	align	 4
@L11997:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12000
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18143
	addu	 r24,r13,r3
	align	 4
@L12000:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18143:
	ld.bu	 r12,r31,109
	or.u	 r13,r0,hi16(_sim_swap_words)
	or.u	 r19,r0,hi16(_use_spec_R)
	ld	 r13,r13,lo16(_sim_swap_words)
	or	 r19,r19,lo16(_use_spec_R)
	xor	 r10,r12,r13
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r10,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L12002
	ld	 r11,r16[r10]
	br.n	 @L18144
	or.u	 r18,r0,hi16(_spec_mode)
	align	 4
@L12002:
	ld	 r11,r15[r10]
	or.u	 r18,r0,hi16(_spec_mode)
@L18144:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12004
	st	 r11,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12005
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12005
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12005
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12013
	or	 r11,r0,0
@L12015:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12014
	bcnd	 eq0,r11,@L12013
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12013
	st	 r10,r12[r25]
	align	 4
@L12014:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12015
@L12013:
	bcnd.n	 ne0,r10,@L18145
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18146
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12020
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12020:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18146:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12019
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12019:
	cmp	 r13,r22,2
@L18145:
	bb0	 ne,r13,@L12034
	bb1.n	 gt,r13,@L12043
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12064
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L12005
	st.b	 r13,r10,8
	align	 4
@L12034:
	ld.hu	 r13,r31,120
	br.n	 @L12005
	st.h	 r13,r10,8
	align	 4
@L12043:
	ld	 r13,r31,120
	br.n	 @L12005
	st	 r13,r10,8
	align	 4
@L12064:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12004:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12005:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12067
	ext	 r11,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18147
	addu	 r13,r13,r11
	align	 4
@L12067:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18147:
	addu	 r24,r13,4
	ld.bu	 r12,r31,109
	or.u	 r13,r0,hi16(_sim_swap_words)
	or.u	 r18,r0,hi16(_use_spec_R)
	ld	 r13,r13,lo16(_sim_swap_words)
	addu	 r12,r12,1
	or	 r18,r18,lo16(_use_spec_R)
	xor	 r10,r12,r13
	or	 r13,r0,1
	extu	 r12,r10,0<5>
	mask	 r11,r10,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L12069
	ld	 r11,r16[r10]
	br.n	 @L18148
	or.u	 r19,r0,hi16(_spec_mode)
	align	 4
@L12069:
	ld	 r11,r15[r10]
	or.u	 r19,r0,hi16(_spec_mode)
@L18148:
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12071
	st	 r11,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12072
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12072
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12072
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12080
	or	 r11,r0,0
@L12082:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12081
	bcnd	 eq0,r11,@L12080
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12080
	st	 r10,r12[r25]
	align	 4
@L12081:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12082
@L12080:
	bcnd.n	 ne0,r10,@L18149
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18150
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12087
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12087:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18150:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12086
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12086:
	cmp	 r13,r22,2
@L18149:
	bb0	 ne,r13,@L12101
	bb1.n	 gt,r13,@L12110
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12131
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L12072
	st.b	 r13,r10,8
	align	 4
@L12101:
	ld.hu	 r13,r31,120
	br.n	 @L12072
	st.h	 r13,r10,8
	align	 4
@L12110:
	ld	 r13,r31,120
	br.n	 @L12072
	st	 r13,r10,8
	align	 4
@L12131:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12071:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12072:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12134
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L12134:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L12140:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12141
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L12143
	ld	 r13,r16[r9]
	br.n	 @L18151
	addu	 r13,r13,r12
	align	 4
@L12143:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18151:
	br.n	 @L12142
	st	 r13,r16[r9]
	align	 4
@L12141:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L12145
	ld	 r13,r16[r9]
	br.n	 @L18152
	addu	 r13,r13,r11
	align	 4
@L12145:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18152:
	st	 r13,r15[r9]
@L12142:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12147
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18153
	addu	 r24,r13,r3
	align	 4
@L12147:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18153:
	or.u	 r19,r0,hi16(_use_spec_R)
	ld	 r13,r19,lo16(_use_spec_R)
	bb0.n	 (31-31),r13,@L12149
	or.u	 r18,r0,hi16(_spec_regs_R)
	ld	 r12,r18,lo16(_spec_regs_R)
	br.n	 @L18154
	or.u	 r18,r0,hi16(_spec_mode)
	align	 4
@L12149:
	or.u	 r19,r0,hi16(_regs_R)
	ld	 r12,r19,lo16(_regs_R)
	or.u	 r18,r0,hi16(_spec_mode)
@L18154:
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12151
	st	 r12,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12152
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12152
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12152
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12160
	or	 r11,r0,0
@L12162:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12161
	bcnd	 eq0,r11,@L12160
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12160
	st	 r10,r12[r25]
	align	 4
@L12161:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12162
@L12160:
	bcnd.n	 ne0,r10,@L18155
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18156
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12167
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12167:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18156:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12166
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12166:
	cmp	 r13,r22,2
@L18155:
	bb0	 ne,r13,@L12181
	bb1.n	 gt,r13,@L12190
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12211
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L12152
	st.b	 r13,r10,8
	align	 4
@L12181:
	ld.hu	 r13,r31,120
	br.n	 @L12152
	st.h	 r13,r10,8
	align	 4
@L12190:
	ld	 r13,r31,120
	br.n	 @L12152
	st	 r13,r10,8
	align	 4
@L12211:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12151:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12152:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12214
	ext	 r11,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18157
	addu	 r13,r13,r11
	align	 4
@L12214:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18157:
	addu	 r24,r13,4
	or.u	 r18,r0,hi16(_use_spec_R)
	ld	 r13,r18,lo16(_use_spec_R)
	bb0.n	 (31-31),r13,@L12216
	or.u	 r19,r0,hi16(_spec_regs_R)
	ld	 r12,r19,lo16(_spec_regs_R)
	br.n	 @L18158
	or.u	 r19,r0,hi16(_spec_mode)
	align	 4
@L12216:
	or.u	 r18,r0,hi16(_regs_R)
	ld	 r12,r18,lo16(_regs_R)
	or.u	 r19,r0,hi16(_spec_mode)
@L18158:
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12218
	st	 r12,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12219
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12219
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12219
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12227
	or	 r11,r0,0
@L12229:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12228
	bcnd	 eq0,r11,@L12227
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12227
	st	 r10,r12[r25]
	align	 4
@L12228:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12229
@L12227:
	bcnd.n	 ne0,r10,@L18159
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18160
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12234
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12234:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18160:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12233
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12233:
	cmp	 r13,r22,2
@L18159:
	bb0	 ne,r13,@L12248
	bb1.n	 gt,r13,@L12257
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12278
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L12219
	st.b	 r13,r10,8
	align	 4
@L12248:
	ld.hu	 r13,r31,120
	br.n	 @L12219
	st.h	 r13,r10,8
	align	 4
@L12257:
	ld	 r13,r31,120
	br.n	 @L12219
	st	 r13,r10,8
	align	 4
@L12278:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12218:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12219:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12281
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+140)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+140)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L12281:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+140)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+140)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L12287:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12288
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L12290
	ld	 r13,r16[r9]
	br.n	 @L18161
	addu	 r13,r13,r12
	align	 4
@L12290:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18161:
	br.n	 @L12289
	st	 r13,r16[r9]
	align	 4
@L12288:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L12292
	ld	 r13,r16[r9]
	br.n	 @L18162
	addu	 r13,r13,r11
	align	 4
@L12292:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18162:
	st	 r13,r15[r9]
@L12289:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12294
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18163
	addu	 r24,r13,r3
	align	 4
@L12294:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18163:
	ld.hu	 r9,r31,108
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12296
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L17635
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L12296:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L17635:
	ld	 r12,r13[r12]
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12298
	st	 r12,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12299
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12299
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12299
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12307
	or	 r11,r0,0
@L12309:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12308
	bcnd	 eq0,r11,@L12307
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12307
	st	 r10,r12[r25]
	align	 4
@L12308:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12309
@L12307:
	bcnd.n	 ne0,r10,@L18164
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18165
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12314
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12314:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18165:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12313
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12313:
	cmp	 r13,r22,2
@L18164:
	bb0	 ne,r13,@L12328
	bb1.n	 gt,r13,@L12337
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12358
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L12299
	st.b	 r13,r10,8
	align	 4
@L12328:
	ld.hu	 r13,r31,120
	br.n	 @L12299
	st.h	 r13,r10,8
	align	 4
@L12337:
	ld	 r13,r31,120
	br.n	 @L12299
	st	 r13,r10,8
	align	 4
@L12358:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12298:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12299:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12361
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L12361:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L12367:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12368
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L12370
	ld	 r13,r16[r9]
	br.n	 @L18166
	addu	 r13,r13,r12
	align	 4
@L12370:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18166:
	br.n	 @L12369
	st	 r13,r16[r9]
	align	 4
@L12368:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L12372
	ld	 r13,r16[r9]
	br.n	 @L18167
	addu	 r13,r13,r11
	align	 4
@L12372:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18167:
	st	 r13,r15[r9]
@L12369:
	ld.hu	 r13,r31,108
	bb0.n	 (31-31),r13,@L12375
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 ne0,r13,@L12375
	or.u	 r2,r0,hi16(@LC853)
	or	 r4,r0,398
	or.u	 r3,r0,hi16(@LC854)
	or.u	 r5,r0,hi16(@LC855)
	or	 r2,r2,lo16(@LC853)
	or	 r3,r3,lo16(@LC854)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC855)
	align	 4
@L12375:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12378
	ext	 r3,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18168
	addu	 r24,r13,r3
	align	 4
@L12378:
	ld	 r13,r15[r9]
	addu	 r24,r13,r3
@L18168:
	ld.hu	 r9,r31,108
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r9,3<5>
	mask	 r11,r9,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12380
	or.u	 r13,r0,hi16(_spec_regs_F)
	mask	 r12,r9,255
	br.n	 @L17641
	or	 r13,r13,lo16(_spec_regs_F)
	align	 4
@L12380:
	or.u	 r13,r0,hi16(_regs_F)
	mask	 r12,r9,255
	or	 r13,r13,lo16(_regs_F)
@L17641:
	ld	 r12,r13[r12]
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12382
	st	 r12,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12383
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12383
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12383
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12391
	or	 r11,r0,0
@L12393:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12392
	bcnd	 eq0,r11,@L12391
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12391
	st	 r10,r12[r25]
	align	 4
@L12392:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12393
@L12391:
	bcnd.n	 ne0,r10,@L18169
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18170
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12398
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12398:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18170:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12397
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12397:
	cmp	 r13,r22,2
@L18169:
	bb0	 ne,r13,@L12412
	bb1.n	 gt,r13,@L12421
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12442
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L12383
	st.b	 r13,r10,8
	align	 4
@L12412:
	ld.hu	 r13,r31,120
	br.n	 @L12383
	st.h	 r13,r10,8
	align	 4
@L12421:
	ld	 r13,r31,120
	br.n	 @L12383
	st	 r13,r10,8
	align	 4
@L12442:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12382:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12383:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12445
	ext	 r11,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18171
	addu	 r13,r13,r11
	align	 4
@L12445:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18171:
	addu	 r24,r13,4
	ld.bu	 r9,r31,109
	or.u	 r12,r0,hi16(_use_spec_F)
	or	 r13,r0,1
	addu	 r11,r9,1
	or	 r12,r12,lo16(_use_spec_F)
	extu	 r10,r11,0<5>
	mask	 r11,r11,30
	ld	 r12,r12[r10]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12447
	or.u	 r13,r0,hi16(_spec_regs_F+4)
	br.n	 @L17643
	or	 r13,r13,lo16(_spec_regs_F+4)
	align	 4
@L12447:
	or.u	 r13,r0,hi16(_regs_F+4)
	or	 r13,r13,lo16(_regs_F+4)
@L17643:
	ld	 r11,r13[r9]
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12449
	st	 r11,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12450
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12450
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12450
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12458
	or	 r11,r0,0
@L12460:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12459
	bcnd	 eq0,r11,@L12458
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12458
	st	 r10,r12[r25]
	align	 4
@L12459:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12460
@L12458:
	bcnd.n	 ne0,r10,@L18172
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18173
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12465
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12465:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18173:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12464
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12464:
	cmp	 r13,r22,2
@L18172:
	bb0	 ne,r13,@L12479
	bb1.n	 gt,r13,@L12488
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12509
	or.u	 r2,r0,hi16(@LC509)
	ld.bu	 r13,r31,120
	br.n	 @L12450
	st.b	 r13,r10,8
	align	 4
@L12479:
	ld.hu	 r13,r31,120
	br.n	 @L12450
	st.h	 r13,r10,8
	align	 4
@L12488:
	ld	 r13,r31,120
	br.n	 @L12450
	st	 r13,r10,8
	align	 4
@L12509:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12449:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12450:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12512
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab+60)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab+60)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L12512:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab+60)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab+60)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L12518:
	ld	 r9,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r9,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r8,r9,0<24>
	ld	 r11,r18[r13]
	mask	 r10,r8,31
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	or	 r21,r12,lo16(_ss_lr_temp)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L12519
	ext	 r9,r9,16<0>
	ld	 r13,r16[r8]
	br.n	 @L18174
	addu	 r13,r13,r9
	align	 4
@L12519:
	ld	 r13,r15[r8]
	addu	 r13,r13,r9
@L18174:
	and	 r24,r13,0xfffc
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd	 eq0,r13,@L12521
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12522
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L12527
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18175
	extu	 r13,r24,0<24>
@L12527:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12522
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12522
	extu	 r13,r24,0<24>
@L18175:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12530
	or	 r11,r0,0
@L12532:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12531
	bcnd	 eq0,r11,@L12530
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12530
	st	 r10,r12[r25]
	align	 4
@L12531:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12532
@L12530:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L12536
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18176
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12537
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12537:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18176:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12536
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12536:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L12550
	bb1.n	 gt,r13,@L12559
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12581
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L12543
	ld.bu	 r13,r10,8
@L17647:
	br.n	 @L12522
	st.b	 r13,r31,120
	align	 4
@L12543:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12545
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12548
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12548:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L12522
	st.b	 r13,r31,120
	align	 4
@L12545:
	br.n	 @L17647
	or	 r13,r0,0
	align	 4
@L12550:
	bcnd	 eq0,r10,@L12552
	ld.hu	 r13,r10,8
@L17648:
	br.n	 @L12522
	st.h	 r13,r31,120
	align	 4
@L12552:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12554
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12557
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12557:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L12522
	st.h	 r13,r31,120
	align	 4
@L12554:
	br.n	 @L17648
	or	 r13,r0,0
	align	 4
@L12559:
	bcnd	 eq0,r10,@L12561
	ld	 r13,r10,8
@L17649:
	br.n	 @L12522
	st	 r13,r31,120
	align	 4
@L12561:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12563
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12566
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12566:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L12522
	st	 r13,r31,120
	align	 4
@L12563:
	br.n	 @L17649
	or	 r13,r0,0
	align	 4
@L12581:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12521:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12522:
	ld	 r13,r31,108
	ld	 r12,r31,120
	st	 r12,r0,r21
	or	 r11,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	or.u	 r18,r0,hi16(_use_spec_R)
	ext	 r7,r13,16<0>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r8,r13,0<24>
	or	 r6,r12,lo16(_ss_lr_temp)
	extu	 r13,r13,0<29>
	mask	 r10,r8,31
	ld	 r13,r18[r13]
	mak	 r11,r11,r10
	or.u	 r12,r0,hi16(_ss_lr_masks)
	and	 r13,r13,r11
	bcnd.n	 eq0,r13,@L12584
	or	 r9,r12,lo16(_ss_lr_masks)
	ld	 r13,r16[r8]
	br.n	 @L18177
	addu	 r13,r13,r7
	align	 4
@L12584:
	ld	 r13,r15[r8]
	addu	 r13,r13,r7
@L18177:
	mak	 r13,r13,2<2>
	addu	 r9,r13,r9
	ld	 r13,r31,108
	ld	 r9,r0,r9
	or	 r12,r0,1
	ext	 r7,r13,16<0>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r8,r13,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r13,r13,0<29>
	xor.c	 r9,r9,r0
	ld	 r11,r19[r13]
	mask	 r10,r8,31
	or.u	 r13,r0,hi16(_ss_lr_temp)
	mak	 r12,r12,r10
	or	 r10,r13,lo16(_ss_lr_temp)
	or.u	 r13,r0,hi16(_ss_lr_masks)
	and	 r11,r11,r12
	bcnd.n	 eq0,r11,@L12586
	or	 r12,r13,lo16(_ss_lr_masks)
	ld	 r13,r16[r8]
	br.n	 @L18178
	addu	 r13,r13,r7
	align	 4
@L12586:
	ld	 r13,r15[r8]
	addu	 r13,r13,r7
@L18178:
	mak	 r13,r13,2<2>
	addu	 r7,r13,r12
	ld.hu	 r11,r31,108
	ld	 r10,r0,r10
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	mask	 r8,r11,255
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r8,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	ld	 r11,r0,r7
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12588
	and	 r10,r10,r11
	ld	 r13,r16[r8]
	br.n	 @L18179
	and	 r13,r13,r9
	align	 4
@L12588:
	ld	 r13,r15[r8]
	and	 r13,r13,r9
@L18179:
	or	 r10,r13,r10
	ld	 r11,r31,108
	st	 r10,r0,r6
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12590
	ext	 r11,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18180
	addu	 r13,r13,r11
	align	 4
@L12590:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18180:
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ss_lr_temp)
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r13,lo16(_ss_lr_temp)
	ld	 r12,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L12729
	st	 r13,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L10109
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10109
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10109
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12601
	or	 r11,r0,0
@L12603:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12602
	bcnd	 eq0,r11,@L12601
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12601
	st	 r10,r12[r25]
	align	 4
@L12602:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12603
@L12601:
	bcnd.n	 ne0,r10,@L18181
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18182
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12608
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12608:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18182:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12607
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12607:
	cmp	 r13,r22,2
@L18181:
	bb0	 ne,r13,@L12759
	bb1.n	 gt,r13,@L12768
	cmp	 r13,r22,1
	bb0.n	 ne,r13,@L17654
	or.u	 r2,r0,hi16(@LC509)
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12655:
	ld	 r9,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r9,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r8,r9,0<24>
	ld	 r11,r19[r13]
	mask	 r10,r8,31
	or	 r13,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r10
	or	 r21,r12,lo16(_ss_lr_temp)
	and	 r11,r11,r13
	bcnd.n	 eq0,r11,@L12656
	ext	 r9,r9,16<0>
	ld	 r13,r16[r8]
	br.n	 @L18183
	addu	 r13,r13,r9
	align	 4
@L12656:
	ld	 r13,r15[r8]
	addu	 r13,r13,r9
@L18183:
	and	 r24,r13,0xfffc
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12658
	or	 r9,r0,0
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L12659
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L12664
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18184
	extu	 r13,r24,0<24>
@L12664:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12659
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12659
	extu	 r13,r24,0<24>
@L18184:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12667
	or	 r11,r0,0
@L12669:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12668
	bcnd	 eq0,r11,@L12667
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12667
	st	 r10,r12[r25]
	align	 4
@L12668:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12669
@L12667:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L12673
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18185
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12674
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12674:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18185:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12673
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12673:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L12687
	bb1.n	 gt,r13,@L12696
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12718
	or.u	 r2,r0,hi16(@LC509)
	bcnd	 eq0,r10,@L12680
	ld.bu	 r13,r10,8
@L17656:
	br.n	 @L12659
	st.b	 r13,r31,120
	align	 4
@L12680:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12682
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12685
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12685:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L12659
	st.b	 r13,r31,120
	align	 4
@L12682:
	br.n	 @L17656
	or	 r13,r0,0
	align	 4
@L12687:
	bcnd	 eq0,r10,@L12689
	ld.hu	 r13,r10,8
@L17657:
	br.n	 @L12659
	st.h	 r13,r31,120
	align	 4
@L12689:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12691
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12694
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12694:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L12659
	st.h	 r13,r31,120
	align	 4
@L12691:
	br.n	 @L17657
	or	 r13,r0,0
	align	 4
@L12696:
	bcnd	 eq0,r10,@L12698
	ld	 r13,r10,8
@L17658:
	br.n	 @L12659
	st	 r13,r31,120
	align	 4
@L12698:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12700
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12703
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12703:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L12659
	st	 r13,r31,120
	align	 4
@L12700:
	br.n	 @L17658
	or	 r13,r0,0
	align	 4
@L12718:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12658:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,120
	bsr.n	 _mem_access
	or	 r5,r0,4
@L12659:
	ld	 r13,r31,108
	ld	 r12,r31,120
	st	 r12,r0,r21
	or	 r11,r0,1
	or.u	 r12,r0,hi16(_ss_lr_temp)
	or.u	 r19,r0,hi16(_use_spec_R)
	ext	 r8,r13,16<0>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r9,r13,0<24>
	or	 r5,r12,lo16(_ss_lr_temp)
	extu	 r13,r13,0<29>
	mask	 r10,r9,31
	ld	 r13,r19[r13]
	mak	 r11,r11,r10
	or.u	 r12,r0,hi16(_ss_lr_masks)
	and	 r13,r13,r11
	bcnd.n	 eq0,r13,@L12721
	or	 r12,r12,lo16(_ss_lr_masks)
	ld	 r13,r16[r9]
	br.n	 @L18186
	addu	 r13,r13,r8
	align	 4
@L12721:
	ld	 r13,r15[r9]
	addu	 r13,r13,r8
@L18186:
	mask	 r13,r13,3
	addu	 r13,r13,1
	lda	 r11,r12[r13]
	ld	 r13,r31,108
	ld	 r7,r0,r11
	extu	 r12,r13,0<29>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r13,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	ext	 r8,r13,16<0>
	or	 r13,r0,1
	ld	 r10,r18[r12]
	mask	 r11,r9,31
	or.u	 r12,r0,hi16(_ss_lr_temp)
	mak	 r13,r13,r11
	or	 r6,r12,lo16(_ss_lr_temp)
	or.u	 r12,r0,hi16(_ss_lr_masks)
	and	 r10,r10,r13
	bcnd.n	 eq0,r10,@L12723
	or	 r12,r12,lo16(_ss_lr_masks)
	ld	 r13,r16[r9]
	br.n	 @L18187
	addu	 r13,r13,r8
	align	 4
@L12723:
	ld	 r13,r15[r9]
	addu	 r13,r13,r8
@L18187:
	mask	 r13,r13,3
	addu	 r13,r13,1
	lda	 r10,r12[r13]
	ld.hu	 r11,r31,108
	ld	 r10,r0,r10
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	mask	 r9,r11,255
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	ld	 r11,r0,r6
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12725
	and.c	 r11,r11,r10
	ld	 r13,r16[r9]
	br.n	 @L18188
	and	 r13,r13,r7
	align	 4
@L12725:
	ld	 r13,r15[r9]
	and	 r13,r13,r7
@L18188:
	or	 r13,r13,r11
	ld	 r11,r31,108
	st	 r13,r0,r5
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12727
	ext	 r11,r11,16<0>
	ld	 r13,r16[r9]
	br.n	 @L18189
	addu	 r13,r13,r11
	align	 4
@L12727:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18189:
	and	 r24,r13,0xfffc
	or.u	 r13,r0,hi16(_ss_lr_temp)
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r13,lo16(_ss_lr_temp)
	ld	 r12,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r12,@L12729
	st	 r13,r31,120
	mask	 r13,r24,3
	bcnd.n	 ne0,r13,@L10109
	or	 r22,r0,4
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L10109
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L10109
	extu	 r13,r24,0<24>
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12738
	or	 r11,r0,0
@L12740:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12739
	bcnd	 eq0,r11,@L12738
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12738
	st	 r10,r12[r25]
	align	 4
@L12739:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12740
@L12738:
	bcnd.n	 ne0,r10,@L18190
	cmp	 r13,r22,2
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18191
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12745
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12745:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18191:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12744
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12744:
	cmp	 r13,r22,2
@L18190:
	bb0	 ne,r13,@L12759
	bb1.n	 gt,r13,@L12768
	cmp	 r13,r22,1
	bb1.n	 ne,r13,@L12789
	or.u	 r2,r0,hi16(@LC509)
@L17654:
	ld.bu	 r13,r31,120
	br.n	 @L10109
	st.b	 r13,r10,8
	align	 4
@L12759:
	ld.hu	 r13,r31,120
	br.n	 @L10109
	st.h	 r13,r10,8
	align	 4
@L12768:
	ld	 r13,r31,120
	br.n	 @L10109
	st	 r13,r10,8
	align	 4
@L12789:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12729:
	or	 r2,r0,1
	or	 r3,r0,r24
	addu	 r4,r31,120
	or	 r5,r0,4
	bsr	 _mem_access
	br	 @L10109
	align	 4
@L12792:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12793
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L12795
	ld	 r13,r16[r9]
	br.n	 @L18192
	addu	 r13,r13,r12
	align	 4
@L12795:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18192:
	br.n	 @L12794
	st	 r13,r16[r9]
	align	 4
@L12793:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L12797
	ld	 r13,r16[r9]
	br.n	 @L18193
	addu	 r13,r13,r11
	align	 4
@L12797:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18193:
	st	 r13,r15[r9]
@L12794:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12799
	or.u	 r18,r0,hi16(_use_spec_R)
	ld.hu	 r12,r31,108
	or	 r18,r18,lo16(_use_spec_R)
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r18[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r18[r12]
	ld	 r11,r31,108
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L12801
	extu	 r21,r11,8<16>
	ld	 r3,r16[r9]
	br.n	 @L18194
	or	 r13,r0,1
	align	 4
@L12801:
	ld	 r3,r15[r9]
	ld	 r11,r31,108
	or	 r13,r0,1
@L18194:
	extu	 r11,r11,0<8>
	or.u	 r19,r0,hi16(_use_spec_R)
	mask	 r9,r11,255
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L12803
	ld	 r13,r16[r9]
	br.n	 @L18195
	addu	 r24,r3,r13
	align	 4
@L12803:
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L18195:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12805
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L12811
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18196
	extu	 r13,r24,0<24>
@L12811:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12806
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12806
	extu	 r13,r24,0<24>
@L18196:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12814
	or	 r11,r0,0
@L12816:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12815
	bcnd	 eq0,r11,@L12814
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12814
	st	 r10,r12[r25]
	align	 4
@L12815:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12816
@L12814:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L12820
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18197
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12821
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12821:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18197:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12820
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12820:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L12834
	bb0.n	 gt,r13,@L12825
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L12843
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L12852
	or.u	 r2,r0,hi16(@LC509)
	br	 @L12865
	align	 4
@L12825:
	bcnd	 eq0,r10,@L12827
	ld.bu	 r13,r10,8
@L17666:
	br.n	 @L12806
	st.b	 r13,r31,112
	align	 4
@L12827:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12829
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12832
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12832:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L12806
	st.b	 r13,r31,112
	align	 4
@L12829:
	br.n	 @L17666
	or	 r13,r0,0
	align	 4
@L12834:
	bcnd	 eq0,r10,@L12836
	ld.hu	 r13,r10,8
@L17667:
	br.n	 @L12806
	st.h	 r13,r31,112
	align	 4
@L12836:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12838
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12841
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12841:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L12806
	st.h	 r13,r31,112
	align	 4
@L12838:
	br.n	 @L17667
	or	 r13,r0,0
	align	 4
@L12843:
	bcnd	 eq0,r10,@L12845
	ld	 r13,r10,8
@L17668:
	br.n	 @L12806
	st	 r13,r31,112
	align	 4
@L12845:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12847
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12850
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12850:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L12806
	st	 r13,r31,112
	align	 4
@L12847:
	br.n	 @L17668
	or	 r13,r0,0
	align	 4
@L12852:
	bcnd	 eq0,r10,@L12854
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,112
	br.n	 @L12806
	st	 r12,r31,116
	align	 4
@L12854:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12856
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12859
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12859:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18198
	addu	 r23,r24,4
	align	 4
@L12856:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18198:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12860
	st	 r12,r31,112
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12863
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12863:
	ld	 r12,r14[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L12806
	st	 r13,r31,116
	align	 4
@L12860:
	or	 r13,r0,0
	br.n	 @L12806
	st	 r13,r31,116
	align	 4
@L12865:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12805:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,112
	bsr.n	 _mem_access
	or	 r5,r0,1
@L12806:
	ld.b	 r13,r31,112
	br.n	 @L12800
	st	 r13,r16[r21]
	align	 4
@L12799:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L12868
	extu	 r21,r11,8<16>
	ld	 r3,r16[r9]
	br.n	 @L18199
	or	 r13,r0,1
	align	 4
@L12868:
	ld	 r3,r15[r9]
	ld	 r11,r31,108
	or	 r13,r0,1
@L18199:
	extu	 r11,r11,0<8>
	or.u	 r18,r0,hi16(_use_spec_R)
	mask	 r9,r11,255
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L12870
	ld	 r13,r16[r9]
	br.n	 @L18200
	addu	 r24,r3,r13
	align	 4
@L12870:
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L18200:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12872
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L12878
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18201
	extu	 r13,r24,0<24>
@L12878:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12873
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12873
	extu	 r13,r24,0<24>
@L18201:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12881
	or	 r11,r0,0
@L12883:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12882
	bcnd	 eq0,r11,@L12881
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12881
	st	 r10,r12[r25]
	align	 4
@L12882:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12883
@L12881:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L12887
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18202
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12888
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12888:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18202:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12887
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12887:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L12901
	bb0.n	 gt,r13,@L12892
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L12910
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L12919
	or.u	 r2,r0,hi16(@LC509)
	br	 @L12932
	align	 4
@L12892:
	bcnd	 eq0,r10,@L12894
	ld.bu	 r13,r10,8
@L17670:
	br.n	 @L12873
	st.b	 r13,r31,112
	align	 4
@L12894:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12896
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12899
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12899:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L12873
	st.b	 r13,r31,112
	align	 4
@L12896:
	br.n	 @L17670
	or	 r13,r0,0
	align	 4
@L12901:
	bcnd	 eq0,r10,@L12903
	ld.hu	 r13,r10,8
@L17671:
	br.n	 @L12873
	st.h	 r13,r31,112
	align	 4
@L12903:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12905
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12908
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12908:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L12873
	st.h	 r13,r31,112
	align	 4
@L12905:
	br.n	 @L17671
	or	 r13,r0,0
	align	 4
@L12910:
	bcnd	 eq0,r10,@L12912
	ld	 r13,r10,8
@L17672:
	br.n	 @L12873
	st	 r13,r31,112
	align	 4
@L12912:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12914
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12917
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12917:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L12873
	st	 r13,r31,112
	align	 4
@L12914:
	br.n	 @L17672
	or	 r13,r0,0
	align	 4
@L12919:
	bcnd	 eq0,r10,@L12921
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,112
	br.n	 @L12873
	st	 r12,r31,116
	align	 4
@L12921:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12923
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12926
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12926:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18203
	addu	 r23,r24,4
	align	 4
@L12923:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18203:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12927
	st	 r12,r31,112
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12930
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12930:
	ld	 r12,r14[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L12873
	st	 r13,r31,116
	align	 4
@L12927:
	or	 r13,r0,0
	br.n	 @L12873
	st	 r13,r31,116
	align	 4
@L12932:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12872:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,112
	bsr.n	 _mem_access
	or	 r5,r0,1
@L12873:
	ld.b	 r13,r31,112
	st	 r13,r15[r21]
@L12800:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12935
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r19[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r19[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_aft_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_aft_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L15499
	ld	 r13,r16[r9]
	br.n	 @L18048
	addu	 r13,r13,r12
	align	 4
@L12935:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r18,r18,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r18[r13]
	or.u	 r13,r0,hi16(_ss_aft_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_aft_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L15501
	ld	 r13,r16[r9]
	br.n	 @L18049
	addu	 r13,r13,r11
	align	 4
@L12941:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12942
	or	 r10,r0,1
	ld	 r12,r31,108
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r11,r12,0<29>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r12,5<24>
	ld	 r13,r18[r11]
	mak	 r12,r10,r12
	or	 r13,r13,r12
	st	 r13,r18[r11]
	ld	 r13,r31,108
	extu	 r9,r13,0<24>
	or.u	 r11,r0,hi16(_ss_fore_tab)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	ld	 r13,r18[r13]
	mak	 r10,r10,r12
	ld.bu	 r12,r31,106
	or	 r11,r11,lo16(_ss_fore_tab)
	and	 r13,r13,r10
	ld	 r12,r11[r12]
	bcnd	 eq0,r13,@L12944
	ld	 r13,r16[r9]
	br.n	 @L18204
	addu	 r13,r13,r12
	align	 4
@L12944:
	ld	 r13,r15[r9]
	addu	 r13,r13,r12
@L18204:
	br.n	 @L12943
	st	 r13,r16[r9]
	align	 4
@L12942:
	ld	 r13,r31,108
	or	 r11,r0,1
	extu	 r9,r13,0<24>
	or.u	 r19,r0,hi16(_use_spec_R)
	extu	 r13,r13,0<29>
	mask	 r12,r9,31
	or	 r19,r19,lo16(_use_spec_R)
	mak	 r11,r11,r12
	ld	 r12,r19[r13]
	or.u	 r13,r0,hi16(_ss_fore_tab)
	ld.bu	 r10,r31,106
	or	 r13,r13,lo16(_ss_fore_tab)
	and	 r12,r12,r11
	ld	 r11,r13[r10]
	bcnd	 eq0,r12,@L12946
	ld	 r13,r16[r9]
	br.n	 @L18205
	addu	 r13,r13,r11
	align	 4
@L12946:
	ld	 r13,r15[r9]
	addu	 r13,r13,r11
@L18205:
	st	 r13,r15[r9]
@L12943:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12948
	or.u	 r19,r0,hi16(_use_spec_R)
	ld.hu	 r12,r31,108
	or	 r19,r19,lo16(_use_spec_R)
	mask	 r11,r12,31
	extu	 r12,r12,3<5>
	or	 r10,r0,1
	ld	 r13,r19[r12]
	mak	 r11,r10,r11
	or	 r13,r13,r11
	st	 r13,r19[r12]
	ld	 r11,r31,108
	extu	 r9,r11,0<24>
	extu	 r13,r11,0<29>
	mask	 r12,r9,31
	ld	 r13,r19[r13]
	mak	 r10,r10,r12
	and	 r13,r13,r10
	bcnd.n	 eq0,r13,@L12950
	extu	 r21,r11,8<16>
	ld	 r3,r16[r9]
	br.n	 @L18206
	or	 r13,r0,1
	align	 4
@L12950:
	ld	 r3,r15[r9]
	ld	 r11,r31,108
	or	 r13,r0,1
@L18206:
	extu	 r11,r11,0<8>
	or.u	 r18,r0,hi16(_use_spec_R)
	mask	 r9,r11,255
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L12952
	ld	 r13,r16[r9]
	br.n	 @L18207
	addu	 r24,r3,r13
	align	 4
@L12952:
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L18207:
	or.u	 r19,r0,hi16(_spec_mode)
	ld	 r13,r19,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L12954
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L12960
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18208
	extu	 r13,r24,0<24>
@L12960:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L12955
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L12955
	extu	 r13,r24,0<24>
@L18208:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L12963
	or	 r11,r0,0
@L12965:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L12964
	bcnd	 eq0,r11,@L12963
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L12963
	st	 r10,r12[r25]
	align	 4
@L12964:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L12965
@L12963:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L12969
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18209
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L12970
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L12970:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18209:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L12969
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L12969:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L12983
	bb0.n	 gt,r13,@L12974
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L12992
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L13001
	or.u	 r2,r0,hi16(@LC509)
	br	 @L13014
	align	 4
@L12974:
	bcnd	 eq0,r10,@L12976
	ld.bu	 r13,r10,8
@L17678:
	br.n	 @L12955
	st.b	 r13,r31,113
	align	 4
@L12976:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12978
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12981
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12981:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L12955
	st.b	 r13,r31,113
	align	 4
@L12978:
	br.n	 @L17678
	or	 r13,r0,0
	align	 4
@L12983:
	bcnd	 eq0,r10,@L12985
	ld.hu	 r13,r10,8
@L17679:
	br.n	 @L12955
	st.h	 r13,r31,113
	align	 4
@L12985:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12987
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12990
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12990:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L12955
	st.h	 r13,r31,113
	align	 4
@L12987:
	br.n	 @L17679
	or	 r13,r0,0
	align	 4
@L12992:
	bcnd	 eq0,r10,@L12994
	ld	 r13,r10,8
@L17680:
	br.n	 @L12955
	st	 r13,r31,113
	align	 4
@L12994:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L12996
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L12999
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L12999:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L12955
	st	 r13,r31,113
	align	 4
@L12996:
	br.n	 @L17680
	or	 r13,r0,0
	align	 4
@L13001:
	bcnd	 eq0,r10,@L13003
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,113
	br.n	 @L12955
	st	 r12,r31,117
	align	 4
@L13003:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L13005
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L13008
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L13008:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18210
	addu	 r23,r24,4
	align	 4
@L13005:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18210:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L13009
	st	 r12,r31,113
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L13012
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L13012:
	ld	 r12,r14[r25]
	mask	 r13,r23,65535
	ld	 r13,r12,r13
	br.n	 @L12955
	st	 r13,r31,117
	align	 4
@L13009:
	or	 r13,r0,0
	br.n	 @L12955
	st	 r13,r31,117
	align	 4
@L13014:
	or	 r4,r0,2969
	or.u	 r3,r0,hi16(@LC510)
	or.u	 r5,r0,hi16(@LC511)
	or	 r2,r2,lo16(@LC509)
	or	 r3,r3,lo16(@LC510)
	bsr.n	 __panic
	or	 r5,r5,lo16(@LC511)
	align	 4
@L12954:
	or	 r2,r0,0
	or	 r3,r0,r24
	addu	 r4,r31,113
	bsr.n	 _mem_access
	or	 r5,r0,1
@L12955:
	ld.bu	 r13,r31,113
	br.n	 @L12949
	st	 r13,r16[r21]
	align	 4
@L12948:
	ld	 r11,r31,108
	or	 r13,r0,1
	or.u	 r18,r0,hi16(_use_spec_R)
	extu	 r9,r11,0<24>
	or	 r18,r18,lo16(_use_spec_R)
	extu	 r12,r11,0<29>
	mask	 r10,r9,31
	ld	 r12,r18[r12]
	mak	 r13,r13,r10
	and	 r12,r12,r13
	bcnd.n	 eq0,r12,@L13017
	extu	 r21,r11,8<16>
	ld	 r3,r16[r9]
	br.n	 @L18211
	or	 r13,r0,1
	align	 4
@L13017:
	ld	 r3,r15[r9]
	ld	 r11,r31,108
	or	 r13,r0,1
@L18211:
	extu	 r11,r11,0<8>
	or.u	 r19,r0,hi16(_use_spec_R)
	mask	 r9,r11,255
	or	 r19,r19,lo16(_use_spec_R)
	extu	 r12,r9,0<5>
	mask	 r11,r11,31
	ld	 r12,r19[r12]
	mak	 r13,r13,r11
	and	 r12,r12,r13
	bcnd	 eq0,r12,@L13019
	ld	 r13,r16[r9]
	br.n	 @L18212
	addu	 r24,r3,r13
	align	 4
@L13019:
	ld	 r13,r15[r9]
	addu	 r24,r3,r13
@L18212:
	or.u	 r18,r0,hi16(_spec_mode)
	ld	 r13,r18,lo16(_spec_mode)
	bcnd.n	 eq0,r13,@L13021
	or.u	 r13,r0,hi16(_ld_text_base)
	ld	 r12,r13,lo16(_ld_text_base)
	or	 r9,r0,0
	cmp	 r13,r24,r12
	bb1.n	 lo,r13,@L13027
	or	 r22,r0,1
	or.u	 r13,r0,hi16(_ld_text_size)
	ld	 r13,r13,lo16(_ld_text_size)
	addu	 r13,r12,r13
	cmp	 r13,r24,r13
	extu	 r13,r13,1<hs>
	bcnd.n	 eq0,r13,@L18213
	extu	 r13,r24,0<24>
@L13027:
	or.u	 r13,r0,hi16(_ld_data_base)
	ld	 r13,r13,lo16(_ld_data_base)
	cmp	 r13,r24,r13
	bb1.n	 lo,r13,@L13022
	or.u	 r13,r0,hi16(_ld_stack_base)
	ld	 r13,r13,lo16(_ld_stack_base)
	cmp	 r13,r24,r13
	bb0.n	 lo,r13,@L13022
	extu	 r13,r24,0<24>
@L18213:
	extu	 r12,r24,0<16>
	extu	 r11,r24,0<8>
	xor	 r13,r13,r12
	xor	 r13,r13,r11
	or.u	 r12,r0,hi16(_store_htable)
	xor	 r13,r13,r24
	or	 r12,r12,lo16(_store_htable)
	mask	 r25,r13,31
	ld	 r10,r12[r25]
	bcnd.n	 eq0,r10,@L13030
	or	 r11,r0,0
@L13032:
	ld	 r13,r10,4
	cmp	 r13,r13,r24
	bb1	 ne,r13,@L13031
	bcnd	 eq0,r11,@L13030
	ld	 r13,r0,r10
	st	 r13,r0,r11
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	br.n	 @L13030
	st	 r10,r12[r25]
	align	 4
@L13031:
	or	 r11,r0,r10
	ld	 r10,r0,r10
	bcnd	 ne0,r10,@L13032
@L13030:
	cmp	 r13,r10,0
	cmp	 r12,r9,1
	and	 r13,r13,r12
	extu	 r13,r13,1<eq>
	bcnd.n	 eq0,r13,@L13036
	or.u	 r23,r0,hi16(_bucket_free_list)
	ld	 r13,r23,lo16(_bucket_free_list)
	bcnd.n	 ne0,r13,@L18214
	or.u	 r11,r0,hi16(_bucket_free_list)
	or	 r2,r0,1
	bsr.n	 _calloc
	or	 r3,r0,16
	bcnd.n	 ne0,r2,@L13037
	st	 r2,r23,lo16(_bucket_free_list)
	or.u	 r2,r0,hi16(@LC506)
	or	 r4,r0,2860
	or.u	 r3,r0,hi16(@LC507)
	or.u	 r5,r0,hi16(@LC508)
	or	 r2,r2,lo16(@LC506)
	or	 r3,r3,lo16(@LC507)
	bsr.n	 __fatal
	or	 r5,r5,lo16(@LC508)
	align	 4
@L13037:
	or.u	 r11,r0,hi16(_bucket_free_list)
@L18214:
	or.u	 r13,r0,hi16(_bugcompat_mode)
	ld	 r10,r11,lo16(_bucket_free_list)
	ld	 r13,r13,lo16(_bugcompat_mode)
	ld	 r12,r0,r10
	bcnd.n	 ne0,r13,@L13036
	st	 r12,r11,lo16(_bucket_free_list)
	or.u	 r12,r0,hi16(_store_htable)
	or	 r12,r12,lo16(_store_htable)
	ld	 r13,r12[r25]
	st	 r13,r0,r10
	st	 r10,r12[r25]
	st	 r24,r10,4
	st	 r0,r10,8
	st	 r0,r10,12
@L13036:
	cmp	 r13,r22,2
	bb0	 ne,r13,@L13050
	bb0.n	 gt,r13,@L13041
	cmp	 r13,r22,4
	bb0.n	 ne,r13,@L13059
	cmp	 r13,r22,8
	bb0.n	 ne,r13,@L13068
	or.u	 r2,r0,hi16(@LC509)
	br	 @L13081
	align	 4
@L13041:
	bcnd	 eq0,r10,@L13043
	ld.bu	 r13,r10,8
@L17682:
	br.n	 @L13022
	st.b	 r13,r31,113
	align	 4
@L13043:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L13045
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,1
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L13048
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L13048:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.bu	 r13,r12,r13
	br.n	 @L13022
	st.b	 r13,r31,113
	align	 4
@L13045:
	br.n	 @L17682
	or	 r13,r0,0
	align	 4
@L13050:
	bcnd	 eq0,r10,@L13052
	ld.hu	 r13,r10,8
@L17683:
	br.n	 @L13022
	st.h	 r13,r31,113
	align	 4
@L13052:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L13054
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,2
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L13057
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L13057:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld.hu	 r13,r12,r13
	br.n	 @L13022
	st.h	 r13,r31,113
	align	 4
@L13054:
	br.n	 @L17683
	or	 r13,r0,0
	align	 4
@L13059:
	bcnd	 eq0,r10,@L13061
	ld	 r13,r10,8
@L17684:
	br.n	 @L13022
	st	 r13,r31,113
	align	 4
@L13061:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L13063
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L13066
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L13066:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r13,r12,r13
	br.n	 @L13022
	st	 r13,r31,113
	align	 4
@L13063:
	br.n	 @L17684
	or	 r13,r0,0
	align	 4
@L13068:
	bcnd	 eq0,r10,@L13070
	ld	 r13,r10,8
	ld	 r12,r10,12
	st	 r13,r31,113
	br.n	 @L13022
	st	 r12,r31,117
	align	 4
@L13070:
	extu	 r25,r24,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L13072
	or	 r2,r0,0
	or	 r3,r0,r24
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L13075
	bsr	 _mem_newblock
	st	 r2,r14[r25]
@L13075:
	ld	 r12,r14[r25]
	mask	 r13,r24,65535
	ld	 r12,r12,r13
	br.n	 @L18215
	addu	 r23,r24,4
	align	 4
@L13072:
	or	 r12,r0,0
	addu	 r23,r24,4
@L18215:
	extu	 r25,r23,15<16>
	ld	 r13,r14[r25]
	bcnd.n	 eq0,r13,@L13076
	st	 r12,r31,113
	or	 r2,r0,0
	or	 r3,r0,r23
	or	 r4,r0,4
	bsr.n	 _mem_valid
	or	 r5,r0,1
	ld	 r13,r14[r25]
	bcnd	 ne0,r13,@L13079
	bsr	 _mem_newblock
	st	 r2,r14[r25]
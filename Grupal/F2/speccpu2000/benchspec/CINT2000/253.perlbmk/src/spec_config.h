#ifndef _config_h_
#define _config_h_
/*
****************************************************************************
* spec_config.h - Attempt to produce a common perl config.h that will 
* work on most Unixes and NT, albeit with limited features.  SPEC is 
* more interested in having a perl that is in a known, mostly consistent 
* state than in having a perl that takes maximum advantage of the 
* differences between every platform.
* 
* These modifications have been started by J. Henning (Digital), based
* in part on suggestions from C. Chan-nui (IBM), A. Carlton (HP), 
* P. Smith (DEC) et. al.
* 
* The original config files have been retained as 
*          sample_configs/perl-<version>/<arch>_config.h_from_Configure 
* which can be used for comparisons.  Most of the output is from 5.004_04,
* but at least one is from 5.005_03.  If you are adding support for
* a platform that is not already included here, please start from
* original_src/perl5.005_03.tar.gz, do a Configure -d (for default), 
* and post the resulting config.h to sample_configs/perl-5.005_03 using the 
* naming convention above.
*
* The porting approach has been to initially try turning off most 
* definitions -- unless all the above config files had them on -- 
* and then selectively turn back on the ones that are needed 
* during testing.
*
* Definitions intended to be set in the SPEC config file are of the 
* form "SPEC_CPU2000_mumble", so as to avoid any conflicts with existing
* perl definitions.  Examples include: 
*     SPEC_CPU2000_AIX
*     SPEC_CPU2000_HP
*     SPEC_CPU2000_NTOS
*     SPEC_CPU2000_DUNIX
*     SPEC_CPU2000_LINUX_SPARC
*     SPEC_CPU2000_LINUX_ALPHA
*     SPEC_CPU2000_LINUX_I386
*     SPEC_CPU2000_LINUX_IA64
*     SPEC_CPU2000_LINUX_PPC32
*     SPEC_CPU2000_LINUX_S390
*     SPEC_CPU2000_LINUX_S390X
*     SPEC_CPU2000_SGI
*     SPEC_CPU2000_SOLARIS
*     SPEC_CPU2000_SOLARIS_X86
*     SPEC_CPU2000_RELIANT_UNIX  (from Siemens)
*     SPEC_CPU2000_MACOSX_PPC
*     SPEC_CPU2000_MACOSX_IA32
*
* Although my hope is that it will not be necessary to override 
* very many of the detailed definitions in this file, there may be 
* some exceptions.  Porters are advised to particularly check:
*
*    LONGSIZE
*    PTRSIZE
*    BYTEORDER
*    SPEC_INT32_T
*
* For most machines, it will be sufficient to define SPEC_CPU2000_LP64, assuming
* that the LP64 convention is followed.  For ILP64 machines (like Cray, I've
* heard), define SPEC_CPU2000_ILP64.  Those folks will also need to define
* SPEC_INT32_T to a 32-bit *only* type to allow things to work.
*
* Revision history:
* 28-Apr-98: jh Initial version
* 6-May-98: jh Put into SPEC harness after initial qa on Digital Unix, 
*           SGI, and NT (Visual C++), and turning on the defines that appear 
*           needed for these 3 platforms.  At this point all 3 pass the 
*           CPU95 ref inputs and they pass at least 66.67% of the supplied 
*           perl tests from perl5.004_04/t.   Those 2/3 have been placed
*           in data/test/input.
*           NT/Intel qa used: cl -DSPEC_CPU2000_NTOS -O1
*           SGI qa used: cc -n32 -DSPEC_CPU2000_SGI -DBSD_TYPES -DBSD_TIME 
*                           -O2 -Olimit=15000
* 8-Jun-98: jh Add HAS_FORK, HAS_MEMSET, and I_FCNTL for AIX, and it validates.
*           IBM qa used: cc -DSPEC_CPU2000_AIX (and all the rest defaults). 
* 9-Jun-98: jh For SCO Unix, need HAS_FCNTL.  
*           Reviewed various warnings:
*              calls to socket-related routines warn about buflen, but 
*                       Jeff Reilly believes that is due to an old version 
*                       of SCO not defining XOPEN_VERSION high enough; will
*                       probably be better with a more recent version of SCO.
*              mg.c, pp_hot.c -- integer conversion resulted in change of sign
*                    -- this ALSO happens if you do a Configure -d with the 
*                    original distribution, and perl seems not to mind.
*                    Ignore it.
*              nit warnings about "=" vs. "=="; values set but never
*                   used; missing return values: we will assume perl author 
*                   knew what he was doing!
*           SCO qa used: icc -lnsl -lsocket, NOT -dn.  Test and ref both work.
*
*           SNI qa is still in progress.  Useful switches so far appear to be
*              -lnsl -lnls -lucb -lsocket with dynamic linking
* 
* 11-Jun-98 jh For HP, add netinet.h, and take away sys/select.h
* Sep-98 Cloyce Spradling added new defines, see osgcpu-1139, -1156.  Note 
*              in particular Cloyce's new references to SPEC_CPU2000_LP64 in
*              both this header and in the code below.
* Sep-98 Phil Ezolt suggested removing HAS_SOCKET, which should make life
*              easier on Linux and on platforms that lack non-shared
*              socket libraries.  Also suggested avoiding USE_STDIO_PTR.
* Nov-98 Changes for benchathon: "CPU2000" not "CPU2000", do not define
*              HAS_HTONL, HAS_HTONS, HAS_NTOHL, HAS_NTOHS because if
*              we don't ask for these routines we can avoid having to
*              find socket.so.  Try to avoid SIGSETJMP
*
* Feb-99 Paul Caprioli added defines for LINUX_SPARC, LINUX_ALPHA, and
*             LINUX_I386
*
* May-01 Eberhard Pasch added defines for LINUX_S390 and LINUX_S390X.
* 
* Jul-05 Cloyce added defines for MacOS X
*****************************************************************************
*/

/* Cloyce's list of things to watch:
 Pid_t
*/
#define SPEC_CPU2000
#define PROTOTYPES 1      /* Everyone should be using them */
#undef PERL_OBJECT

#ifndef SPEC_INT32_T
#define SPEC_INT32_T int
#endif

#if defined(SPEC_CPU2000_LINUX_ALPHA)  \
   || defined(SPEC_CPU2000_LINUX_SPARC) \
   || defined(SPEC_CPU2000_LINUX_PPC32) \
   || defined(SPEC_CPU2000_LINUX_I386)  \
   || defined(SPEC_CPU2000_LINUX_S390)  \
   || defined(SPEC_CPU2000_LINUX_S390X)  \
   || defined(SPEC_CPU2000_LINUX_IA64) 
#define SPEC_CPU2000_LINUX
#endif

#if defined(SPEC_CPU2000_LINUX_ALPHA) \
   || defined(SPEC_CPU2000_LINUX_S390X)
#ifndef SPEC_CPU2000_LP64
#define SPEC_CPU2000_LP64
#endif
#endif

#if defined(SPEC_CPU2000_MACOSX_PPC)  \
   || defined(SPEC_CPU2000_MACOSX_IA32)
#define SPEC_CPU2000_MACOSX
#endif

/***************************************************************************
 * Here are defines relating to types, type sizes, etc.
 ***************************************************************************/
/* INTSIZE: This symbol contains the value of sizeof(int) */
/* LONGSIZE: This symbol contains the value of sizeof(long) */
/* SHORTSIZE: This symbol contains the value of sizeof(short) */
/* PTRSIZE:
 *	This symbol contains the size of a pointer, so that the C preprocessor
 *	can make decisions based on it.
 */
#if defined(SPEC_CPU2000_NEED_BOOL)
#define bool int
#endif
#if defined(SPEC_CPU2000_ILP64)
#define INTSIZE 8
#define PTRSIZE 8
#if SPEC_INT32_T = int
#error You must set SPEC_INT32_T to be some integral type with a width
#error of exactly 32 bits!
#endif
#else
#define INTSIZE 4
#endif
#if defined(SPEC_CPU2000_DUNIX) \
  || defined(SPEC_CPU2000_LP64) \
  || defined(SPEC_CPU2000_ILP64)
#define LONGSIZE 8
#define PTRSIZE 8
#elif defined(SPEC_CPU2000_P64)
#define LONGSIZE 4
#define PTRSIZE 8
#else
#define LONGSIZE 4
#define PTRSIZE 4
#endif
#define SHORTSIZE 2
/* HAS_LONG_DOUBLE:
 *	This symbol will be defined if the C compiler supports long
 *	doubles.
 */
/* LONG_DOUBLESIZE:
 *	This symbol contains the size of a long double, so that the 
 *	C preprocessor can make decisions based on it.  It is only
 *	defined if the system supports long doubles.
 */
/*#undef HAS_LONG_DOUBLE*/
/*#undef LONG_DOUBLESIZE*/

/* HAS_LONG_LONG:
 *	This symbol will be defined if the C compiler supports
 *	long long.
 */
/* LONGLONGSIZE:
 *	This symbol contains the size of a long long, so that the 
 *	C preprocessor can make decisions based on it.  It is only
 *	defined if the system supports long long.
 */
/*#undef HAS_LONG_LONG*/
#if defined(SPEC_CPU2000_NTOS) && defined(SPEC_CPU2000_P64)
# define HAS_LONG_LONG
#endif /* SPEC_CPU2000_NTOS && SPEC_CPU2000_P64 */

#ifdef HAS_LONG_LONG
#define LONGLONGSIZE 8
#endif

/* DOUBLESIZE:
 *	This symbol contains the size of a double, so that the C preprocessor
 *	can make decisions based on it.
 */
/* We shouldn't be using double too much, so hopefully this will turn out
   okay. */
#define DOUBLESIZE 8

/* BYTEORDER:
 *	This symbol holds the hexadecimal constant defined in byteorder,
 *	i.e. 0x1234 or 0x4321, etc...
 */
#if   defined(SPEC_CPU2000_AIX)  \
 ||   defined(SPEC_CPU2000_SGI)  \
 ||   defined(SPEC_CPU2000_HP)  \
 ||   defined(SPEC_CPU2000_RELIANT_UNIX)  \
 ||   defined(SPEC_CPU2000_LINUX_SPARC) \
 ||   defined(SPEC_CPU2000_LINUX_PPC32) \
 ||   defined(SPEC_CPU2000_LINUX_S390)  \
 ||   defined(SPEC_CPU2000_LINUX_S390X)  \
 ||   defined(SPEC_CPU2000_MACOSX_PPC)  \
 ||   defined(SPEC_CPU2000_SOLARIS)
#  if defined(SPEC_CPU2000_LP64)
#    ifndef BYTEORDER
#      define BYTEORDER 0x87654321
#    endif /* BYTEORDER */
#  else
#    ifndef BYTEORDER
#      define BYTEORDER 0x4321
#    endif /* BYTEORDER */
#  endif /* SPEC_CPU2000_LP64 */

#elif defined(SPEC_CPU2000_DUNIX) || \
      defined(SPEC_CPU2000_LINUX_ALPHA)
#  ifndef BYTEORDER
#    define BYTEORDER 0x12345678	
#  endif /* BYTEORDER */

#elif defined(SPEC_CPU2000_LINUX_I386) \
   || defined(SPEC_CPU2000_LINUX_IA64) \
   || defined(SPEC_CPU2000_MACOSX_IA32) \
   || defined(SPEC_CPU2000_SOLARIS_X86) \
   || defined(SPEC_CPU2000_SCO_UW2) \
   || defined(SPEC_CPU2000_NTOS)
#  if defined(SPEC_CPU2000_LP64)
#    ifndef BYTEORDER
#      define BYTEORDER 0x12345678
#    endif /* BYTEORDER */
#  else
#    ifndef BYTEORDER
#      define BYTEORDER 0x1234
#    endif /* BYTEORDER */
#  endif /* SPEC_CPU2000_LP64 */
#endif

#ifdef SPEC_CPU2000_NTOS
/* DOS backwards compatible dain bramage */
#  ifndef SPEC_CPU2000_NUMFILE_FLAGS
#    define SPEC_CPU2000_NUMFILE_FLAGS	O_RDONLY|O_BINARY
#  endif /* SPEC_CPU2000_NUMFILE_FLAGS */
#else
#  ifndef SPEC_CPU2000_NUMFILE_FLAGS
#    define SPEC_CPU2000_NUMFILE_FLAGS	O_RDONLY
#  endif /* SPEC_CPU2000_NUMFILE_FLAGS */
#endif

#ifdef NeXT
#  ifdef __LITTLE_ENDIAN__
#    ifndef BYTEORDER
#      define BYTEORDER 0x1234
#    endif /* BYTEORDER */
#  else /* __BIG_ENDIAN__ */
#    ifndef BYTEORDER
#      define BYTEORDER 0x4321
#    endif /* BYTEORDER */
#  endif /* ENDIAN CHECK */
#endif /* NeXT */

#if (BYTEORDER == 0x1234) || (BYTEORDER == 0x12345678) || \
    defined(__LITTLE_ENDIAN__)
#  ifndef SPEC_CPU2000_NUMFILE
#    define SPEC_CPU2000_NUMFILE "lenums"
#  endif /* SPEC_CPU2000_NUMFILE */
#else
#  ifndef SPEC_CPU2000_NUMFILE
#    define SPEC_CPU2000_NUMFILE "benums"
#  endif /* SPEC_CPU2000_NUMFILE */
#endif

/*
 * This file was produced by running the config_h.SH script, which
 * gets its values from config.sh, which is generally produced by
 * running Configure.
 *
 * Feel free to modify any of this as the need arises.  Note, however,
 * that running config_h.SH again will wipe out any changes you've made.
 * For a more permanent change edit config.sh and rerun config_h.SH.
 *
 * $Id: Config_h.U,v 3.0.1.5 1997/02/28 14:57:43 ram Exp $
 */

/*
 * Package name      : perl5
 * Source directory  : .
 * Configuration time: Fri Jun  4 11:01:00 CDT 1999
 * Configured by     : cloyce
 * Target system     : linux brak.headgear.org 2.2.9-ac1 #3 fri jun 4 01:55:24 cdt 1999 i686 unknown 
 */

/* MEM_ALIGNBYTES:
 *	This symbol contains the number of bytes required to align a
 *	double. Usual values are 2, 4 and 8.
 *	On NeXT starting with 3.2, you can build "Fat" Multiple Architecture
 *	Binaries (MAB) for targets with varying alignment.  This only matters
 *	for perl, where the config.h can be generated and installed on one
 *	system, and used by a different architecture to build an extension.
 *	The default is eight, for safety.
 */
#define MEM_ALIGNBYTES 8

/* ARCHNAME:
 *	This symbol holds a string representing the architecture name.
 *	It may be used to construct an architecture-dependant pathname
 *	where library files may be held under a private library, for
 *	instance.
 */
#define ARCHNAME "SPEC CPU2000 BENCHMARKING ONLY - Warning, some functions may not work as expected or may be skipped!!"

/* I_MACH_CTHREADS:
 *    This symbol, if defined, indicates to the C program that it should
 *    include <mach/cthreads.h>.
 */
/*#undef	I_MACH_CTHREADS*/

/* I_PTHREAD:
 *    This symbol, if defined, indicates to the C program that it should
 *    include <pthread.h>.
 */
/*#undef   I_PTHREAD*/

/* HAS_PTHREAD_YIELD:
 *	This symbol, if defined, indicates that the pthread_yield 
 *	routine is available to yield the execution of the current
 *	thread.
 */
/* HAS_SCHED_YIELD:
 *	This symbol, if defined, indicates that the sched_yield
 *	routine is available to yield the execution of the current
 *	thread.
 */
/*#undef HAS_PTHREAD_YIELD*/
/*#undef HAS_SCHED_YIELD*/

/* PTHREADS_CREATED_JOINABLE:
 *	This symbol, if defined, indicates that pthreads are created
 *	in the joinable (aka undetached) state.
 */
/*#undef PTHREADS_CREATED_JOINABLE*/

/* USE_THREADS:
 *	This symbol, if defined, indicates that Perl should
 *	be built to use threads.
 */
/* OLD_PTHREADS_API:
 *	This symbol, if defined, indicates that Perl should
 *	be built to use the old draft POSIX threads API.
 */
/*#undef	USE_THREADS*/
/*#undef	OLD_PTHREADS_API*/

/* BIN:
 *	This symbol holds the path of the bin directory where the package will
 *	be installed. Program must be prepared to deal with ~name substitution.
 */
/* BIN_EXP:
 *	This symbol is the filename expanded version of the BIN symbol, for
 *	programs that do not want to deal with that at run-time.
 */
#define BIN "."
#define BIN_EXP "."

/* CAT2:
 *	This macro catenates 2 tokens together.
 */
/* STRINGIFY:
 *	This macro surrounds its token with double quotes.
 */
#if 42 == 1
#define CAT2(a,b)a/**/b
#define STRINGIFY(a)"a"
		/* If you can get stringification with catify, tell me how! */
#endif
#if 42 == 42
#define CAT2(a,b)a ## b
#define StGiFy(a)# a
#define STRINGIFY(a)StGiFy(a)
#endif
#if 42 != 1 && 42 != 42
#include "Bletch: How does this C preprocessor catenate tokens?"
#endif

/* CPPSTDIN:
 *	This symbol contains the first part of the string which will invoke
 *	the C preprocessor on the standard input and produce to standard
 *	output.	 Typical value of "cc -E" or "/lib/cpp", but it can also
 *	call a wrapper. See CPPRUN.
 */
/* CPPMINUS:
 *	This symbol contains the second part of the string which will invoke
 *	the C preprocessor on the standard input and produce to standard
 *	output.  This symbol will have the value "-" if CPPSTDIN needs a minus
 *	to specify standard input, otherwise the value is "".
 */
#if   defined(SPEC_CPU2000_AIX)    \
   || defined(SPEC_CPU2000_DUNIX)  \
   || defined(SPEC_CPU2000_SGI)    
#define CPPSTDIN "cppstdin"
#define CPPMINUS ""

#elif defined(SPEC_CPU2000_LINUX)  \
   || defined(SPEC_CPU2000_MACOSX) \
   || defined(SPEC_CPU2000_SOLARIS) \
   || defined(SPEC_CPU2000_SOLARIS_X86)
#define CPPSTDIN "cc -E"
#define CPPMINUS "-"

#elif defined(SPEC_CPU2000_NTOS)
#define CPPSTDIN "cl -E"              /* this is probably not so good */
#define CPPMINUS ""

#else
#define CPPSTDIN "cppstdin"           /* a reasonable default?? */
#define CPPMINUS ""
#endif


/* HAS_ALARM:
 *	This symbol, if defined, indicates that the alarm routine is
 *	available.
 */
/*#undef HAS_ALARM*/

/* HASATTRIBUTE:
 *	This symbol indicates the C compiler can check for function attributes,
 *	such as printf formats. This is normally only supported by GNU cc.
 */
#ifdef SPEC_CPU2000_LINUX_IA64
#define HASATTRIBUTE
#else
/*#undef HASATTRIBUTE*/
#endif
#ifndef HASATTRIBUTE
#define __attribute__(_arg_)
#endif

/* HAS_BCMP:
 *	This symbol is defined if the bcmp() routine is available to
 *	compare blocks of memory.
 */
/*#undef HAS_BCMP*/

/* HAS_BCOPY:
 *	This symbol is defined if the bcopy() routine is available to
 *	copy blocks of memory.
 */
/*#undef HAS_BCOPY*/

/* HAS_BZERO:
 *	This symbol is defined if the bzero() routine is available to
 *	set a memory block to 0.
 */
/*#undef HAS_BZERO*/

/* CASTI32:
 *	This symbol is defined if the C compiler can cast negative
 *	or large floating point numbers to 32-bit ints.
 */
/*#undef	CASTI32*/

/* CASTNEGFLOAT:
 *	This symbol is defined if the C compiler can cast negative
 *	numbers to unsigned longs, ints and shorts.
 */
/* CASTFLAGS:
 *	This symbol contains flags that say what difficulties the compiler
 *	has casting odd floating values to unsigned long:
 *		0 = ok
 *		1 = couldn't cast < 0
 *		2 = couldn't cast >= 0x80000000
 *		4 = couldn't cast in argument expression list
 */
/*#undef	CASTNEGFLOAT*/
#define CASTFLAGS 1

/* HAS_CHOWN:
 *	This symbol, if defined, indicates that the chown routine is
 *	available.
 */
/*#undef HAS_CHOWN*/

/* HAS_CHROOT:
 *	This symbol, if defined, indicates that the chroot routine is
 *	available.
 */
/*#undef HAS_CHROOT*/

/* HAS_CHSIZE:
 *	This symbol, if defined, indicates that the chsize routine is available
 *	to truncate files.  You might need a -lx to get this routine.
 */
/*#undef	HAS_CHSIZE*/

/* VOID_CLOSEDIR:
 *	This symbol, if defined, indicates that the closedir() routine
 *	does not return a value.
 */
/*#undef VOID_CLOSEDIR*/

/* HASCONST:
 *	This symbol, if defined, indicates that this C compiler knows about
 *	the const type. There is no need to actually test for that symbol
 *	within your programs. The mere use of the "const" keyword will
 *	trigger the necessary tests.
 */
#define HASCONST
#ifndef HASCONST
#define const
#endif

/* HAS_CRYPT:
 *	This symbol, if defined, indicates that the crypt routine is available
 *	to encrypt passwords and the like.
 */
/*#undef HAS_CRYPT*/

/* HAS_CUSERID:
 *	This symbol, if defined, indicates that the cuserid routine is
 *	available to get character login names.
 */
/*#undef HAS_CUSERID*/

/* HAS_DBL_DIG:
 *	This symbol, if defined, indicates that this system's <float.h>
 *	or <limits.h> defines the symbol DBL_DIG, which is the number
 *	of significant digits in a double precision number.  If this
 *	symbol is not defined, a guess of 15 is usually pretty good.
 */
/*#undef HAS_DBL_DIG*/

/* HAS_DIFFTIME:
 *	This symbol, if defined, indicates that the difftime routine is
 *	available.
 */
/*#undef HAS_DIFFTIME*/

/* HAS_DLERROR:
 *	This symbol, if defined, indicates that the dlerror routine is
 *	available to return a string describing the last error that
 *	occurred from a call to dlopen(), dlclose() or dlsym().
 */
#ifndef SPEC_CPU2000_AIX 
#define HAS_DLERROR
#else
/*#undef HAS_DLERROR*/
#endif

/* HAS_DUP2:
 *	This symbol, if defined, indicates that the dup2 routine is
 *	available to duplicate file descriptors.
 */
#ifndef SPEC_CPU2000_AIX
#define HAS_DUP2
#else
/*#undef HAS_DUP2*/
#endif

/* HAS_FCHMOD:
 *	This symbol, if defined, indicates that the fchmod routine is available
 *	to change mode of opened files.  If unavailable, use chmod().
 */
/*#undef HAS_FCHMOD*/

/* HAS_FCHOWN:
 *	This symbol, if defined, indicates that the fchown routine is available
 *	to change ownership of opened files.  If unavailable, use chown().
 */
/*#undef HAS_FCHOWN*/

/* HAS_FCNTL:
 *	This symbol, if defined, indicates to the C program that
 *	the fcntl() function exists.
 */
#if defined(SPEC_CPU2000_DUNIX)  \
 || defined(SPEC_CPU2000_LINUX)  \
 || defined(SPEC_CPU2000_MACOSX)  \
 || defined(SPEC_CPU2000_HP)     \
 || defined(SPEC_CPU2000_RELIANT_UNIX) \
 || defined(SPEC_CPU2000_SGI)    \
 || defined(SPEC_CPU2000_SCO_UW2)    \
 || defined(SPEC_CPU2000_SOLARIS) \
 || defined(SPEC_CPU2000_SOLARIS_X86)
#define HAS_FCNTL
#endif

/* HAS_FGETPOS:
 *	This symbol, if defined, indicates that the fgetpos routine is
 *	available to get the file position indicator, similar to ftell().
 */
/*#undef HAS_FGETPOS*/
#if (defined(SPEC_CPU2000_LINUX) || defined(SPEC_CPU2000_GLIBC22)) && \
    !defined(SPEC_CPU2000_NO_HAS_FGETPOS)
# define HAS_FGETPOS
#endif

/* FLEXFILENAMES:
 *	This symbol, if defined, indicates that the system supports filenames
 *	longer than 14 characters.
 */
#define	FLEXFILENAMES

/* HAS_FLOCK:
 *	This symbol, if defined, indicates that the flock routine is
 *	available to do file locking.
 */
/*#undef HAS_FLOCK*/

/* HAS_FORK:
 *	This symbol, if defined, indicates that the fork routine is
 *	available.
 */
#if !defined(SPEC_CPU2000_NTOS)
#define HAS_FORK
#endif

/* HAS_FSETPOS:
 *	This symbol, if defined, indicates that the fsetpos routine is
 *	available to set the file position indicator, similar to fseek().
 */
/*#undef HAS_FSETPOS*/
#if (defined(SPEC_CPU2000_LINUX) || defined(SPEC_CPU2000_GLIBC22)) && \
    !defined(SPEC_CPU2000_NO_HAS_FSETPOS)
# define HAS_FSETPOS
#endif

/* I_SYS_MOUNT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/mount.h>.
 */
/*#undef I_SYS_MOUNT*/

/* HAS_FSTATFS:
 *	This symbol, if defined, indicates that the fstatfs routine is
 *	available to stat the filesystem of a file descriptor.
 */
/*#undef HAS_FSTATFS*/

/* HAS_STRUCT_STATFS_FLAGS:
 *	This symbol, if defined, indicates that the struct statfs has
 *	the f_flags member for mount flags.
 */
/*#undef HAS_STRUCT_STATFS_FLAGS*/

/* I_SYS_STATVFS:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/statvfs.h>.
 */
/*#undef I_SYS_STATVFS*/

/* I_SYS_STATVFS:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/statvfs.h>.
 */
/*#undef I_SYS_STATVFS*/

/* HAS_FSTATVFS:
 *	This symbol, if defined, indicates that the fstatvfs routine is
 *	available to stat the filesystem of a file descriptor.
 */
/*#undef HAS_FSTATVFS*/

/* I_MNTENT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <mntent.h>.
 */
/*#undef I_MNTENT*/

/* HAS_GETMNTENT:
 *	This symbol, if defined, indicates that the getmntent routine is
 *	available to lookup mount entries in some data base or other.
 */
/*#undef HAS_GETMNTENT*/

/* HAS_HASMNTOPT:
 *	This symbol, if defined, indicates that the hasmntopt routine is
 *	available to query mount entries returned by getmntent.
 */
/*#undef HAS_HASMNTOPT*/

/* HAS_GETTIMEOFDAY:
 *	This symbol, if defined, indicates that the gettimeofday() system
 *	call is available for a sub-second accuracy clock. Usually, the file
 *	<sys/resource.h> needs to be included (see I_SYS_RESOURCE).
 *	The type "Timeval" should be used to refer to "struct timeval".
 */
/*#undef HAS_GETTIMEOFDAY*/
#ifdef HAS_GETTIMEOFDAY
#define Timeval struct timeval	/* Structure used by gettimeofday() */
#endif

/* HAS_GETGROUPS:
 *	This symbol, if defined, indicates that the getgroups() routine is
 *	available to get the list of process groups.  If unavailable, multiple
 *	groups are probably not supported.
 */
/* HAS_SETGROUPS:
 *	This symbol, if defined, indicates that the setgroups() routine is
 *	available to set the list of process groups.  If unavailable, multiple
 *	groups are probably not supported.
 */
/*#undef HAS_GETGROUPS*/
/*#undef HAS_SETGROUPS*/

/* HAS_SETHOSTENT:
 *	This symbol, if defined, indicates that the sethostent() routine is
 *	available.
 */
/*#undef HAS_SETHOSTENT*/

/* HAS_SETNETENT:
 *	This symbol, if defined, indicates that the setnetent() routine is
 *	available.
 */
/*#undef HAS_SETNETENT*/

/* HAS_SETPROTOENT:
 *	This symbol, if defined, indicates that the setprotoent() routine is
 *	available.
 */
/*#undef HAS_SETPROTOENT*/

/* HAS_SETSERVENT:
 *	This symbol, if defined, indicates that the setservent() routine is
 *	available.
 */
/*#undef HAS_SETSERVENT*/

/* HAS_SETVBUF:
 *	This symbol, if defined, indicates that the setvbuf routine is
 *	available to change buffering on an open stdio stream.
 *	to a line-buffered mode.
 */
/*#undef HAS_SETVBUF*/

/* HAS_GETHOSTENT:
 *	This symbol, if defined, indicates that the gethostent routine is
 *	available to lookup host names in some data base or other.
 */
/*#undef HAS_GETHOSTENT*/

/* HAS_UNAME:
 *	This symbol, if defined, indicates that the C program may use the
 *	uname() routine to derive the host name.  See also HAS_GETHOSTNAME
 *	and PHOSTNAME.
 */
/*#undef HAS_UNAME*/

/* HAS_GETLOGIN:
 *	This symbol, if defined, indicates that the getlogin routine is
 *	available to get the login name.
 */
/*#undef HAS_GETLOGIN*/

/* HAS_GETPGRP2:
 *	This symbol, if defined, indicates that the getpgrp2() (as in DG/UX)
 *	routine is available to get the current process group.
 */
/*#undef HAS_GETPGRP2*/

/* HAS_GETPPID:
 *	This symbol, if defined, indicates that the getppid routine is
 *	available to get the parent process ID.
 */
/*#undef HAS_GETPPID*/

/* HAS_GETPRIORITY:
 *	This symbol, if defined, indicates that the getpriority routine is
 *	available to get a process's priority.
 */
/*#undef HAS_GETPRIORITY*/

/* HAS_HTONL:
 *	This symbol, if defined, indicates that the htonl() routine (and
 *	friends htons() ntohl() ntohs()) are available to do network
 *	order byte swapping.
 */
/* HAS_HTONS:
 *	This symbol, if defined, indicates that the htons() routine (and
 *	friends htonl() ntohl() ntohs()) are available to do network
 *	order byte swapping.
 */
/* HAS_NTOHL:
 *	This symbol, if defined, indicates that the ntohl() routine (and
 *	friends htonl() htons() ntohs()) are available to do network
 *	order byte swapping.
 */
/* HAS_NTOHS:
 *	This symbol, if defined, indicates that the ntohs() routine (and
 *	friends htonl() htons() ntohl()) are available to do network
 *	order byte swapping.
 */
/*#undef HAS_HTONL*/
/*#undef HAS_HTONS*/
/*#undef HAS_NTOHL	*/
/*#undef HAS_NTOHS		*/

/* HAS_ISASCII:
 *	This manifest constant lets the C program know that isascii 
 *	is available.
 */
#define HAS_ISASCII

/* HAS_KILLPG:
 *	This symbol, if defined, indicates that the killpg routine is available
 *	to kill process groups.  If unavailable, you probably should use kill
 *	with a negative process number.
 */
/*#undef HAS_KILLPG*/

/* HAS_LINK:
 *	This symbol, if defined, indicates that the link routine is
 *	available to create hard links.
 */
/*#undef HAS_LINK*/

/* HAS_LOCALECONV:
 *	This symbol, if defined, indicates that the localeconv routine is
 *	available for numeric and monetary formatting conventions.
 */
/*#undef HAS_LOCALECONV*/

/* HAS_LOCKF:
 *	This symbol, if defined, indicates that the lockf routine is
 *	available to do file locking.
 */
/*#undef HAS_LOCKF*/

/* HAS_LSTAT:
 *	This symbol, if defined, indicates that the lstat routine is
 *	available to do file stats on symbolic links.
 */
/*#undef HAS_LSTAT*/

/* HAS_MBLEN:
 *	This symbol, if defined, indicates that the mblen routine is available
 *	to find the number of bytes in a multibye character.
 */
/*#undef HAS_MBLEN*/

/* HAS_MBSTOWCS:
 *	This symbol, if defined, indicates that the mbstowcs routine is
 *	available to covert a multibyte string into a wide character string.
 */
/*#undef	HAS_MBSTOWCS*/

/* HAS_MBTOWC:
 *	This symbol, if defined, indicates that the mbtowc routine is available
 *	to covert a multibyte to a wide character.
 */
/*#undef HAS_MBTOWC*/

/* HAS_MEMCMP:
 *	This symbol, if defined, indicates that the memcmp routine is available
 *	to compare blocks of memory.
 */
/*#undef HAS_MEMCMP*/

/* HAS_MEMCPY:
 *	This symbol, if defined, indicates that the memcpy routine is available
 *	to copy blocks of memory.
 */
/*#undef HAS_MEMCPY*/

/* HAS_MEMMOVE:
 *	This symbol, if defined, indicates that the memmove routine is available
 *	to copy potentially overlapping blocks of memory. This should be used
 *	only when HAS_SAFE_BCOPY is not defined. If neither is there, roll your
 *	own version.
 */
/*#undef HAS_MEMMOVE*/

/* HAS_MEMSET:
 *	This symbol, if defined, indicates that the memset routine is available
 *	to set blocks of memory.
 */
#define HAS_MEMSET

/* HAS_MKDIR:
 *	This symbol, if defined, indicates that the mkdir routine is available
 *	to create directories.  Otherwise you should fork off a new process to
 *	exec /bin/mkdir.
 */
#define HAS_MKDIR

/* HAS_MKFIFO:
 *	This symbol, if defined, indicates that the mkfifo routine is
 *	available to create FIFOs. Otherwise, mknod should be able to
 *	do it for you. However, if mkfifo is there, mknod might require
 *	super-user privileges which mkfifo will not.
 */
/*#undef HAS_MKFIFO*/

/* HAS_MKTIME:
 *	This symbol, if defined, indicates that the mktime routine is
 *	available.
 */
/*#undef HAS_MKTIME*/

/* HAS_MSG:
 *	This symbol, if defined, indicates that the entire msg*(2) library is
 *	supported (IPC mechanism based on message queues).
 */
/*#undef HAS_MSG*/

/* HAS_NICE:
 *	This symbol, if defined, indicates that the nice routine is
 *	available.
 */
/*#undef HAS_NICE*/

/* HAS_OPEN3:
 *	This manifest constant lets the C program know that the three
 *	argument form of open(2) is available.
 */
/*#undef HAS_OPEN3*/

/* HAS_PATHCONF:
 *	This symbol, if defined, indicates that pathconf() is available
 *	to determine file-system related limits and options associated
 *	with a given filename.
 */
/* HAS_FPATHCONF:
 *	This symbol, if defined, indicates that pathconf() is available
 *	to determine file-system related limits and options associated
 *	with a given open file descriptor.
 */
/*#undef HAS_PATHCONF*/
/*#undef HAS_FPATHCONF */
/* HAS_PAUSE:
 *	This symbol, if defined, indicates that the pause routine is
 *	available to suspend a process until a signal is received.
 */
/*#undef HAS_PAUSE*/

/* HAS_PIPE:
 *	This symbol, if defined, indicates that the pipe routine is
 *	available to create an inter-process channel.
 */
#define HAS_PIPE

/* HAS_POLL:
 *	This symbol, if defined, indicates that the poll routine is
 *	available to poll active file descriptors.
 */
/*#undef HAS_POLL*/

/* HAS_READDIR:
 *	This symbol, if defined, indicates that the readdir routine is
 *	available to read directory entries. You may have to include
 *	<dirent.h>. See I_DIRENT.
 */
/*#undef HAS_READDIR*/

/* HAS_SEEKDIR:
 *	This symbol, if defined, indicates that the seekdir routine is
 *	available. You may have to include <dirent.h>. See I_DIRENT.
 */
/*#undef HAS_SEEKDIR*/

/* HAS_TELLDIR:
 *	This symbol, if defined, indicates that the telldir routine is
 *	available. You may have to include <dirent.h>. See I_DIRENT.
 */
/*#undef HAS_TELLDIR*/

/* HAS_REWINDDIR:
 *	This symbol, if defined, indicates that the rewinddir routine is
 *	available. You may have to include <dirent.h>. See I_DIRENT.
 */
/*#undef HAS_REWINDDIR*/

/* HAS_READLINK:
 *	This symbol, if defined, indicates that the readlink routine is
 *	available to read the value of a symbolic link.
 */
/*#undef HAS_READLINK*/

/* HAS_RENAME:
 *	This symbol, if defined, indicates that the rename routine is available
 *	to rename files.  Otherwise you should do the unlink(), link(), unlink()
 *	trick.
 */
#define HAS_RENAME

/* HAS_RMDIR:
 *	This symbol, if defined, indicates that the rmdir routine is
 *	available to remove directories. Otherwise you should fork off a
 *	new process to exec /bin/rmdir.
 */
#define HAS_RMDIR

/* HAS_SAFE_BCOPY:
 *	This symbol, if defined, indicates that the bcopy routine is available
 *	to copy potentially overlapping memory blocks. Otherwise you should
 *	probably use memmove() or memcpy(). If neither is defined, roll your
 *	own version.
 */
/*#undef HAS_SAFE_BCOPY*/

/* HAS_SAFE_MEMCPY:
 *	This symbol, if defined, indicates that the memcpy routine is available
 *	to copy potentially overlapping memory blocks. Otherwise you should
 *	probably use memmove() or memcpy(). If neither is defined, roll your
 *	own version.
 */
/*#undef HAS_SAFE_MEMCPY*/

/* HAS_SANE_MEMCMP:
 *	This symbol, if defined, indicates that the memcmp routine is available
 *	and can be used to compare relative magnitudes of chars with their high
 *	bits set.  If it is not defined, roll your own version.
 */
/*#undef HAS_SANE_MEMCMP*/

/* HAS_SELECT:
 *	This symbol, if defined, indicates that the select routine is
 *	available to select active file descriptors. If the timeout field
 *	is used, <sys/time.h> may need to be included.
 */
#define HAS_SELECT

/* HAS_SEM:
 *	This symbol, if defined, indicates that the entire sem*(2) library is
 *	supported.
 */
/*#undef HAS_SEM*/

/* HAS_SETEGID:
 *	This symbol, if defined, indicates that the setegid routine is available
 *	to change the effective gid of the current program.
 */
/*#undef HAS_SETEGID*/

/* HAS_SETEUID:
 *	This symbol, if defined, indicates that the seteuid routine is available
 *	to change the effective uid of the current program.
 */
/*#undef HAS_SETEUID*/

/* HAS_SETLINEBUF:
 *	This symbol, if defined, indicates that the setlinebuf routine is
 *	available to change stderr or stdout from block-buffered or unbuffered
 *	to a line-buffered mode.
 */
/*#undef HAS_SETLINEBUF*/

/* HAS_SETLOCALE:
 *	This symbol, if defined, indicates that the setlocale routine is
 *	available to handle locale-specific ctype implementations.
 */
/*#undef HAS_SETLOCALE*/

/* HAS_SETPGRP2:
 *	This symbol, if defined, indicates that the setpgrp2() (as in DG/UX)
 *	routine is available to set the current process group.
 */
/*#undef HAS_SETPGRP2*/

/* HAS_SETPRIORITY:
 *	This symbol, if defined, indicates that the setpriority routine is
 *	available to set a process's priority.
 */
/*#undef HAS_SETPRIORITY*/

/* HAS_SETREGID:
 *	This symbol, if defined, indicates that the setregid routine is
 *	available to change the real and effective gid of the current
 *	process.
 */
/* HAS_SETRESGID:
 *	This symbol, if defined, indicates that the setresgid routine is
 *	available to change the real, effective and saved gid of the current
 *	process.
 */
/*#undef HAS_SETREGID*/
/*#undef HAS_SETRESGID*/

/* HAS_SETREUID:
 *	This symbol, if defined, indicates that the setreuid routine is
 *	available to change the real and effective uid of the current
 *	process.
 */
/* HAS_SETRESUID:
 *	This symbol, if defined, indicates that the setresuid routine is
 *	available to change the real, effective and saved uid of the current
 *	process.
 */
/*#undef HAS_SETREUID*/
/*#undef HAS_SETRESUID*/

/* HAS_SETRGID:
 *	This symbol, if defined, indicates that the setrgid routine is available
 *	to change the real gid of the current program.
 */
/*#undef HAS_SETRGID*/

/* HAS_SETRUID:
 *	This symbol, if defined, indicates that the setruid routine is available
 *	to change the real uid of the current program.
 */
/*#undef HAS_SETRUID*/

/* HAS_SETSID:
 *	This symbol, if defined, indicates that the setsid routine is
 *	available to set the process group ID.
 */
/*#undef HAS_SETSID*/

/* HAS_SHM:
 *	This symbol, if defined, indicates that the entire shm*(2) library is
 *	supported.
 */
/*#undef HAS_SHM*/

/* Shmat_t:
 *	This symbol holds the return type of the shmat() system call.
 *	Usually set to 'void *' or 'char *'.
 */
/* HAS_SHMAT_PROTOTYPE:
 *	This symbol, if defined, indicates that the sys/shm.h includes
 *	a prototype for shmat().  Otherwise, it is up to the program to
 *	guess one.  Shmat_t shmat _((int, Shmat_t, int)) is a good guess,
 *	but not always right so it should be emitted by the program only
 *	when HAS_SHMAT_PROTOTYPE is not defined to avoid conflicting defs.
 */
#define Shmat_t void *
/*#undef HAS_SHMAT_PROTOTYPE*/

/* HAS_SIGACTION:
 *	This symbol, if defined, indicates that Vr4's sigaction() routine
 *	is available.
 */
/*#undef HAS_SIGACTION*/

/* HAS_SOCKET:
 *	This symbol, if defined, indicates that the BSD socket interface is
 *	supported.
 */
/* HAS_SOCKETPAIR:
 *	This symbol, if defined, indicates that the BSD socketpair() call is
 *	supported.
 */
/*#undef HAS_SOCKET*/
/*#undef HAS_SOCKETPAIR*/

/* HAS_UNION_SEMUN:
 *	This symbol, if defined, indicates that the union semun is
 *	defined by including <sys/sem.h>.  If not, the user code
 *	probably needs to define it as:
 *	union semun {
 *	    int val;
 *	    struct semid_ds *buf;
 *	    unsigned short *array;
 *	}
 */
/* USE_SEMCTL_SEMUN:
 *	This symbol, if defined, indicates that union semun is
 *	used for semctl IPC_STAT.
 */
/* USE_SEMCTL_SEMID_DS:
 *	This symbol, if defined, indicates that struct semid_ds * is
 *	used for semctl IPC_STAT.
 */
/*#undef HAS_UNION_SEMUN*/
/*#undef USE_SEMCTL_SEMUN*/
/*#undef USE_SEMCTL_SEMID_DS*/

/* USE_STAT_BLOCKS:
 *	This symbol is defined if this system has a stat structure declaring
 *	st_blksize and st_blocks.
 */
/*#undef USE_STAT_BLOCKS*/

/* USE_STDIO_PTR:
 *	This symbol is defined if the _ptr and _cnt fields (or similar)
 *	of the stdio FILE structure can be used to access the stdio buffer
 *	for a file handle.  If this is defined, then the FILE_ptr(fp)
 *	and FILE_cnt(fp) macros will also be defined and should be used
 *	to access these fields.
 */
/* FILE_ptr:
 *	This macro is used to access the _ptr field (or equivalent) of the
 *	FILE structure pointed to by its argument. This macro will always be
 *	defined if USE_STDIO_PTR is defined.
 */
/* STDIO_PTR_LVALUE:
 *	This symbol is defined if the FILE_ptr macro can be used as an
 *	lvalue.
 */
/* FILE_cnt:
 *	This macro is used to access the _cnt field (or equivalent) of the
 *	FILE structure pointed to by its argument. This macro will always be
 *	defined if USE_STDIO_PTR is defined.
 */
/* STDIO_CNT_LVALUE:
 *	This symbol is defined if the FILE_cnt macro can be used as an
 *	lvalue.
 */
/*#undef USE_STDIO_PTR*/
#ifdef USE_STDIO_PTR
#define FILE_ptr(fp)	((fp)->_ptr)
#define STDIO_PTR_LVALUE
#define FILE_cnt(fp)	((fp)->_cnt)
/*#undef STDIO_CNT_LVALUE*/
#endif

/* USE_STDIO_BASE:
 *	This symbol is defined if the _base field (or similar) of the
 *	stdio FILE structure can be used to access the stdio buffer for
 *	a file handle.  If this is defined, then the FILE_base(fp) macro
 *	will also be defined and should be used to access this field.
 *	Also, the FILE_bufsiz(fp) macro will be defined and should be used
 *	to determine the number of bytes in the buffer.  USE_STDIO_BASE
 *	will never be defined unless USE_STDIO_PTR is.
 */
/* FILE_base:
 *	This macro is used to access the _base field (or equivalent) of the
 *	FILE structure pointed to by its argument. This macro will always be
 *	defined if USE_STDIO_BASE is defined.
 */
/* FILE_bufsiz:
 *	This macro is used to determine the number of bytes in the I/O
 *	buffer pointed to by _base field (or equivalent) of the FILE
 *	structure pointed to its argument. This macro will always be defined
 *	if USE_STDIO_BASE is defined.
 */
/*#undef USE_STDIO_BASE*/
#ifdef USE_STDIO_BASE
#define FILE_base(fp)	((fp)->_base)
#define FILE_bufsiz(fp)	((fp)->_cnt + (fp)->_ptr - (fp)->_base)
#endif

/* USE_STAT_BLOCKS:
 *	This symbol is defined if this system has a stat structure declaring
 *	st_blksize and st_blocks.
 */
/*#undef USE_STAT_BLOCKS*/

/* HAS_STRCHR:
 *	This symbol is defined to indicate that the strchr()/strrchr()
 *	functions are available for string searching. If not, try the
 *	index()/rindex() pair.
 */
/* HAS_INDEX:
 *	This symbol is defined to indicate that the index()/rindex()
 *	functions are available for string searching.
 */
/*#undef HAS_STRCHR*/
/*#undef HAS_INDEX*/

/* HAS_STRCOLL:
 *	This symbol, if defined, indicates that the strcoll routine is
 *	available to compare strings using collating information.
 */
/*#undef HAS_STRCOLL*/

/* USE_STRUCT_COPY:
 *	This symbol, if defined, indicates that this C compiler knows how
 *	to copy structures.  If undefined, you'll need to use a block copy
 *	routine of some sort instead.
 */
#define	USE_STRUCT_COPY

/* HAS_STRERROR:
 *	This symbol, if defined, indicates that the strerror routine is
 *	available to translate error numbers to strings. See the writeup
 *	of Strerror() in this file before you try to define your own.
 */
/* HAS_SYS_ERRLIST:
 *	This symbol, if defined, indicates that the sys_errlist array is
 *	available to translate error numbers to strings. The extern int
 *	sys_nerr gives the size of that table.
 */
/* Strerror:
 *	This preprocessor symbol is defined as a macro if strerror() is
 *	not available to translate error numbers to strings but sys_errlist[]
 *	array is there.
 */
#ifndef SPEC_CPU2000_AIX
#define HAS_STRERROR
#define HAS_SYS_ERRLIST
#endif
#define Strerror(e) "unknown"

/* HAS_STRTOD:
 *	This symbol, if defined, indicates that the strtod routine is
 *	available to provide better numeric string conversion than atof().
 */
/*#undef HAS_STRTOD*/

/* HAS_STRTOL:
 *	This symbol, if defined, indicates that the strtol routine is available
 *	to provide better numeric string conversion than atoi() and friends.
 */
/*#undef HAS_STRTOL*/

/* HAS_STRTOUL:
 *	This symbol, if defined, indicates that the strtoul routine is
 *	available to provide conversion of strings to unsigned long.
 */
/*#undef HAS_STRTOUL*/

/* HAS_STRXFRM:
 *	This symbol, if defined, indicates that the strxfrm() routine is
 *	available to transform strings.
 */
/*#undef HAS_STRXFRM*/

/* HAS_SYMLINK:
 *	This symbol, if defined, indicates that the symlink routine is available
 *	to create symbolic links.
 */
/*#undef HAS_SYMLINK*/

/* HAS_SYSCALL:
 *	This symbol, if defined, indicates that the syscall routine is
 *	available to call arbitrary system calls. If undefined, that's tough.
 */
/*#undef HAS_SYSCALL*/

/* HAS_SYSCONF:
 *	This symbol, if defined, indicates that sysconf() is available
 *	to determine system related limits and options.
 */
/*#undef HAS_SYSCONF*/

/* HAS_SYSTEM:
 *	This symbol, if defined, indicates that the system routine is
 *	available to issue a shell command.
 */
/*#undef HAS_SYSTEM*/

/* HAS_TCGETPGRP:
 *	This symbol, if defined, indicates that the tcgetpgrp routine is
 *	available to get foreground process group ID.
 */
/*#undef HAS_TCGETPGRP*/

/* HAS_TCSETPGRP:
 *	This symbol, if defined, indicates that the tcsetpgrp routine is
 *	available to set foreground process group ID.
 */
/*#undef HAS_TCSETPGRP*/

/* Time_t:
 *	This symbol holds the type returned by time(). It can be long,
 *	or time_t on BSD sites (in which case <sys/types.h> should be
 *	included).
 */
#ifdef SPEC_CPU2000_AIX
#define Time_t int
#else
#define Time_t time_t		/* Time type */
#endif

/* HAS_TIMES:
 *	This symbol, if defined, indicates that the times() routine exists.
 *	Note that this became obsolete on some systems (SUNOS), which now
 * use getrusage(). It may be necessary to include <sys/times.h>.
 */
#define HAS_TIMES

/* HAS_TRUNCATE:
 *	This symbol, if defined, indicates that the truncate routine is
 *	available to truncate files.
 */
#ifndef SPEC_CPU2000_NTOS
#define HAS_TRUNCATE
#endif

/* HAS_TZNAME:
 *	This symbol, if defined, indicates that the tzname[] array is
 *	available to access timezone names.
 */
/*#undef HAS_TZNAME*/

/* HAS_UMASK:
 *	This symbol, if defined, indicates that the umask routine is
 *	available to set and get the value of the file creation mask.
 */
#define HAS_UMASK

/* HAS_VFORK:
 *	This symbol, if defined, indicates that vfork() exists.
 */
/*#undef HAS_VFORK*/

/* Signal_t:
 *	This symbol's value is either "void" or "int", corresponding to the
 *	appropriate return type of a signal handler.  Thus, you can declare
 *	a signal handler using "Signal_t (*handler)()", and define the
 *	handler using "Signal_t handler(sig)".
 */
#define Signal_t void	/* Signal handler's return type */

/* HASVOLATILE:
 *	This symbol, if defined, indicates that this C compiler knows about
 *	the volatile declaration.
 */
#define	HASVOLATILE
#ifndef HASVOLATILE
#define volatile
#endif

/* HAS_VPRINTF:
 *	This symbol, if defined, indicates that the vprintf routine is available
 *	to printf with a pointer to an argument list.  If unavailable, you
 *	may need to write your own, probably in terms of _doprnt().
 */
/* USE_CHAR_VSPRINTF:
 *	This symbol is defined if this system has vsprintf() returning type
 *	(char*).  The trend seems to be to declare it as "int vsprintf()".  It
 *	is up to the package author to declare vsprintf correctly based on the
 *	symbol.
 */
#ifdef SPEC_CPU2000_AIX
/*#undef HAS_VPRINTF*/
#else
#define HAS_VPRINTF
#endif
/*#undef USE_CHAR_VSPRINTF*/

/* HAS_WAIT4:
 *	This symbol, if defined, indicates that wait4() exists.
 */
/*#undef HAS_WAIT4*/

/* HAS_WAITPID:
 *	This symbol, if defined, indicates that the waitpid routine is
 *	available to wait for child process.
 */
/*#undef HAS_WAITPID*/

/* HAS_WCSTOMBS:
 *	This symbol, if defined, indicates that the wcstombs routine is
 *	available to convert wide character strings to multibyte strings.
 */
/*#undef HAS_WCSTOMBS*/

/* HAS_WCTOMB:
 *	This symbol, if defined, indicates that the wctomb routine is available
 *	to covert a wide character to a multibyte.
 */
/*#undef HAS_WCTOMB*/

/* EBCDIC:
 *	This symbol, if defined, indicates that this system uses
 *	EBCDIC encoding.
 */
#ifdef SPEC_CPU2000_OS390
# define EBCDIC
#else
/*# undef EBCDIC*/
#endif

/* Fpos_t:
 *	This symbol holds the type used to declare file positions in libc.
 *	It can be fpos_t, long, uint, etc... It may be necessary to include
 *	<sys/types.h> to get any typedef'ed information.
 */
#define Fpos_t fpos_t		/* File position type */

/* Gid_t:
 *	This symbol holds the return type of getgid() and the type of
 *	argument to setrgid() and related functions.  Typically,
 *	it is the type of group ids in the kernel. It can be int, ushort,
 *	uid_t, etc... It may be necessary to include <sys/types.h> to get
 *	any typedef'ed information.
 */
#define Gid_t gid_t		/* Type for getgid(), etc... */

/* Groups_t:
 *	This symbol holds the type used for the second argument to
 *	[gs]etgroups().  Usually, this is the same of gidtype, but
 *	sometimes it isn't.  It can be int, ushort, uid_t, etc... 
 *	It may be necessary to include <sys/types.h> to get any 
 *	typedef'ed information.  This is only required if you have
 *	getgroups() or setgroups().
 */
#if defined(HAS_GETGROUPS) || defined(HAS_SETGROUPS)
#define Groups_t gid_t	/* Type for 2nd arg to [gs]etgroups() */
#endif

/* DB_Prefix_t:
 *	This symbol contains the type of the prefix structure element
 *	in the <db.h> header file.  In older versions of DB, it was
 *	int, while in newer ones it is u_int32_t.
 */
/* DB_Hash_t:
 *	This symbol contains the type of the prefix structure element
 *	in the <db.h> header file.  In older versions of DB, it was
 *	int, while in newer ones it is size_t.
 */
#ifdef SPEC_CPU2000_NTOS
#define DB_Hash_t	int
#define DB_Prefix_t	int
#else
#define DB_Hash_t	u_int32_t
#define DB_Prefix_t	size_t
#endif

/* I_DIRENT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <dirent.h>. Using this symbol also triggers the definition
 *	of the Direntry_t define which ends up being 'struct dirent' or
 *	'struct direct' depending on the availability of <dirent.h>.
 */
/* DIRNAMLEN:
 *	This symbol, if defined, indicates to the C program that the length
 *	of directory entry names is provided by a d_namlen field.  Otherwise
 *	you need to do strlen() on the d_name field.
 */
/* Direntry_t:
 *	This symbol is set to 'struct direct' or 'struct dirent' depending on
 *	whether dirent is available or not. You should use this pseudo type to
 *	portably declare your directory entries.
 */
#define I_DIRENT
/*#undef DIRNAMLEN*/
#ifdef SPEC_CPU2000_NTOS
#define Direntry_t struct direct
#else
#define Direntry_t struct dirent
#endif

/* I_DLFCN:
 *	This symbol, if defined, indicates that <dlfcn.h> exists and should
 *	be included.
 */
/*#undef I_DLFCN*/

/* I_FCNTL:
 *	This manifest constant tells the C program to include <fcntl.h>.
 */
#if defined(SPEC_CPU2000_DUNIX)  \
 || defined(SPEC_CPU2000_NTOS)   \
 || defined(SPEC_CPU2000_LINUX)  \
 || defined(SPEC_CPU2000_MACOSX) \
 || defined(SPEC_CPU2000_HP)     \
 || defined(SPEC_CPU2000_RELIANT_UNIX) \
 || defined(SPEC_CPU2000_SCO_UW2)    \
 || defined(SPEC_CPU2000_AIX) \
 || defined(SPEC_CPU2000_SOLARIS) \
 || defined(SPEC_CPU2000_SOLARIS_X86) \
 || defined(SPEC_CPU2000_SGI)
#define I_FCNTL	/ **/
#endif

/* I_FLOAT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <float.h> to get definition of symbols like DBL_MAX or
 *	DBL_MIN, i.e. machine dependent floating point values.
 */
#define I_FLOAT

/* I_GRP:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <grp.h>.
 */
/* GRPASSWD:
 *	This symbol, if defined, indicates to the C program that struct group
 *	contains gr_passwd.
 */
/* HAS_SETGRENT:
 *	This symbol, if defined, indicates that the getgrent routine is
 *	available for initializing sequential access of the group database.
 */
/* HAS_GETGRENT:
 *	This symbol, if defined, indicates that the getgrent routine is
 *	available for sequential access of the group database.
 */
/* HAS_ENDGRENT:
 *	This symbol, if defined, indicates that the getgrent routine is
 *	available for finalizing sequential access of the group database.
 */
#define I_GRP
/*#undef GRPASSWD*/
/*#undef HAS_SETGRENT*/
/*#undef HAS_GETGRENT*/
/*#undef HAS_ENDGRENT*/

/* I_LIMITS:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <limits.h> to get definition of symbols like WORD_BIT or
 *	LONG_MAX, i.e. machine dependant limitations.
 */
#define I_LIMITS

/* I_MATH:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <math.h>.
 */
#define I_MATH

/* I_MEMORY:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <memory.h>.
 */
/*#undef I_MEMORY*/

/* I_NDBM:
 *	This symbol, if defined, indicates that <ndbm.h> exists and should
 *	be included.
 */
/*#undef I_NDBM*/

/* I_NET_ERRNO:
 *	This symbol, if defined, indicates that <net/errno.h> exists and 
 *	should be included.
 */
/*#undef I_NET_ERRNO*/

/* I_NETINET_IN:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <netinet/in.h>. Otherwise, you may try <sys/in.h>.
 */
#if defined(SPEC_CPU2000_HP)
#define I_NETINET_IN
#endif

/* I_NETDB:
 *	This symbol, if defined, indicates that <netdb.h> exists and
 *	should be included.
 */
/*#undef I_NETDB*/

/* I_PWD:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <pwd.h>.
 */
/* PWQUOTA:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_quota.
 */
/* PWAGE:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_age.
 */
/* PWCHANGE:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_change.
 */
/* PWCLASS:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_class.
 */
/* PWEXPIRE:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_expire.
 */
/* PWCOMMENT:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_comment.
 */
/* PWGECOS:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_gecos.
 */
/* PWPASSWD:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_passwd.
 */
/* HAS_SETPWENT:
 *	This symbol, if defined, indicates that the getpwrent routine is
 *	available for initializing sequential access of the passwd database.
 */
/* HAS_GETPWENT:
 *	This symbol, if defined, indicates that the getpwent routine is
 *	available for sequential access of the password database.
 */
/* HAS_ENDPWENT:
 *	This symbol, if defined, indicates that the getpwent routine is
 *	available for finalizing sequential access of the passwd database.
 */
#ifdef SPEC_CPU2000_NTOS
#undef I_PWD
#else
#define I_PWD
#endif
/*#undef PWQUOTA*/
/*#undef PWAGE*/
/*#undef PWCHANGE*/
/*#undef PWCLASS*/
/*#undef PWEXPIRE*/
/*#undef PWCOMMENT*/
/*#undef PWGECOS*/
/*#undef PWPASSWD*/
/*#undef HAS_SETPWENT*/
/*#undef HAS_GETPWENT*/
/*#undef HAS_ENDPWENT*/

/* I_STDDEF:
 *	This symbol, if defined, indicates that <stddef.h> exists and should
 *	be included.
 */
#define I_STDDEF

/* I_STDLIB:
 *	This symbol, if defined, indicates that <stdlib.h> exists and should
 *	be included.
 */
#define I_STDLIB

/* I_STRING:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <string.h> (USG systems) instead of <strings.h> (BSD systems).
 */
#define I_STRING

/* I_SYS_DIR:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/dir.h>.
 */
/*#undef I_SYS_DIR*/

/* I_SYS_FILE:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/file.h> to get definition of R_OK and friends.
 */
/*#undef I_SYS_FILE*/

/* I_SYS_IOCTL:
 *	This symbol, if defined, indicates that <sys/ioctl.h> exists and should
 *	be included. Otherwise, include <sgtty.h> or <termio.h>.
 */
#ifdef SPEC_CPU2000_NTOS
/*#undef	I_SYS_IOCTL*/
#else
#define	I_SYS_IOCTL
#endif

/* I_SYS_NDIR:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/ndir.h>.
 */
/*#undef I_SYS_NDIR*/

/* I_SYS_PARAM:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/param.h>.
 */
/*#undef I_SYS_PARAM*/

/* I_SYS_RESOURCE:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/resource.h>.
 */
#ifdef SPEC_CPU2000_NTOS
/*#undef I_SYS_RESOURCE*/
#else
#define I_SYS_RESOURCE
#endif

/* I_SYS_SELECT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/select.h> in order to get definition of struct timeval.
 */
#if defined(SPEC_CPU2000_NTOS) || \
    defined(SPEC_CPU2000_HP)
/*#undef I_SYS_SELECT*/
#else
#define I_SYS_SELECT
#endif

/* I_SYS_TIMES:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/times.h>.
 */
#ifdef SPEC_CPU2000_NTOS
/*#undef I_SYS_TIMES*/
#else
#define	I_SYS_TIMES
#endif

/* I_SYS_TYPES:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/types.h>.
 */
#define	I_SYS_TYPES

/* I_SYS_UN:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/un.h> to get UNIX domain socket definitions.
 */
/*#undef I_SYS_UN*/

/* I_SYS_WAIT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/wait.h>.
 */
/*#undef I_SYS_WAIT*/

/* I_TERMIO:
 *	This symbol, if defined, indicates that the program should include
 *	<termio.h> rather than <sgtty.h>.  There are also differences in
 *	the ioctl() calls that depend on the value of this symbol.
 */
/* I_TERMIOS:
 *	This symbol, if defined, indicates that the program should include
 *	the POSIX termios.h rather than sgtty.h or termio.h.
 *	There are also differences in the ioctl() calls that depend on the
 *	value of this symbol.
 */
/* I_SGTTY:
 *	This symbol, if defined, indicates that the program should include
 *	<sgtty.h> rather than <termio.h>.  There are also differences in
 *	the ioctl() calls that depend on the value of this symbol.
 */
/*#undef I_TERMIO*/
/*#undef I_TERMIOS*/
/*#undef I_SGTTY*/

/* I_TIME:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <time.h>.
 */
/* I_SYS_TIME:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/time.h>.
 */
/* I_SYS_TIME_KERNEL:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/time.h> with KERNEL defined.
 */
#if defined(SPEC_CPU2000_NTOS) || \
    defined(SPEC_CPU2000_AIX)
#define I_TIME		/ **/
#endif
#ifndef SPEC_CPU2000_NTOS
#define I_SYS_TIME
#endif
/*#undef I_SYS_TIME_KERNEL*/

/* I_UNISTD:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <unistd.h>.
 */
#ifdef SPEC_CPU2000_NTOS
/*#undef I_UNISTD*/
#else
#define I_UNISTD
#endif

/* I_UTIME:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <utime.h>.
 */
#define I_UTIME

/* I_STDARG:
 *	This symbol, if defined, indicates that <stdarg.h> exists and should
 *	be included.
 */
/* I_VARARGS:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <varargs.h>.
 */
#define I_STDARG
/*#undef I_VARARGS*/

/* I_VFORK:
 *	This symbol, if defined, indicates to the C program that it should
 *	include vfork.h.
 */
/*#undef I_VFORK*/

/* Off_t:
 *	This symbol holds the type used to declare offsets in the kernel.
 *	It can be int, long, off_t, etc... It may be necessary to include
 *	<sys/types.h> to get any typedef'ed information.
 */
#define Off_t off_t		/* <offset> type */

/* Mode_t:
 *	This symbol holds the type used to declare file modes 
 *	for systems calls.  It is usually mode_t, but may be
 *	int or unsigned short.  It may be necessary to include <sys/types.h>
 *	to get any typedef'ed information.
 */
#define Mode_t mode_t	 /* file mode parameter for system calls */

/* VAL_O_NONBLOCK:
 *	This symbol is to be used during open() or fcntl(F_SETFL) to turn on
 *	non-blocking I/O for the file descriptor. Note that there is no way
 *	back, i.e. you cannot turn it blocking again this way. If you wish to
 *	alternatively switch between blocking and non-blocking, use the
 *	ioctl(FIOSNBIO) call instead, but that is not supported by all devices.
 */
/* VAL_EAGAIN:
 *	This symbol holds the errno error code set by read() when no data was
 *	present on the non-blocking file descriptor.
 */
/* RD_NODATA:
 *	This symbol holds the return code from read() when no data is present
 *	on the non-blocking file descriptor. Be careful! If EOF_NONBLOCK is
 *	not defined, then you can't distinguish between no data and EOF by
 *	issuing a read(). You'll have to find another way to tell for sure!
 */
/* EOF_NONBLOCK:
 *	This symbol, if defined, indicates to the C program that a read() on
 *	a non-blocking file descriptor will return 0 on EOF, and not the value
 *	held in RD_NODATA (-1 usually, in that case!).
 */
#define VAL_O_NONBLOCK O_NONBLOCK
#define VAL_EAGAIN EAGAIN
#define RD_NODATA -1
#define EOF_NONBLOCK

/* CAN_PROTOTYPE:
 *	If defined, this macro indicates that the C compiler can handle
 *	function prototypes.
 */
/* _:
 *	This macro is used to declare function parameters for folks who want
 *	to make declarations with prototypes using a different style than
 *	the above macros.  Use double parentheses.  For example:
 *
 *		int main _((int argc, char *argv[]));
 */
#define	CAN_PROTOTYPE
#ifdef CAN_PROTOTYPE
#define	_(args) args
#else
#define	_(args) ()
#endif

/* RANDBITS:
 *	This symbol contains the number of bits of random number the rand()
 *	function produces.  Usual values are 15, 16, and 31.
 */
#define RANDBITS 31

/* Select_fd_set_t:
 *	This symbol holds the type used for the 2nd, 3rd, and 4th
 *	arguments to select.  Usually, this is 'fd_set *', if HAS_FD_SET
 *	is defined, and 'int *' otherwise.  This is only useful if you 
 *	have select(), of course.
 */
#ifdef SPEC_CPU2000_NTOS
#define Select_fd_set_t 	int *
#else
#define Select_fd_set_t 	fd_set *
#endif

/* Pid_t:
 *	This symbol holds the type used to declare process ids in the kernel.
 *	It can be int, uint, pid_t, etc... It may be necessary to include
 *	<sys/types.h> to get any typedef'ed information.
 */
#define Pid_t pid_t		/* PID type */

/* Size_t:
 *	This symbol holds the type used to declare length parameters
 *	for string functions.  It is usually size_t, but may be
 *	unsigned long, int, etc.  It may be necessary to include
 *	<sys/types.h> to get any typedef'ed information.
 */
#define Size_t size_t	 /* length paramater for string functions */

/* SSize_t:
 *	This symbol holds the type used by functions that return
 *	a count of bytes or an error condition.  It must be a signed type.
 *	It is usually ssize_t, but may be long or int, etc.
 *	It may be necessary to include <sys/types.h> or <unistd.h>
 *	to get any typedef'ed information.
 *	We will pick a type such that sizeof(SSize_t) == sizeof(Size_t).
 */
#ifdef SPEC_CPU2000_NTOS
#ifdef SPEC_CPU2000_P64
#define SSize_t __int64	 /* signed count of bytes */
#else
#define SSize_t int	 /* signed count of bytes */
#endif /* SPEC_CPU2000_P64 */
#else
#define SSize_t ssize_t	 /* signed count of bytes */
#endif

/* STDCHAR:
 *	This symbol is defined to be the type of char used in stdio.h.
 *	It has the values "unsigned char" or "char".
 */
#if defined(SPEC_CPU2000_SOLARIS)  \
 || defined(SPEC_CPU2000_SOLARIS_X86)
#define STDCHAR char
#else
#define STDCHAR unsigned char
#endif

/* Uid_t:
 *	This symbol holds the type used to declare user ids in the kernel.
 *	It can be int, ushort, uid_t, etc... It may be necessary to include
 *	<sys/types.h> to get any typedef'ed information.
 */
#define Uid_t uid_t		/* UID type */

/* LOC_SED:
 *	This symbol holds the complete pathname to the sed program.
 */
/*#undef LOC_SED*/

/* OSNAME:
 *	This symbol contains the name of the operating system, as determined
 *	by Configure.  You shouldn't rely on it too much; the specific
 *	feature tests from Configure are generally more reliable.
 */
#define OSNAME "spec"

/* ARCHLIB:
 *	This variable, if defined, holds the name of the directory in
 *	which the user wants to put architecture-dependent public
 *	library files for perl5.  It is most often a local directory
 *	such as /usr/local/lib.  Programs using this variable must be
 *	prepared to deal with filename expansion.  If ARCHLIB is the
 *	same as PRIVLIB, it is not defined, since presumably the
 *	program already searches PRIVLIB.
 */
/* ARCHLIB_EXP:
 *	This symbol contains the ~name expanded version of ARCHLIB, to be used
 *	in programs that are not prepared to deal with ~ expansion at run-time.
 */
#define ARCHLIB "."
#define ARCHLIB_EXP "."

/* BINCOMPAT3:
 *	This symbol, if defined, indicates that Perl 5.004 should be
 *	binary-compatible with Perl 5.003.
 */
/*#undef BINCOMPAT3*/

/* CSH:
 *	This symbol, if defined, indicates that the C-shell exists.
 *	If defined, contains the full pathname of csh.
 */
/*#undef CSH*/

/* HAS_ENDHOSTENT:
 *	This symbol, if defined, indicates that the endhostent() routine is
 *	available to close whatever was being used for host queries.
 */
/*#undef HAS_ENDHOSTENT*/

/* HAS_ENDNETENT:
 *	This symbol, if defined, indicates that the endnetent() routine is
 *	available to close whatever was being used for network queries.
 */
/*#undef HAS_ENDNETENT*/

/* HAS_ENDPROTOENT:
 *	This symbol, if defined, indicates that the endprotoent() routine is
 *	available to close whatever was being used for protocol queries.
 */
/*#undef HAS_ENDPROTOENT*/

/* HAS_ENDSERVENT:
 *	This symbol, if defined, indicates that the endservent() routine is
 *	available to close whatever was being used for service queries.
 */
/*#undef HAS_ENDSERVENT*/

/* HAS_GETHOSTBYADDR:
 *	This symbol, if defined, indicates that the gethostbyaddr() routine is
 *	available to look up hosts by their IP addresses.
 */
/*#undef HAS_GETHOSTBYADDR*/

/* HAS_GETHOSTBYNAME:
 *	This symbol, if defined, indicates that the gethostbyname() routine is
 *	available to look up host names in some data base or other.
 */
/*#undef HAS_GETHOSTBYNAME*/

/* HAS_GETNETBYADDR:
 *	This symbol, if defined, indicates that the getnetbyaddr() routine is
 *	available to look up networks by their IP addresses.
 */
/*#undef HAS_GETNETBYADDR*/

/* HAS_GETNETBYNAME:
 *	This symbol, if defined, indicates that the getnetbyname() routine is
 *	available to look up networks by their names.
 */
/*#undef HAS_GETNETBYNAME*/

/* HAS_GETNETENT:
 *	This symbol, if defined, indicates that the getnetent() routine is
 *	available to look up network names in some data base or another.
 */
/*#undef HAS_GETNETENT*/

/* HAS_GETPROTOENT:
 *	This symbol, if defined, indicates that the getprotoent() routine is
 *	available to look up protocols in some data base or another.
 */
/*#undef HAS_GETPROTOENT*/

/* HAS_GETPROTOBYNAME:
 *	This symbol, if defined, indicates that the getprotobyname()
 *	routine is available to look up protocols by their name.
 */
/* HAS_GETPROTOBYNUMBER:
 *	This symbol, if defined, indicates that the getprotobynumber()
 *	routine is available to look up protocols by their number.
 */
/*#undef HAS_GETPROTOBYNAME*/
/*#undef HAS_GETPROTOBYNUMBER*/

/* HAS_GETSERVENT:
 *	This symbol, if defined, indicates that the getservent() routine is
 *	available to look up network services in some data base or another.
 */
/*#undef HAS_GETSERVENT*/

/* HAS_GETSERVBYNAME:
 *	This symbol, if defined, indicates that the getservbyname()
 *	routine is available to look up services by their name.
 */
/* HAS_GETSERVBYPORT:
 *	This symbol, if defined, indicates that the getservbyport()
 *	routine is available to look up services by their port.
 */
/*#undef HAS_GETSERVBYNAME*/
/*#undef HAS_GETSERVBYPORT*/

/* DLSYM_NEEDS_UNDERSCORE:
 *	This symbol, if defined, indicates that we need to prepend an
 *	underscore to the symbol name before calling dlsym().  This only
 *	makes sense if you *have* dlsym, which we will presume is the
 *	case if you're using dl_dlopen.xs.
 */
/*#undef 	DLSYM_NEEDS_UNDERSCORE*/

/* SETUID_SCRIPTS_ARE_SECURE_NOW:
 *	This symbol, if defined, indicates that the bug that prevents
 *	setuid scripts from being secure is not present in this kernel.
 */
/* DOSUID:
 *	This symbol, if defined, indicates that the C program should
 *	check the script that it is executing for setuid/setgid bits, and
 *	attempt to emulate setuid/setgid on systems that have disabled
 *	setuid #! scripts because the kernel can't do it securely.
 *	It is up to the package designer to make sure that this emulation
 *	is done securely.  Among other things, it should do an fstat on
 *	the script it just opened to make sure it really is a setuid/setgid
 *	script, it should make sure the arguments passed correspond exactly
 *	to the argument on the #! line, and it should not trust any
 *	subprocesses to which it must pass the filename rather than the
 *	file descriptor of the script to be executed.
 */
#undef SETUID_SCRIPTS_ARE_SECURE_NOW
#undef DOSUID

/* Gconvert:
 *	This preprocessor macro is defined to convert a floating point
 *	number to a string without a trailing decimal point.  This
 *	emulates the behavior of sprintf("%g"), but is sometimes much more
 *	efficient.  If gconvert() is not available, but gcvt() drops the
 *	trailing decimal point, then gcvt() is used.  If all else fails,
 *	a macro using sprintf("%g") is used. Arguments for the Gconvert
 *	macro are: value, number of digits, whether trailing zeros should
 *	be retained, and the output buffer.
 *	Possible values are:
 *		d_Gconvert='gconvert((x),(n),(t),(b))'
 *		d_Gconvert='gcvt((x),(n),(b))'
 *		d_Gconvert='sprintf((b),"%.*g",(n),(x))'
 *	The last two assume trailing zeros should not be kept.
 */
#define Gconvert(x,n,t,b) sprintf((b),"%.*g",(n),(x))

/* HAS_GNULIBC:
 *	This symbol, if defined, indicates to the C program that 
 *	the GNU C library is being used.
 */
/*#undef HAS_GNULIBC*/

/* HAS_LCHOWN:
 *	This symbol, if defined, indicates that the lchown routine is
 *	available to operate on a symbolic link (instead of following the
 *	link).
 */
/*#undef HAS_LCHOWN*/

/* HAS_GETPGID:
 *	This symbol, if defined, indicates to the C program that 
 *	the getpgid(pid) function is available to get the
 *	process group id.
 */
/*#undef HAS_GETPGID*/

/* HAS_GETPGRP:
 *	This symbol, if defined, indicates that the getpgrp routine is
 *	available to get the current process group.
 */
/* USE_BSD_GETPGRP:
 *	This symbol, if defined, indicates that getpgrp needs one
 *	arguments whereas USG one needs none.
 */
/*#undef HAS_GETPGRP*/
/*#undef USE_BSD_GETPGRP*/

/* HAS_INET_ATON:
 *	This symbol, if defined, indicates to the C program that the
 *	inet_aton() function is available to parse IP address "dotted-quad"
 *	strings.
 */
/*#undef HAS_INET_ATON*/

/* HAS_SETPGID:
 *	This symbol, if defined, indicates to the C program that 
 *	the setpgid(pid, gpid) function is available to set the
 *	process group id.
 */
/*#undef HAS_SETPGID*/

/* HAS_SETPGRP:
 *	This symbol, if defined, indicates that the setpgrp routine is
 *	available to set the current process group.
 */
/* USE_BSD_SETPGRP:
 *	This symbol, if defined, indicates that setpgrp needs two
 *	arguments whereas USG one needs none.  See also HAS_SETPGID
 *	for a POSIX interface.
 */
/* USE_BSDPGRP:
 *	This symbol, if defined, indicates that the BSD notion of process
 *	group is to be used. For instance, you have to say setpgrp(pid, pgrp)
 *	instead of the USG setpgrp().  This should be obsolete since
 *	there are systems which have BSD-ish setpgrp but USG-ish getpgrp.
 */
/*#undef HAS_SETPGRP*/
/*#undef USE_BSD_SETPGRP*/
/*#undef USE_BSDPGRP*/

/* USE_SFIO:
 *	This symbol, if defined, indicates that sfio should
 *	be used.
 */
/*#undef	USE_SFIO*/

/* Sigjmp_buf:
 *	This is the buffer type to be used with Sigsetjmp and Siglongjmp.
 */
/* Sigsetjmp:
 *	This macro is used in the same way as sigsetjmp(), but will invoke
 *	traditional setjmp() if sigsetjmp isn't available.
 *	See HAS_SIGSETJMP.
 */
/* Siglongjmp:
 *	This macro is used in the same way as siglongjmp(), but will invoke
 *	traditional longjmp() if siglongjmp isn't available.
 *	See HAS_SIGSETJMP.
 */
/* try to avoid SIGSETJMP... SPEC CPU2000 9-Nov-98
#ifndef SPEC_CPU2000_NTOS
#define HAS_SIGSETJMP	
#endif
*/
/*#undef HAS_SIGSETJMP*/

#ifdef HAS_SIGSETJMP
#define Sigjmp_buf sigjmp_buf
#define Sigsetjmp(buf,save_mask) sigsetjmp((buf),(save_mask))
#define Siglongjmp(buf,retval) siglongjmp((buf),(retval))
#else
#define Sigjmp_buf jmp_buf
#define Sigsetjmp(buf,save_mask) setjmp((buf))
#define Siglongjmp(buf,retval) longjmp((buf),(retval))
#endif

/* USE_DYNAMIC_LOADING:
 *	This symbol, if defined, indicates that dynamic loading of
 *	some sort is available.
 */
#undef USE_DYNAMIC_LOADING

/* I_ARPA_INET:
 *	This symbol, if defined, indicates that <arpa/inet.h> exists and should
 *	be included.
 */
/*#undef I_ARPA_INET*/

/* I_DBM:
 *	This symbol, if defined, indicates that <dbm.h> exists and should
 *	be included.
 */
/* I_RPCSVC_DBM:
 *	This symbol, if defined, indicates that <rpcsvc/dbm.h> exists and
 *	should be included.
 */
/*#undef I_DBM*/
/*#undef I_RPCSVC_DBM*/

/* I_LOCALE:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <locale.h>.
 */
#define	I_LOCALE

/* I_SFIO:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sfio.h>.
 */
/*#undef	I_SFIO*/

/* I_SYS_STAT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/stat.h>.
 */
#define	I_SYS_STAT

/* I_VALUES:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <values.h> to get definition of symbols like MINFLOAT or
 *	MAXLONG, i.e. machine dependant limitations.  Probably, you
 *	should use <limits.h> instead, if it is available.
 */
/*#undef I_VALUES*/

/* Free_t:
 *	This variable contains the return type of free().  It is usually
 * void, but occasionally int.
 */
/* Malloc_t:
 *	This symbol is the type of pointer returned by malloc and realloc.
 */
#define Malloc_t void *
#define Free_t void

/* OLDARCHLIB:
 *	This variable, if defined, holds the name of the directory in
 *	which the user has perl5.000 or perl5.001 architecture-dependent
 *	public library files for perl5.  For the most part, these
 *	files will work with 5.002 (and later), but that is not
 *	guaranteed.
 */
/* OLDARCHLIB_EXP:
 *	This symbol contains the ~name expanded version of OLDARCHLIB, to be
 *	used in programs that are not prepared to deal with ~ expansion at 
 *	run-time.
 */
#undef OLDARCHLIB
#undef OLDARCHLIB_EXP

/* PRIVLIB:
 *	This symbol contains the name of the private library for this package.
 *	The library is private in the sense that it needn't be in anyone's
 *	execution path, but it should be accessible by the world.  The program
 *	should be prepared to do ~ expansion.
 */
/* PRIVLIB_EXP:
 *	This symbol contains the ~name expanded version of PRIVLIB, to be used
 *	in programs that are not prepared to deal with ~ expansion at run-time.
 */
#define PRIVLIB "."
#define PRIVLIB_EXP "."

/* SH_PATH:
 *	This symbol contains the full pathname to the shell used on this
 *	on this system to execute Bourne shell scripts.  Usually, this will be
 *	/bin/sh, though it's possible that some systems will have /bin/ksh,
 *	/bin/pdksh, /bin/ash, /bin/bash, or even something such as
 *	D:/bin/sh.exe.
 */
#ifdef SPEC_CPU2000_NTOS
#define SH_PATH "cmd /x /c"
#else
#define SH_PATH "/bin/sh"
#endif

/* SIG_NAME:
 *	This symbol contains a list of signal names in order of
 *	signal number. This is intended
 *	to be used as a static array initialization, like this:
 *		char *sig_name[] = { SIG_NAME };
 *	The signals in the list are separated with commas, and each signal
 *	is surrounded by double quotes. There is no leading SIG in the signal
 *	name, i.e. SIGQUIT is known as "QUIT".
 *	Gaps in the signal numbers (up to NSIG) are filled in with NUMnn,
 *	etc., where nn is the actual signal number (e.g. NUM37).
 *	The signal number for sig_name[i] is stored in sig_num[i].
 *	The last element is 0 to terminate the list with a NULL.  This
 *	corresponds to the 0 at the end of the sig_num list.
 */
/* SIG_NUM:
 *	This symbol contains a list of signal numbers, in the same order as the
 *	SIG_NAME list. It is suitable for static array initialization, as in:
 *		int sig_num[] = { SIG_NUM };
 *	The signals in the list are separated with commas, and the indices
 *	within that list and the SIG_NAME list match, so it's easy to compute
 *	the signal name from a number or vice versa at the price of a small
 *	dynamic linear lookup. 
 *	Duplicates are allowed, but are moved to the end of the list.
 *	The signal number corresponding to sig_name[i] is sig_number[i].
 *	if (i < NSIG) then sig_number[i] == i.  
 *	The last element is 0, corresponding to the 0 at the end of
 *	the sig_name list.
 */
#ifdef SPEC_CPU2000_AIX 
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","ABRT","EMT","FPE","KILL","BUS","SEGV","SYS","PIPE","ALRM","TERM","IOINT","STOP","TSTP","CONT","CHLD","TTIN","TTOU","AIO","XCPU","XFSZ","NUM26","MSG","WINCH","PWR","USR1","USR2","PROF","DANGER","VTALRM","MIGRATE","PRE","VIRT","ALRM1","WAITING","NUM40","NUM41","NUM42","NUM43","NUM44","NUM45","NUM46","NUM47","NUM48","NUM49","NUM50","NUM51","NUM52","NUM53","NUM54","NUM55","NUM56","NUM57","NUM58","NUM59","GRANT","RETRACT","SOUND","MAX","IOT","LOST","URG","CLD","IO","PTY","KAP","SAK",0
#define SIG_NUM 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,6,6,16,20,23,23,60,63,0

#elif defined(SPEC_CPU2000_DUNIX)
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","ABRT","EMT","FPE","KILL","BUS","SEGV","SYS","PIPE","ALRM","TERM","IOINT","STOP","TSTP","CONT","CHLD","TTIN","TTOU","AIO","XCPU","XFSZ","VTALRM","PROF","WINCH","INFO","USR1","USR2","RESV","RTMIN","NUM34","NUM35","NUM36","NUM37","NUM38","NUM39","NUM40","NUM41","NUM42","NUM43","NUM44","NUM45","NUM46","NUM47","MAX","IOT","LOST","URG","CLD","IO","POLL","PTY","PWR","RTMAX",0
#define SIG_NUM 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,6,6,16,20,23,23,23,29,48,0

#elif defined(SPEC_CPU2000_LINUX) || \
      defined(SPEC_CPU2000_MACOSX) 
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","ABRT","BUS","FPE","KILL","USR1","SEGV","USR2","PIPE","ALRM","TERM","STKFLT","CHLD","CONT","STOP","TSTP","TTIN","TTOU","URG","XCPU","XFSZ","VTALRM","PROF","WINCH","IO","PWR","UNUSED","IOT","CLD","POLL",0
#define SIG_NUM 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,6,17,29,0

#elif defined(SPEC_CPU2000_NTOS)
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","ABRT","EMT","FPE","KILL","BUS","SEGV","SYS","PIPE","ALRM","TERM","USR1","USR2","CHLD","PWR","WINCH","URG","IO","STOP","TSTP","CONT","TTIN","TTOU","VTALRM","PROF","XCPU","XFSZ","WAITING","LWP","FREEZE","THAW","RTMIN","NUM37","NUM38","NUM39","NUM40","NUM41","NUM42","RTMAX","IOT","CLD","POLL",0
#define SIG_NUM 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,6,18,22,0

#elif defined(SPEC_CPU2000_SGI)
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","ABRT","EMT","FPE","KILL","BUS","SEGV","SYS","PIPE","ALRM","TERM","USR1","USR2","CHLD","PWR","WINCH","URG","IO","STOP","TSTP","CONT","TTIN","TTOU","VTALRM","PROF","XCPU","XFSZ","32","CKPT","RESTART","NUM35","NUM36","NUM37","NUM38","NUM39","NUM40","NUM41","NUM42","NUM43","NUM44","NUM45","NUM46","PTINTR","PTRESCHED","RTMIN","NUM50","NUM51","NUM52","NUM53","NUM54","NUM55","NUM56","NUM57","NUM58","NUM59","NUM60","NUM61","NUM62","NUM63","RTMAX","IOT","CLD","POLL",0
#define SIG_NUM 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,6,18,22,0

#elif defined(SPEC_CPU2000_SOLARIS) \
 || defined(SPEC_CPU2000_SOLARIS_X86)
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","ABRT","EMT","FPE","KILL","BUS","SEGV","SYS","PIPE","ALRM","TERM","USR1","USR2","CHLD","PWR","WINCH","URG","IO","STOP","TSTP","CONT","TTIN","TTOU","VTALRM","PROF","XCPU","XFSZ","WAITING","LWP","FREEZE","THAW","CANCEL","LOST","RTMIN","NUM39","NUM40","NUM41","NUM42","NUM43","NUM44","RTMAX","IOT","CLD","POLL",0
#define SIG_NUM 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,6,18,22,0

#else
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","ABRT","EMT","FPE","KILL","BUS","SEGV","SYS","PIPE","ALRM","TERM","IOINT","STOP","TSTP","CONT","CHLD","TTIN","TTOU","AIO","XCPU","XFSZ","VTALRM","PROF","WINCH","INFO","USR1","USR2","RESV","RTMIN","NUM34","NUM35","NUM36","NUM37","NUM38","NUM39","NUM40","NUM41","NUM42","NUM43","NUM44","NUM45","NUM46","NUM47","MAX","IOT","LOST","URG","CLD","IO","POLL","PTY","PWR","RTMAX",0
#define SIG_NUM 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,6,6,16,20,23,23,23,29,48,0
#endif

/* SELECT_MIN_BITS:
 *	This symbol holds the minimum number of bits operated by select.
 *	That is, if you do select(n, ...), how many bits at least will be
 *	cleared in the masks if some activity is detected.  Usually this
 *	is either n or 32*ceil(n/32), especially many little-endians do
 *	the latter.  This is only useful if you have select(), naturally.
 */
#define SELECT_MIN_BITS 	32

/* SITEARCH:
 *	This symbol contains the name of the private library for this package.
 *	The library is private in the sense that it needn't be in anyone's
 *	execution path, but it should be accessible by the world.  The program
 *	should be prepared to do ~ expansion.
 *	The standard distribution will put nothing in this directory.
 *	Individual sites may place their own extensions and modules in
 *	this directory.
 */
/* SITEARCH_EXP:
 *	This symbol contains the ~name expanded version of SITEARCH, to be used
 *	in programs that are not prepared to deal with ~ expansion at run-time.
 */
#define SITEARCH "."
#define SITEARCH_EXP "."

/* SITELIB:
 *	This symbol contains the name of the private library for this package.
 *	The library is private in the sense that it needn't be in anyone's
 *	execution path, but it should be accessible by the world.  The program
 *	should be prepared to do ~ expansion.
 *	The standard distribution will put nothing in this directory.
 *	Individual sites may place their own extensions and modules in
 *	this directory.
 */
/* SITELIB_EXP:
 *	This symbol contains the ~name expanded version of SITELIB, to be used
 *	in programs that are not prepared to deal with ~ expansion at run-time.
 */
#define SITELIB "."
#define SITELIB_EXP "."

/* STARTPERL:
 *	This variable contains the string to put in front of a perl
 *	script to make sure (one hopes) that it runs with perl and not
 *	some shell.
 */
#define STARTPERL "./perl"

/* USE_PERLIO:
 *	This symbol, if defined, indicates that the PerlIO abstraction should
 *	be used throughout.  If not defined, stdio should be
 *	used in a fully backward compatible manner.
 */
#undef	USE_PERLIO

/* HAS_GETHOST_PROTOS:
 *	This symbol, if defined, indicates that <netdb.h> includes
 *	prototypes for gethostent(), gethostbyname(), and
 *	gethostbyaddr().  Otherwise, it is up to the program to guess
 *	them.  See netdbtype.U for probing for various Netdb_xxx_t types.
 */
/*#undef	HAS_GETHOST_PROTOS*/

/* HAS_GETNET_PROTOS:
 *	This symbol, if defined, indicates that <netdb.h> includes
 *	prototypes for getnetent(), getnetbyname(), and
 *	getnetbyaddr().  Otherwise, it is up to the program to guess
 *	them.  See netdbtype.U for probing for various Netdb_xxx_t types.
 */
/*#undef	HAS_GETNET_PROTOS*/

/* HAS_GETPROTO_PROTOS:
 *	This symbol, if defined, indicates that <netdb.h> includes
 *	prototypes for getprotoent(), getprotobyname(), and
 *	getprotobyaddr().  Otherwise, it is up to the program to guess
 *	them.  See netdbtype.U for probing for various Netdb_xxx_t types.
 */
/*#undef HAS_GETPROTO_PROTOS*/

/* HAS_GETSERV_PROTOS:
 *	This symbol, if defined, indicates that <netdb.h> includes
 *	prototypes for getservent(), getservbyname(), and
 *	getservbyaddr().  Otherwise, it is up to the program to guess
 *	them.  See netdbtype.U for probing for various Netdb_xxx_t types.
 */
/*#undef	HAS_GETSERV_PROTOS*/

/* Netdb_host_t:
 *	This symbol holds the type used for the 1st argument
 *	to gethostbyaddr().
 */
/* Netdb_hlen_t:
 *	This symbol holds the type used for the 2nd argument
 *	to gethostbyaddr().
 */
/* Netdb_name_t:
 *	This symbol holds the type used for the argument to
 *	gethostbyname().
 */
/* Netdb_net_t:
 *	This symbol holds the type used for the 1st argument to
 *	getnetbyaddr().
 */
#define Netdb_host_t		const char *
#define Netdb_hlen_t		int
#define Netdb_name_t		const char *
#define Netdb_net_t		unsigned long

/* VOIDFLAGS:
 *	This symbol indicates how much support of the void type is given by this
 *	compiler.  What various bits mean:
 *
 *	    1 = supports declaration of void
 *	    2 = supports arrays of pointers to functions returning void
 *	    4 = supports comparisons between pointers to void functions and
 *		    addresses of void functions
 *	    8 = suports declaration of generic void pointers
 *
 *	The package designer should define VOIDUSED to indicate the requirements
 *	of the package.  This can be done either by #defining VOIDUSED before
 *	including config.h, or by defining defvoidused in Myinit.U.  If the
 *	latter approach is taken, only those flags will be tested.  If the
 *	level of void support necessary is not present, defines void to int.
 */
#ifndef VOIDUSED
#define VOIDUSED 15
#endif
#define VOIDFLAGS 15
#if (VOIDFLAGS & VOIDUSED) != VOIDUSED
#define void int		/* is void to be avoided? */
#define M_VOID			/* Xenix strikes again */
#endif

/* TRUNCATE_UINT4:
 *      This symbol, if defined, will cause the TO32 macro to be defined.
 *      TO32, which is used in the MD5 module, is used after bitwise
 *      operations (normally) to ensure that the result only contains 32
 *      bits.
 */
#ifdef SPEC_CPU2000_ILP64
#define TRUNCATE_UINT4
#endif

#endif

#ifdef SPEC_CPU2000_NTOS
#include <win32.h>
#endif

#if defined(SPEC_CPU99_AIX) \
|| defined(SPEC_CPU99_DUNIX) \
|| defined(SPEC_CPU99_HP) \
|| defined(SPEC_CPU99_ILP64) \
|| defined(SPEC_CPU99_LINUX) \
|| defined(SPEC_CPU99_LP64) \
|| defined(SPEC_CPU99_NTOS) \
|| defined(SPEC_CPU99_RELIANT_UNIX) \
|| defined(SPEC_CPU99_SCO_UW2) \
|| defined(SPEC_CPU99_SGI) \
|| defined(SPEC_CPU99_SOLARIS) \
|| defined(SPEC_CPU98_AIX) \
|| defined(SPEC_CPU98_DUNIX) \
|| defined(SPEC_CPU98_HP) \
|| defined(SPEC_CPU98_ILP64) \
|| defined(SPEC_CPU98_LINUX) \
|| defined(SPEC_CPU98_LP64) \
|| defined(SPEC_CPU98_NTOS) \
|| defined(SPEC_CPU98_RELIANT_UNIX) \
|| defined(SPEC_CPU98_SCO_UW2) \
|| defined(SPEC_CPU98_SGI) \
|| defined(SPEC_CPU98_SOLARIS) 

#error ...This suite is now called CPU2000! 
#error ...Please stop using CPU98/CPU99 defines! 
#error ...By order of J. Reilly, Sheriff, and
#error ...and rest of the CPU posse.

#endif

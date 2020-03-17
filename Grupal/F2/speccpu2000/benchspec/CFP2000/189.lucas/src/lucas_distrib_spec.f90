        program lucas_distrib
!...Modified version of lucas_mayer for the SPEC98 benchmarks program.
        implicit none
!...program version suffix
        character(*),parameter :: version='2.4d'

!...Code to test primality of Mersenne numbers, using arbitrary-precision (array-integer) arithmetic.
!   Author: Ernst W. Mayer.
!
!   This version (v2.4d) dated 20 April 1998.
!
!   Accomplishes the Mersenne-mod squaring via the weighted discrete Fourier transform technique
!   of Crandall and Fagin (Mathematics of Computation 62 (205), pp.305-324, January 1994).
!
!   For each binary exponent p, generates first p terms of the Lucas-Lehmer sequence,
!
!       s(n+1) = s(n)**2 - 2, s(1) = 4
!
!   modulo N = 2^p-1. If N divides s(p-1) (specifically, the (p-1)th remainder term = 0), N is prime.
!   If p > 1,398,269 (congratulations Joel and George!) and p /= 2976221 (congrats Gordon and George!)
!   and p /= 3021377 (congrats Roland, George and Scott!), fame (or at least a fleeting sort of
!   notoriety :-) awaits. If not, maybe you'll have learned something nonetheless. In either case,
!   go have a beer...(assuming you're of age, and are not a recovering alcoholic - wouldn't want to
!   be accused by irate parents of driving the kiddies to the bottle ;-)
!
!   Oh, and make it a good sort. Life's too short to drink crappy brewhahas, as a college buddy of
!   mine used to call them.
!
!   Calls MERS_MOD_SQUARE, FFT_SQUARE and a host of small utility routines.
!
!   Copyright 1998 by Ernst W. Mayer. This program may be used and distributed freely as long as
!   this header is included. You may modify this program freely, as long as any redistribution
!   contains the original header and a summary of the changes.
!
!   NOTES REGARDING THE PUBLIC RELEASE: since I have no control over what happens to this software
!   once it's in the public domain, I guess I'd better add some quasi-legalese caveats and suchlike:
!
!    ***********************************************************************************************
!    * This software is offered "as is," without warranty of any kind. It is intended only for the *
!    * private recreational use of our audience. If it causes your CPU to go "poof," that's tough. *
!    ***********************************************************************************************
!
!   REVISION CONTROL:
!
!   02/27/97: v2.1  - First working version of the DWT, power-of-2 FFT with precomputed trigs.
!
!   04/28/97: v2.2  - dumb-but-effective unrolled-outer-loop FFT,
!                     fixed some redundancies in indexing, better memory usage in FFT.
!
!   05/15/97: v2.3  - automatic setting of FFT length and bits per digit;
!  -07/27/97        - added a second error check, based on a combination of absolute and
!                     relative difference between two FFT checksums;
!                   - speeded up carry propagation by faster NINT and interleaving with FFT bit reversal;
!                   - implemented slightly cache-friendlier FFT;
!                   - in interactive mode prints out average # of bits/digit;
!                   - in non-interative mode reads file, thus allowing range tests;
!                   - writes to status and restart files every 10,000 iterations,
!                     (to customize this, simply change the value of parameter INCR below),
!                     and automatically looks for a restart/range file when started;
!                     if it finds either, starts in non-interactive (range test) mode,
!                     otherwise starts in interactive mode.
!                   - purge integer*8's to improve portability;
!                   - convert base_index from integer*1 to individual bits of default integers;
!                   - allow for automatic invocation of extended (> 8-byte) precision real for
!                     accuracy-sensitive, once-initialized parameters (e.g. DWT weight factors,
!                     FFT sincos arrays) on systems which support this.
!                   - replace integer multiplies/divides by powers of 2 in critical loops
!                     with bitwise leftward/rightward shifts;
!
!   10/24/97: v2.4  - radix-4 FFT. Uglier but faster...
!   11/05/97: v2.4b - radix-8 FFT. Even uglier, even faster.
!                   - use a single DWT weights array for forward and inverse weights.
!                   - implemented one-pass combined FFT wrapper/squaring routine
!                     (see documentation in routine FFT_SQUARE). 5-10% faster,
!                     and slightly more accurate, to boot.
!   02/19/98: v2.4c - Combined FFT/square routine, specialized to FFT lengths of
!                     128, 256 and 512K (exponents ranging from 1.33-10.2 million.) 7-10% faster.
!   04/08/98; v2.4d - Fixed bug in 64-bit residue printout (affected ~1 of 16 hex digits on average.)
!  -04/20/98        - Replaced +-rnd_const rounding with DNINT for purposes of SPEC benchmark.
!                   - Implemented cache-friendly 2-D array scheme: FFT arrays are dimensioned (N/8+pad) x 8.
!                   - 
!
!   FOR REAL-WORLD Mersenne searching, join GIMPS! http://www.mersenne.org
!
!   RUNNING THE PROGRAM: see the instructions in ftp://nigel.mae.cwru.edu/pub/mayer/README
!
!   ACKNOWLEDGEMENTS: special thanks are due to the following people:
!
!    Richard Crandall and Barry Fagin - for the discrete weighted transform.
!
!    George Woltman - for organizing the Great Internet Mersenne Prime Search,
!       and for freely sharing his time and keen computational insights.
!
!    Ed Haletky, John Henning and Greg Gaertner (Digital Equipment Corp.) - EH for
!       patiently answering N ( >> 1) questions regarding the Alpha architecture,
!       JH and GG for helping to shape the version of the program submitted for
!       the SPEC98 benchmark search
!
!    Peter Montgomery - for help with theoretical and implementation questions,
!       loan of his fast factoring code, and for testing the program on his SGI
!       (and uncovering several compiler bugs along the way).
!
!    Will Edgington - for maintaining the factors database, and his help in
!       organizing the Mersenne cofactor primality testing project. Will is
!       also point man on the effort (?) to port this code to C - get in touch
!       with him at wedgingt@garlic.com if you are interested in working on this.
!
!    ...as well as the numerous people who have been kind enough to try the code out
!    and send timings, compiler options for various architectures and suggestions
!    for improvement. Particular thanks in this regard to:
!    
!    Phil Brett - for providing the VMS binaries.
!    
!
!   Send QUESTIONS, COMMENTS or SUGGESTIONS to me at:
!       Ernst Mayer
!       Dept. of Mechanical and Aerospace Engineering
!       Case Western Reserve University
!       10900 Euclid Avenue
!       Cleveland, OH 44106-7222 USA
!       E-mail: mayer@nigel.mae.cwru.edu
!          or  ewmayer@aol.com
!       http://k2.scl.cwru.edu/cse/emae/faculty/mayer/
!
!       Alright, let's go hunt for some primes!     
!
!   P.S.: for people used to looking at C code (especially K & R-style),
!       and who are surprised to be able to actually follow what's going
!       on here, I apologize unreservedly. ;->
!
!   P.P.S.: for Fortran 90 wonks who are disappointed at not seeing dozens
!       of modules and interfaces, public and private parts (mine or the
!       code's), pure procedures and the like, I do NOT apologize. Keep
!       it simple, keep it clean!
!

!...parameters..

!...these select the precision for once-initialized quantities

!...This invokes extended-precision FP math (if available), which is slow, but hey, we only need it once...
!*      integer, parameter :: r16=max(selected_real_kind(18,50),selected_real_kind(15,30))
!...Strictly real*8 is better for timing tests (e.g. the SPEC98 benchmark).
        integer, parameter :: r16=selected_real_kind(15,30)

!...Quasi-generic cache-and-bank-conflict-avoiding array padding parameter is here.
!   Unctuous TV announcer: "Are you suffering from cache flow problems? Bank foreclosures? Then try my new array padding!"

        integer, parameter :: pad=33

!...Here are in-and-out (real savage-like) files, in case one wants the non-interactive version.

        character(*),parameter :: ifile     ='lucas2.in'
        character(*),parameter :: ofile     ='lucas2.out'

!...scalars and fixed-size arrays...

        integer :: bits0,base(0:1),diagnose,i,ilo,ihi,j,k,l,n,n32,nbits,p,slot,word
        real*8  :: baseinv(0:1)
        real*8, parameter :: one=1,two=2
        integer :: itmp(8)

!...allocatable arrays...

        integer,allocatable :: base_index(:) !* the bits of this one tell whether a given digit is long or short
        integer,allocatable :: ceil(:)
        real*8, allocatable :: a(:,:),wt(:)
        real(kind=r16),allocatable :: tmp16(:)

!...Known Mersenne prime exponents (as of 04/20/98):
        integer, parameter :: knowns(37)=(/2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217,4253,4423 &
        ,9689,9941,11213,19937,21701,23209,44497,86243,110503,132049,216091,756839,859433,1257787,1398269,2976221,3021377/)

!...external functions...

        character*16, external :: hex_res64

!...read exponent p, FFT array size n (must = 2^k) and number of iterations (ihi = p-1 for full test)...

!*      open(unit=1,file=ifile,status='old',action='read')
!*      read (1,*) p,n,ihi

!For SPEC purposes - we want to create a new output file JLH 31-Mar-1998
!*      open(unit=1,file=ofile,status='replace')
!*      close(1)

!...Interactive mode is here:
1     print*,'Enter exponent, runlength, iterations, diagnose flag (1=yes)>'

        read*,p,n,ihi,diagnose

!...minimum runlength is 256.
        if(n<256)then; print*,'minimum runlength is 256.'; GOTO 1; endif

        ilo=1
        if(ihi > p-1) ihi=p-1 !* Full LL test requires p-1 iterations, so no use doing more
        n32=ceiling(one*n/32)
        close(1)

!...allocate the arrays...

        allocate(a(0:n/8-1+pad,0:7),ceil(0:n),base_index(0:(n-1)/32),wt(n+1),tmp16(n))

!...find the Crandall/Fagin weighting factors and number of bits per digit...

        do j=0,n
          ceil(j)=ceiling(one*p*j/n)
          if(j < n) tmp16(j+1)=two**(ceil(j)-one*p*j/n)
        enddo
        wt(1:n)=tmp16
        wt(n+1)=2       !* define extra element needed for inverse weights

        bits0=p/n
        base(0)=2**bits0; base(1)=2*base(0)
        base_index=0    !* set all the bits = 0

        do j=1,n
          word=(j-1)/32         !* integer-divide to get the right word into which to put the current bit...
          slot=j-32*word-1      !* ...and find the slot for the bit, remembering that bits are numbered from 0 to 31.
          if(ceil(j)-ceil(j-1)>bits0)then       !* If it's a bigword...
            base_index(word)=ibset(base_index(word),slot)       !* move a 1 into the appropriate slot.
          endif
        enddo

        baseinv=one/base

        deallocate(ceil,tmp16)  !* ...and deallocate arrays which are no longer needed.

!...check that the identity wt(j)*wt(n+2-j) = 2 holds for 1 <= j <= n.
        do j=1,n
          if(abs(2d0-wt(j)*wt(n+2-j))>1d-14)print*,'WARNING: product of weight factors not 2: J=',j,wt(j)*wt(n+2-j)
        enddo

!...initialize residual...

        a=0; a(0,0)=4

!...here's the big one - the series of squaring steps
        call mers_mod_square(a,base,baseinv,base_index,wt,n,pad,ilo,ihi,p,ofile,diagnose)

!...output results...

!...Here are some bells and whistles (of the non-audible variety) in case it's a Mersenne prime.
!   Note that for serious Lucas-Lehmer tests (of the multi-day variety, i.e. p > 1 million or so),
!   one should use a code that writes checkpoint files, i.e. is restartable in case of an interrupt.
!   See the GIMPS homepage at http://www.mersenne.org, or EWM's source code archive at
!   ftp://nigel.mae.cwru.edu/pub/mayer.
!   
        if(ihi==p-1.and.maxval(abs(a))==0) then
!...It's a known M-prime...
          if(any(p==knowns))then
            write(*,'(a,i8,a)')'M(',p,' ) is a known MERSENNE PRIME.'
          else
!...It's a NEW M-prime!
            write(*,'(a,i8,a)')'M(',p,' ) is a NEW MERSENNE PRIME!!!'
            write(*,*)'please send e-mail to mayer@nigel.mae.cwru.edu and woltman@magicnet.net'
          endif
        else
!...If not prime, print the least-significant 64 bits of the Lucas-Lehmer residue.

          write(*,'(a)')'  exponent     residue' 
          write(*,'(i10,a18)')p,hex_res64(a,n,pad,p,nbits,bits0,base_index(0),p-1)

        endif

!...This is the end
        end program lucas_distrib

!*********************************   SUBPROGRAMS   *****************************************

!...
!   Character function to take an n-digit LL residue, convert it to positive-digit form,
!   and print the lowest-order 64 bits in hex form.
!...
        character*16 function hex_res64(a,n,pad,p,nbits,bits0,base_index_word,iter)
!...
        implicit none
        integer,intent(in) :: n,pad,p,bits0,base_index_word,iter
        real*8, intent(in) :: a(0:n/8-1+pad,0:7)
        integer :: i,j,l,bits(8),itmp(8),nbits,ndigit,nrem
        logical :: flag

!...These are to get around F90's habit of printing integers beginning only with the first nonzero digit
!   (whether binary, octal, hex or decimal)
        character(1),parameter :: hex_chars(0:15)=(/'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'/)

	flag=.false.
	hex_res64=' '

!...if most-significant digit in the balanced-representation form is < 0, add M(p) to the residue.
!   Since we assume M(p) has >> 64 bits and we seek only the lowest-order 64, can do this by just
!   subtracting 1 from a(1) and then normalizing the lowest-order few digits that give a total >= 64 bits.

        OUTER: do i=7,0,-1
          do j=n/8-1,0,-1
            if(a(j,i)/=0) then
              if(a(j,i)<0) flag=.true.  !* Here would perform a(1)=a(1)-1, but in this version must leave residue unchanged...
              EXIT OUTER
            endif
          enddo
        enddo OUTER

!...form binary residue from the last few digits. The "umstaendliche Methode" (that's German for "kludge")
!   I use here is to force F90 to print the leading zeros, and also because the DEC (V4) compiler bugged out
!   when faced with MVBITS applied to 32 (rather than 64) bit integers.

        nbits=0
        nrem=0
        i=0
        j=17
        ndigit=min(8,n) !***This assumes littlewords contain at least 8 bits***!
        itmp(1:ndigit)=nint(a(0:ndigit-1,0))
        if(flag) itmp(1)=itmp(1)-1      !* ...so subtract the one here.

!...Set the number of bits in the least-significant 8 words.
        do i=1,ndigit
          bits(i)=bits0+ibits(base_index_word,i-1,1)
        enddo

        RES: do i=1,ndigit-1

!...First, convert least-significant few digits to positive-digit form...

          if(itmp(i)<0.and.i < ndigit) then; itmp(i)=itmp(i)+2**bits(i); itmp(i+1)=itmp(i+1)-1; endif
          if(itmp(i)<0.and.i ==ndigit) then; itmp(i)=itmp(i)+2**bits(i); endif
          if(nrem /= 0)then
            itmp(i)=ishft(itmp(i),nrem)+ishft(itmp(i-1),nrem-bits(i-1))
!*          call mvbits(itmp(i-1),bits(i)-nrem,nrem,itmp(i),0) !* DEC's compiler has a bug in this intrinsic applied to 4-byte integers
            bits(i)=bits(i)+nrem
          endif

!...now convert to hexadecimal form.

          do l=1,bits(i)-3,4
            j=j-1
            if(j == 0)EXIT RES
            hex_res64(j    :  j)=hex_chars(ibits(itmp(i),l-1,4))
            nbits=nbits+4
          enddo
          nrem=mod(bits(i),4)
        enddo RES
!...
        end function hex_res64

!***************

        subroutine mers_mod_square(a,base,baseinv,base_index,wt,n,pad,ilo,ihi,p,ofile,diagnose)

!...Subroutine to perform Mersenne-mod squaring using Crandall and Fagin's discrete weighted transform.
!
!   Calls FFT_SQUARE.
!
        implicit none
        integer, save :: nwarn=0,n2,n8,n8bit
        integer, intent(in) :: n,pad,p,base(0:1),diagnose
        integer :: i,ii,ilo,ihi,iter,j,k,l,nh,slot,word,row,col
        integer :: nbits,bits0
        integer :: iprintevery
        integer, allocatable, save :: index(:)
        integer, intent(in) :: base_index(0:(n-1)/32)
        real*8,  intent(in) :: baseinv(0:1),wt(n+1)
        real*8, intent(inout) :: a(0:n/8-1+pad,0:7)
        real*8, allocatable, save :: b(:,:)
        real*8 :: cy,check1r,check2r,check1i,check2i,frac,fracmax,temp
        real*8, parameter :: err_rel=1d-7,rnd_const = 0.75d0 * 2d0**53 !* this is for the fast NINT
        real*8, save :: err_abs
        character(*) :: ofile
        logical, save :: first_entry=.true.
        character*16, external :: hex_res64

!...initialize things upon first entry
        if(first_entry) then
          first_entry=.false.
          bits0=p/n
          n2=n/2
          n8=n/8; n8bit=nint(log(1d0*n8)/log(2d0))
          allocate(b(0:n/8-1+pad,0:7),index(n))
          err_abs=4*dnint(sqrt(1d0*n))

!...bit-reversal array is here.
!
!   We subtract 1 to make INDEX a convenient pointer to the 2-D zero-offset arrays A & B;
!   this greatly simplifies the conversion of a single integer bit-reversal datum
!   (the numbers stored in INDEX) to a row and a column number as needed by A & B.
!   For instance if A is (1024+pad) x 8, using zero-offset, the high element has 1-D index 8191,
!   or 1111111111111 in binary. Then COL = leftmost 3 bits = 7, ROW = rightmost 10 bits = 1023.
!
          do i=1,n
            index(i)=i-1
          enddo
          j=1
          do i=1,n,2
            if(j > i) then
              k=index(j)
              index(j)=index(i)
              index(i)=k
              k=index(j+1)
              index(j+1)=index(i+1)
              index(i+1)=k
            endif
            nh=n/2
            do
            if( nh < 2 .or. j < nh ) EXIT
              j=j-nh
              nh=nh/2
            enddo
            j=j+nh
          enddo
        endif                           

!...at start of each iteration cycle, initialize B-array by foward-weighting and bit-reversing A...
        if(ilo==1)then
          b=a   !* on first iteration, only A(1) nonzero, and WT(1)=INDEX(1)=1, so no weighting or BR needed.
        else
          k=0
          do i=0,7
            do j=0,n8-1
              k=k+1

              col=ibits(index(k),n8bit,3)       !* high    3 bits of INDEX(K) give the column
              row=ibits(index(k),0,n8bit)       !* low N8BIT bits of INDEX(K) give the row

              b(row,col)=a(j,i)*wt(k)
            enddo
          enddo
        endif

!...iteration loop is here. On all but the last iteration of the cycle, do forward weighting
!   and bit-reversal in the same loop with the carry propagation...

        do iter=ilo+1,ihi-1

!...perform the FFT-based squaring...

        call fft_square(b,a,index,n,pad,check1i)

!...and make a pass to do all the carries.

        check2r=0; check2i=0    !* sum of the outputs is initialized here.
        cy=0
        fracmax=0
        k=0

        do i=0,7
        do j=0,n8-1
          k=k+1

!...this is for the error checking
!*        temp = (a(j,i) + rnd_const) - rnd_const
          temp = dnint(a(j,i))
          frac = a(j,i)-temp
          check2i=check2i+temp
          check2r=check2r+frac

          a(j,i)=cy+a(j,i)*wt(n+2-k)*.5d0
!*        temp = (a(j,i) + rnd_const) - rnd_const
          temp = dnint(a(j,i))
          frac = a(j,i)-temp
          fracmax = max(abs(frac),fracmax)

!* If you suspect your machine/compiler isn't implementing the fast rounding properly, uncomment the next line.
!* If any of the results do differ, if it's only by one in the least-significant digit, it's OK (that's the IEEE standard at work.)
!*        if(temp/=dnint(a(j,i)))then; print'(a,2e25.15)','WARNING: results of RND_CONST and DNINT differ: ',temp,dnint(a(j,i)); PAUSE; endif

          word=ishft(k-1,-5)            !* word=(j-1)/32
          slot=k-ishft(word,+5)-1       !* slot=j-32*word-1
          ii=ibits(base_index(word),slot,1)

!*        cy = (temp*baseinv(ii)+rnd_const)-rnd_const
          cy = dnint(temp*baseinv(ii))
          l=index(k)                            !* Instead of doing  separately,
          col=ibits(l,n8bit,3)
          row=ibits(l,0,n8bit)
          b(row,col)=(temp-cy*base(ii))*wt(k)   !* interleave the fwd weighting and FFT bit reversal right here...
          a(j,i)=temp-cy*base(ii)		!* this puts a copy of the unweighted residue digit back into A...
        enddo
        enddo

!...Error checking is here.
        check2i = check2i+dnint(check2r)
        temp = abs(check1i-check2i)
        frac = temp/max(1d0,min(check1i,check2i))

        if(temp > err_abs .and. frac > err_rel)then
          open(unit=1,file=ofile,status='unknown',position='append')
      write(1,'(2(a,i8),2(a,f20.0),a,f5.0,a,e12.5)')'M',p,&
      ' Checksum warning on iteration',iter,'; Sum(in)=',check1i, &
      '; Sum(out)=',check2i,'; AbsErr=',temp,'; RelErr=',frac
          close(1)
        endif
        if(fracmax >= 0.4d0)then
          open(unit=1,file=ofile,status='unknown',position='append')

          write(*,'(a,i8,a,i8,a,f16.12)')'M',p,' Roundoff warning on iteration',iter,' maxerr =',fracmax

          write(1,'(a,i8,a,i8,a,f16.12)')'M',p,' Roundoff warning on iteration',iter,' maxerr =',fracmax
!...In range test mode, any fractional part > 0.45 is cause for termination.
          if(fracmax > 0.45 ) then

            write(*,*)'FATAL ERROR...Halting execution.'

            write(1,*)'FATAL ERROR...Halting execution.'
            close(1)
            STOP
          endif
          close(1)
        endif

!...make a final pass to fold any high-order carry element into the lowest-order digits
!   Assume j will get no larger than, say, 4, and thus fix WORD=0.

        k=0
        do
          if(cy==0)EXIT
          ii=ibits(base_index(0),k,1)
          k=k+1
!*print*,'word=',k,'inp carry=',cy
          l=index(k)
          col=ibits(l,n8bit,3)
          row=ibits(l,0,n8bit)
!*print*,'word=',k,'old digit=',b(row,col)*wt(n+2-k)*.5d0
          b(row,col)=b(row,col)*wt(n+2-k)*.5d0+cy
!*        cy = (b(row,col)*baseinv(ii) + rnd_const) - rnd_const
          cy = dnint(b(row,col)*baseinv(ii))
!*print*,'word=',k,'new digit=',b(row,col)-cy*base(ii)
          a(k-1,0)=b(row,col)-cy*base(ii)     !* this puts a copy of the unweighted residue digit back into A...
          b(row,col)=(b(row,col)-cy*base(ii))*wt(k)
        enddo
        b(0,0)=b(0,0)-2
	a(0,0)=a(0,0)-2

!* this prints the most-significant and 3 least-significant words of the residue, which can be useful for diagnostic purposes.

!* SPEC CPU2000: The following code is usually commented out in production.  Turn it on in a limited manner: we'll 
!* print about 50 of the iterations, and also print the initial 20.  That way if miscompares are reported we have 
!* a starting point for diagnosis.

	if(diagnose==1)then
	   iprintevery = ihi/50
	   if (mod(iter,iprintevery).eq.0  .or. iter.le.20) then
	     write(*,'(a,i10,a18)')'iteration=',iter,hex_res64(a,n,pad,p,nbits,bits0,base_index(0),iter)
!	     print'(a,i8,a)','iteration =',iter,' upper 2 and lower 4 residue digits are:'
!	     print'(4(a,f20.10))','digit * wtinv =',b(n8-1,7),' * ',wt(  2)*0.5d0,' = ',b(n8-1,7)*wt(  2)*0.5d0, 'Adigit=',a(n8-1,7)
!	     print'(4(a,f20.10))','digit * wtinv =',b(n8-2,7),' * ',wt(  3)*0.5d0,' = ',b(n8-2,7)*wt(  3)*0.5d0, 'Adigit=',a(n8-2,7)
!	     print*,'...'													
!	     print'(4(a,f20.10))','digit * wtinv =',b(1,4)   ,' * ',wt(n-2)*0.5d0,' = ',b(1,4)   *wt(n-2)*0.5d0, 'Adigit=',a(3,0)
!	     print'(4(a,f20.10))','digit * wtinv =',b(0,4)   ,' * ',wt(n-1)*0.5d0,' = ',b(0,4)   *wt(n-1)*0.5d0, 'Adigit=',a(2,0)
!	     print'(4(a,f20.10))','digit * wtinv =',b(1,0)   ,' * ',wt(n  )*0.5d0,' = ',b(1,0)   *wt(n  )*0.5d0, 'Adigit=',a(1,0)
!	     print'(4(a,f20.10))','digit * wtinv =',b(0,0)   ,' * ',wt(n+1)*0.5d0,' = ',b(0,0)   *wt(n+1)*0.5d0, 'Adigit=',a(0,0)
	   endif
!	PAUSE
	endif

        enddo


!...On last iteration, skip forward weighting and bit reversal, i.e. do carries and return result in A.

        iter=ihi

        call fft_square(b,a,index,n,pad,check1i)
        check2r=0; check2i=0
        cy=0
        fracmax=0
        k=0

        do i=0,7
        do j=0,n8-1
          k=k+1

!*        temp = (a(j,i) + rnd_const) - rnd_const
          temp = dnint(a(j,i))
          frac = a(j,i)-temp
          check2i=check2i+temp
          check2r=check2r+frac

          a(j,i)=cy+a(j,i)*wt(n+2-k)*.5d0
!*        temp = (a(j,i) + rnd_const) - rnd_const
          temp = dnint(a(j,i))
          frac = a(j,i)-temp
          fracmax = max(abs(frac),fracmax)

          word=ishft(k-1,-5)
          slot=k-ishft(word,+5)-1
          ii=ibits(base_index(word),slot,1)

!*        cy = (temp*baseinv(ii)+rnd_const)-rnd_const
          cy = dnint(temp*baseinv(ii))
          a(j,i)= temp-cy*base(ii)
        enddo
        enddo

!...Error checking is here.
        check2i = check2i+dnint(check2r)
        temp = abs(check1i-check2i)
        frac = temp/max(1d0,min(check1i,check2i))

        if(temp > err_abs .and. frac > err_rel)then
          open(unit=1,file=ofile,status='unknown',position='append')
       write(*,'(2(a,i8),2(a,f20.0),a,f5.0,a,e12.5)')'M',p,&
             ' Checksum warning on iteration',iter,'; Sum(in)=',check1i,&
             '; Sum(out)=',check2i,'; AbsErr=',temp,'; RelErr=',frac
      write(1,'(2(a,i8),2(a,f20.0),a,f5.0,a,e12.5)')'M',p,&
            ' Checksum warning on iteration',iter,'; Sum(in)=',check1i,&
            '; Sum(out)=',check2i,'; AbsErr=',temp,'; RelErr=',frac
          close(1)
        endif
        if(fracmax >= 0.4d0)then
          open(unit=1,file=ofile,status='unknown',position='append')

          write(*,'(a,i8,a,i8,a,f16.12)')'M',p,' Roundoff warning on iteration',iter,' maxerr =',fracmax

          write(1,'(a,i8,a,i8,a,f16.12)')'M',p,' Roundoff warning on iteration',iter,' maxerr =',fracmax
!...In range test mode, any fractional part > 0.45 is cause for termination.
          if(fracmax > 0.45 ) then

            write(*,*)'FATAL ERROR...Halting execution.'

            write(1,*)'FATAL ERROR...Halting execution.'
            close(1)
            STOP
          endif
          close(1)
        endif

!...make a final pass to fold any high-order carry element into the lowest-order digits.
!   Since j will get no larger than 2 or 3, assume WORD=0.

        k=0
        do
          if(cy==0)EXIT
          ii=ibits(base_index(0),k,1)
          a(k,0)=a(k,0)+cy
!*        cy  =(a(k,0)*baseinv(ii) + rnd_const) - rnd_const
          cy  =dnint(a(k,0)*baseinv(ii))
          a(k,0)= a(k,0)-cy*base(ii)
          k=k+1
        enddo
        a(0,0)=a(0,0)-2

!* this prints the most-significant and 3 least-significant words of the residue, which can be useful for diagnostic purposes.
!*      print'(a,i4,f25.15,a,3f25.15)','iteration =',iter,a(n8-1,7),'...',a(2,0),a(1,0),a(0,0)
!*      PAUSE

!...
        end subroutine mers_mod_square

!***************

        subroutine fft_square(b,a,index,n,pad,check1i)

!...One-pass combined fwdFFT/wrapper/squaring/wrapper/invFFT routine, specialized
!   to (real) FFT lengths 128, 256 and 512K.
!
!   On entry, B contains the (forward-weighted, bit-reversed) to be squared; the (ordered) result
!   is  returned in A. B is destroyed during the procedure.
!
!   This version dated 12/31/97.
!   
!...
        implicit none
        integer, intent(in) :: n,pad,index(n)
!* if your compiler does not support real(kind=r16), use real*8 and keep an eye out for error warnings
!*      integer, parameter :: r16=max(selected_real_kind(18,50),selected_real_kind(15,30))
!* for timing tests just use real*8
        integer, parameter :: r16=selected_real_kind(15,30)
        integer :: i,ilo,ihi,incr,j,j1,j2,j3,j4,k,k1,k2,k3,k4,l,m,mm,np3,row,col
        integer, save :: n2,n2bit,n4,n8,n8bit
        real*8, save :: isqrt2,n2inv
        real*8, intent(inout) :: a(0:n/8-1+pad,0:7),b(0:n/8-1+pad,0:7),check1i
        real*8 :: check1r,c0,s0,r1,r2,r3,i1,i2,i3,tt
        real*8 :: tr1,tr2,ti1,ti2,c1,s1,c2,s2,c3,s3
        real*8 :: t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16
        real*8, parameter :: rnd_const = 0.75d0 * 2d0**53 !* this assumes IEEE-compliant real*8
        real*8, allocatable, save :: w1(:,:),w2(:,:)
        real(kind=r16), save :: mt,one=1,two=2,one_fourth,pi,theta,theta2,theta4,mt2,mt4
        logical, save :: first_entry=.true.

!...initialize things upon first entry
        if(first_entry) then
          first_entry=.false.
          isqrt2=one/sqrt(two)
          n2inv=2d0/n
          one_fourth=one/4
          pi=acos(-one)
          n2=n/2; n2bit=nint(log(1d0*n2)/log(2d0))
          n4=n/4
          n8=n/8; n8bit=n2bit-2

!...This implementation allows only shorter FFT lengths which coincide with radix-8 loops, i.e. n=2^4,7,10,13,16

        if(mod(n2bit,3)/=0.and.n2bit<=14)then
          print*,'FATAL: smaller FFT lengths must be >=256 and radix-8 compatible, i.e. one of 2^10,13,16.'
          STOP
        endif

!...trig array for wrapper/squaring part is here
          allocate(w1(2,n4))
          pi=acos(-one)
          theta=pi/n4   !* = 4*pi/n
          do j=2,n4     !* [1+exp(4*pi*i*j/n)]/4, j=0, ... , n/4-1 are here
            mt=(j-1)*theta
            w1(1,j)=(cos(mt)+1)*one_fourth
            w1(2,j)= sin(mt)   *one_fourth
          enddo

!...FFT trig arrays are here: first, calculate the needed dimension...
          j=0
          mm=1
          do
          if( ishft(mm,+4) > n ) EXIT   !* n=2^16: exit when mm=2^15
            j=j+mm
            mm=8*mm
          enddo

        if(mod(n2bit,3)==0)then
          allocate(w2(2,3*j))
        else
          allocate(w2(2,3*j+n4))
        endif

!...then calculate the sincos data. First part is for the radix-8 loops.

          j=1
          mm=1
          do
          if( ishft(mm,+4) > n ) EXIT   !* For n>=16, need mm=1 data; n>=128, need mm=8; n>=1024 need mm=64
            theta =pi/mm                !* n>=8192 need mm=512; n>=65536 need mm=4096; n>=524288 need mm=32768
            theta2=theta /2
            theta4=theta2/2
            w2(1,j:j+2)=1d0
            w2(2,j:j+2)=0d0
            j=j+3
            do m=1,mm-1
              mt =m*theta
              mt2=m*theta2
              mt4=m*theta4
              w2(1,j  )=cos(mt)
              w2(2,j  )=sin(mt)
              w2(1,j+1)=cos(mt2)
              w2(2,j+1)=sin(mt2)
              w2(1,j+2)=cos(mt4)
              w2(2,j+2)=sin(mt4)
              j=j+3
            enddo
            mm=8*mm
          enddo

!...Now take care of the radix-2 and radix-4 wrappers.
!...log_2(n/2) == 1 (mod 3) sincos data are here...
          if(mod(n2bit,3)==1)then
            theta =pi/n4
            w2(1,j)=1d0
            w2(2,j)=0d0
            j=j+1
            do m=1,n4-1
              mt =m*theta
              w2(1,j)=cos(mt)
              w2(2,j)=sin(mt)
              j=j+1
            enddo
!...log_2(n/2) == 2 (mod 3) sincos data are here.
          elseif(mod(n2bit,3)==2)then
            theta =pi/n8
            theta2=theta/2
            w2(1,j:j+1)=1d0
            w2(2,j:j+1)=0d0
            j=j+2
            do m=1,n8-1
              mt =m*theta
              mt2=m*theta2
              w2(1,j  )=cos(mt)
              w2(2,j  )=sin(mt)
              w2(1,j+1)=cos(mt2)
              w2(2,j+1)=sin(mt2)
              j=j+2
            enddo
          endif
        endif

!***BEGIN forward transform***
!...Outer loop, which is executed log_2(n2)/3 times, is unrolled in this implementation.
!   We copy the bit-reversed vector B into A on the first pass pair. The pass numbering
!   reflects the passes which would be performed in a standard radix-2 algorithm.
!...Passes 1-3...

          incr=n
          ilo=0; ihi=incr-1
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here; puts are into 8 adjacent rows of the receiving array
              j1=ishft(i,-3)
!       the minimum FFT size of 256 (= 8 x 16) is required to ensure the 16 data to be fetched are in the same row.
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=t5
              ti1=t6
              tr2=t7
              ti2=t8
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              t7=t3+ti2
              t3=t3-ti2
              t8=t4-tr2
              t4=t4+tr2

              tr1=t13
              ti1=t14
              tr2=t15
              ti2=t16
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              t15=t11+ti2
              t11=t11-ti2
              t16=t12-tr2
              t12=t12+tr2
!       now combine the two half-transforms.
              a(j1  ,0)=t1+t9
              a(j1+1,0)=t2+t10
              a(j1  ,4)=t1-t9
              a(j1+1,4)=t2-t10

              tr1=(t11-t12)*isqrt2      !* mpy by (1+i)/sqrt(2) is here...
              ti1=(t11+t12)*isqrt2
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4+ti1
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4-ti1

              a(j1  ,2)=t5-t14      !* mpy by i is inlined here...
              a(j1+1,2)=t6+t13
              a(j1  ,6)=t5+t14
              a(j1+1,6)=t6-t13

              tr1=(t15+t16)*isqrt2      !* mpy by (1-i)/sqrt(2) is here...
              ti1=(t16-t15)*isqrt2
              a(j1  ,3)=t7-tr1      !* and get (i-1)/sqrt by flipping signs here.
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
        if(n2==8)GOTO 1
!...Passes 4-6...
        incr=ishft(incr,-3)
          j=4   !* 3*(1)+1
          ilo=0; ihi=incr-1
          do m=1,8
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =a(row   ,col)
              t2 =a(row+1 ,col)
              t3 =a(row+2 ,col)
              t4 =a(row+3 ,col)
              t5 =a(row+4 ,col)
              t6 =a(row+5 ,col)
              t7 =a(row+6 ,col)
              t8 =a(row+7 ,col)
              t9 =a(row+8 ,col)
              t10=a(row+9 ,col)
              t11=a(row+10,col)
              t12=a(row+11,col)
              t13=a(row+12,col)
              t14=a(row+13,col)
              t15=a(row+14,col)
              t16=a(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11-s1*t12
              t12=c1*t12+s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15-s1*t16
              t16=c1*t16+s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7-s2*t8
              ti1=c2*t8+s2*t7
              t7=t3+ti1
              t3=t3-ti1
              t8=t4-tr1
              t4=t4+tr1
              tr1=c2*t13-s2*t14
              ti1=c2*t14+s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15-s2*t16
              ti1=c2*t16+s2*t15
              t15=t11+ti1
              t11=t11-ti1
              t16=t12-tr1
              t12=t12+tr1
!       now combine the two half-transforms
              tr1=c3*t9 -s3*t10
              ti1=c3*t10+s3*t9
              b(j1  ,0)=t1+tr1
              b(j1+1,0)=t2+ti1
              b(j1  ,4)=t1-tr1
              b(j1+1,4)=t2-ti1

              tt =tr2*t11-ti2*t12
              ti1=tr2*t12+ti2*t11
              tr1=tt-ti1                !* mpy by (1+i)/sqrt(2) is here...
              ti1=tt+ti1
              b(j1  ,1)=t3+tr1
              b(j1+1,1)=t4+ti1
              b(j1  ,5)=t3-tr1
              b(j1+1,5)=t4-ti1

              tr1=c3*t13-s3*t14
              ti1=c3*t14+s3*t13
              b(j1  ,2)=t5-ti1      !* mpy by i is inlined here...
              b(j1+1,2)=t6+tr1
              b(j1  ,6)=t5+ti1
              b(j1+1,6)=t6-tr1

              tt =tr2*t15-ti2*t16
              ti1=tr2*t16+ti2*t15
              tr1=tt+ti1                !* mpy by (1-i)/sqrt(2) is here...
              ti1=ti1-tt
              b(j1  ,3)=t7-tr1      !* and get (i-1)/sqrt by flipping signs here.
              b(j1+1,3)=t8-ti1
              b(j1  ,7)=t7+tr1
              b(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==64)then; a=b; GOTO 1; endif
!...Passes 7-9...
        incr=ishft(incr,-3)
!*      j=28  !* 3*(1+8)+1
          ilo=0; ihi=incr-1
          do m=1,64
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11-s1*t12
              t12=c1*t12+s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15-s1*t16
              t16=c1*t16+s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7-s2*t8
              ti1=c2*t8+s2*t7
              t7=t3+ti1
              t3=t3-ti1
              t8=t4-tr1
              t4=t4+tr1
              tr1=c2*t13-s2*t14
              ti1=c2*t14+s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15-s2*t16
              ti1=c2*t16+s2*t15
              t15=t11+ti1
              t11=t11-ti1
              t16=t12-tr1
              t12=t12+tr1
!       now combine the two half-transforms
              tr1=c3*t9 -s3*t10
              ti1=c3*t10+s3*t9
              a(j1  ,0)=t1+tr1
              a(j1+1,0)=t2+ti1
              a(j1  ,4)=t1-tr1
              a(j1+1,4)=t2-ti1

              tt =tr2*t11-ti2*t12
              ti1=tr2*t12+ti2*t11
              tr1=tt-ti1                !* mpy by (1+i)/sqrt(2) is here...
              ti1=tt+ti1
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4+ti1
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4-ti1

              tr1=c3*t13-s3*t14
              ti1=c3*t14+s3*t13
              a(j1  ,2)=t5-ti1      !* mpy by i is inlined here...
              a(j1+1,2)=t6+tr1
              a(j1  ,6)=t5+ti1
              a(j1+1,6)=t6-tr1

              tt =tr2*t15-ti2*t16
              ti1=tr2*t16+ti2*t15
              tr1=tt+ti1                !* mpy by (1-i)/sqrt(2) is here...
              ti1=ti1-tt
              a(j1  ,3)=t7-tr1      !* and get (i-1)/sqrt by flipping signs here.
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==512)GOTO 1
!...Passes 10-12...
        incr=ishft(incr,-3)
!*        j=220 !* 3*(1+8+64)+1
          ilo=0; ihi=incr-1
          do m=1,512
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =a(row   ,col)
              t2 =a(row+1 ,col)
              t3 =a(row+2 ,col)
              t4 =a(row+3 ,col)
              t5 =a(row+4 ,col)
              t6 =a(row+5 ,col)
              t7 =a(row+6 ,col)
              t8 =a(row+7 ,col)
              t9 =a(row+8 ,col)
              t10=a(row+9 ,col)
              t11=a(row+10,col)
              t12=a(row+11,col)
              t13=a(row+12,col)
              t14=a(row+13,col)
              t15=a(row+14,col)
              t16=a(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11-s1*t12
              t12=c1*t12+s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15-s1*t16
              t16=c1*t16+s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7-s2*t8
              ti1=c2*t8+s2*t7
              t7=t3+ti1
              t3=t3-ti1
              t8=t4-tr1
              t4=t4+tr1
              tr1=c2*t13-s2*t14
              ti1=c2*t14+s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15-s2*t16
              ti1=c2*t16+s2*t15
              t15=t11+ti1
              t11=t11-ti1
              t16=t12-tr1
              t12=t12+tr1
!       now combine the two half-transforms
              tr1=c3*t9 -s3*t10
              ti1=c3*t10+s3*t9
              b(j1  ,0)=t1+tr1
              b(j1+1,0)=t2+ti1
              b(j1  ,4)=t1-tr1
              b(j1+1,4)=t2-ti1

              tt =tr2*t11-ti2*t12
              ti1=tr2*t12+ti2*t11
              tr1=tt-ti1                !* mpy by (1+i)/sqrt(2) is here...
              ti1=tt+ti1
              b(j1  ,1)=t3+tr1
              b(j1+1,1)=t4+ti1
              b(j1  ,5)=t3-tr1
              b(j1+1,5)=t4-ti1

              tr1=c3*t13-s3*t14
              ti1=c3*t14+s3*t13
              b(j1  ,2)=t5-ti1      !* mpy by i is inlined here...
              b(j1+1,2)=t6+tr1
              b(j1  ,6)=t5+ti1
              b(j1+1,6)=t6-tr1

              tt =tr2*t15-ti2*t16
              ti1=tr2*t16+ti2*t15
              tr1=tt+ti1                !* mpy by (1-i)/sqrt(2) is here...
              ti1=ti1-tt
              b(j1  ,3)=t7-tr1      !* and get (i-1)/sqrt by flipping signs here.
              b(j1+1,3)=t8-ti1
              b(j1  ,7)=t7+tr1
              b(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==4096)then; a=b; GOTO 1; endif
!...Passes 13-15...
        incr=ishft(incr,-3)
!*        j=1756        !* 3*(1+8+64+512)+1
          ilo=0; ihi=incr-1
          do m=1,4096
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11-s1*t12
              t12=c1*t12+s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15-s1*t16
              t16=c1*t16+s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7-s2*t8
              ti1=c2*t8+s2*t7
              t7=t3+ti1
              t3=t3-ti1
              t8=t4-tr1
              t4=t4+tr1
              tr1=c2*t13-s2*t14
              ti1=c2*t14+s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15-s2*t16
              ti1=c2*t16+s2*t15
              t15=t11+ti1
              t11=t11-ti1
              t16=t12-tr1
              t12=t12+tr1
!       now combine the two half-transforms
              tr1=c3*t9 -s3*t10
              ti1=c3*t10+s3*t9
              a(j1  ,0)=t1+tr1
              a(j1+1,0)=t2+ti1
              a(j1  ,4)=t1-tr1
              a(j1+1,4)=t2-ti1

              tt =tr2*t11-ti2*t12
              ti1=tr2*t12+ti2*t11
              tr1=tt-ti1                !* mpy by (1+i)/sqrt(2) is here...
              ti1=tt+ti1
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4+ti1
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4-ti1

              tr1=c3*t13-s3*t14
              ti1=c3*t14+s3*t13
              a(j1  ,2)=t5-ti1      !* mpy by i is inlined here...
              a(j1+1,2)=t6+tr1
              a(j1  ,6)=t5+ti1
              a(j1+1,6)=t6-tr1

              tt =tr2*t15-ti2*t16
              ti1=tr2*t16+ti2*t15
              tr1=tt+ti1                !* mpy by (1-i)/sqrt(2) is here...
              ti1=ti1-tt
              a(j1  ,3)=t7-tr1      !* and get (i-1)/sqrt by flipping signs here.
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==32768)GOTO 1
!...Pass 16...
        incr=ishft(incr,-3)
        if(n2==65536)then
          ilo=0; ihi=incr-1
          do m=1,32768
            c1=w2(1,j)
            s1=w2(2,j)
            do i=ilo,ihi,4
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-1)
              j2=ibits(ishft(i,-1),0,n8bit)     !* putcol=[i/2] mod (row length)
!       gather the needed data (2 64-bit complex, i.e. 4 64-bit reals)...
              t1=a(row   ,col)
              t2=a(row+1 ,col)
              t3=a(row+2 ,col)
              t4=a(row+3 ,col)
              tr1=c1*t3-s1*t4
              ti1=c1*t4+s1*t3
              b(j2  ,j1  )=t1+tr1
              b(j2+1,j1  )=t2+ti1
              b(j2  ,j1+4)=t1-tr1
              b(j2+1,j1+4)=t2-ti1
            enddo
            j=j+1
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          a=b
          GOTO 1
        endif
!...Passes 16 and 17...
        if(n2==131072)then
          ilo=0; ihi=incr-1
          do m=1,32768
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            do i=ilo,ihi,8
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-2)
              j2=ibits(ishft(i,-2),0,n8bit)     !* putcol=[i/4] mod (row length)
!       gather the needed data (4 64-bit complex, i.e. 8 64-bit reals)...
              t1=a(row   ,col)
              t2=a(row+1 ,col)
              t3=a(row+2 ,col)
              t4=a(row+3 ,col)
              t5=a(row+4 ,col)
              t6=a(row+5 ,col)
              t7=a(row+6 ,col)
              t8=a(row+7 ,col)
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              tr2=c2*t7-s2*t8
              ti2=c2*t8+s2*t7
              b(j2  ,j1  )=t1+tr1
              b(j2+1,j1  )=t2+ti1
              b(j2  ,j1+2)=t3-ti2
              b(j2+1,j1+2)=t4+tr2
              b(j2  ,j1+4)=t1-tr1
              b(j2+1,j1+4)=t2-ti1
              b(j2  ,j1+6)=t3+ti2
              b(j2+1,j1+6)=t4-tr2
            enddo
            j=j+2
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          a=b
          GOTO 1
        endif
!...Passes 16-18...
!*        j=14044       !* 3*(1+8+64+512+4096)+1
          ilo=0; ihi=incr-1
          do m=1,32768
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =a(row   ,col)
              t2 =a(row+1 ,col)
              t3 =a(row+2 ,col)
              t4 =a(row+3 ,col)
              t5 =a(row+4 ,col)
              t6 =a(row+5 ,col)
              t7 =a(row+6 ,col)
              t8 =a(row+7 ,col)
              t9 =a(row+8 ,col)
              t10=a(row+9 ,col)
              t11=a(row+10,col)
              t12=a(row+11,col)
              t13=a(row+12,col)
              t14=a(row+13,col)
              t15=a(row+14,col)
              t16=a(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11-s1*t12
              t12=c1*t12+s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15-s1*t16
              t16=c1*t16+s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7-s2*t8
              ti1=c2*t8+s2*t7
              t7=t3+ti1
              t3=t3-ti1
              t8=t4-tr1
              t4=t4+tr1
              tr1=c2*t13-s2*t14
              ti1=c2*t14+s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15-s2*t16
              ti1=c2*t16+s2*t15
              t15=t11+ti1
              t11=t11-ti1
              t16=t12-tr1
              t12=t12+tr1
!       now combine the two half-transforms
              tr1=c3*t9 -s3*t10
              ti1=c3*t10+s3*t9
              b(j1  ,0)=t1+tr1
              b(j1+1,0)=t2+ti1
              b(j1  ,4)=t1-tr1
              b(j1+1,4)=t2-ti1

              tt =tr2*t11-ti2*t12
              ti1=tr2*t12+ti2*t11
              tr1=tt-ti1                !* mpy by (1+i)/sqrt(2) is here...
              ti1=tt+ti1
              b(j1  ,1)=t3+tr1
              b(j1+1,1)=t4+ti1
              b(j1  ,5)=t3-tr1
              b(j1+1,5)=t4-ti1

              tr1=c3*t13-s3*t14
              ti1=c3*t14+s3*t13
              b(j1  ,2)=t5-ti1      !* mpy by i is inlined here...
              b(j1+1,2)=t6+tr1
              b(j1  ,6)=t5+ti1
              b(j1+1,6)=t6-tr1

              tt =tr2*t15-ti2*t16
              ti1=tr2*t16+ti2*t15
              tr1=tt+ti1                !* mpy by (1-i)/sqrt(2) is here...
              ti1=ti1-tt
              b(j1  ,3)=t7-tr1      !* and get (i-1)/sqrt by flipping signs here.
              b(j1+1,3)=t8-ti1
              b(j1  ,7)=t7+tr1
              b(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==262144)then; a=b; GOTO 1; endif
!...Pass 19...
        incr=ishft(incr,-3)
        if(n2==524288)then
          ilo=0; ihi=incr-1
          do m=1,262144
            c1=w2(1,j)
            s1=w2(2,j)
            do i=ilo,ihi,4
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-1)
              j2=ibits(ishft(i,-1),0,n8bit)     !* putcol=[i/2] mod (row length)
!       gather the needed data (2 64-bit complex, i.e. 4 64-bit reals)...
              t1=b(row   ,col)
              t2=b(row+1 ,col)
              t3=b(row+2 ,col)
              t4=b(row+3 ,col)
              tr1=c1*t3-s1*t4
              ti1=c1*t4+s1*t3
              a(j2  ,j1  )=t1+tr1
              a(j2+1,j1  )=t2+ti1
              a(j2  ,j1+4)=t1-tr1
              a(j2+1,j1+4)=t2-ti1
            enddo
            j=j+1
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          GOTO 1
        endif
!...Passes 19 and 20...
        if(n2==1048576)then
          ilo=0; ihi=incr-1
          do m=1,262144
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            do i=ilo,ihi,8
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-2)
              j2=ibits(ishft(i,-2),0,n8bit)     !* putcol=[i/4] mod (row length)
!       gather the needed data (4 64-bit complex, i.e. 8 64-bit reals)...
              t1=b(row   ,col)
              t2=b(row+1 ,col)
              t3=b(row+2 ,col)
              t4=b(row+3 ,col)
              t5=b(row+4 ,col)
              t6=b(row+5 ,col)
              t7=b(row+6 ,col)
              t8=b(row+7 ,col)
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              tr2=c2*t7-s2*t8
              ti2=c2*t8+s2*t7
              a(j2  ,j1  )=t1+tr1
              a(j2+1,j1  )=t2+ti1
              a(j2  ,j1+2)=t3-ti2
              a(j2+1,j1+2)=t4+tr2
              a(j2  ,j1+4)=t1-tr1
              a(j2+1,j1+4)=t2-ti1
              a(j2  ,j1+6)=t3+ti2
              a(j2+1,j1+6)=t4-tr2
            enddo
            j=j+2
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          GOTO 1
        endif
!...Passes 19-21...
!*        j=112348       !* 3*(1+8+64+512+4096+32768)+1
          ilo=0; ihi=incr-1
          do m=1,262144
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3-s1*t4
              t4=c1*t4+s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7-s1*t8
              t8=c1*t8+s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11-s1*t12
              t12=c1*t12+s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15-s1*t16
              t16=c1*t16+s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5-s2*t6
              ti1=c2*t6+s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7-s2*t8
              ti1=c2*t8+s2*t7
              t7=t3+ti1
              t3=t3-ti1
              t8=t4-tr1
              t4=t4+tr1
              tr1=c2*t13-s2*t14
              ti1=c2*t14+s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15-s2*t16
              ti1=c2*t16+s2*t15
              t15=t11+ti1
              t11=t11-ti1
              t16=t12-tr1
              t12=t12+tr1
!       now combine the two half-transforms
              tr1=c3*t9 -s3*t10
              ti1=c3*t10+s3*t9
              a(j1  ,0)=t1+tr1
              a(j1+1,0)=t2+ti1
              a(j1  ,4)=t1-tr1
              a(j1+1,4)=t2-ti1

              tt =tr2*t11-ti2*t12
              ti1=tr2*t12+ti2*t11
              tr1=tt-ti1                !* mpy by (1+i)/sqrt(2) is here...
              ti1=tt+ti1
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4+ti1
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4-ti1

              tr1=c3*t13-s3*t14
              ti1=c3*t14+s3*t13
              a(j1  ,2)=t5-ti1      !* mpy by i is inlined here...
              a(j1+1,2)=t6+tr1
              a(j1  ,6)=t5+ti1
              a(j1+1,6)=t6-tr1

              tt =tr2*t15-ti2*t16
              ti1=tr2*t16+ti2*t15
              tr1=tt+ti1                !* mpy by (1-i)/sqrt(2) is here...
              ti1=ti1-tt
              a(j1  ,3)=t7-tr1      !* and get (i-1)/sqrt by flipping signs here.
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==2097152)GOTO 1
!...
        print*,'FFT length not a power of 2 or out of range'
        STOP
!***END forward transform***

1       continue
!***BEGIN wrapper/squaring portion***
!
!   Quite a few operations are eliminated via use of algebraic identities in
!   the derivation of the squaring formula: the old version takes
!   forward (complex) FFT terms H[1...N/2], uses the following formula
!   to generate the terms F[1...N/2] of the true DFT:
!   
! F[j] = {( H[j]+H~[N/2-j] ) - I*exp(+2*pi*I*j/N)*( H[j]+H~[N/2-j] )}/2,
!   
!       j=0, ... , N/2-1
!   
!   (where ~ denotes the complex conjugate), then does a pointwise squaring
!   to obtain terms G[1...N/2], and does a similar complex wrapper prior to
!   entering the inverse FFT:
!   
!   I[j] = {( G[j]+G~[N/2-j] ) + I*exp(-2*pi*I*j/N)*( G[j]-G~[N/2-j] )}/2,
!   
!       j=0, ... , N/2-1.
!   
!   By combining the 3 steps into one (and doing some algebra), one
!   can get directly from the H's to the I's via
!   
!   I[j] = H[j]^2 + {1 + exp(4*pi*I*j/N)}*{H[j]-H~[N/2-j]}^2/4,
!   
!       j=0, ... , N/2-1,
!   
!   and of course there are lots of symmetries in the calculation of
!   I[j] and I[j-n/2] which can be (and are) exploited.
!
!   Note that the implementation uses unit-offset vectors, i.e. j=1, ...
!   
!...this is for the error checking.
        tt=a(0,0)+a(1,0)
!*      check1i =(tt + rnd_const) - rnd_const   !* square of the sum of the inputs is here...
        check1i =dnint(tt)
        check1r = tt-check1i
!*      check1r =( (2*check1i+check1r)*check1r + rnd_const) - rnd_const
        check1r =dnint((2*check1i+check1r)*check1r)
        check1i=check1i**2+check1r

!...unscrambling of data returned by forward transform, pointwise squaring and rescrambling
!   prior to inverse transform are performed in one fell swoop (or better, one swell loop :)

!...j=0 (for which I(0)=Re{H(0)}^2+i*Im{H(0)}^2) is done separately.
        tt=a(0,0)
        a(0,0)=(tt+a(1,0))**2*n2inv
        a(1,0)=(tt-a(1,0))**2*n2inv
        tt=a(0,0)
        b(0,0)=0.5d0*(tt+a(1,0))
        b(1,0)=0.5d0*(tt-a(1,0))

!...unscrambling of data returned by forward transform, pointwise squaring and rescrambling
!   prior to inverse transform are performed in one fell swoop (or better, one fell loop :)
        np3=n+3
        do j=2,n4

!...fetch indices:
          j1=ishft(j,1)
          j3=np3-j1
          j1=j1-1
!...put indices.
          k1=index(j1)
          k3=index(j3)
!...INDEX is unit-offset, but A is zero-offset...
          j3=j3-1
          j1=j1-1
!...after this segment, j1,3 store cols of 2 fetched data, j2,4 store rows (of real datum of each complex pair)
          j2=ibits(j1,0,n8bit)
          j1=ibits(j1,n8bit,3)
          j4=ibits(j3,0,n8bit)
          j3=ibits(j3,n8bit,3)

!...after this segment, k1,3 store cols of 2 put data, k2,4 store put rows (of real datum of each complex pair)
          k2=ibits(k1,0,n8bit)
          k1=ibits(k1,n8bit,3)
          k4=ibits(k3,0,n8bit)
          k3=ibits(k3,n8bit,3)

!...gather the 2 complex elements which are to be combined...
          r1=a(j2  ,j1)      !* Re{H(j)}
          i1=a(j2+1,j1)      !* Im{H(j)}
          r2=a(j4  ,j3)      !* Re{H(n2-j)}
          i2=a(j4+1,j3)      !* Im{H(n2-j)}

!...define temporaries needed for the squaring...
          c0=w1(1,j)    !* (cos+1)/4
          s0=w1(2,j)    !* sin/4

!       calculate cross-product terms...
          r3=r1*r2+i1*i2; r3=r3+r3      !* r3 := Re{2*H(j)*H~(n2-j)}
          i3=i1*r2-r1*i2; i3=i3+i3      !* i3 := Im{2*H(j)*H~(n2-j)}

!       now calculate square terms and store back in the same temporaries
          tt=(r1+i1)*(r1-i1); i1=r1*i1; i1=i1+i1; r1=tt !* (r1,i1) := H(j)^2
          tt=(r2+i2)*(r2-i2); i2=r2*i2; i2=i2+i2; r2=tt !* (r2,i2) := H(n2-j)^2

!       use that (H[j] - H~[N/2-j])^2 = H(j)^2 - 2*H(j)*H~(n2-j) + H~(n2-j)^2
          r3=r1+r2-r3
          i3=i1-i2-i3

          tt=c0*r3-s0*i3        !* Re{(1 + exp(4*pi*I*j/N)) * (H[j] - H~[N/2-j])^2/4}
          i3=s0*r3+c0*i3        !* Im{(1 + exp(4*pi*I*j/N)) * (H[j] - H~[N/2-j])^2/4}

!...and now complete and store the results.
          b(k2  ,k1)= (r1-tt)*n2inv  !* Re{I(j)}
          b(k2+1,k1)= (i1-i3)*n2inv  !* Im{I(j)}
!...n2-j terms are as above, but with the replacements: r1<-->r2, i1<-->i2, i3|-->-i3.
          b(k4  ,k3)= (r2-tt)*n2inv  !* Re{I(n2-j)}
          b(k4+1,k3)= (i2+i3)*n2inv  !* Im{I(n2-j)}
        enddo

!...j=n/4 (for which I(j)=H(j)^2) is done separately.
        r1=a(0,4); i1=a(1,4)
        tt=(r1+i1)*(r1-i1); i1=r1*i1
        b(2,0)=    tt *n2inv
        b(3,0)=(i1+i1)*n2inv

!***END wrapper/squaring portion***

!***BEGIN inverse transform***
!...Outer loop, which is executed log_2(n2)/3 times, is unrolled in this implementation.
!   We copy the bit-reversed vector B into A on the first pass pair. The pass numbering
!   reflects the passes which would be performed in a standard radix-2 algorithm.
!...Passes 1-3...
          incr=n
          ilo=0; ihi=incr-1
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=t5
              ti1=t6
              tr2=t7
              ti2=t8
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              t7=t3-ti2 !
              t3=t3+ti2 !
              t8=t4+tr2 !
              t4=t4-tr2 !
                        
              tr1=t13
              ti1=t14
              tr2=t15
              ti2=t16
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              t15=t11-ti2       !
              t11=t11+ti2       !
              t16=t12+tr2       !
              t12=t12-tr2       !
!       now combine the two half-transforms.
              a(j1  ,0)=t1+t9
              a(j1+1,0)=t2+t10
              a(j1  ,4)=t1-t9
              a(j1+1,4)=t2-t10

              tr1=(t11+t12)*isqrt2      !
              ti1=(t11-t12)*isqrt2      !
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4-ti1    !
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4+ti1    !

              a(j1  ,2)=t5+t14    !
              a(j1+1,2)=t6-t13    !
              a(j1  ,6)=t5-t14    !
              a(j1+1,6)=t6+t13    !

              tr1=(t15-t16)*isqrt2      !
              ti1=(t16+t15)*isqrt2      !
              a(j1  ,3)=t7-tr1
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
        if(n2==8)RETURN
!...Passes 4-6...
        incr=ishft(incr,-3)
          j=4   !* 3*(1)+1
          ilo=0; ihi=incr-1
          do m=1,8
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =a(row   ,col)
              t2 =a(row+1 ,col)
              t3 =a(row+2 ,col)
              t4 =a(row+3 ,col)
              t5 =a(row+4 ,col)
              t6 =a(row+5 ,col)
              t7 =a(row+6 ,col)
              t8 =a(row+7 ,col)
              t9 =a(row+8 ,col)
              t10=a(row+9 ,col)
              t11=a(row+10,col)
              t12=a(row+11,col)
              t13=a(row+12,col)
              t14=a(row+13,col)
              t15=a(row+14,col)
              t16=a(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11+s1*t12
              t12=c1*t12-s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15+s1*t16
              t16=c1*t16-s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7+s2*t8
              ti1=c2*t8-s2*t7
              t7=t3-ti1
              t3=t3+ti1
              t8=t4+tr1
              t4=t4-tr1
              tr1=c2*t13+s2*t14
              ti1=c2*t14-s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15+s2*t16
              ti1=c2*t16-s2*t15
              t15=t11-ti1
              t11=t11+ti1
              t16=t12+tr1
              t12=t12-tr1
!       now combine the two half-transforms
              tr1=c3*t9 +s3*t10
              ti1=c3*t10-s3*t9
              b(j1  ,0)=t1+tr1
              b(j1+1,0)=t2+ti1
              b(j1  ,4)=t1-tr1
              b(j1+1,4)=t2-ti1

              tt =tr2*t11+ti2*t12
              ti1=tr2*t12-ti2*t11
              tr1=tt+ti1
              ti1=tt-ti1
              b(j1  ,1)=t3+tr1
              b(j1+1,1)=t4-ti1
              b(j1  ,5)=t3-tr1
              b(j1+1,5)=t4+ti1

              tr1=c3*t13+s3*t14
              ti1=c3*t14-s3*t13
              b(j1  ,2)=t5+ti1
              b(j1+1,2)=t6-tr1
              b(j1  ,6)=t5-ti1
              b(j1+1,6)=t6+tr1

              tt =tr2*t15+ti2*t16
              ti1=tr2*t16-ti2*t15
              tr1=tt-ti1
              ti1=ti1+tt
              b(j1  ,3)=t7-tr1
              b(j1+1,3)=t8-ti1
              b(j1  ,7)=t7+tr1
              b(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==64)then; a=b; RETURN; endif
!...Passes 7-9...
        incr=ishft(incr,-3)
!*        j=28  !* 3*(1+8)+1
          ilo=0; ihi=incr-1
          do m=1,64
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11+s1*t12
              t12=c1*t12-s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15+s1*t16
              t16=c1*t16-s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7+s2*t8
              ti1=c2*t8-s2*t7
              t7=t3-ti1
              t3=t3+ti1
              t8=t4+tr1
              t4=t4-tr1
              tr1=c2*t13+s2*t14
              ti1=c2*t14-s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15+s2*t16
              ti1=c2*t16-s2*t15
              t15=t11-ti1
              t11=t11+ti1
              t16=t12+tr1
              t12=t12-tr1
!       now combine the two half-transforms
              tr1=c3*t9 +s3*t10
              ti1=c3*t10-s3*t9
              a(j1  ,0)=t1+tr1
              a(j1+1,0)=t2+ti1
              a(j1  ,4)=t1-tr1
              a(j1+1,4)=t2-ti1

              tt =tr2*t11+ti2*t12
              ti1=tr2*t12-ti2*t11
              tr1=tt+ti1
              ti1=tt-ti1
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4-ti1
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4+ti1

              tr1=c3*t13+s3*t14
              ti1=c3*t14-s3*t13
              a(j1  ,2)=t5+ti1
              a(j1+1,2)=t6-tr1
              a(j1  ,6)=t5-ti1
              a(j1+1,6)=t6+tr1

              tt =tr2*t15+ti2*t16
              ti1=tr2*t16-ti2*t15
              tr1=tt-ti1
              ti1=ti1+tt
              a(j1  ,3)=t7-tr1
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==512)RETURN
!...Passes 10-12...
        incr=ishft(incr,-3)
!*        j=220 !* 3*(1+8+64)+1
          ilo=0; ihi=incr-1
          do m=1,512
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =a(row   ,col)
              t2 =a(row+1 ,col)
              t3 =a(row+2 ,col)
              t4 =a(row+3 ,col)
              t5 =a(row+4 ,col)
              t6 =a(row+5 ,col)
              t7 =a(row+6 ,col)
              t8 =a(row+7 ,col)
              t9 =a(row+8 ,col)
              t10=a(row+9 ,col)
              t11=a(row+10,col)
              t12=a(row+11,col)
              t13=a(row+12,col)
              t14=a(row+13,col)
              t15=a(row+14,col)
              t16=a(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11+s1*t12
              t12=c1*t12-s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15+s1*t16
              t16=c1*t16-s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7+s2*t8
              ti1=c2*t8-s2*t7
              t7=t3-ti1
              t3=t3+ti1
              t8=t4+tr1
              t4=t4-tr1
              tr1=c2*t13+s2*t14
              ti1=c2*t14-s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15+s2*t16
              ti1=c2*t16-s2*t15
              t15=t11-ti1
              t11=t11+ti1
              t16=t12+tr1
              t12=t12-tr1
!       now combine the two half-transforms
              tr1=c3*t9 +s3*t10
              ti1=c3*t10-s3*t9
              b(j1  ,0)=t1+tr1
              b(j1+1,0)=t2+ti1
              b(j1  ,4)=t1-tr1
              b(j1+1,4)=t2-ti1

              tt =tr2*t11+ti2*t12
              ti1=tr2*t12-ti2*t11
              tr1=tt+ti1
              ti1=tt-ti1
              b(j1  ,1)=t3+tr1
              b(j1+1,1)=t4-ti1
              b(j1  ,5)=t3-tr1
              b(j1+1,5)=t4+ti1

              tr1=c3*t13+s3*t14
              ti1=c3*t14-s3*t13
              b(j1  ,2)=t5+ti1
              b(j1+1,2)=t6-tr1
              b(j1  ,6)=t5-ti1
              b(j1+1,6)=t6+tr1

              tt =tr2*t15+ti2*t16
              ti1=tr2*t16-ti2*t15
              tr1=tt-ti1
              ti1=ti1+tt
              b(j1  ,3)=t7-tr1
              b(j1+1,3)=t8-ti1
              b(j1  ,7)=t7+tr1
              b(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==4096)then; a=b; RETURN; endif
!...Passes 13-15...
        incr=ishft(incr,-3)
!*        j=1756        !* 3*(1+8+64+512)+1
          ilo=0; ihi=incr-1
          do m=1,4096
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11+s1*t12
              t12=c1*t12-s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15+s1*t16
              t16=c1*t16-s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7+s2*t8
              ti1=c2*t8-s2*t7
              t7=t3-ti1
              t3=t3+ti1
              t8=t4+tr1
              t4=t4-tr1
              tr1=c2*t13+s2*t14
              ti1=c2*t14-s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15+s2*t16
              ti1=c2*t16-s2*t15
              t15=t11-ti1
              t11=t11+ti1
              t16=t12+tr1
              t12=t12-tr1
!       now combine the two half-transforms
              tr1=c3*t9 +s3*t10
              ti1=c3*t10-s3*t9
              a(j1  ,0)=t1+tr1
              a(j1+1,0)=t2+ti1
              a(j1  ,4)=t1-tr1
              a(j1+1,4)=t2-ti1

              tt =tr2*t11+ti2*t12
              ti1=tr2*t12-ti2*t11
              tr1=tt+ti1
              ti1=tt-ti1
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4-ti1
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4+ti1

              tr1=c3*t13+s3*t14
              ti1=c3*t14-s3*t13
              a(j1  ,2)=t5+ti1
              a(j1+1,2)=t6-tr1
              a(j1  ,6)=t5-ti1
              a(j1+1,6)=t6+tr1

              tt =tr2*t15+ti2*t16
              ti1=tr2*t16-ti2*t15
              tr1=tt-ti1
              ti1=ti1+tt
              a(j1  ,3)=t7-tr1
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==32768)RETURN
!...Pass 16...
        incr=ishft(incr,-3)
        if(n2==65536)then
          ilo=0; ihi=incr-1
          do m=1,32768
            c1=w2(1,j)
            s1=w2(2,j)
            do i=ilo,ihi,4
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-1)
              j2=ibits(ishft(i,-1),0,n8bit)     !* putcol=[i/2] mod (row length)
!       gather the needed data (2 64-bit complex, i.e. 4 64-bit reals)...
              t1=a(row   ,col)
              t2=a(row+1 ,col)
              t3=a(row+2 ,col)
              t4=a(row+3 ,col)
              tr1=c1*t3+s1*t4
              ti1=c1*t4-s1*t3
              b(j2  ,j1  )=t1+tr1
              b(j2+1,j1  )=t2+ti1
              b(j2  ,j1+4)=t1-tr1
              b(j2+1,j1+4)=t2-ti1
            enddo
            j=j+1
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          a=b
          RETURN
        endif
!...Passes 16 and 17...
        if(n2==131072)then
          ilo=0; ihi=incr-1
          do m=1,32768
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            do i=ilo,ihi,8
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-2)
              j2=ibits(ishft(i,-2),0,n8bit)     !* putcol=[i/4] mod (row length)
!       gather the needed data (4 64-bit complex, i.e. 8 64-bit reals)...
              t1=a(row   ,col)
              t2=a(row+1 ,col)
              t3=a(row+2 ,col)
              t4=a(row+3 ,col)
              t5=a(row+4 ,col)
              t6=a(row+5 ,col)
              t7=a(row+6 ,col)
              t8=a(row+7 ,col)
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              tr2=c2*t7+s2*t8
              ti2=c2*t8-s2*t7
              b(j2  ,j1  )=t1+tr1
              b(j2+1,j1  )=t2+ti1
              b(j2  ,j1+2)=t3+ti2
              b(j2+1,j1+2)=t4-tr2
              b(j2  ,j1+4)=t1-tr1
              b(j2+1,j1+4)=t2-ti1
              b(j2  ,j1+6)=t3-ti2
              b(j2+1,j1+6)=t4+tr2
            enddo
            j=j+2
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          a=b
          RETURN
        endif
!...Passes 16-18...
!*        j=14044       !* 3*(1+8+64+512+4096)+1
          ilo=0; ihi=incr-1
          do m=1,32768
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =a(row   ,col)
              t2 =a(row+1 ,col)
              t3 =a(row+2 ,col)
              t4 =a(row+3 ,col)
              t5 =a(row+4 ,col)
              t6 =a(row+5 ,col)
              t7 =a(row+6 ,col)
              t8 =a(row+7 ,col)
              t9 =a(row+8 ,col)
              t10=a(row+9 ,col)
              t11=a(row+10,col)
              t12=a(row+11,col)
              t13=a(row+12,col)
              t14=a(row+13,col)
              t15=a(row+14,col)
              t16=a(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11+s1*t12
              t12=c1*t12-s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15+s1*t16
              t16=c1*t16-s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7+s2*t8
              ti1=c2*t8-s2*t7
              t7=t3-ti1
              t3=t3+ti1
              t8=t4+tr1
              t4=t4-tr1
              tr1=c2*t13+s2*t14
              ti1=c2*t14-s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15+s2*t16
              ti1=c2*t16-s2*t15
              t15=t11-ti1
              t11=t11+ti1
              t16=t12+tr1
              t12=t12-tr1
!       now combine the two half-transforms
              tr1=c3*t9 +s3*t10
              ti1=c3*t10-s3*t9
              b(j1  ,0)=t1+tr1
              b(j1+1,0)=t2+ti1
              b(j1  ,4)=t1-tr1
              b(j1+1,4)=t2-ti1

              tt =tr2*t11+ti2*t12
              ti1=tr2*t12-ti2*t11
              tr1=tt+ti1
              ti1=tt-ti1
              b(j1  ,1)=t3+tr1
              b(j1+1,1)=t4-ti1
              b(j1  ,5)=t3-tr1
              b(j1+1,5)=t4+ti1

              tr1=c3*t13+s3*t14
              ti1=c3*t14-s3*t13
              b(j1  ,2)=t5+ti1
              b(j1+1,2)=t6-tr1
              b(j1  ,6)=t5-ti1
              b(j1+1,6)=t6+tr1

              tt =tr2*t15+ti2*t16
              ti1=tr2*t16-ti2*t15
              tr1=tt-ti1
              ti1=ti1+tt
              b(j1  ,3)=t7-tr1
              b(j1+1,3)=t8-ti1
              b(j1  ,7)=t7+tr1
              b(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==262144)then; a=b; RETURN; endif
!...Pass 19...
        incr=ishft(incr,-3)
        if(n2==524288)then
          ilo=0; ihi=incr-1
          do m=1,262144
            c1=w2(1,j)
            s1=w2(2,j)
            do i=ilo,ihi,4
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-1)
              j2=ibits(ishft(i,-1),0,n8bit)     !* putcol=[i/2] mod (row length)
!       gather the needed data (2 64-bit complex, i.e. 4 64-bit reals)...
              t1=b(row   ,col)
              t2=b(row+1 ,col)
              t3=b(row+2 ,col)
              t4=b(row+3 ,col)
              tr1=c1*t3+s1*t4
              ti1=c1*t4-s1*t3
              a(j2  ,j1  )=t1+tr1
              a(j2+1,j1  )=t2+ti1
              a(j2  ,j1+4)=t1-tr1
              a(j2+1,j1+4)=t2-ti1
            enddo
            j=j+1
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          RETURN
        endif
!...Passes 19 and 20...
        if(n2==1048576)then
          ilo=0; ihi=incr-1
          do m=1,262144
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            do i=ilo,ihi,8
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       row and column indices of fetch are here
              j1=ishft(col,-2)
              j2=ibits(ishft(i,-2),0,n8bit)     !* putcol=[i/4] mod (row length)
!       gather the needed data (4 64-bit complex, i.e. 8 64-bit reals)...
              t1=b(row   ,col)
              t2=b(row+1 ,col)
              t3=b(row+2 ,col)
              t4=b(row+3 ,col)
              t5=b(row+4 ,col)
              t6=b(row+5 ,col)
              t7=b(row+6 ,col)
              t8=b(row+7 ,col)
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              tr2=c2*t7+s2*t8
              ti2=c2*t8-s2*t7
              a(j2  ,j1  )=t1+tr1
              a(j2+1,j1  )=t2+ti1
              a(j2  ,j1+2)=t3+ti2
              a(j2+1,j1+2)=t4-tr2
              a(j2  ,j1+4)=t1-tr1
              a(j2+1,j1+4)=t2-ti1
              a(j2  ,j1+6)=t3-ti2
              a(j2+1,j1+6)=t4+tr2
            enddo
            j=j+2
            ilo=ilo+incr; ihi=ihi+incr
          enddo
          RETURN
        endif
!...Passes 19-21...
!*        j=112348       !* 3*(1+8+64+512+4096+32768)+1
          ilo=0; ihi=incr-1
          do m=1,262144
            c1=w2(1,j  )
            s1=w2(2,j  )
            c2=w2(1,j+1)
            s2=w2(2,j+1)
            c3=w2(1,j+2)
            s3=w2(2,j+2)
            tr2=c3*isqrt2
            ti2=s3*isqrt2
            do i=ilo,ihi,16
!       row and column indices of fetch are here
              col=ibits(i,n8bit,3)
              row=ibits(i,0,n8bit)
!       column index of put is here
              j1=ishft(i,-3)
!       gather the needed data (8 64-bit complex, i.e. 16 64-bit reals)...
              t1 =b(row   ,col)
              t2 =b(row+1 ,col)
              t3 =b(row+2 ,col)
              t4 =b(row+3 ,col)
              t5 =b(row+4 ,col)
              t6 =b(row+5 ,col)
              t7 =b(row+6 ,col)
              t8 =b(row+7 ,col)
              t9 =b(row+8 ,col)
              t10=b(row+9 ,col)
              t11=b(row+10,col)
              t12=b(row+11,col)
              t13=b(row+12,col)
              t14=b(row+13,col)
              t15=b(row+14,col)
              t16=b(row+15,col)
!       first get the 4 length-2 transforms...
              tt=c1*t3+s1*t4
              t4=c1*t4-s1*t3
              t3=t1-tt
              t1=t1+tt
              tt=t4
              t4=t2-tt
              t2=t2+tt
              tt=c1*t7+s1*t8
              t8=c1*t8-s1*t7
              t7=t5-tt
              t5=t5+tt
              tt=t8
              t8=t6-tt
              t6=t6+tt
              tt =c1*t11+s1*t12
              t12=c1*t12-s1*t11
              t11=t9 -tt
              t9 =t9 +tt
              tt =t12
              t12=t10-tt
              t10=t10+tt
              tt =c1*t15+s1*t16
              t16=c1*t16-s1*t15
              t15=t13-tt
              t13=t13+tt
              tt =t16
              t16=t14-tt
              t14=t14+tt
!       combine to get the 2 length-4 transform...
              tr1=c2*t5+s2*t6
              ti1=c2*t6-s2*t5
              t5=t1-tr1
              t1=t1+tr1
              t6=t2-ti1
              t2=t2+ti1
              tr1=c2*t7+s2*t8
              ti1=c2*t8-s2*t7
              t7=t3-ti1
              t3=t3+ti1
              t8=t4+tr1
              t4=t4-tr1
              tr1=c2*t13+s2*t14
              ti1=c2*t14-s2*t13
              t13=t9 -tr1
              t9 =t9 +tr1
              t14=t10-ti1
              t10=t10+ti1
              tr1=c2*t15+s2*t16
              ti1=c2*t16-s2*t15
              t15=t11-ti1
              t11=t11+ti1
              t16=t12+tr1
              t12=t12-tr1
!       now combine the two half-transforms
              tr1=c3*t9 +s3*t10
              ti1=c3*t10-s3*t9
              a(j1  ,0)=t1+tr1
              a(j1+1,0)=t2+ti1
              a(j1  ,4)=t1-tr1
              a(j1+1,4)=t2-ti1

              tt =tr2*t11+ti2*t12
              ti1=tr2*t12-ti2*t11
              tr1=tt+ti1
              ti1=tt-ti1
              a(j1  ,1)=t3+tr1
              a(j1+1,1)=t4-ti1
              a(j1  ,5)=t3-tr1
              a(j1+1,5)=t4+ti1

              tr1=c3*t13+s3*t14
              ti1=c3*t14-s3*t13
              a(j1  ,2)=t5+ti1
              a(j1+1,2)=t6-tr1
              a(j1  ,6)=t5-ti1
              a(j1+1,6)=t6+tr1

              tt =tr2*t15+ti2*t16
              ti1=tr2*t16-ti2*t15
              tr1=tt-ti1
              ti1=ti1+tt
              a(j1  ,3)=t7-tr1
              a(j1+1,3)=t8-ti1
              a(j1  ,7)=t7+tr1
              a(j1+1,7)=t8+ti1
            enddo
            j=j+3
            ilo=ilo+incr; ihi=ihi+incr
          enddo
        if(n2==2097152)RETURN
!...
        print*,'FFT length not a power of 2 or out of range'
        STOP
!***END inverse transform***

!...
        end subroutine fft_square

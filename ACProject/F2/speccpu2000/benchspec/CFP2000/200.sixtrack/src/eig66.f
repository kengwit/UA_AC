********************************************************************
C      
C ANFANG UNTERPROGRAMM
      subroutine eig66(fm,reval,aieval,revec,aievec)
      implicit real*8 (a-h,o-z)
      parameter(pieni=1d-17)
      parameter(zero=0.0d0,half=5.0d-1,one=1.0d0)
      parameter(halfm=-5.0d-1,onem=-1.0d0)
      parameter(two=2.0d0,three=3.0d0,four=4.0d0)
      parameter(c1e3=1.0d3,c2e3=2.0d3,c4e3=4.0d3,c1e4=1.0d4)
      parameter(c1e12=1.0d12,c1e13=1.0d13,c1e15=1.0d15,c1e16=1.0d16)
      parameter(c180e0=180.0d0,c1e6=1.0d6)
      parameter(c1m1=1.0d-1,c1m3=1.0d-3,c1m6=1.0d-6,c1m7=1.0d-7)
      parameter(c1m8=1.0d-8,c1m9=1.0d-9,c1m10=1.0d-10)
      parameter(c1m12=1.0d-12,c1m13=1.0d-13,c1m14=1.0d-14)
      parameter(c1m15=1.0d-15,c1m17=1.0d-17,c1m18=1.0d-18)
      parameter(c1m21=1.0d-21,c1m24=1.0d-24,c1m38=1.0d-38)
      parameter(five=5.0d0,six=6.0d0,seven=7.0d0,eight=8.0d0)
      parameter(nine=9.0d0,ten=10.0d0)
      parameter(c24e0=24.0d0,c120e0=120.0d0,c16e0=16.0d0,c40e0=40.0d0)
      parameter(c80e0=80.0d0,c72e0=72.0d0)
      parameter(c12e0=12.0d0,c32e0=32.0d0,c48e0=48.0d0,c160e0=160.0d0)
      parameter(pmap=938.2723128d0,pmae=.5109990615d0)
      parameter(crade=2.8179409238d-15)
C  THIS ROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C  OF THE FULL MATRIX FM.
C  THE EIGENVECTORS ARE NORMALIZED SO THAT THE REAL AND
C  IMAGINARY PART OF VECTORS 1, 3, AND 5 HAVE +1 ANTISYMMETRIC
C  PRODUCT:
C      REVEC1 J AIVEC1 = 1 ; REVEC3 J AIVEC3 = 1 ;
C      REVEC5 J AIVEC5 = 1.
C  THE EIGENVECTORS 2 ,4, AND 6 HAVE THE OPPOSITE NORMALIZATION.
C  WRITTEN BY F. NERI, FEB 26 1986.
C      
      parameter (ndim2=6)
      integer nn
      integer ilo,ihi,mdim,info
C     integer nnr
      real*8 reval(ndim2),aieval(ndim2),
     @revec(ndim2,ndim2),aievec(ndim2,ndim2)
      real*8 fm(ndim2,ndim2),aa(ndim2,ndim2)
      integer i,i1
C    &            ,j
      real*8 ort(ndim2),vv(ndim2,ndim2)
C     real*8 pbkt(ndim2)
      save
C  COPY MATRIX TO TEMPORARY STORAGE (THE MATRIX AA IS DESTROYED)
      do 10 i=1,ndim2
        do 10 i1=1,ndim2
          aa(i1,i) = fm(i1,i)
   10 continue
      ilo = 1
      ihi = ndim2
      mdim = ndim2
      nn = ndim2
C  COMPUTE EIGENVALUES AND EIGENVECTORS USING DOUBLE
C  PRECISION EISPACK ROUTINES:
      call ety(mdim,nn,ilo,ihi,aa,ort)
      call etyt(mdim,nn,ilo,ihi,aa,ort,vv)
      call ety2(mdim,nn,ilo,ihi,aa,reval,aieval,vv,info)
      if ( info .ne. 0 ) then
        write(6,*) '  ERROR IN EIG6'
        return
      endif
C      CALL NEIGV(VV,PBKT)
      do 30 i=1,ndim2/2
        do 20 jet=1,ndim2
          revec(jet,2*i-1)=vv(jet,2*i-1)
          revec(jet,2*i)=vv(jet,2*i-1)
          aievec(jet,2*i-1)=vv(jet,2*i)
   20   aievec(jet,2*i)=-vv(jet,2*i)
   30 continue
      do 40 i=1,ndim2
        if(abs(reval(i)**2+aieval(i)**2 -one).gt.c1m10) then
          write(6,*) ' EIG6: EIGENVALUES OFF THE UNIT CIRCLE!'
          return
        endif
   40 continue
      return
      end

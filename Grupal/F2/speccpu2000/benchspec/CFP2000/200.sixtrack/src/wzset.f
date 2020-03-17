C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine wzset
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
      parameter ( xcut=7.77d0, ycut=7.46d0 )
      parameter ( h=1.d0/63.d0 )
      parameter ( nx=490, ny=470 )
      parameter ( idin = (nx+2)*(ny+2) )
      parameter ( half=0.5d0, one=1.d0 )
      common /wzcom1/ hrecip, kstep
      common /wzcom2/ wtreal(idin), wtimag(idin)
      save
c
c  *********************************************************************
c
c  This subroutine must be called before subroutine WZSUB can be used to
c  compute values of the complex error function w(z).
c
c  Parameters xcut and ycut specify the opposite corners (xcut,0) and
c  (0,ycut) of the rectangle inside which interpolation is to be used
c  by subroutine WZSUB.
c
c  Parameter h is the side of the squares of the interpolation grid.
c
c  Parameters nx and ny must be set to the nearest integers to xcut/h
c  and ycut/h respectively (or to larger values).
c
c  Calls MYWWERF new version of (CERN library) WWERF (C335)
c
c  (G.A.Erskine, 29.09.1995)
c
c  *********************************************************************
c
c  Start.
      hrecip = 1.d0/h
      kstep = nx+2
      k = 0
      do 2 j=0,ny+1
         do 1 i=0,nx+1
            k = k+1
            call mywwerf( i*h,j*h,wr,wi)
            wtreal(k) = wr
            wtimag(k) = wi
 1       continue
 2    continue
      end

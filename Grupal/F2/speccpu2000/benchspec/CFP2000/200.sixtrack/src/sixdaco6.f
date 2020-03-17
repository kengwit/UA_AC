C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine sixdaco6(isixda)
C-----------------------------------------------------------------------
C  INITIALIZATION FOR 6-DIMENSIONAL CLOSED ORBIT
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
      character*16 coel
C     character*16 bez,bezb,bezr,erbez,bezl
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor),
     &idalloc
      common/norf/nordf,nvarf,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax
      common/tcorrc/coel(10)
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
      save
      write(6,*) ' ENTERING INITIALIZATION OF DA FOR 6-D CO '
      call daini(nord,nvar,0)
C     write(6,*) ' NO,NV = ',nord,nvar
      write(6,9912) nord,nvar
9912  format(' NO,NV = ',I8,I8)
      call daall(iscrda,100,'$$IS      ',nord,nvar)
      call daeps(preda)
      if(isixda.eq.1) call clor6
      if(isixda.eq.2) call umlau6
      call dadal(iscrda,100)
      return
      end

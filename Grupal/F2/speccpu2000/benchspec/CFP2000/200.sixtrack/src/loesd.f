C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine loesd (rmat, vec,dimakt,dimtot,kod)
C-----------------------------------------------------------------------
C  SOLUTION OF A SYSTEM OF LINEAR EQUATIONS
C  VEC1 = VEC2 * RMAT , WITH VEC2 AS RESULT
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
      parameter(pieni=1d-17)
      parameter(zero=0.0d0,half=0.5d0,one=1.0d0)
      parameter(two=2.0d0,three=3.0d0,four=4.0d0)
      parameter(c1e1=1.0d1,c1e2=1.0d2,c1m2=1.0d-2)
      parameter(c1e3=1.0d3,c2e3=2.0d3,c4e3=4.0d3,c1e4=1.0d4)
      parameter(c1e12=1.0d12,c1e13=1.0d13,c1e15=1.0d15,c1e16=1.0d16)
      parameter(c180e0=180.0d0,c1e6=1.0d6)
      parameter(c1m1=1.0d-1,c1m3=1.0d-3,c1m6=1.0d-6,c1m9=1.0d-9)
      parameter(c1m10=1.0d-10,c1m12=1.0d-12,c1m13=1.0d-13)
      parameter(c1m15=1.0d-15)
      parameter(c1m18=1.0d-18,c1m21=1.0d-21,c1m24=1.0d-24)
      parameter(c1m38=1.0d-38)
      parameter(pmap=938.2723128d0,pmae=.5109990615d0)
      parameter(crade=2.8179409238d-15)
      integer dimtot,dimakt
      dimension rmat(dimtot,dimakt),vec(dimakt)
      save
      data eps /1.d-20/
      kod=1
      do 50 j=1,dimakt
        emax=zero
        do 10 ik=j,dimakt
          if(abs(emax).gt.abs(rmat(j,ik)) .or.emax.ne.emax) goto 10
          emax=rmat(j,ik)
          indi=ik
   10   continue
        if(abs(emax).lt.eps) then
          write(6,*) '  ****   ERROR IN LOESD   **** '
          return
        endif
   20   do 30 l=j,dimakt
          r=rmat(l,j)
          rmat(l,j)=rmat(l,indi)
          rmat(l,indi)=r
          rmat(l,j)=rmat(l,j)/emax
   30   continue
        r=vec(indi)
        vec(indi)=vec(j)
        vec(j)=r/emax
        if(j.eq.dimakt) goto 60
        jy=j+1
        do 50 jk=jy,dimakt
          r=rmat(j,jk)
          do 40 kk=jy,dimakt
            rmat(kk,jk)= rmat(kk,jk)-r*rmat(kk,j)
   40     continue
          vec(jk)=vec(jk)-vec(j)*r
   50 continue
   60 n=dimakt
      n1=dimakt-1
      do 70 j=1,n1
        do 70 k=1,j
          vec(n-j)=vec(n-j)-rmat(n-k+1,n-j)*vec(n-k+1)
   70 continue
      kod = 0
      return
      end

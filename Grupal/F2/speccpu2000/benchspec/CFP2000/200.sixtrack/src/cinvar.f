C ANFANG UNTERPROGRAMM
C---------------------------------------------------------------------
      subroutine cinvar(a,b,c,d,j,e,xinv,invx)
C---------------------------------------------------------------------
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
      common/invari/ dani(ninv+1)
      dimension xinv(ninv),invx(ninv)
      save
C---------------------------------------------------------------------
      if(abs(a).le.b) then
        do 10 i=1,ninv
          if((c.ge.zero.and.c.ge.dani(i).and.c.lt.dani(i+1)).or. (c.lt.
     +    zero.and.d.ge.dani(i).and.d.lt.dani(i+1))) then
            j=j+1
            if(abs(xinv(i)).le.pieni) then
              xinv(i)=e
              invx(i)=1
            else
              xinv(i)=xinv(i)+e
              invx(i)=invx(i)+1
            endif
          endif
   10   continue
      endif
      return
      end

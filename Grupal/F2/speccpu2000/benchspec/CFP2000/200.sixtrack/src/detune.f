C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine detune(iv,ekk,ep,beta,dtu,dtup,dfac)
C  USED FOR SUBRE - CALCULATES DETUNING
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
      dimension dfac(10),dtu(2,5),ep(2),beta(2),dtup(2,5,0:4,0:4)
      save
      if(iv.lt.2) then
        print*
        print*,'       ***** ERROR IN DETUNE *****'
        print*
        print*,'       IV LESS THAN 2, NO DETUNING POSSIBLE'
        print*
        return
      endif
      pi=four*atan(one)
      iv2=2*iv
      iv3=iv+1
      vtu1=-ekk*(half**iv2)*dfac(iv2)/pi
      dtu1=zero
      dtu2=zero
      do 10 iv4=1,iv3
        iv5=iv4-1
        iv6=iv-iv5
        vor=one
        if(mod(iv6,2).ne.0) vor=-one
        vtu2=vor/(dfac(iv5+1)**2)/(dfac(iv6+1)**2)*(beta(1)**iv5)* (beta
     +  (2)**iv6)
        if(iv5.ne.0) then
          dtu1=dtu1+vtu2*iv5*(ep(1)**(iv5-1))*(ep(2)**iv6)
          dtup(1,iv,iv5-1,iv6)=dtup(1,iv,iv5-1,iv6)+vtu2*iv5*vtu1
        endif
        if(iv6.ne.0) then
          dtu2=dtu2+vtu2*iv6*(ep(1)**iv5)*(ep(2)**(iv6-1))
          dtup(2,iv,iv5,iv6-1)=dtup(2,iv,iv5,iv6-1)+vtu2*iv6*vtu1
        endif
   10 continue
      dtu(1,iv)=dtu(1,iv)+vtu1*dtu1
      dtu(2,iv)=dtu(2,iv)+vtu1*dtu2
      return
      end

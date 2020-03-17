C-----------------------------------------------------------------------
      subroutine distance(x,clo,di0,t,dam)
C-----------------------------------------------------------------------
C  CALCULATION OF DISTANCE IN PHASE SPACE FOR POST-PROCESSING
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
      common/cor/dpscor,sigcor,icode,idam,its6d
      dimension x(2,6),x1(2,6),clo(6),di0(4),t(6,6),phi(3)
      save
C-----------------------------------------------------------------------
      pi=four*atan(one)
      if(icode.ge.4.and.its6d.eq.0) then
        do 10 i=1,2
          do 10 j=1,4
            x(i,j)=x(i,j)-di0(j)*x(i,6)
   10   continue
      endif
      do 60 i=1,2
        do 20 j=1,6
          x(i,j)=x(i,j)-clo(j)
   20   continue
        if(its6d.eq.1) then
          x(i,2)=x(i,2)/(one+x(i,6)+clo(6))
          x(i,4)=x(i,4)/(one+x(i,6)+clo(6))
        endif
        do 40 iq=1,6
          x1(i,iq)=zero
          do 30 jq=1,6
            x1(i,iq)=x1(i,iq)+t(jq,iq)*x(i,jq)
   30     continue
   40   continue
        do 50 j=1,6
          x(i,j)=x1(i,j)
   50   continue
   60 continue
      do 70 i=1,2
        x(i,5)=x(i,5)*sigcor
        x(i,6)=x(i,6)*dpscor
   70 continue
      do 80 i=1,3
        ii=2*i
        sx=x(2,ii-1)*x(1,ii)-x(1,ii-1)*x(2,ii)
        cx=x(1,ii-1)*x(2,ii-1)+x(1,ii)*x(2,ii)
        if(abs(sx).gt.pieni.or.abs(cx).gt.pieni) then
          phi(i)=atan2(sx,cx)
        else
          phi(i)=zero
        endif
   80 continue
      dam=sqrt((phi(1)*phi(1)+phi(2)*phi(2)+phi(3)*phi(3))/idam)/pi
C-----------------------------------------------------------------------
      return
      end

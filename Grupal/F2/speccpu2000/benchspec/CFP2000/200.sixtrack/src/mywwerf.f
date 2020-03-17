      subroutine mywwerf(zr,zi,wr,wi)
      implicit real*8 (a-h,o-z)
 
      parameter (z1 = 1d0, hf = z1/2d0, z10 = 10d0)
      parameter (c1 = 74d0/z10, c2 = 83d0/z10, 
     +           c3 = z10/32d0, c4 = 16d0/z10)
      parameter (c = 1.12837 91670 95512 57d0, p = (2d0*c4)**33)
      dimension rr(37),ri(37)
      save
 
      x=zr
      y=zi
      xa=abs(x)
      ya=abs(y)
      if(ya .lt. c1 .and. xa .lt. c2) then
        zhr=ya+c4
        zhi=xa
        rr(37)=0d0
        ri(37)=0d0
        do 1 n = 36,1,-1
C         t=zh+n*dconjg(r(n+1))
          tr=zhr+n*rr(n+1)
          ti=zhi-n*ri(n+1)
C         r(n)=hf*t/(dreal(t)**2+dimag(t)**2)
          rr(n)=hf*tr/(tr**2+ti**2)
          ri(n)=hf*ti/(tr**2+ti**2)
    1   continue
        xl=p
        sr=0d0
        si=0d0
        do 2 n = 33,1,-1
          xl=c3*xl
C         s=r(n)*(s+xl)
C         s=(rr(n),ri(n))*(sr+xl,si+xl)
          sr=(rr(n)*(sr+xl)+ri(n)*(si+xl))/((sr+xl)**2+(si+xl)**2)
          si=(ri(n)*(sr+xl)-rr(n)*(si+xl))/((sr+xl)**2+(si+xl)**2)
    2   continue
        vr=c*sr
        vi=c*si
      else
        zhr=ya
        zhi=xa
        rr(1)=0d0
        ri(1)=0d0
        do 3 n = 9,1,-1
C         t=zh+n*dconjg(r(1))
          tr=zhr+n*rr(1) 
          ti=zhi-n*ri(1)
C         r(1)=hf*t/(dreal(t)**2+dimag(t)**2)
          rr(1)=hf*tr/(tr**2+ti**2)
          ri(1)=hf*ti/(tr**2+ti**2)
    3   continue
        vr=c*rr(1)
        vi=c*ri(1)
      endif
      if(ya .eq. 0) then
        vr=exp(-xa**2)
      elseif(y .lt. 0) then
C       v=2*exp(-dcmplx(xa,ya)**2)-v
        vr=2*exp(xa)*cos(ya)-vr
        vi=2*exp(xa)*sin(ya)-vi
        if(x .gt. 0 .or. x .lt.0 ) vi=-vi
      endif
      wr=vr
      wi=vi
      return
      end

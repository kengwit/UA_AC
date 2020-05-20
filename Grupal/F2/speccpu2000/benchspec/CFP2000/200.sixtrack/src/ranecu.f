C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine ranecu(rvec,len,mcut)
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      real*8 rvec0,rvec,pi,r
      real*8 pieni
      real*8 zero,half,one
      real*8 two,three,four
      real*8 c1e1,c1e2,c1m2
      real*8 c1e3,c2e3,c4e3,c1e4
      real*8 c1e12,c1e13,c1e15,c1e16
      real*8 c180e0,c1e6
      real*8 c1m1,c1m3,c1m6,c1m9
      real*8 c1m10,c1m12,c1m13
      real*8 c1m15
      real*8 c1m18,c1m21,c1m24
      real*8 c1m38
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
      dimension rvec(*),r(2)
      save
      data iseed1,iseed2 / 12345, 67890 /
      pi = four*atan(one)
C     DO 100 I = 1,LEN
      i=1
   10 do 20 j = 1,2
        k = iseed1/53668
        iseed1 = 40014*(iseed1-k*53668) - k*12211
        if (iseed1.lt.0) iseed1 = iseed1+2147483563
        k = iseed2/52774
        iseed2 = 40692*(iseed2-k*52774) - k*3791
        if (iseed2.lt.0) iseed2 = iseed2+2147483399
        iz = iseed1-iseed2
        if (iz.lt.1) iz = iz+2147483562
        r(j) = iz
        r(j) = r(j)*4.656613d-10
   20 continue
      rvec0 = ((-two*log(r(1)))**half)*cos(two*pi*r(2))
      if(dabs(rvec0).le.mcut.or.mcut.eq.0) then
        rvec(i) = rvec0
        i=i+1
      endif
      if(i.le.len) goto 10
C     RVEC(I) = ((-TWO*LOG(R(1)))**HALF)*COS(TWO*PI*R(2))
C 100 CONTINUE
      return
 
      entry recuin(is1,is2)
      iseed1 = is1
      iseed2 = is2
      return
 
      entry recuut(is1,is2)
      is1 = iseed1
      is2 = iseed2
      return
      end

C ANFANG UNTERPROGRAMM
C---------------------------------------------------------------------
      subroutine fft(ar,ai,m,n)
C---------------------------------------------------------------------
C      A(N) IS A COMPLEX ARRAY. THE INPUT IS A(N)=(X(N),0.0), WHERE
C      X(N) IS THE SEQUENCE TO FOURIER ANALYSE.
C      N=2**M.
C      THIS ROUTINE ONLY WORKS FOR N EQUAL TO A POWER OF TWO.
C      AFTER RETURN A(N)=(..,..) CONTAINS THE COEFFICIENTS OF THE FFT.
C      TO COMPUTE POWER SPECTRA DO   ...=ABS(A(N))
C
C      WRITTEN BY : RUI DILAO
C
C---------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
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
      dimension ar(n),ai(n)
      save
      n=2**m
      nv2=n/2
      nm1=n-1
      j=1
      do 30 i=1,nm1
        if(i.gt.j)goto 10
        tr=ar(j)
        ti=ai(j)
        ar(j)=ar(i)
        ai(j)=ai(i)
        ar(i)=tr
        ai(i)=ti
   10   k=nv2
   20   if(k.ge.j)goto 30
        j=j-k
        k=k/2
        goto 20
   30 j=j+k
      pi=four*atan(one)
      do 50 l=1,m
        le=2**l
        le1=le/2
        ur=one
        ui=zero
        wr=cos(pi/le1)
        wi=-sin(pi/le1)
        do 50 j=1,le1
          do 40 i=j,n,le
            ip=i+le1
C           t=a(ip)*u
C           t=(ar(ip),ai(ip))*(ur,ui)
            tr=ar(ip)*ur-ai(ip)*ui            
            ti=ar(ip)*ui+ai(ip)*ur            
            ar(ip)=ar(i)-tr
            ai(ip)=ai(i)-ti
            ar(i)=ar(i)+tr
            ai(i)=ai(i)+ti
   40     continue
C  50   u=u*w
C       u=(ur,ui)*(wr,wi)
        uur=ur*wr-ui*wi
        ui=ur*wi+ui*wr
        ur=uur
   50 continue
      do 60 i=1,n
        ar(i)=ar(i)/n*2
        ai(i)=ai(i)/n*2
   60 continue
      return
      end

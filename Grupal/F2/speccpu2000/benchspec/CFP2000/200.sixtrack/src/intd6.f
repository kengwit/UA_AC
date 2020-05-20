C ANFANG UNTERPROGRAMM
      subroutine intd6(v,h,x)
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
      parameter (ndim=3)
      parameter (nt=12)
      common /ii/no,nv
      external dlie6
*DAE{ (NO,NV) V(NT),X(NT),H                                             *FOX
*DAI{ (NO,NV) B1,B2,B3,B4
      integer v(nt),x(nt),h
      integer b1,b2,b3,b4
*      
      integer          iscrda
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
*      
      parameter
     *(lx 2 = 1 +1 +1 +1 )
      save
      data    b1,b2,b3,b4/ lx  2 * 0 /
*      
      call daall(v,nt,'V         ',no,nv)                               *FOX
      call daall(x,nt,'X         ',no,nv)                               *FOX
      call daall(h,1,'H         ',no,nv)
      call daall(b1,1,'B1        ',no,nv)
      call daall(b2,1,'B2        ',no,nv)
      call daall(b3,1,'B3        ',no,nv)
      call daall(b4,1,'B4        ',no,nv)
*DA }  
      call dacon(b4,zero)
      call dacon(h,zero)
      do 10 i=1,ndim
        call dacfu(v(2*i-1),dlie6,b3)
        call dacfu(v(2*i),dlie6,b1)
        call damul(b1,x(2*i-1),b2)
        call damul(b3,x(2*i),b1)
        call dasub(b2,b1,b3)
        call daadd(b3,b4,b2)
        call dacop(b2,b4)
   10 continue
      call dacop(b4,h)
      call dadal(b4,1)
      call dadal(b3,1)
      call dadal(b2,1)
      call dadal(b1,1)
      return
      end
C ANFANG UNTERPROGRAMM
      subroutine ctrbasi6(hc,hr,ctr,cjg)
      implicit real*8 (a-h,o-z)
      parameter(pieni=1d-17)
      parameter(zero=0.0d0,half=5.0d-1,one=1.0d0)
      parameter(halfm=-5.0d-1,onem=-1.0d0)
      parameter(two=2.0d0,three=3.0d0,four=4.0d0)
      parameter(c1e3=1.0d3,c2e3=2.0d3,c4e3=4.0d3,c1e4=1.0d4)
      parameter(c1e12=1.0d12,c1e13=1.0d13,c1e15=1.0d15,c1e16=1.0d16)
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
      parameter(isz=200)
*****************************************************************
* FROM CARTESIAN TO RESONANCE BASIS : HC IS TRANSFORMED INTO HR *
*  HR SITS IN IRWIN SPACE                                       *
*****************************************************************
      parameter (ndim=3)
      parameter (nt=12)
      common /ii/no,nv
*DAE{ (NO,NV) HR,HC,CTR(NT),CJG(NT)
*DAI{ (NO,NV) B1,B2
      integer hr,hc,ctr(nt),cjg(nt)
      integer b1,b2
*      
      integer          iscrda
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
*      
      parameter
     *(lx 2 = 1 +1 )
      save
      data    b1,b2/ lx  2 * 0 /
*      
      call daall(hr,1,'hr        ',no,nv)
      call daall(hc,1,'hc        ',no,nv)
      call daall(ctr,nt,'ctr       ',no,nv)
      call daall(cjg,nt,'cjg       ',no,nv)
      call daall(b1,1,'b1        ',no,nv)
      call daall(b2,1,'b2        ',no,nv)
*DA }  
* GOING INTO IRWIN SPACE
      call cpart6(hc,b1)
* HR = B1 O CTR ; GOING INTO THE RESONANCE BASIS INSIDE IRWIN SPACE
      call trx6(b1,hr,ctr)
      call trx6(hr,hc,cjg)
      call dalin(hr,half,hc,half,b1)
      call dalin(hr,half,hc,halfm,b2)
      call dacop(b1,hc)
      call dacop(b2,hr)
      call dadal(b2,1)
      call dadal(b1,1)
      return
      end

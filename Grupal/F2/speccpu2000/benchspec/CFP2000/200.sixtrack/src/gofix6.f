C ANFANG UNTERPROGRAMM
      subroutine gofix6(xy,x,v,w,rel,a1,a1i,nord)
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
*****************************************************************
* X = A1 XY A1I WHERE X IS TO THE FIXED POINT TO ORDER NORD     *
*****************************************************************
      parameter (ndim=3)
      parameter (ndim2=6)
      parameter (nt=12)
      common /ii/no,nv
*DAE{ (NO,NV) XY(NT),X(NT),W(NT),V(NT),REL(NT),A1(NT),A1I(NT)           *FOX
      integer xy(nt),x(nt),w(nt),v(nt),rel(nt),a1(nt),a1i(nt)
*      
      integer          iscrda
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
      save
*      
*      
      call daall(xy,nt,'XY        ',no,nv)
      call daall(x,nt,'X         ',no,nv)                               *FOX
      call daall(w,nt,'W         ',no,nv)
      call daall(v,nt,'V         ',no,nv)                               *FOX
      call daall(rel,nt,'REL       ',no,nv)
      call daall(a1,nt,'A1        ',no,nv)
      call daall(a1i,nt,'A1I       ',no,nv)
*DA }  
* COMPUTATION OF A1 AND A1I USING DAINV
      call danot(nord)
      call dacopd6(xy,x)
      call dalind6(x,one,rel,onem,v)
      call dainv(v,nv,w,nv)
      call dacond6(x,zero)
      call dacct(w,ndim2,x,nv,v,ndim2)
      call dalind6(rel,one,v,one,a1)
      call dalind6(rel,one,v,onem,a1i)
      call danot(no)
* X = A1I O XY O A1
      call dacct(a1i,ndim2,xy,nv,v,ndim2)
      call dacct(v,ndim2,a1,nv,x,ndim2)
      return
      end

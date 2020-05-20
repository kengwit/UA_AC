C ANFANG UNTERPROGRAMM
      subroutine creat6
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
      parameter (ntt=20)
      parameter (nsuper=1)
      common /nom/ bb2,rot,roti,ctr,cjg,rtc,bb1,rel
      common /vecteur/v,w,x,xy,h,ft,a2,a2i,cm,a1,a1i
      common /tune/ angle(ndim),abm(-ntt:ntt,-ntt:ntt,-ntt:ntt)
     @,aam(-ntt:ntt,-ntt:ntt,-ntt:ntt),tue(-ntt:ntt,-ntt:ntt,-ntt:ntt)
      common /super/hs,fs
      integer j(20)
      common /ii/no,nv
*DAE{ (NO,NV) BB1,BB2,ROT(NT),ROTI(NT),REL(NT),FS
*DAE{ (NO,NV) CTR(NT),CJG(NT),RTC(NT),A2(NT),A2I(NT),CM(NT),A1(NT)
*DAE{ (NO,NV) V(NT),W(NT),X(NT),XY(NT),H,FT,A1I(NT),HS 
      integer bb1,bb2,rot(nt),roti(nt),rel(nt),fs
      integer ctr(nt),cjg(nt),rtc(nt),a2(nt),a2i(nt),cm(nt),a1(nt)
      integer v(nt),w(nt),x(nt),xy(nt),h,ft,a1i(nt),hs
*      
      integer          iscrda
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
      save
*      
*      
      call daall(bb1,1,'BB1       ',no,nv)
      call daall(bb2,1,'BB2       ',no,nv)
      call daall(rot,nt,'ROT       ',no,nv)
      call daall(roti,nt,'ROTI      ',no,nv)
      call daall(rel,nt,'REL       ',no,nv)
      call daall(fs,1,'FS        ',no,nv)
      call daall(ctr,nt,'CTR       ',no,nv)
      call daall(cjg,nt,'CJG       ',no,nv)
      call daall(rtc,nt,'RTC       ',no,nv)
      call daall(a2,nt,'A2        ',no,nv)
      call daall(a2i,nt,'A2I       ',no,nv)
      call daall(cm,nt,'CM        ',no,nv)
      call daall(a1,nt,'A1        ',no,nv)
      call daall(v,nt,'V         ',no,nv)
      call daall(w,nt,'W         ',no,nv)
      call daall(x,nt,'X         ',no,nv)
      call daall(xy,nt,'XY        ',no,nv)
      call daall(h,1,'H         ',no,nv)
      call daall(ft,1,'FT        ',no,nv)
      call daall(a1i,nt,'A1I       ',no,nv)
      call daall(hs,1,'HS        ',no,nv)
*DA }  
       
      do 10 i=1,nv
        j(i)=0
        call davar(rel(i),zero,i)
        call dacop(rel(i),cjg(i))
   10 continue
      do 20 i=1,2*ndim
        call davar(xy(i),zero,i)
        call davar(a1(i),zero,i)
        call davar(a1i(i),zero,i)
        call davar(x(i),zero,i)
        call davar(v(i),zero,i)
   20 call davar(w(i),zero,i)
       
      do 30 i=1,ndim
        call daadd(rel(2*i-1),rel(2*i),rtc(2*i-1))
        call dasub(rel(2*i-1),rel(2*i),rtc(2*i))
        call dalin(rel(2*i-1),half,rel(2*i),half,ctr(2*i-1))
   30 call dalin(rel(2*i-1),half,rel(2*i),halfm,ctr(2*i))
      do 40 i=2*ndim+1,nv
        call davar(rot(i),zero,i)
        call davar(roti(i),zero,i)
        call davar(a2(i),zero,i)
        call davar(a2i(i),zero,i)
        call davar(cm(i),zero,i)
        call davar(xy(i),zero,i)
        call davar(a1(i),zero,i)
        call davar(a1i(i),zero,i)
        call davar(x(i),zero,i)
        call davar(v(i),zero,i)
        call davar(w(i),zero,i)
        call davar(rtc(i),zero,i)
   40 call davar(ctr(i),zero,i)
      return
      end

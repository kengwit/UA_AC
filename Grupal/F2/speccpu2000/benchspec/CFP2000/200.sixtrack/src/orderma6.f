C ANFANG UNTERPROGRAMM
      subroutine orderma6(h,ft,x,v,w,rel,cjg,ctr,rtc,roti)
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
* ANALYSES THE NON-LINEAR PART X OF THE MAP. THE RESULTING MAP  *
* HAS ITS NON-LINEAR TUNE SHIFTS IN H. (R X = FT^-1 ROT H FT)   *
* H IS FACTORIZED AS FROM LEFT-TO-RIGHT AND THE REVERSE FOR FT. *
*****************************************************************
      parameter (ndim=3)
      parameter (ndim2=6)
      parameter (nt=12)
      common /ii/no,nv
*DAE{ (NO,NV) X(NT),W(NT),V(NT),REL(NT),CJG(NT),CTR(NT),RTC(NT),H,FT    *FOX
*DAE{ (NO,NV) ROTI(NT)
*DAI{ (NO,NV) B1,B5,B6,B9
      integer x(nt),w(nt),v(nt),rel(nt),cjg(nt),ctr(nt),rtc(nt),h,ft
      integer roti(nt)
      integer b1,b5,b6,b9
*      
      integer          iscrda
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
*      
      parameter
     *(lx 3 = 1 +1 +1 +1 )
      save
      data    b1,b5,b6,b9/ lx  3 * 0 /
*      
      call daall(x,nt,'X         ',no,nv)                               *FOX
      call daall(w,nt,'W         ',no,nv)
      call daall(v,nt,'V         ',no,nv)                               *FOX
      call daall(rel,nt,'REL       ',no,nv)
      call daall(cjg,nt,'CJG       ',no,nv)
      call daall(ctr,nt,'CTR       ',no,nv)
      call daall(rtc,nt,'RTC       ',no,nv)
      call daall(h,1,'H         ',no,nv)
      call daall(ft,1,'FT        ',no,nv)
      call daall(roti,nt,'ROTI      ',no,nv)
      call daall(b1,1,'B1        ',no,nv)
      call daall(b5,1,'B5        ',no,nv)
      call daall(b6,1,'B6        ',no,nv)
      call daall(b9,1,'B9        ',no,nv)
*DA }  
      call dacon(h,zero)
      call dacon(ft,zero)
      do 10 k=2,no-1
        kp=k+1
* IF K>2 V = H(K)^-1 X(K)
        call fexpo6(h,x,v,3,k,onem,-1)
* EXTRACTING K TH DEGREE OF V ----> W
        call taked6(v,k,w)
* W = EXP(B5) + ...
        call intd6(w,b5,rel)
* B5 ON EXIT IS THE NEW CONTRIBUTION TO H
* B6 IS THE NEW CONTRIBUTION TO FT
        call analie6(b5,b6,cjg,ctr,rtc)
        call daadd(b5,h,b1)
        call dacop(b1,h)
* EXP(B9) = EXP( : ROTI B6 :)
        call trx6(b6,b9,roti)
* V = EXP(-B6) REL
        call fexpo6(b6,rel,v,kp,kp,onem,1)
* W = V O X
        call dacct(v,ndim2,x,nv,w,ndim2)
        write(6,*) ' ORDERMAP K= ', k+1
* X = EXP(B9) W
        call fexpo6(b9,w,x,kp,kp,one,1)
* B6 IS THE NEW CONTRIBUTION TO FT
        call daadd(b6,ft,b1)
        call dacop(b1,ft)
   10 continue
      call dadal(b9,1)
      call dadal(b6,1)
      call dadal(b5,1)
      call dadal(b1,1)
      return
      end

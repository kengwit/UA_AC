C ANFANG UNTERPROGRAMM
      subroutine fexpo6(h,x,v,nrmin,nrmax,sca,ifac)
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
* MORE OR LESS V = EXP(:H:) X ; SEE BELOW                       *
*****************************************************************
      parameter (ndim=3)
      parameter (nt=12)
      common /ii/no,nv
*DAE{ (NO,NV) X(NT),V(NT),H                                             *FOX
*DAI{ (NO,NV) BM,B0
      integer x(nt),v(nt),h
      integer bm,b0
*      
      integer          iscrda
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
*      
      parameter
     *(lx 2 = 1 +1 )
      save
      data    bm,b0/ lx  2 * 0 /
*      
      call daall(x,nt,'X         ',no,nv)                               *FOX
      call daall(v,nt,'V         ',no,nv)                               *FOX
      call daall(h,1,'H         ',no,nv)
      call daall(bm,1,'BM        ',no,nv)
      call daall(b0,1,'B0        ',no,nv)
*DA }  
      do 10 i=1,2*ndim
   10 call dacop(x(i),v(i))
       
* V = X IF NRMAX = 2
      if(nrmax.eq.2) then
        call dadal(b0,1)
        call dadal(bm,1)
        return
      endif
*      
      if(ifac.eq.100) goto 60
* IFAC =1 ---> V = EXP(:SCA*H(NRMAX):)...EXP(:SCA*H(NRMIN):)X
      if(ifac.eq.1) then
        do 30 i=nrmax,nrmin,-1
          call take6(h,i,b0)
          call dacmu(b0,sca,bm)
          do 20 j=1,2*ndim
            non=no
            call lie6(bm,v(j),b0,i,non)
   20     call dacop(b0,v(j))
   30   continue
      else
* IFAC =-1 ---> V = EXP(:SCA*H(NRMIN):)...EXP(:SCA*H(NRMAX):)X
        do 50 i=nrmin,nrmax
          call take6(h,i,b0)
          call dacmu(b0,sca,bm)
          do 40 j=1,2*ndim
            non=no
            call lie6(bm,v(j),b0,i,non)
   40     call dacop(b0,v(j))
   50   continue
      endif
      call dadal(b0,1)
      call dadal(bm,1)
      return
* IFAC =100 ---> V = EXP(:SCA*H:) X
   60 call dacmu(h,sca,bm)
      do 70 j=1,2*ndim
        non=no
        call lie6(bm,v(j),b0,nrmin,non)
   70 call dacop(b0,v(j))
      call dadal(b0,1)
      call dadal(bm,1)
      return
      end

C ANFANG UNTERPROGRAMM
      subroutine mainia6(iqmod6,eps,cor6d)
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
      parameter (nsuper=1)
CCC   THE FOLLOWING IS THE ONLY PARAMETER CHANGED SO FAR FOR 6D
      parameter (ndim2=6)
      parameter (nt=12)
      parameter (ntt=20)
      dimension cor6d(2,2)
C     dimension old(ndim)
      common/tasm/tasm(6,6)
      dimension jj(8)
      common /nom/ bb2,rot,roti,ctr,cjg,rtc,bb1,rel
      common /vecteur/v,w,x,xy,h,ft,a2,a2i,cm,a1,a1i
      integer j(nt)
C     integer opt(nt)
      common /stable/sta(ndim),dsta(ndim)
      common /istable/ista(ndim),idsta(ndim)
      common /ii/no,nv
      common /tune/ angle(ndim),abm(-ntt:ntt,-ntt:ntt,-ntt:ntt)
     @,aam(-ntt:ntt,-ntt:ntt,-ntt:ntt),tue(-ntt:ntt,-ntt:ntt,-ntt:ntt)
      common /super/hs,fs
      external resl6
*DAE{ (NO,NV) BB1,BB2,ROT(NT),ROTI(NT),REL(NT),FS
*DAE{ (NO,NV) CTR(NT),CJG(NT),RTC(NT),A2(NT),A2I(NT),CM(NT),A1(NT)
*DAE{ (NO,NV) V(NT),W(NT),X(NT),XY(NT),H,FT,A1I(NT),HS                  *FOX
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
      call daall(v,nt,'V         ',no,nv)                               *FOX
      call daall(w,nt,'W         ',no,nv)
      call daall(x,nt,'X         ',no,nv)                               *FOX
      call daall(xy,nt,'XY        ',no,nv)
      call daall(h,1,'H         ',no,nv)
      call daall(ft,1,'FT        ',no,nv)
      call daall(a1i,nt,'A1I       ',no,nv)
      call daall(hs,1,'HS        ',no,nv)
*DA }  
      x2pi=atan(one)*eight
      infile=18
      outfil=19
      rewind infile
      nord=1
      fac=one
      sta(1)=1
      sta(2)=1
      sta(3)=1
      iham=0
      do 10 i=1,ndim
   10 dsta(i)=one-sta(i)
      do 20 i=1,ndim
        ista(i)=int(sta(i)+.01)
   20 idsta(i)=int(dsta(i)+.01)
      call creat6
      call dacond6(xy,zero)
      do 30 i=ndim2+1,nv
   30 call davar(xy(i),zero,i)
      mfile=infile
      do 40 i=1,ndim2
        call darea(xy(i),mfile)
   40 call dapok(xy(i),j,zero)
   50 continue
      itype=3
      mfile=outfil
      call gofix6(xy,x,v,w,rel,a1,a1i,nord)
      call midbloc6(x,a2,a2i,angle)
      if(one-angle(ndim)/x2pi.gt..5) then
C       write(6,*) (angle(i)/x2pi,i=1,ndim-1),angle(ndim)/x2pi
        write(6,9917) (angle(i)/x2pi,i=1,ndim-1),angle(ndim)/x2pi
9917    format(3F24.16)
      else
C       write(6,*) (angle(i)/x2pi,i=1,ndim-1),one-angle(ndim)/x2pi
        write(6,9917) (angle(i)/x2pi,i=1,ndim-1),one-angle(ndim)/x2pi
      endif
      call ambm6(no,iham)
      call rotatio6
      call dacopd6(a2i,v)
      call dacct(v,ndim2,x,nv,w,ndim2)
      call dacct(w,ndim2,a2,nv,v,ndim2)
      call dacct(v,ndim2,roti,nv,x,ndim2)
      call orderma6(h,ft,x,v,w,rel,cjg,ctr,rtc,roti)
c      call daprid6(a1i,mfile)
c      call daprid6(a2i,mfile)
c      call dapri(ft,mfile)
      do 60 i=1,ndim
   60 angle(i)=angle(i)*fac
      call rotatio6
c      call daprid6(rot,mfile)
      call dacmu(h,fac,bb2)
      call dacop(bb2,h)
c      call dapri(h,mfile)
c      call dapri(ft,mfile)
c      call daprid6(a2,mfile)
c      call daprid6(a1,mfile)
      do 70 i = 1,6
        jj(i)=1
        call dapek(a2(1),jj,tas)
        tasm(1,i) = tas
        call dapek(a2(2),jj,tas)
        tasm(2,i) = tas
        call dapek(a2(3),jj,tas)
        tasm(3,i) = tas
        call dapek(a2(4),jj,tas)
        tasm(4,i) = tas
        call dapek(a2(5),jj,tas)
        tasm(5,i) = tas
        call dapek(a2(6),jj,tas)
        tasm(6,i) = tas
        jj(i)=0
   70 continue
      do 80 i = 1,5
        tasm(6,i)=tasm(6,i)*c1m3
        tasm(i,6)=tasm(i,6)*c1e3
   80 continue
      if(iqmod6.eq.1) then
        call dacond6(x,zero)
        coe=-two/x2pi
        call ctrbasi6(h,hs,ctr,cjg)
        do 901 i=1,ndim
        call dader(2*i-1,h,bb1)
        call datra(2*i,bb1,bb2)
        call dacmu(bb2,coe,x(i))
 901    continue
        jj(7)=1
        call dapek(x(1),jj,coefh1)
        call dapek(x(2),jj,coefv1)
        jj(7)=0
        jj(8)=1
        call dapek(x(1),jj,coefh2)
        call dapek(x(2),jj,coefv2)
        jj(8)=0
        det1=coefh1*coefv2-coefv1*coefh2
        if(abs(det1).le.pieni) call error(90)
        cor6d(1,1)=coefv2/det1
        cor6d(1,2)=-coefh2/det1
        cor6d(2,1)=-coefv1/det1
        cor6d(2,2)=coefh1/det1
      endif
      call dadal(hs,1)
      call dadal(a1i,nt)
      call dadal(ft,1)
      call dadal(h,1)
      call dadal(xy,nt)
      call dadal(x,nt)
      call dadal(w,nt)
      call dadal(v,nt)
      call dadal(a1,nt)
      call dadal(cm,nt)
      call dadal(a2i,nt)
      call dadal(a2,nt)
      call dadal(rtc,nt)
      call dadal(cjg,nt)
      call dadal(ctr,nt)
      call dadal(fs,1)
      call dadal(rel,nt)
      call dadal(roti,nt)
      call dadal(rot,nt)
      call dadal(bb2,1)
      call dadal(bb1,1)
      return
      end

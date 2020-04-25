C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine envars(j,dpp,rv)
C-----------------------------------------------------------------------
C  CALCULATION OF : MOMENTUM-DEPENDING ELEMENT-MATRICES AND
C                   CHANGE OF PATH LENGTHS FOR EACH PARTICLE.
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
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
      character*16 bez,bezb,bezr,erbez,coel,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro
      common/erroc/erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,numlr,nde(2),nwr(4),
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/cor/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),izu0,mmac,mcut
      common/rand0c/bezr(3,nele)
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/elbe(nblo),ilin,nt,iprint,ntco,eui,euii,nlin
      common/linopc/bez(nele),bezb(nblo),bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,
     &ncororb(nele)
      common/apert/apx(nele),apz(nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),
     &ratioe(nele),iratioe(nele),icoe
      common/sea/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh
      common/posti3/toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/titc/sixtit,commen
      common/tit/ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/clobaux(2,nele),sigman(2,npart,nele),sigman6(2,nele),
     &sigman2(2,npart,nele),sigmanq(2,npart,nele),clobeam(2,npart,nele),
     &beamoff(2,npart,nele),partnum,emitnx,emitnz,gammar,nbeam,nvbeam,
     &ibeco,ibtyp
      common/syos/as(6,2,npart,nele),
     &al(6,2,npart,nele),sigm(mpa),dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor),
     &idalloc
      common/norf/nordf,nvarf,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax
      common/tcorrc/coel(10)
      save
C-----------------------------------------------------------------------
      dpd=one+dpp
      dpsq=sqrt(dpd)
      do 190 i=1,il
        if(abs(el(i)).le.pieni) goto 190
        kz1=kz(i)+1
        goto(10,30,90,50,70,80,120,170,180),kz1
C-----------------------------------------------------------------------
C  DRIFTLENGTH
C-----------------------------------------------------------------------
   10   do 20 l=1,2
          al(1,l,j,i)=one
          al(2,l,j,i)=el(i)
          al(3,l,j,i)=zero
          al(4,l,j,i)=one
   20   as(6,l,j,i)=-rv*al(2,l,j,i)/c2e3
        as(1,1,j,i)=el(i)*(one-rv)*c1e3
        goto 190
C-----------------------------------------------------------------------
C  RECTANGULAR MAGNET
C  HORIZONTAL
C-----------------------------------------------------------------------
   30   ih=1
   40   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        fok1=(tan(fok*half))/rho
        si=sin(fok)
        co=cos(fok)
        al(1,ih,j,i)=one
        al(2,ih,j,i)=rho*si
        al(3,ih,j,i)=zero
        al(4,ih,j,i)=one
        al(5,ih,j,i)=-dpp*(rho*(one-co)/dpsq)*c1e3
        al(6,ih,j,i)=-dpp*(two*tan(fok*half)/dpsq)*c1e3
        sm1=cos(fok)
        sm2=sin(fok)*rho
        sm3=-sin(fok)/rho
        sm5=-rho*dpsq*(one-sm1)
        sm6=-sm2*dpsq/rho
        sm12=el(i)-sm1*sm2
        sm23=sm2*sm3
        as3=-rv*(dpp*rho/(two*dpsq)*sm23+sm5)
        as4=-rv*sm23/c2e3
        as6=-rv*(el(i)+sm1*sm2)/c4e3
        as(1,ih,j,i)=(-rv*(dpp*dpp/(four*dpd)*sm12+dpp*(el(i)-sm2)) +el
     +  (i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp/(two*rho*dpsq)*sm12+sm6)+fok1*as3
        as(3,ih,j,i)=as3
        as(4,ih,j,i)=as4+two*as6*fok1
        as(5,ih,j,i)=-rv*sm12/(c4e3*rho*rho)+as6*fok1*fok1+fok1*as4
        as(6,ih,j,i)=as6
C--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        g=tan(fok*half)/rho
        gl=el(i)*g
        al(1,ih,j,i)=one-gl
        al(2,ih,j,i)=el(i)
        al(3,ih,j,i)=-g*(two-gl)
        al(4,ih,j,i)=al(1,ih,j,i)
        as6=-rv*al(2,ih,j,i)/c2e3
        as(4,ih,j,i)=-two*as6*fok1
        as(5,ih,j,i)=as6*fok1*fok1
        as(6,ih,j,i)=as6
        goto 190
C-----------------------------------------------------------------------
C  SEKTORMAGNET
C  HORIZONTAL
C-----------------------------------------------------------------------
   50   ih=1
   60   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        si=sin(fok)
        co=cos(fok)
        rhoc=rho*(one-co)/dpsq
        siq=si/dpsq
        al(1,ih,j,i)=co
        al(2,ih,j,i)=rho*si
        al(3,ih,j,i)=-si/rho
        al(4,ih,j,i)=co
        al(5,ih,j,i)=-dpp*rhoc*c1e3
        al(6,ih,j,i)=-dpp*siq*c1e3
        sm12=el(i)-al(1,ih,j,i)*al(2,ih,j,i)
        sm23=al(2,ih,j,i)*al(3,ih,j,i)
        as(1,ih,j,i)=(-rv*(dpp*dpp/(four*dpd)*sm12 +dpp*(el(i)-al
     +  (2,ih,j,i)))+el(i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp/(two*rho*dpsq)*sm12-dpd*siq)
        as(3,ih,j,i)=-rv*(dpp*rho/(two*dpsq)*sm23-dpd*rhoc)
        as(4,ih,j,i)=-rv*sm23/c2e3
        as(5,ih,j,i)=-rv*sm12/(c4e3*rho*rho)
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
C--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        al(1,ih,j,i)=one
        al(2,ih,j,i)=el(i)
        al(3,ih,j,i)=zero
        al(4,ih,j,i)=one
        as(6,ih,j,i)=-rv*al(2,ih,j,i)/c2e3
        goto 190
C-----------------------------------------------------------------------
C  RECTANGULAR MAGNET VERTIKAL
C-----------------------------------------------------------------------
   70   ih=2
        goto 40
C-----------------------------------------------------------------------
C  SEKTORMAGNET VERTIKAL
C-----------------------------------------------------------------------
   80   ih=2
        goto 60
C-----------------------------------------------------------------------
C  QUADRUPOLE
C  FOCUSSING
C-----------------------------------------------------------------------
   90   fok=ek(i)/(one+dpp)
        aek=abs(fok)
        if(abs(fok).le.pieni) goto 10
        ih=0
        hi=sqrt(aek)
        fi=el(i)*hi
        if(fok.gt.zero) goto 110
  100   ih=ih+1
        al(1,ih,j,i)=cos(fi)
        hi1=sin(fi)
        al(2,ih,j,i)=hi1/hi
        al(3,ih,j,i)=-hi1*hi
        al(4,ih,j,i)=al(1,ih,j,i)
        as(1,ih,j,i)=el(i)*(one-rv)*c1e3
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=-rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        if(ih.eq.2) goto 190
C--DEFOCUSSING
  110   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        al(1,ih,j,i)=hc
        al(2,ih,j,i)=hs/hi
        al(3,ih,j,i)=hs*hi
        al(4,ih,j,i)=hc
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=+rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        if(ih.eq.1) goto 100
        goto 190
C-----------------------------------------------------------------------
C  COMBINED FUNCTION MAGNET HORIZONTAL
C  FOCUSSING
C-----------------------------------------------------------------------
  120   ih=0
        fokq=ek(i)
  130   wf=ed(i)/dpsq
        fok=fokq/(dpd)-wf*wf
        if(abs(fok).le.pieni) goto 10
        afok=abs(fok)
        hi=sqrt(afok)
        fi=hi*el(i)
        if(fok.gt.zero) goto 160
  140   ih=ih+1
        si=sin(fi)
        co=cos(fi)
        wfa=wf/afok*(one-co)/dpsq
        wfhi=wf/hi*si/dpsq
        al(1,ih,j,i)=co
        al(2,ih,j,i)=si/hi
        al(3,ih,j,i)=-si*hi
        al(4,ih,j,i)=co
        al(5,ih,j,i)=-wfa*dpp*c1e3
        al(6,ih,j,i)=-wfhi*dpp*c1e3
        sm12=el(i)-al(1,ih,j,i)*al(2,ih,j,i)
        sm23=al(2,ih,j,i)*al(3,ih,j,i)
        as(1,ih,j,i)=(-rv*(dpp*dpp/(four*dpd)*sm12+dpp* (el(i)-al
     +  (2,ih,j,i)))/afok*wf*wf+el(i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp*wf/(two*dpsq)*sm12-dpd*wfhi)
        as(3,ih,j,i)=-rv*(dpp*half/afok/dpd*ed(i)*sm23-dpd*wfa)
        as(4,ih,j,i)=-rv*sm23/c2e3
        as(5,ih,j,i)=-rv*sm12*afok/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        ih=ih+1
        if(ih.gt.2) ih=1
        aek=abs(ek(i)/dpd)
        hi=sqrt(aek)
        fi=hi*el(i)
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        al(1,ih,j,i)=hc
        al(2,ih,j,i)=el(i)
        if(abs(hi).le.pieni) goto 150
        al(2,ih,j,i)=hs/hi
  150   al(3,ih,j,i)=hs*hi
        al(4,ih,j,i)=hc
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=+rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        goto 190
C--DEFOCUSSING
  160   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        al(1,ih,j,i)=hc
        al(2,ih,j,i)=hs/hi
        al(3,ih,j,i)=hs*hi
        al(4,ih,j,i)=hc
        wfa=wf/afok*(one-hc)/dpsq
        wfhi=wf/hi*hs/dpsq
        al(5,ih,j,i)= wfa*dpp*c1e3
        al(6,ih,j,i)=-wfhi*dpp*c1e3
        sm12=el(i)-al(1,ih,j,i)*al(2,ih,j,i)
        sm23=al(2,ih,j,i)*al(3,ih,j,i)
        as(1,ih,j,i)=(rv*(dpp*dpp/(four*dpd)*sm12 +dpp*(el(i)-al
     +  (2,ih,j,i)))/afok*wf*wf+el(i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp*wf/(two*dpsq)*sm12-dpd*wfhi)
        as(3,ih,j,i)=rv*(dpp*half/afok/dpd*ed(i)*sm23-dpd*wfa)
        as(4,ih,j,i)=-rv*sm23/c2e3
        as(5,ih,j,i)=+rv*sm12*afok/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        ih=ih+1
        if(ih.gt.2) ih=1
        aek=abs(ek(i)/dpd)
        hi=sqrt(aek)
        fi=hi*el(i)
        si=sin(fi)
        co=cos(fi)
        al(1,ih,j,i)=co
        al(2,ih,j,i)=si/hi
        al(3,ih,j,i)=-si*hi
        al(4,ih,j,i)=co
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=-rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        goto 190
C-----------------------------------------------------------------------
C  COMBINED FUNCTION MAGNET VERTICAL
C-----------------------------------------------------------------------
  170   ih=1
        fokq=-ek(i)
        goto 130
C-----------------------------------------------------------------------
C  EDGE FOCUSSING
C-----------------------------------------------------------------------
  180   rhoi=ed(i)/dpsq
        fok=rhoi*tan(el(i)*rhoi*half)
        al(1,1,j,i)=one
        al(2,1,j,i)=zero
        al(3,1,j,i)=fok
        al(4,1,j,i)=one
        al(1,2,j,i)=one
        al(2,2,j,i)=zero
        al(3,2,j,i)=-fok
        al(4,2,j,i)=one
        goto 190
C-----------------------------------------------------------------------
C   NONLINEAR INSERTION
C-----------------------------------------------------------------------
  190 continue
      return
      end

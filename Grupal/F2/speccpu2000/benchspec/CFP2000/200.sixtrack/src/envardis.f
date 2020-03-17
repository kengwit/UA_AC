C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine envardis(dpp,aeg,bl1eg,bl2eg)
C-----------------------------------------------------------------------
C  CALCULATION OF ELEMENT MATRICES
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
      character*16 bez,bezb,bezr,erbez,bezl
C     character*16 coel
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
      dimension aeg(nele,2,6),bl1eg(nblo,2,6),bl2eg(nblo,2,6)
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
          aeg(i,l,1)=one
          aeg(i,l,2)=el(i)
          aeg(i,l,3)=zero
   20   aeg(i,l,4)=one
        goto 190
C-----------------------------------------------------------------------
C  RECTANGULAR MAGNET
C  HORIZONTAL
C-----------------------------------------------------------------------
   30   ih=1
   40   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        si=sin(fok)
        co=cos(fok)
        aeg(i,ih,1)=one
        aeg(i,ih,2)=rho*si
        aeg(i,ih,3)=zero
        aeg(i,ih,4)=one
        aeg(i,ih,5)=-rho*(one-co)/dpsq
        aeg(i,ih,6)=-two*tan(fok*half)/dpsq
C--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        g=tan(fok*half)/rho
        gl=el(i)*g
        aeg(i,ih,1)=one-gl
        aeg(i,ih,2)=el(i)
        aeg(i,ih,3)=-g*(two-gl)
        aeg(i,ih,4)=aeg(i,ih,1)
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
        aeg(i,ih,1)=co
        aeg(i,ih,2)=rho*si
        aeg(i,ih,3)=-si/rho
        aeg(i,ih,4)=co
        aeg(i,ih,5)=-rho*(one-co)/dpsq
        aeg(i,ih,6)=-si/dpsq
C--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        aeg(i,ih,1)=one
        aeg(i,ih,2)=el(i)
        aeg(i,ih,3)=zero
        aeg(i,ih,4)=one
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
        if(abs(fok).le.pieni) goto 10
        ih=0
        hi=sqrt(abs(fok))
        fi=el(i)*hi
        if(fok.gt.zero) goto 110
  100   ih=ih+1
        aeg(i,ih,1)=cos(fi)
        hi1=sin(fi)
        aeg(i,ih,2)=hi1/hi
        aeg(i,ih,3)=-hi1*hi
        aeg(i,ih,4)=aeg(i,ih,1)
        if(ih.eq.2) goto 190
C--DEFOCUSSING
  110   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        aeg(i,ih,1)=hc
        aeg(i,ih,2)=hs/hi
        aeg(i,ih,3)=hs*hi
        aeg(i,ih,4)=hc
        if(ih.eq.1) goto 100
        goto 190
C-----------------------------------------------------------------------
C  COMBINED FUNCTION MAGNET HORIZONTAL
C  FOCUSSING
C-----------------------------------------------------------------------
  120   ih=0
        fokq=ek(i)
  130   wf=ed(i)/dpsq
        fok=fokq/dpd-wf*wf
        if(abs(fok).le.pieni) goto 10
        afok=abs(fok)
        hi=sqrt(afok)
        fi=hi*el(i)
        if(fok.gt.zero) goto 160
  140   ih=ih+1
        si=sin(fi)
        co=cos(fi)
        aeg(i,ih,1)=co
        aeg(i,ih,2)=si/hi
        aeg(i,ih,3)=-si*hi
        aeg(i,ih,4)=co
        aeg(i,ih,5)=-wf/afok*(one-co)/dpsq
        aeg(i,ih,6)=-wf/hi*si/dpsq
        ih=ih+1
        if(ih.gt.2) ih=1
        hi=sqrt(abs(ek(i)/dpd))
        fi=hi*el(i)
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        aeg(i,ih,1)=hc
        aeg(i,ih,2)=el(i)
        if(abs(hi).le.pieni) goto 150
        aeg(i,ih,2)=hs/hi
  150   aeg(i,ih,3)=hs*hi
        aeg(i,ih,4)=hc
        goto 190
C--DEFOCUSSING
  160   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        aeg(i,ih,1)=hc
        aeg(i,ih,2)=hs/hi
        aeg(i,ih,3)=hs*hi
        aeg(i,ih,4)=hc
        aeg(i,ih,5)= wf/afok*(one-hc)/dpsq
        aeg(i,ih,6)=-wf/hi*hs/dpsq
        ih=ih+1
        if(ih.gt.2) ih=1
        hi=sqrt(abs(ek(i)/dpd))
        fi=hi*el(i)
        si=sin(fi)
        co=cos(fi)
        aeg(i,ih,1)=co
        aeg(i,ih,2)=si/hi
        aeg(i,ih,3)=-si*hi
        aeg(i,ih,4)=co
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
        aeg(i,1,1)=one
        aeg(i,1,2)=zero
        aeg(i,1,3)=fok
        aeg(i,1,4)=one
        aeg(i,2,1)=one
        aeg(i,2,2)=zero
        aeg(i,2,3)=-fok
        aeg(i,2,4)=one
        goto 190
C-----------------------------------------------------------------------
C   NONLINEAR INSERTION
C-----------------------------------------------------------------------
  190 continue
      call blockdis(aeg,bl1eg,bl2eg)
      return
      end

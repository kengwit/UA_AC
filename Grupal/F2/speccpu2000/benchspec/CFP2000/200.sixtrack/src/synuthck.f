C ANFANG - NEBENPROGRAMM -
C-----------------------------------------------------------------------
      subroutine synuthck
C-----------------------------------------------------------------------
C
C  TRACK THICK LENS PART
C
C
C  F. SCHMIDT
C-----------------------------------------------------------------------
C  VECTOR VERSION 2 OCTOBER 1996
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
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/tasm/tasm(6,6)
C     integer rdummy(6)
C     character*10 cmonth
C     character*4 cpto
C     character*80 day,runtim
C     character*8 cdate,ctime,progrm
      logical pstop
      common/main1/
     &ekv(npart,nele),fokqv(npart),
     &aaiv(mmul,nmac,nblz),bbiv(mmul,nmac,nblz),
     &smiv(nmac,nblz),zsiv(nmac,nblz),xsiv(nmac,nblz),
     &xsv(npart),zsv(npart),
     &qw(2),qwc(3),clo0(2),clop0(2),
     &eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),
     &xv(2,npart),yv(2,npart),dam(npart),ekkv(npart),
     &sigmv(npart),dpsv(npart),dp0v(npart),
     &sigmv6(npart),dpsv6(npart),
     &ejv(npart),ejfv(npart),
     &xlv(npart),zlv(npart),
     &pstop(npart),rvv(npart),ejf0v(npart),
     &numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),
     &sm1(npart),sm2(npart),sm3(npart),sm12(npart),
     &as3(npart),as4(npart),as6(npart),sm23(npart),
     &rhoc(npart),siq(npart),aek(npart),afok(npart),
     &hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),
     &wfa(npart),wfhi(npart),rhoi(npart),
     &hi(npart),fi(npart),hi1(npart),
     &xvl(2,npart),yvl(2,npart),ejvl(npart),
     &dpsvl(npart),oidpsv(npart),
     &sigmvl(npart),iv(npart),
     &aperv(npart,2),ixv(npart),
     &clov(2,npart),clopv(2,npart),alf0v(npart,2),
     &bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),
     &hv(6,2,npart,nblo),bl1v(6,2,npart,nblo),
     &tas(npart,6,6),qwcs(npart,3),
     &di0xs(npart),di0zs(npart),dip0xs(npart),dip0zs(npart),
     &xau(2,6),cloau(6),di0au(4),tau(6,6),tasau(npart,6,6),
     &wx(3),x1(6),x2(6),
     &fake(2,20)
      common/main4/ e0f,numx
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),
     &stracks(nblz),dpsv1(npart),nwri
      save
C---------------------------------------  SUBROUTINE 'ENVARS' IN-LINE
      do 10 j=1,napx
        dpd(j)=one+dpsv(j)
        dpsq(j)=sqrt(dpd(j))
   10 continue
      do 160 l=1,il
        if(abs(el(l)).le.pieni) goto 160
        kz1=kz(l)+1
        goto(20,40,80,60,40,60,100,100,140),kz1
C-----------------------------------------------------------------------
C  DRIFTLENGTH
C-----------------------------------------------------------------------
   20   do 30 j=1,napx
          as(6,1,j,l)=-rvv(j)*el(l)/c2e3
          as(6,2,j,l)=as(6,1,j,l)
          as(1,1,j,l)=el(l)*(one-rvv(j))*c1e3
   30   continue
        goto 160
C-----------------------------------------------------------------------
C  RECTANGULAR MAGNET
C  HORIZONTAL
C-----------------------------------------------------------------------
   40   fokm=el(l)*ed(l)
        if(abs(fokm).le.pieni) goto 20
        if(kz1.eq.2) then
          ih1=1
          ih2=2
        else
C  RECTANGULAR MAGNET VERTICAL
          ih1=2
          ih2=1
        endif
        do 50 j=1,napx
          fok(j)=fokm/dpsq(j)
          rho(j)=(one/ed(l))*dpsq(j)
          fok1(j)=(tan(fok(j)*half))/rho(j)
          si(j)=sin(fok(j))
          co(j)=cos(fok(j))
          al(2,ih1,j,l)=rho(j)*si(j)
          al(5,ih1,j,l)=-dpsv(j)*(rho(j)*(one-co(j))/dpsq(j))*c1e3
          al(6,ih1,j,l)=-dpsv(j)*(two*tan(fok(j)*half)/dpsq(j))*c1e3
          sm1(j)=cos(fok(j))
          sm2(j)=sin(fok(j))*rho(j)
          sm3(j)=-sin(fok(j))/rho(j)
          sm12(j)=el(l)-sm1(j)*sm2(j)
          sm23(j)=sm2(j)*sm3(j)
          as3(j)=-rvv(j)*(dpsv(j)*rho(j)/(two*dpsq(j))*sm23(j)- rho(j)
     +    *dpsq(j)*(one-sm1(j)))
          as4(j)=-rvv(j)*sm23(j)/c2e3
          as6(j)=-rvv(j)*(el(l)+sm1(j)*sm2(j))/c4e3
          as(1,ih1,j,l)=(-rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12(j)+
     +    dpsv(j)*(el(l)-sm2(j)))+el(l)*(one-rvv(j)))*c1e3
          as(2,ih1,j,l)=-rvv(j)*(dpsv(j)/(two*rho(j)*dpsq(j))*sm12(j)-
     +    sm2(j)*dpsq(j)/rho(j))+fok1(j)*as3(j)
          as(3,ih1,j,l)=as3(j)
          as(4,ih1,j,l)=as4(j)+two*as6(j)*fok1(j)
          as(5,ih1,j,l)=-rvv(j)*sm12(j)/(c4e3*rho(j)*rho(j))+ as6(j)
     +    *fok1(j)*fok1(j)+fok1(j)*as4(j)
          as(6,ih1,j,l)=as6(j)
C--VERTIKAL
          g(j)=tan(fok(j)*half)/rho(j)
          gl(j)=el(l)*g(j)
          al(1,ih2,j,l)=one-gl(j)
          al(3,ih2,j,l)=-g(j)*(two-gl(j))
          al(4,ih2,j,l)=al(1,ih2,j,l)
          as6(j)=-rvv(j)*al(2,ih2,j,l)/c2e3
          as(4,ih2,j,l)=-two*as6(j)*fok1(j)
          as(5,ih2,j,l)=as6(j)*fok1(j)*fok1(j)
          as(6,ih2,j,l)=as6(j)
   50   continue
        goto 160
C-----------------------------------------------------------------------
C  SEKTORMAGNET
C  HORIZONTAL
C-----------------------------------------------------------------------
   60   fokm=el(l)*ed(l)
        if(abs(fokm).le.pieni) goto 20
        if(kz1.eq.4) then
          ih1=1
          ih2=2
        else
C  SECTOR MAGNET VERTICAL
          ih1=2
          ih2=1
        endif
        do 70 j=1,napx
          fok(j)=fokm/dpsq(j)
          rho(j)=(one/ed(l))*dpsq(j)
          si(j)=sin(fok(j))
          co(j)=cos(fok(j))
          rhoc(j)=rho(j)*(one-co(j))/dpsq(j)
          siq(j)=si(j)/dpsq(j)
          al(1,ih1,j,l)=co(j)
          al(2,ih1,j,l)=rho(j)*si(j)
          al(3,ih1,j,l)=-si(j)/rho(j)
          al(4,ih1,j,l)=co(j)
          al(5,ih1,j,l)=-dpsv(j)*rhoc(j)*c1e3
          al(6,ih1,j,l)=-dpsv(j)*siq(j)*c1e3
          sm12(j)=el(l)-al(1,ih1,j,l)*al(2,ih1,j,l)
          sm23(j)=al(2,ih1,j,l)*al(3,ih1,j,l)
          as(1,ih1,j,l)=(-rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12(j)
     +    +dpsv(j)*(el(l)-al(2,ih1,j,l)))+el(l)*(one-rvv(j)))*c1e3
          as(2,ih1,j,l)=-rvv(j)*(dpsv(j)/(two*rho(j)*dpsq(j))*sm12(j)-
     +    dpd(j)*siq(j))
          as(3,ih1,j,l)=-rvv(j)*(dpsv(j)*rho(j)/(two*dpsq(j))*sm23(j)-
     +    dpd(j)*rhoc(j))
          as(4,ih1,j,l)=-rvv(j)*sm23(j)/c2e3
          as(5,ih1,j,l)=-rvv(j)*sm12(j)/(c4e3*rho(j)*rho(j))
          as(6,ih1,j,l)=-rvv(j)*(el(l)+al(1,ih1,j,l)*al(2,ih1,j,l))/c4e3
C--VERTIKAL
          as(6,ih2,j,l)=-rvv(j)*al(2,ih2,j,l)/c2e3
   70   continue
        goto 160
C-----------------------------------------------------------------------
C  QUADRUPOLE
C  FOCUSSING
C-----------------------------------------------------------------------
   80   do 90 j=1,napx
          fok(j)=ekv(j,l)*oidpsv(j)
          aek(j)=abs(fok(j))
          hi(j)=sqrt(aek(j))
          fi(j)=el(l)*hi(j)
          if(fok(j).le.zero) then
            al(1,1,j,l)=cos(fi(j))
            hi1(j)=sin(fi(j))
            if(abs(hi(j)).le.pieni) then
              al(2,1,j,l)=el(l)
            else
              al(2,1,j,l)=hi1(j)/hi(j)
            endif
            al(3,1,j,l)=-hi1(j)*hi(j)
            al(4,1,j,l)=al(1,1,j,l)
            as(1,1,j,l)=el(l)*(one-rvv(j))*c1e3
            as(4,1,j,l)=-rvv(j)*al(2,1,j,l)*al(3,1,j,l)/c2e3
            as(5,1,j,l)=-rvv(j)*(el(l)-al(1,1,j,l)*al(2,1,j,l))* aek(j)
     +      /c4e3
            as(6,1,j,l)=-rvv(j)*(el(l)+al(1,1,j,l)*al(2,1,j,l))/c4e3
C--DEFOCUSSING
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,2,j,l)=hc(j)
            if(abs(hi(j)).le.pieni) then
              al(2,2,j,l)=el(l)
            else
              al(2,2,j,l)=hs(j)/hi(j)
            endif
            al(3,2,j,l)=hs(j)*hi(j)
            al(4,2,j,l)=hc(j)
            as(4,2,j,l)=-rvv(j)*al(2,2,j,l)*al(3,2,j,l)/c2e3
            as(5,2,j,l)=+rvv(j)*(el(l)-al(1,2,j,l)*al(2,2,j,l))* aek(j)
     +      /c4e3
            as(6,2,j,l)=-rvv(j)*(el(l)+al(1,2,j,l)*al(2,2,j,l))/c4e3
          else
            al(1,2,j,l)=cos(fi(j))
            hi1(j)=sin(fi(j))
            if(abs(hi(j)).le.pieni) then
              al(2,2,j,l)=el(l)
            else
              al(2,2,j,l)=hi1(j)/hi(j)
            endif
            al(3,2,j,l)=-hi1(j)*hi(j)
            al(4,2,j,l)=al(1,2,j,l)
            as(1,2,j,l)=el(l)*(one-rvv(j))*c1e3
            as(4,2,j,l)=-rvv(j)*al(2,2,j,l)*al(3,2,j,l)/c2e3
            as(5,2,j,l)=-rvv(j)*(el(l)-al(1,2,j,l)*al(2,2,j,l))* aek(j)
     +      /c4e3
            as(6,2,j,l)=-rvv(j)*(el(l)+al(1,2,j,l)*al(2,2,j,l))/c4e3
C--DEFOCUSSING
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,1,j,l)=hc(j)
            if(abs(hi(j)).le.pieni) then
              al(2,1,j,l)=el(l)
            else
              al(2,1,j,l)=hs(j)/hi(j)
            endif
            al(3,1,j,l)=hs(j)*hi(j)
            al(4,1,j,l)=hc(j)
            as(4,1,j,l)=-rvv(j)*al(2,1,j,l)*al(3,1,j,l)/c2e3
            as(5,1,j,l)=+rvv(j)*(el(l)-al(1,1,j,l)*al(2,1,j,l))* aek(j)
     +      /c4e3
            as(6,1,j,l)=-rvv(j)*(el(l)+al(1,1,j,l)*al(2,1,j,l))/c4e3
          endif
   90   continue
        goto 160
C-----------------------------------------------------------------------
C  COMBINED FUNCTION MAGNET HORIZONTAL
C  FOCUSSING
C-----------------------------------------------------------------------
  100   if(kz1.eq.7) then
          do 110 j=1,napx
            fokqv(j)=ekv(j,l)
  110     continue
          ih1=1
          ih2=2
        else
C  COMBINED FUNCTION MAGNET VERTICAL
          do 120 j=1,napx
            fokqv(j)=-ekv(j,l)
  120     continue
          ih1=2
          ih2=1
        endif
        do 130 j=1,napx
          wf(j)=ed(l)/dpsq(j)
          fok(j)=fokqv(j)/dpd(j)-wf(j)*wf(j)
          afok(j)=abs(fok(j))
          hi(j)=sqrt(afok(j))
          fi(j)=hi(j)*el(l)
          if(afok(j).le.pieni) then
            as(6,1,j,l)=-rvv(j)*el(l)/c2e3
            as(6,2,j,l)=as(6,1,j,l)
            as(1,1,j,l)=el(l)*(one-rvv(j))*c1e3
          endif
          if(fok(j).lt.-pieni) then
            si(j)=sin(fi(j))
            co(j)=cos(fi(j))
            wfa(j)=wf(j)/afok(j)*(one-co(j))/dpsq(j)
            wfhi(j)=wf(j)/hi(j)*si(j)/dpsq(j)
            al(1,ih1,j,l)=co(j)
            al(2,ih1,j,l)=si(j)/hi(j)
            al(3,ih1,j,l)=-si(j)*hi(j)
            al(4,ih1,j,l)=co(j)
            al(5,ih1,j,l)=-wfa(j)*dpsv(j)*c1e3
            al(6,ih1,j,l)=-wfhi(j)*dpsv(j)*c1e3
            sm12(j)=el(l)-al(1,ih1,j,l)*al(2,ih1,j,l)
            sm23(j)=al(2,ih1,j,l)*al(3,ih1,j,l)
            as(1,ih1,j,l)=(-rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12
     +      (j)+ dpsv(j)*(el(l)-al(2,ih1,j,l)))/afok(j)*wf(j)*wf(j)+el
     +      (l)* (one-rvv(j)))*c1e3
            as(2,ih1,j,l)=-rvv(j)*(dpsv(j)*wf(j)/(two*dpsq(j))*sm12(j)-
     +      dpd(j)*wfhi(j))
            as(3,ih1,j,l)=-rvv(j)*(dpsv(j)*half/afok(j)/dpd(j)* ed(l)
     +      *sm23(j)-dpd(j)*wfa(j))
            as(4,ih1,j,l)=-rvv(j)*sm23(j)/c2e3
            as(5,ih1,j,l)=-rvv(j)*sm12(j)*afok(j)/c4e3
            as(6,ih1,j,l)=-rvv(j)*(el(l)+al(1,ih1,j,l)*al(2,ih1,j,l))
     +      /c4e3
            aek(j)=abs(ekv(j,l)/dpd(j))
            hi(j)=sqrt(aek(j))
            fi(j)=hi(j)*el(l)
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,ih2,j,l)=hc(j)
            if(abs(hi(j)).gt.pieni) al(2,ih2,j,l)=hs(j)/hi(j)
            al(3,ih2,j,l)=hs(j)*hi(j)
            al(4,ih2,j,l)=hc(j)
            as(4,ih2,j,l)=-rvv(j)*al(2,ih2,j,l)*al(3,ih2,j,l)/c2e3
            as(5,ih2,j,l)=+rvv(j)*(el(l)-al(1,ih2,j,l)*al(2,ih2,j,l))*
     +      aek(j)/c4e3
            as(6,ih2,j,l)=-rvv(j)*(el(l)+al(1,ih2,j,l)*al(2,ih2,j,l))
     +      /c4e3
          endif
C--DEFOCUSSING
          if(fok(j).gt.pieni) then
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,ih1,j,l)=hc(j)
            al(2,ih1,j,l)=hs(j)/hi(j)
            al(3,ih1,j,l)=hs(j)*hi(j)
            al(4,ih1,j,l)=hc(j)
            wfa(j)=wf(j)/afok(j)*(one-hc(j))/dpsq(j)
            wfhi(j)=wf(j)/hi(j)*hs(j)/dpsq(j)
            al(5,ih1,j,l)= wfa(j)*dpsv(j)*c1e3
            al(6,ih1,j,l)=-wfhi(j)*dpsv(j)*c1e3
            sm12(j)=el(l)-al(1,ih1,j,l)*al(2,ih1,j,l)
            sm23(j)=al(2,ih1,j,l)*al(3,ih1,j,l)
            as(1,ih1,j,l)=(rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12(j)
     +      +dpsv(j)*(el(l)-al(2,ih1,j,l)))/afok(j)*wf(j)*wf(j)+el(l)*
     +      (one-rvv(j)))*c1e3
            as(2,ih1,j,l)=-rvv(j)*(dpsv(j)*wf(j)/(two*dpsq(j))*sm12(j)-
     +      dpd(j)*wfhi(j))
            as(3,ih1,j,l)=rvv(j)*(dpsv(j)*half/afok(j)/dpd(j)* ed(l)
     +      *sm23(j)-dpd(j)*wfa(j))
            as(4,ih1,j,l)=-rvv(j)*sm23(j)/c2e3
            as(5,ih1,j,l)=+rvv(j)*sm12(j)*afok(j)/c4e3
            as(6,ih1,j,l)=-rvv(j)*(el(l)+al(1,ih1,j,l)*al(2,ih1,j,l))
     +      /c4e3
            aek(j)=abs(ekv(j,l)/dpd(j))
            hi(j)=sqrt(aek(j))
            fi(j)=hi(j)*el(l)
            si(j)=sin(fi(j))
            co(j)=cos(fi(j))
            al(1,ih2,j,l)=co(j)
            al(2,ih2,j,l)=si(j)/hi(j)
            al(3,ih2,j,l)=-si(j)*hi(j)
            al(4,ih2,j,l)=co(j)
            as(4,ih2,j,l)=-rvv(j)*al(2,ih2,j,l)*al(3,ih2,j,l)/c2e3
            as(5,ih2,j,l)=-rvv(j)*(el(l)-al(1,ih2,j,l)*al(2,ih2,j,l))*
     +      aek(j)/c4e3
            as(6,ih2,j,l)=-rvv(j)*(el(l)+al(1,ih2,j,l)*al(2,ih2,j,l))
     +      /c4e3
          endif
  130   continue
        goto 160
C-----------------------------------------------------------------------
C  EDGE FOCUSSING
C-----------------------------------------------------------------------
  140   do 150 j=1,napx
          rhoi(j)=ed(l)/dpsq(j)
          fok(j)=rhoi(j)*tan(el(l)*rhoi(j)*half)
          al(3,1,j,l)=fok(j)
          al(3,2,j,l)=-fok(j)
  150   continue
  160 continue
C---------------------------------------  END OF 'ENVARS' (2)
      return
      end

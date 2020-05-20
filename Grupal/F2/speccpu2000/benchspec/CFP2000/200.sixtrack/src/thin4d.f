C ANFANG - NEBENPROGRAMM -
C-----------------------------------------------------------------------
      subroutine thin4d(nthinerr)
C-----------------------------------------------------------------------
C
C  TRACK THIN LENS PART
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
      parameter(cc=1.12837916709551d0)
      parameter(xlim=5.33d0)
      parameter(ylim=4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),
     &r2b(npart),rb(npart),rkb(npart),
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),
     &crzb(npart),cbxb(npart),cbzb(npart)
C     dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),
C    &sx2b(npart),sz2b(npart),r2b(npart),rb(npart),rkb(npart),
C    &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),
C    &crzb(npart),cbxb(npart),cbzb(npart),rxerf(npart,33),
C    &ryerf(npart,33),xerf(npart),yerf(npart),
C    &txerf(npart),tyerf(npart),sauxerf(npart),sxerf(npart),
C    &syerf(npart),qerf(npart),herf(npart),xherf(npart),yherf(npart),
C    &xlerf(npart),tnerf(npart),ncerf(npart),nuerf(npart)
      save
      nthinerr=0
      do 640 n=1,numl
        numx=n-1
        if(irip.eq.1) call ripple(n)
        if(mod(numx,nwri).eq.0) call writebin(nthinerr)
        if(nthinerr.ne.0) return
        do 630 i=1,iu
          ix=ic(i)-nblo
          goto(10,630,630,630,630,630,630,630,630,630,30,50,70,90,110,
     +    130,150,170,190,210,420,440,460,480,500,520,540,560,580,600,
     +    620,390,230,250,270,290,310,330,350,370,680,700,720),ktrack(i)
          goto 630
   10     stracki=strack(i)
          do 20 j=1,napx
            xv(1,j)=xv(1,j)+stracki*yv(1,j)
            xv(2,j)=xv(2,j)+stracki*yv(2,j)
   20     continue
          goto 630
C--HORIZONTAL DIPOLE
   30     do 40 j=1,napx
            yv(1,j)=yv(1,j)+strackc(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+stracks(i)*oidpsv(j)
   40     continue
          goto 620
C--NORMAL QUADRUPOLE
   50     do 60 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
   60     continue
          goto 620
C--NORMAL SEXTUPOLE
   70     do 80 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
   80     continue
          goto 620
C--NORMAL OCTUPOLE
   90     do 100 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
  100     continue
          goto 620
C--NORMAL DECAPOLE
  110     do 120 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
  120     continue
          goto 620
C--NORMAL DODECAPOLE
  130     do 140 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
  140     continue
          goto 620
C--NORMAL 14-POLE
  150     do 160 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
  160     continue
          goto 620
C--NORMAL 16-POLE
  170     do 180 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
  180     continue
          goto 620
C--NORMAL 18-POLE
  190     do 200 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
  200     continue
          goto 620
C--NORMAL 20-POLE
  210     do 220 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+
     &      stracks(i)*crkve)
  220     continue
          goto 620
  230     continue
          do 240 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)
     +      +dpsv1(j))*dki(ix,1)*tiltc(i)
     +      +c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)
     +      +dpsv1(j))*dki(ix,1)*tilts(i)
     +      +c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  240     continue
          goto 620
  250     continue
          do 260 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)
     +      +dpsv1(j))*dki(ix,1)*tiltc(i)
     +      +c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)
     +      +dpsv1(j))*dki(ix,1)*tilts(i)
     +      +c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  260     continue
          goto 390
  270     continue
          do 280 j=1,napx
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)
     +      +c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)
     +      +c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  280     continue
          goto 620
  290     continue
          do 300 j=1,napx
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)
     +      +c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)
     +      +c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  300     continue
          goto 390
  310     continue
          do 320 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)
     +      -dpsv1(j))*dki(ix,2)*tilts(i)
     +      +c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)
     +      -dpsv1(j))*dki(ix,2)*tiltc(i)
     +      -c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  320     continue
          goto 620
  330     continue
          do 340 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)
     +      -dpsv1(j))*dki(ix,2)*tilts(i)
     +      +c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)
     +      -dpsv1(j))*dki(ix,2)*tiltc(i)
     +      -c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  340     continue
          goto 390
  350     continue
          do 360 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)
     +      +c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)
     +      -c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  360     continue
          goto 620
  370     continue
          do 380 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)
     +      +c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)
     +      -c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  380     continue
  390     r0=ek(ix)
          nmz=nmu(ix)
          if(nmz.ge.2) then
            do 410 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
              yv1j=bbiv(1,1,i)+bbiv(2,1,i)*xlvj+aaiv(2,1,i)*zlvj
              yv2j=aaiv(1,1,i)-bbiv(2,1,i)*zlvj+aaiv(2,1,i)*xlvj
              crkve=xlvj
              cikve=zlvj
                do 400 k=3,nmz
                  crkveuk=crkve*xlvj-cikve*zlvj
                  cikve=crkve*zlvj+cikve*xlvj
                  crkve=crkveuk
                  yv1j=yv1j+bbiv(k,1,i)*crkve+aaiv(k,1,i)*cikve
                  yv2j=yv2j-bbiv(k,1,i)*cikve+aaiv(k,1,i)*crkve
  400           continue
              yv(1,j)=yv(1,j)+(tiltc(i)*yv1j-tilts(i)*yv2j)*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*yv2j+tilts(i)*yv1j)*oidpsv(j)
  410       continue
          else
            do 415 j=1,napx
              yv(1,j)=yv(1,j)+(tiltc(i)*bbiv(1,1,i)-
     +        tilts(i)*aaiv(1,1,i))*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*aaiv(1,1,i)+
     +        tilts(i)*bbiv(1,1,i))*oidpsv(j)
  415       continue    
          endif
          goto 620
C--SKEW ELEMENTS
C--VERTICAL DIPOLE
  420     do 430 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+strackc(i)*oidpsv(j)
  430     continue
          goto 620
C--SKEW QUADRUPOLE
  440     do 450 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  450     continue
          goto 620
C--SKEW SEXTUPOLE
  460     do 470 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  470     continue
          goto 620
C--SKEW OCTUPOLE
  480     do 490 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  490     continue
          goto 620
C--SKEW DECAPOLE
  500     do 510 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  510     continue
          goto 620
C--SKEW DODECAPOLE
  520     do 530 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  530     continue
          goto 620
C--SKEW 14-POLE
  540     do 550 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  550     continue
          goto 620
C--SKEW 16-POLE
  560     do 570 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  570     continue
          goto 620
C--SKEW 18-POLE
  580     do 590 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  590     continue
          goto 620
C--SKEW 20-POLE
  600     do 610 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+
     &      (xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+
     &      (xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            crkveuk=crkve*xlv(j)-cikve*zlv(j)
            cikve=crkve*zlv(j)+cikve*xlv(j)
            crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-
     &      stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+
     &      stracks(i)*cikve)
  610     continue
          goto 620
  680     continue
          do 690 j=1,napx
              crkveb(j)=xv(1,j)-clobeam(1,j,ix)+ed(ix)
              cikveb(j)=xv(2,j)-clobeam(2,j,ix)+ek(ix)
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)
     &      goto 690
            tkb(j)=rho2b(j)/(two*sigman2(1,j,ix))
            yv(1,j)=yv(1,j)+oidpsv(j)*(strack(i)*crkveb(j)/rho2b(j)*
     &      (one-exp(-tkb(j)))-beamoff(1,j,ix))
            yv(2,j)=yv(2,j)+oidpsv(j)*(strack(i)*cikveb(j)/rho2b(j)*
     &      (one-exp(-tkb(j)))-beamoff(2,j,ix))
  690     continue
          goto 620
  700     continue
          if(ibtyp.eq.0) then
            do 701 j=1,napx
              r2b(j)=two*(sigman2(1,j,ix)-sigman2(2,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=xv(1,j)-clobeam(1,j,ix)+ed(ix)
              cikveb(j)=xv(2,j)-clobeam(2,j,ix)+ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*
     &        cbzb(j))*sign(one,crkveb(j))-beamoff(1,j,ix))
              yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*
     &        cbxb(j))*sign(one,cikveb(j))-beamoff(2,j,ix))
  701       continue
          else if(ibtyp.eq.1) then
            do 702 j=1,napx
              r2b(j)=two*(sigman2(1,j,ix)-sigman2(2,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=xv(1,j)-clobeam(1,j,ix)+ed(ix)
              cikveb(j)=xv(2,j)-clobeam(2,j,ix)+ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
  702       continue
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do 703 j=1,napx
              yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*
     &        cbzb(j))*sign(one,crkveb(j))-beamoff(1,j,ix))
              yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*
     &        cbxb(j))*sign(one,cikveb(j))-beamoff(2,j,ix))
  703       continue
          endif
          goto 620
  720     continue
          if(ibtyp.eq.0) then
            do 721 j=1,napx
              r2b(j)=two*(sigman2(2,j,ix)-sigman2(1,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=xv(1,j)-clobeam(1,j,ix)+ed(ix)
              cikveb(j)=xv(2,j)-clobeam(2,j,ix)+ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*
     &        cbzb(j))*sign(one,crkveb(j))-beamoff(1,j,ix))
              yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*
     &        cbxb(j))*sign(one,cikveb(j))-beamoff(2,j,ix))
  721       continue
          else if(ibtyp.eq.1) then
            do 722 j=1,napx
              r2b(j)=two*(sigman2(2,j,ix)-sigman2(1,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=xv(1,j)-clobeam(1,j,ix)+ed(ix)
              cikveb(j)=xv(2,j)-clobeam(2,j,ix)+ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
  722       continue
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do 723 j=1,napx
              yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*
     &        cbzb(j))*sign(one,crkveb(j))-beamoff(1,j,ix))
              yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*
     &        cbxb(j))*sign(one,cikveb(j))-beamoff(2,j,ix))
  723       continue
          endif
  620     continue
          kpz=abs(kp(ix))
          if(kpz.eq.0) goto 630
          if(kpz.eq.1) then
            call lostpar2(i,ix,nthinerr)
            if(nthinerr.ne.0) return
            goto 630
          endif
          if(kpz.eq.3) then
            call lostpar3(i,ix,nthinerr)
            if(nthinerr.ne.0) return
          endif
  630   continue
        call lostpart(nthinerr)
        if(nthinerr.ne.0) return
        if(ntwin.ne.2) call dist1
        if(mod(n,nwr(4)).eq.0) call write6(n)
  640 continue
      return
      end

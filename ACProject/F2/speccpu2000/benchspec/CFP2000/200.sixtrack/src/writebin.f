C ANFANG - NEBENPROGRAMM -
C-----------------------------------------------------------------------
      subroutine writebin(nthinerr)
C-----------------------------------------------------------------------
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
      do 10 ia=1,napx
        if(.not.pstop(nlostp(ia)).and..not.pstop(nlostp(ia)+1).and.
     +  (mod(nlostp(ia),2).ne.0)) then
          ia2=(nlostp(ia)+1)/2
          ie=ia+1
          if(ntwin.ne.2) then
            write(91-ia2,iostat=ierro)
     +      numx,nlostp(ia),dam(ia),
     +      xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),sigmv(ia),dpsv(ia),e0
            call flush(91-ia2)
          else
            write(91-ia2,iostat=ierro)
     +      numx,nlostp(ia),dam(ia),
     +      xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),sigmv(ia),dpsv(ia),e0,
     +      nlostp(ia)+1,dam(ia),
     +      xv(1,ie),yv(1,ie),xv(2,ie),yv(2,ie),sigmv(ie),dpsv(ie),e0
            call flush(91-ia2)
          endif
          if(ierro.ne.0) then
            write(6,*)
            write(6,*) '*** ERROR ***,PROBLEMS WRITTING TO FILE # : ',
     +      91-ia2
            write(6,*) 'ERROR CODE : ',ierro
            write(6,*)
            call flush(6)
            call flush(12)
            nthinerr=3000
            return
          endif
        endif
   10 continue
      return
      end

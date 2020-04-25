C ANFANG - NEBENPROGRAMM -
C-----------------------------------------------------------------------
      subroutine trauthck(nthinerr)
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
      dimension nbeaux(nele)
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
      do 5 i=1,npart
        nlostp(i)=i
   5  continue
      do 10 i=1,nblz
        ktrack(i)=0
        strack(i)=zero
        strackc(i)=zero
        stracks(i)=zero
   10 continue
c--beam-beam element
      if(nbeam.ge.1) then
        do 15 i=1,nele
          nbeaux(i)=0
   15   continue
        do 16 ix=1,nele
          do 16 j=1,napx
            if(kz(ix).eq.20) then
c--round beam
              if(sigman(1,j,ix).eq.sigman(2,j,ix)) then
                if(nbeaux(ix).eq.2.or.nbeaux(ix).eq.3)
     &          then
                  call error(89)
                else
                  nbeaux(ix)=1
                  sigman2(1,j,ix)=sigman(1,j,ix)**2
                endif
              endif
c--elliptic beam x>z
              if(sigman(1,j,ix).gt.sigman(2,j,ix)) then
                if(nbeaux(ix).eq.1.or.nbeaux(ix).eq.3)
     &          then
                  call error(89)
                else
                  nbeaux(ix)=2
                  sigman2(1,j,ix)=sigman(1,j,ix)**2
                  sigman2(2,j,ix)=sigman(2,j,ix)**2
                  sigmanq(1,j,ix)=sigman(1,j,ix)/sigman(2,j,ix)
                  sigmanq(2,j,ix)=sigman(2,j,ix)/sigman(1,j,ix)
                endif
              endif
c--elliptic beam z>x
              if(sigman(1,j,ix).lt.sigman(2,j,ix)) then
                if(nbeaux(ix).eq.1.or.nbeaux(ix).eq.2)
     &          then
                  call error(89)
                else
                  nbeaux(ix)=3
                  sigman2(1,j,ix)=sigman(1,j,ix)**2
                  sigman2(2,j,ix)=sigman(2,j,ix)**2
                  sigmanq(1,j,ix)=sigman(1,j,ix)/sigman(2,j,ix)
                  sigmanq(2,j,ix)=sigman(2,j,ix)/sigman(1,j,ix)
                endif
              endif
            endif
   16   continue
      endif
      do 290 i=1,iu
        if(mout2.eq.1.and.i.eq.1) call write4
        ix=ic(i)
        if(ix.gt.nblo) goto 30
        ktrack(i)=1
        do 20 jb=1,mel(ix)
          jx=mtyp(ix,jb)
          strack(i)=strack(i)+el(jx)
   20   continue
        if(abs(strack(i)).le.pieni) ktrack(i)=31
        goto 290
   30   ix=ix-nblo
        kpz=abs(kp(ix))
        if(kpz.eq.6) then
          ktrack(i)=2
          goto 290
        endif
   40   kzz=kz(ix)
        if(kzz.eq.0) then
          ktrack(i)=31
          goto 290
        endif
c--beam-beam element
        if(kzz.eq.20.and.nbeam.ge.1) then
          strack(i)=two*crad*partnum*gammar*c1e6
          if(abs(strack(i)).le.pieni) then
            ktrack(i)=31
            goto 290
          endif
          if(nbeaux(ix).eq.1) then
            ktrack(i)=41
            if(ibeco.eq.1) then
              do 42 j=1,napx
              crkveb(j)=ed(ix)
              cikveb(j)=ek(ix)
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)
     &  goto 42
            tkb(j)=rho2b(j)/(two*sigman2(1,j,ix))
                beamoff(1,j,ix)=strack(i)*crkveb(j)/rho2b(j)*
     &          (one-exp(-tkb(j)))
                beamoff(2,j,ix)=strack(i)*cikveb(j)/rho2b(j)*
     &          (one-exp(-tkb(j)))
   42         continue
            endif
          endif
          if(nbeaux(ix).eq.2) then
            ktrack(i)=42
            if(ibeco.eq.1) then
            if(ibtyp.eq.0) then
            do 43 j=1,napx
              r2b(j)=two*(sigman2(1,j,ix)-sigman2(2,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=ed(ix)
              cikveb(j)=ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              beamoff(1,j,ix)=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*
     &        sign(one,crkveb(j))
              beamoff(2,j,ix)=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*
     &        sign(one,cikveb(j))
   43       continue
            else if(ibtyp.eq.1) then
            do 44 j=1,napx
              r2b(j)=two*(sigman2(1,j,ix)-sigman2(2,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=ed(ix)
              cikveb(j)=ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
   44       continue
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do 45 j=1,napx
              beamoff(1,j,ix)=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*
     &        sign(one,crkveb(j))
              beamoff(2,j,ix)=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*
     &        sign(one,cikveb(j))
   45       continue
            endif
            endif
          endif
          if(nbeaux(ix).eq.3) then
            ktrack(i)=43
            if(ibeco.eq.1) then
            if(ibtyp.eq.0) then
            do 46 j=1,napx
              r2b(j)=two*(sigman2(2,j,ix)-sigman2(1,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=ed(ix)
              cikveb(j)=ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              beamoff(1,j,ix)=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*
     &        sign(one,crkveb(j))
              beamoff(2,j,ix)=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*
     &        sign(one,cikveb(j))
   46       continue
            else if(ibtyp.eq.1) then
            do 47 j=1,napx
              r2b(j)=two*(sigman2(2,j,ix)-sigman2(1,j,ix))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              crkveb(j)=ed(ix)
              cikveb(j)=ek(ix)
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,j,ix)+
     &        cikveb(j)*cikveb(j)/sigman2(2,j,ix))/two
              xbb(j)=sigmanq(2,j,ix)*xrb(j)
              zbb(j)=sigmanq(1,j,ix)*zrb(j)
   47       continue
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do 48 j=1,napx
              beamoff(1,j,ix)=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*
     &        sign(one,crkveb(j))
              beamoff(2,j,ix)=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*
     &        sign(one,cikveb(j))
   48       continue
            endif
            endif
          endif
          goto 290
        endif
        if(mout2.eq.1.and.icextal(i).ne.0) then
          write(27,'(a16,2x,1p,2d14.6,d17.9)') bez(ix),extalign(i,1),
     &    extalign(i,2),extalign(i,3)
        endif
        if(kzz.lt.0) goto 180
        goto(50,60,70,80,90,100,110,120,130,140,150),kzz
        ktrack(i)=31
        goto 290
   50   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=11
        strack(i)=smiv(1,i)*c1e3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   60   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=12
        strack(i)=smiv(1,i)
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   70   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=13
        strack(i)=smiv(1,i)*c1m3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   80   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=14
        strack(i)=smiv(1,i)*c1m6
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   90   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=15
        strack(i)=smiv(1,i)*c1m9
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  100   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=16
        strack(i)=smiv(1,i)*c1m12
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  110   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=17
        strack(i)=smiv(1,i)*c1m15
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  120   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=18
        strack(i)=smiv(1,i)*c1m18
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  130   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=19
        strack(i)=smiv(1,i)*c1m21
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  140   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=20
        strack(i)=smiv(1,i)*c1m24
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  150   r0=ek(ix)
        nmz=nmu(ix)
        if(abs(r0).le.pieni.or.nmz.eq.0) then
          if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).le.pieni) then
            ktrack(i)=31
          else if(abs(dki(ix,1)).gt.pieni.and.abs(dki(ix,2)).le.pieni)
     +    then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=33
              strack(i)=dki(ix,1)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=35
              strack(i)=dki(ix,1)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          else if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).gt.pieni)
     +    then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=37
              strack(i)=dki(ix,2)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=39
              strack(i)=dki(ix,2)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          endif
        else
          if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).le.pieni) then
            ktrack(i)=32
          else if(abs(dki(ix,1)).gt.pieni.and.abs(dki(ix,2)).le.pieni)
     +    then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=34
              strack(i)=dki(ix,1)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=36
              strack(i)=dki(ix,1)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          else if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).gt.pieni)
     +    then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=38
              strack(i)=dki(ix,2)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=40
              strack(i)=dki(ix,2)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          endif
        endif
        if(abs(r0).le.pieni.or.nmz.eq.0) goto 290
        if(mout2.eq.1) then
          benkcc=ed(ix)*benkc(irm(ix))
          r0a=one
          r000=r0*r00(irm(ix))
          do 160 j=1,mmul
            fake(1,j)=bbiv(j,1,i)*r0a/benkcc
            fake(2,j)=aaiv(j,1,i)*r0a/benkcc
  160     r0a=r0a*r000
          write(9,'(a16)') bez(ix)
          write(9,'(1p,3d23.15)') (fake(1,j), j=1,3)
          write(9,'(1p,3d23.15)') (fake(1,j), j=4,6)
          write(9,'(1p,3d23.15)') (fake(1,j), j=7,9)
          write(9,'(1p,3d23.15)') (fake(1,j), j=10,12)
          write(9,'(1p,3d23.15)') (fake(1,j), j=13,15)
          write(9,'(1p,3d23.15)') (fake(1,j), j=16,18)
          write(9,'(1p,2d23.15)') (fake(1,j), j=19,20)
          write(9,'(1p,3d23.15)') (fake(2,j), j=1,3)
          write(9,'(1p,3d23.15)') (fake(2,j), j=4,6)
          write(9,'(1p,3d23.15)') (fake(2,j), j=7,9)
          write(9,'(1p,3d23.15)') (fake(2,j), j=10,12)
          write(9,'(1p,3d23.15)') (fake(2,j), j=13,15)
          write(9,'(1p,3d23.15)') (fake(2,j), j=16,18)
          write(9,'(1p,2d23.15)') (fake(2,j), j=19,20)
          do 170 j=1,20
            fake(1,j)=zero
  170     fake(2,j)=zero
        endif
        goto 290
  180   kzz=-kzz
        goto(190,200,210,220,230,240,250,260,270,280),kzz
        ktrack(i)=31
        goto 290
  190   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=21
        strack(i)=smiv(1,i)*c1e3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  200   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=22
        strack(i)=smiv(1,i)
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  210   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=23
        strack(i)=smiv(1,i)*c1m3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  220   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=24
        strack(i)=smiv(1,i)*c1m6
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  230   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=25
        strack(i)=smiv(1,i)*c1m9
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  240   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=26
        strack(i)=smiv(1,i)*c1m12
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  250   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=27
        strack(i)=smiv(1,i)*c1m15
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  260   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=28
        strack(i)=smiv(1,i)*c1m18
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  270   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=29
        strack(i)=smiv(1,i)*c1m21
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  280   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=30
        strack(i)=smiv(1,i)*c1m24
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
  290 continue
      do 300 j=1,napx
        dpsv1(j)=dpsv(j)*c1e3/(one+dpsv(j))
  300 continue
      nwri=nwr(3)
      if(nwri.eq.0) nwri=numl+numlr+1
      if(idp.eq.0.or.ition.eq.0) then
        call thck4d(nthinerr)
      else
        hsy(3)=c1m3*hsy(3)*ition
        do 310 jj=1,nele
          if(kz(jj).eq.12) hsyc(jj)=c1m3*hsyc(jj)*itionc(jj)
  310   continue
        if(abs(phas).ge.pieni) then
          call thck6dua(nthinerr)
        else
          call thck6d(nthinerr)
        endif
      endif
      return
      end

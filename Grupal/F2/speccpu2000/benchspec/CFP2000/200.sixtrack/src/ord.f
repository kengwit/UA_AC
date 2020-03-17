C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine ord
C-----------------------------------------------------------------------
C  ORGANISATION OF BLOCKS, NONLINEAR ELEMENTS AND RANDOM NUMBERS
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
      dimension ilf(nblz),ilfr(nblz),jra(nele,5),iran(nele),inz(nele)
      dimension exterr1(nblz,40),extalig1(nblz,3),icext1(nblz),
     &icextal1(nblz)
      save
C-----------------------------------------------------------------------
      do 10 i=1,nblz
        ilf(i)=0
        ilfr(i)=0
   10 continue
      do 20 i=1,nele
        iran(i)=0
        inz(i)=0
        do 20 j=1,5
          jra(i,j)=0
   20 continue
      if(mper.eq.1) goto 40
      do 30 i=2,mper
        do 30 j=1,mbloz
          ii=(i-1)*mbloz+j
          ihi=j
          if(msym(i).lt.0) ihi=mbloz-j+1
          ic(ii)=msym(i)*ic(ihi)
   30 if(ic(ii).lt.-nblo) ic(ii)=-ic(ii)
C--ORGANISATION OF RANDOM NUMBERS
   40 iu=mper*mbloz
      izu=0
      nra1=nran
      iorg=iorg-1
      if(iorg.ge.0) then
        if(iorg.eq.0) then
          do 50 i=1,iu
            ix=ic(i)
            if(ix.le.nblo) goto 50
            ix=ix-nblo
            kpz=kp(ix)
            kzz=kz(ix)
            if(kpz.eq.6.or.kzz.eq.0) goto 50
            mzu(i)=izu
            izu=izu+3
            if(kzz.eq.11.and.abs(ek(ix)).gt.pieni) izu=izu+2*mmul
            if(izu.gt.nran) call error(30)
   50     continue
        else
          do 70 i=1,iorg
            do 60 j=1,il
              if(bez(j).eq.bezr(1,i)) then
                jra(i,1)=j
                if(kz(j).eq.0) call error(31)
                jra(i,2)=kz(j)
              endif
              if(bez(j).eq.bezr(2,i)) then
                jra(i,3)=j
                if(kz(j).eq.0) call error(31)
                jra(i,4)=kz(j)
              endif
   60       continue
            kzz1=jra(i,2)
            kzz2=jra(i,4)
            if(kzz1.ne.0.and.kzz2.eq.0) then
              jra(i,5)=nra1
              nra1=nra1+mran*3
              if(kzz1.eq.11.and.abs(ek(jra(i,1))).gt.pieni) nra1=nra1
     +        +mran*2*mmul
              if(nra1.gt.nzfz) call error(32)
            endif
            if(kzz1.eq.11.and.(kzz2.ne.11.and.kzz2.ne.0)) call error(33)
   70     continue
          do 110 i=1,iu
            ix=ic(i)
            if(ix.le.nblo) goto 110
            ix=ix-nblo
            kpz=kp(ix)
            kzz=kz(ix)
            if(kpz.eq.6.or.kzz.eq.0) goto 110
            do 80 j=1,iorg
              if(bez(ix).eq.bezr(1,j)) goto 90
   80       continue
            goto 100
   90       jra3=jra(j,3)
            if(jra3.ne.0) then
              mzu(i)=iran(jra3)
              iran(ix)=mzu(i)
            else
              inz(j)=inz(j)+1
              if(inz(j).gt.mran) call error(34)
              mzu(i)=jra(j,5)
              iran(ix)=mzu(i)
              jra(j,5)=jra(j,5)+3
              if(jra(j,2).eq.11) jra(j,5)=jra(j,5)+2*mmul
            endif
            goto 110
  100       mzu(i)=izu
            iran(ix)=izu
            izu=izu+3
            if(kzz.eq.11.and.abs(ek(ix)).gt.pieni) izu=izu+2*mmul
            if(izu.gt.nran) call error(30)
  110     continue
        endif
      else
        do 115 i=1,iu
          ix=ic(i)
          if(ix.le.nblo) goto 115
          ix=ix-nblo
          kpz=kp(ix)
          kzz=kz(ix)
          if(kpz.eq.6.or.kzz.eq.0) goto 115
          izu=izu+3
          if(kzz.eq.11.and.abs(ek(ix)).gt.pieni) izu=izu+2*mmul
          if(izu.gt.nran) call error(30)
  115   continue
      endif
      if(kanf.ne.1) then
C--UMSPEICHERUNG AUF DEN STARTPUNKT
        kanf1=kanf-1
        do 130 i=1,kanf1
          if(iorg.ge.0) ilfr(i)=mzu(i)
          ilf(i)=ic(i)
          icext1(i)=icext(i)
          icextal1(i)=icextal(i)
          extalig1(i,1)=extalign(i,1)
          extalig1(i,2)=extalign(i,2)
          extalig1(i,3)=extalign(i,3)
          do 120 ii=1,40
            exterr1(i,ii)=exterr(i,ii)
  120     continue
  130   continue
        do 150 i=kanf,iu
          if(iorg.ge.0) mzu(i-kanf1)=mzu(i)
          ic(i-kanf1)=ic(i)
          icext(i-kanf1)=icext(i)
          icextal(i-kanf1)=icextal(i)
          extalign(i-kanf1,1)=extalign(i,1)
          extalign(i-kanf1,2)=extalign(i,2)
          extalign(i-kanf1,3)=extalign(i,3)
          do 140 ii=1,40
            exterr(i-kanf1,ii)=exterr(i,ii)
  140     continue
  150   continue
        do 170 i=1,kanf1
          if(iorg.ge.0) mzu(iu-kanf1+i)=ilfr(i)
          ic(iu-kanf1+i)=ilf(i)
          icext(iu-kanf1+i)=icext1(i)
          icextal(iu-kanf1+i)=icextal1(i)
          extalign(iu-kanf1+i,1)=extalig1(i,1)
          extalign(iu-kanf1+i,2)=extalig1(i,2)
          extalign(iu-kanf1+i,3)=extalig1(i,3)
          do 160 ii=1,40
            exterr(iu-kanf1+i,ii)=exterr1(i,ii)
  160     continue
  170   continue
      endif
      izu=0
      do 190 i=1,iu
        ix=ic(i)
        if(ix.le.nblo) goto 190
        ix=ix-nblo
        kpz=kp(ix)
        kzz=kz(ix)
        if(kpz.eq.6.or.kzz.eq.0) goto 190
        if(icextal(i).ne.0) then
          izu=izu+2
          xrms(ix)=one
          zrms(ix)=one
          zfz(izu)=extalign(i,1)
          izu=izu+1
          zfz(izu)=extalign(i,2)
          tiltc(i)=cos(extalign(i,3)*c1m3)
          tilts(i)=sin(extalign(i,3)*c1m3)
        else
          izu=izu+3
        endif
        if(kzz.eq.11.and.abs(ek(ix)).gt.pieni.and.icext(i).ne.0) then
          do 180 j=1,mmul
            izu=izu+1
            zfz(izu)=exterr(i,20+j)
            izu=izu+1
            zfz(izu)=exterr(i,j)
  180     continue
        else if(kzz.eq.11.and.abs(ek(ix)).gt.pieni.and.
     &  icext(i).eq.0) then
          izu=izu+2*mmul
        endif
  190 continue
      return
      end

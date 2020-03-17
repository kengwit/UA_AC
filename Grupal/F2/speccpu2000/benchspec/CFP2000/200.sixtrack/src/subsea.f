C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine subsea(dpp)
C-----------------------------------------------------------------------
C  CALCULATION OF DRIVINGTERMS OF RESONANCES INCLUDING SUBRESONANCE
C  USED FOR SEARCH
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
      integer dj
      dimension t(5,4)
      dimension beta(2),alfa(2),phi(2),phibf(2)
      dimension aa(mmul),bb(mmul)
      dimension qw(2),dpr(5)
      dimension nnf(10),ep(2)
      dimension ab1(10),ab2(10),re(10,18),ip(10,18)
      dimension b(10,10),nz2(9),e(10,10)
      dimension chy(9,18),shy(9,18)
      dimension cr(mmul),ci(mmul)
      save
C-----------------------------------------------------------------------
      ium=5
      do 10 i=1,ium
        dpr(i)=zero
   10 continue
      do 20 i=1,ium
        do 20 j=1,4
          t(i,j)=zero
   20 continue
      do 30 i=1,2
        beta(i)=zero
        alfa(i)=zero
        phi(i)=zero
        phibf(i)=zero
        qw(i)=zero
        ep(i)=zero
   30 continue
      do 40 i=1,10
        nnf(i)=0
        do 40 j=1,18
          ip(i,j)=0
          re(i,j)=zero
   40 continue
      do 50 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   50 continue
      do 100 i=1,9
        nz2(i)=0
        do 90 j=1,18
          chy(i,j)=zero
          shy(i,j)=zero
          do 80 k=1,10
            do 60 ii=1,10
              e(k,ii)=zero
              b(k,ii)=zero
   60       continue
            do 70 l=1,5
              rtc(i,j,k,l)=zero
              rts(i,j,k,l)=zero
   70       continue
   80     continue
   90   continue
  100 continue
      btc=zero
      bts=zero
      phy=zero
      dt=zero
      del=zero
      ns=0
      ik=0
      pie=two*pi
      etl=zero
      radi=totl/pie
      dpr(1)=dpp*c1e3
      call clorb2(dpp)
      call betalf(dpp,qw)
      if(ierro.ne.0) call error(22+ierro)
      call envar(dpp)
C--STARTVALUES OF THE TRAJECTORIES
      do 110 l=1,2
        ll=2*l
        alfa(l)=alf0(l)
        beta(l)=bet0(l)
        t(1,ll-1)=clo(l)
  110 t(1,ll)=clop(l)
      do 120 i=1,4
        do 120 j=1,4
          t(i+1,j)=ta(j,i)
  120 t(i+1,j)=ta(j,i)
C--EP=EMITTANCE IN PI*MM*MRAD
      ep(1)=tam1*tam1/beta(1)
      ep(2)=tam2*tam2/beta(2)
C--SINGLE TURN BLOCKLOOP
      izu=0
      do 740 k=1,iu
        do 130 k1=1,10
          ab1(k1)=zero
  130   ab2(k1)=zero
        ix=ic(k)
        if(ix.gt.nblo) goto 210
        jj=0
        dj=1
        if(ix.gt.0) goto 140
        ix=-ix
        jj=mel(ix)+1
        dj=-1
  140   jm=mel(ix)
C--BLOCKELEMENTLOOP
        do 200 j=1,jm
          jj=jj+dj
          jk=mtyp(ix,jj)
          if(ithick.eq.1.and.kz(jk).ne.0) goto 170
          if(ithick.eq.0.and.kz(jk).ne.0) goto 740
C--PURE DRIFTLENGTH
          etl=etl+el(jk)
          do 150 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=pi2
            endif
            do 150 i=1,ium
  150     t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
          do 160 l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=pi2-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  160     phi(l)=phi(l)+dphi
          goto 200
C--MAGNETELEMENT
  170     continue
          if(kz(jk).ne.8) etl=etl+el(jk)
          do 180 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do 180 i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a(jk,l,5)
  180     t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
          do 190 l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  190     phi(l)=phi(l)+dphi
  200   continue
        goto 740
C--NL-INSERTION
  210   ix=ix-nblo
        qu=zero
        qv=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 740
        kzz=kz(ix)
        if(kzz.eq.0) goto 740
        dyy1=zero
        dyy2=zero
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(t(1,1)-xs)*tiltc(k)+(t(1,3)-zs)*tilts(k)
        zl=-(t(1,1)-xs)*tilts(k)+(t(1,3)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 350
        goto(220,230,240,250,260,270,280,290,300,310,320),kzz
        goto 740
C--HORIZONTAL DIPOLE
  220   ekk=ekk*c1e3
        mpe=20
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
        goto 460
C--NORMAL QUADRUPOLE
  230   continue
        dyy1=ekk*(tiltc(k)*xl+tilts(k)*zl)
        dyy2=ekk*(-tiltc(k)*zl+tilts(k)*xl)
        mpe=20
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        ab1(2)=qu
        ab2(2)=-qv
        goto 460
C--NORMAL SEXTUPOLE
  240   ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*xl+tiltsk*zl)
        qv=ekk*two*(tiltck*zl-tiltsk*xl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltck
        ab2(3)=ekk*tiltsk
        goto 460
C--NORMAL OCTUPOLE
  250   ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        ab2(3)=three*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltck
        ab2(4)=ekk*tiltsk
        goto 460
C--NORMAL DECAPOLE
  260   ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=6*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        ab2(4)=four*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltck
        ab2(5)=ekk*tiltsk
        goto 460
C--NORMAL DODECAPOLE
  270   ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=10*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=10*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        ab2(5)=5*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltck
        ab2(6)=ekk*tiltsk
        goto 460
C--NORMAL 14-POLE
  280   ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=15*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=20*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(5)=15*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        ab2(6)=6*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltck
        ab2(7)=ekk*tiltsk
        goto 460
C--NORMAL 16-POLE
  290   ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=21*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=35*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=35*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=21*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        ab2(7)=7*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltck
        ab2(8)=ekk*tiltsk
        goto 460
C--NORMAL 18-POLE
  300   ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        ab2(7)=28*ekk*(-tiltck5*cxzyi+tiltsk5*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=56*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=70*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=56*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=28*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        ab2(8)=8*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltck
        ab2(9)=ekk*tiltsk
        goto 460
C--NORMAL 20-POLE
  310   ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        goto 460
  320   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)
     +      +dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)
     +      +dppi*tilts(k)
            do 323 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  323       continue
          else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)
     +      +dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)
     +      +dppi*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)+(qu*zl-dppi*dpp)*tilts(k)
     +      +dppi*tilts(k)
            t(1,4)=t(1,4)+(-qu*zl+dppi*dpp)*tiltc(k)
     +      -dppi*(one-tiltc(k))
            do 326 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  326       continue
          else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)
     +      +dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)
     +      -dppi*(one-tiltc(k))
          endif
        endif
        mpe=9
        mx=0
        if(abs(r0).le.pieni) goto 740
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 740
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        cr(1)=one
        cr(2)=xl
        ci(2)=zl
        cxzyr=xl
        cxzyi=zl
        cxzr=cxzyr
        cxzi=cxzyi
        dyy1=zero
        dyy2=zero
        qu=zero
        qv=zero
        lmin=3
        if(nmz.eq.1) lmin=2
        do 330 l=lmin,mmul
          cr(l)=zero
  330   ci(l)=zero
        do 340 l=1,nmz
          l1=l-1
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
          if(l.gt.2) then
            cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
            cr(l)=cxzyr
            ci(l)=cxzyi
          endif
          dyy1=dyy1+bb(l)*cr(l)+aa(l)*ci(l)
          dyy2=dyy2-bb(l)*ci(l)+aa(l)*cr(l)
          if(l.gt.1.and.ium.ne.1) then
            qu=qu+l1*(bb(l)*cr(l1)+aa(l)*ci(l1))
            qv=qv+l1*(bb(l)*ci(l1)-aa(l)*cr(l1))
          endif
  340   continue
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
        izu=izu+2*mmul-2*nmz
        goto 460
C--SKEW ELEMENTS
  350   kzz=-kzz
        goto(360,370,380,390,400,410,420,430,440,450),kzz
        goto 740
C--VERTICAL DIPOLE
  360   ekk=ekk*c1e3
        mpe=20
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
        goto 460
C--SKEW QUADRUPOLE
  370   continue
        dyy1=ekk*(tiltc(k)*zl-tilts(k)*xl)
        dyy2=ekk*(tiltc(k)*xl+tilts(k)*zl)
        mpe=2
        mx=-1
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        ab1(2)=qu
        ab2(2)=-qv
        goto 460
C--SKEW SEXTUPOLE
  380   ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*zl-tiltsk*xl)
        qv=-ekk*two*(tiltck*xl+tiltsk*zl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltsk
        ab2(3)=ekk*tiltck
        goto 460
C--SKEW OCTUPOLE
  390   ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*zl-tiltsk*xl)
        ab2(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltsk
        ab2(4)=ekk*tiltck
        goto 460
C--SKEW DECAPOLE
  400   ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*zl-tiltsk*xl)
        ab2(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltsk
        ab2(5)=ekk*tiltck
        goto 460
C--SKEW DODECAPOLE
  410   ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*zl-tiltsk*xl)
        ab2(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltsk
        ab2(6)=ekk*tiltck
        goto 460
C--SKEW 14-POLE
  420   ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=15*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*zl-tiltsk*xl)
        ab2(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltsk
        ab2(7)=ekk*tiltck
        goto 460
C--SKEW 16-POLE
  430   ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*zl-tiltsk*xl)
        ab2(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltsk
        ab2(8)=ekk*tiltck
        goto 460
C--SKEW 18-POLE
  440   ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyi-tiltsk5*cxzyr)
        ab2(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*zl-tiltsk*xl)
        ab2(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltsk
        ab2(9)=ekk*tiltck
        goto 460
C--SKEW 20-POLE
  450   ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
  460   t(1,2)=t(1,2)+dyy1
        t(1,4)=t(1,4)+dyy2
        do 470 i=2,ium
          t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
          t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
  470   continue
        do 480 l=1,2
          ll=2*l
          alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
  480   continue
        if(mpe.gt.9.or.(mpe.eq.9.and.nmz.le.1)) goto 740
        if(mpe.lt.nta) goto 740
        if(mpe.gt.nte) mpe=nte
        if(nta.gt.2) goto 500
        if(mx.eq.-1.or.mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.
     +  eq.5.or.mx.eq.6.or.mx.eq.7) goto 500
C-----------------------------------------------------------------------
C  SKEW-QUADRUPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
        do 490 l=2,nmz
          l1=l-1
  490   ab2(2)=ab2(2)+l1*(aa(l)*cr(l1)-bb(l)*ci(l1))
  500   b1=beta(1)
        b2=beta(2)
        sb1=sqrt(b1)
        sb2=sqrt(b2)
        b(3,1)=b1
        b(1,3)=b2
        b(2,2)=sb1*sb2
        if(nta.gt.3) goto 520
        if(mpe.eq.2.or.(mpe.eq.9.and.nmz.le.2)) goto 650
        if(mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq
     +  .6.or.mx.eq.7) goto 520
C-----------------------------------------------------------------------
C  REGULAR-SEXTUPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
        l2=1
        do 510 l=3,nmz
          l1=l-2
          ab1(3)=ab1(3)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(3)=ab2(3)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  510   l2=l2*l/l1
  520   b(4,1)=b1*sb1
        b(1,4)=b2*sb2
        b(3,2)=b1*sb2
        b(2,3)=b2*sb1
        if(nta.gt.4) goto 540
        if(mpe.eq.3.or.(mpe.eq.9.and.nmz.le.3)) goto 650
        if(mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq
     +  .7) goto 540
C-----------------------------------------------------------------------
C  REGULAR-OCTUPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
        l2=1
        do 530 l=4,nmz
          l1=l-3
          ab1(4)=ab1(4)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(4)=ab2(4)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  530   l2=l2*l/l1
  540   b(5,1)=b1*b1
        b(1,5)=b2*b2
        b(4,2)=b(3,2)*sb1
        b(2,4)=b(2,3)*sb2
        b(3,3)=b1*b2
        if(nta.gt.5) goto 560
        if(mpe.eq.4.or.(mpe.eq.9.and.nmz.le.4)) goto 650
        if(mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7)
     +  goto 560
C-----------------------------------------------------------------------
C  REGULAR-DEKAPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
        l2=1
        do 550 l=5,nmz
          l1=l-4
          ab1(5)=ab1(5)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(5)=ab2(5)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  550   l2=l2*l/l1
  560   b(6,1)=b(5,1)*sb1
        b(1,6)=b(1,5)*sb2
        b(5,2)=b(4,2)*sb1
        b(2,5)=b(2,4)*sb2
        b(4,3)=b(4,2)*sb2
        b(3,4)=b(2,4)*sb1
        if(nta.gt.6) goto 580
        if(mpe.eq.5.or.(mpe.eq.9.and.nmz.le.5)) goto 650
        if(mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 580
C-----------------------------------------------------------------------
C  REGULAR-12-POLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
        l2=1
        do 570 l=6,nmz
          l1=l-5
          ab1(6)=ab1(6)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(6)=ab2(6)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  570   l2=l2*l/l1
  580   b(7,1)=b(6,1)*sb1
        b(1,7)=b(1,6)*sb2
        b(6,2)=b(5,2)*sb1
        b(2,6)=b(2,5)*sb2
        b(5,3)=b(5,2)*sb2
        b(3,5)=b(2,5)*sb1
        b(4,4)=b(3,4)*sb1
        if(nta.gt.7) goto 600
        if(mpe.eq.6.or.(mpe.eq.9.and.nmz.le.6)) goto 650
        if(mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 600
C-----------------------------------------------------------------------
C  REGULAR-14-POLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
        l2=1
        do 590 l=7,nmz
          l1=l-6
          ab1(7)=ab1(7)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(7)=ab2(7)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  590   l2=l2*l/l1
  600   b(8,1)=b(7,1)*sb1
        b(1,8)=b(1,7)*sb2
        b(7,2)=b(7,1)*sb2
        b(2,7)=b(1,7)*sb1
        b(6,3)=b(5,3)*sb1
        b(3,6)=b(3,5)*sb2
        b(5,4)=b(4,4)*sb1
        b(4,5)=b(4,4)*sb2
        if(nta.gt.8) goto 620
        if(mpe.eq.7.or.(mpe.eq.9.and.nmz.le.7)) goto 650
        if(mx.eq.6.or.mx.eq.7) goto 620
C-----------------------------------------------------------------------
C  REGULAR-16-POLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
        l2=1
        do 610 l=8,nmz
          l1=l-7
          ab1(8)=ab1(8)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(8)=ab2(8)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  610   l2=l2*l/l1
  620   b(9,1)=b(8,1)*sb1
        b(1,9)=b(1,8)*sb2
        b(8,2)=b(8,1)*sb2
        b(2,8)=b(1,8)*sb1
        b(7,3)=b(7,2)*sb2
        b(3,7)=b(2,7)*sb1
        b(6,4)=b(6,3)*sb2
        b(4,6)=b(3,6)*sb1
        b(5,5)=b(4,5)*sb1
        if(mpe.eq.8.or.(mpe.eq.9.and.nmz.le.8)) goto 650
        if(mx.eq.7) goto 640
C-----------------------------------------------------------------------
C  REGULAR-18-POLE
C-----------------------------------------------------------------------
        l2=1
        do 630 l=9,nmz
          l1=l-8
          ab1(9)=ab1(9)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(9)=ab2(9)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  630   l2=l2*l/l1
  640   b(10,1)=b(9,1)*sb1
        b(1,10)=b(1,9)*sb2
        b(9,2)=b(9,1)*sb2
        b(2,9)=b(1,9)*sb1
        b(8,3)=b(8,2)*sb2
        b(3,8)=b(2,8)*sb1
        b(4,7)=b(3,7)*sb1
        b(7,4)=b(7,3)*sb2
        b(5,6)=b(4,6)*sb1
        b(6,5)=b(6,4)*sb2
C-----------------------------------------------------------------------
  650   do 670 np=1,mpe
          n2e=2*np
          do 660 nv=1,n2e
            n2=nv-np
            nn2=abs(n2)
            nn1=np-nn2
            re1=nn1*qxt+n2*qzt
            ip(np,nv)=int(re1+half)+ipt
            if(-re1.gt.pieni) ip(np,nv)=-int(abs(re1)+half)-ipt
C--RE=DISTANCE FROM THE RESONANCE
            re(np,nv)=re1-ip(np,nv)
            res=re(np,nv)/radi
            chy(np,nv)=cos(nn1*phi(1)+n2*phi(2)-res*etl)
            shy(np,nv)=sin(nn1*phi(1)+n2*phi(2)-res*etl)
  660     continue
  670   continue
        do 730 np=nta,mpe
          np2=np
          nkk=0
  680     nkk=nkk+1
          n2e=2*np2
          do 720 i=1,nkk
            do 710 nv=1,n2e
              nn2=abs(nv-np2)
              nv1=np2-nn2+(i-1)*2+1
              nv2=np-nv1+2
              rn2=nn2*half
C--EVENESS OF N2
              mm=0
              gerad=rn2-aint(rn2)
              if(abs(gerad).le.pieni) mm=1
C--MM=0 =>N2 UNEVEN, MM=1 => N2 EVEN
              if (mm.eq.0) goto 690
              btc=ab1(np)*b(nv1,nv2)*chy(np2,nv)
              bts=ab1(np)*b(nv1,nv2)*shy(np2,nv)
              goto 700
  690         btc=ab2(np)*b(nv1,nv2)*chy(np2,nv)
              bts=ab2(np)*b(nv1,nv2)*shy(np2,nv)
  700         rtc(np2,nv,np,i)=rtc(np2,nv,np,i)+btc
              rts(np2,nv,np,i)=rts(np2,nv,np,i)+bts
  710       continue
  720     continue
          np2=np2-2
          if(np2.ge.1) goto 680
  730   continue
  740 continue
      nnf(1)=1
      nnf(2)=1
      nnf(3)=2
      nz2(2)=2
      sea=sqrt(ep(1))
      seb=sqrt(ep(2))
      ea=ep(1)
      eb=ep(2)
      e(3,1)=one/eb
      e(1,3)=one/ea
      e(2,2)=one/seb/sea
      nnf(4)=6
      nz2(3)=4
      e(4,1)=sea/eb
      e(1,4)=seb/ea
      e(3,2)=one/seb
      e(2,3)=one/sea
      nnf(5)=24
      nz2(4)=8
      e(5,1)=ea/eb
      e(1,5)=eb/ea
      e(4,2)=sea/seb
      e(2,4)=seb/sea
      e(3,3)=one
      nnf(6)=120
      nz2(5)=16
      e(6,1)=e(5,1)*sea
      e(1,6)=e(1,5)*seb
      e(5,2)=ea/seb
      e(2,5)=eb/sea
      e(4,3)=sea
      e(3,4)=seb
      nnf(7)=720
      nz2(6)=32
      e(7,1)=e(6,1)*sea
      e(1,7)=e(1,6)*seb
      e(6,2)=e(5,2)*sea
      e(2,6)=e(2,5)*seb
      e(5,3)=ea
      e(3,5)=eb
      e(4,4)=sea*seb
      nnf(8)=5040
      nz2(7)=64
      e(8,1)=e(7,1)*sea
      e(1,8)=e(1,7)*seb
      e(7,2)=e(6,2)*sea
      e(2,7)=e(2,6)*seb
      e(6,3)=ea*sea
      e(3,6)=eb*seb
      e(5,4)=ea*seb
      e(4,5)=sea*eb
      nnf(9)=40320
      nz2(8)=128
      e(9,1)=e(8,1)*sea
      e(1,9)=e(1,8)*seb
      e(8,2)=e(7,2)*sea
      e(2,8)=e(2,7)*seb
      e(7,3)=ea*ea
      e(3,7)=eb*eb
      e(6,4)=e(5,4)*sea
      e(4,6)=e(4,5)*seb
      e(5,5)=ea*eb
      nnf(10)=362880
      nz2(9)=256
      e(10,1)=e(9,1)*sea
      e(1,10)=e(1,9)*seb
      e(9,2)=e(8,2)*sea
      e(2,9)=e(2,8)*seb
      e(8,3)=e(7,3)*sea
      e(3,8)=e(3,7)*seb
      e(7,4)=e(6,4)*sea
      e(4,7)=e(4,6)*seb
      e(6,5)=e(5,5)*sea
      e(5,6)=e(5,5)*seb
      do 780 np=nta,nte
        vdt1=nnf(np)/(nz2(np)*pi)
        np2=np
        nkk=0
  750   nkk=nkk+1
        n2e=2*np2
        do 770 i=1,nkk
          do 760 nv=1,n2e
            n2=nv-np2
            nn2=abs(n2)
            nn1=np2-nn2
            nv1=nn1+(i-1)*2+1
            nv2=np-nv1+2
            nv11=nv1-1
            nv21=nv2-1
            nf1=nn1+i
            nf3=nkk-i+1
            nf4=nf3+nn2
            vdt2=vdt1*e(nv1,nv2)/(nnf(nf1)*nnf(i)*nnf(nf3)*nnf(nf4))
            vdt3=nn2*ea+nn1*eb
            if(n2.ge.0) vdt3=n2*nv21*ea+nn1*nv11*eb
            rtc(np2,nv,np,i)=rtc(np2,nv,np,i)*vdt2*vdt3
            rts(np2,nv,np,i)=rts(np2,nv,np,i)*vdt2*vdt3
  760     continue
  770   continue
        np2=np2-2
        if(np2.ge.1) goto 750
  780 continue
      return
      end

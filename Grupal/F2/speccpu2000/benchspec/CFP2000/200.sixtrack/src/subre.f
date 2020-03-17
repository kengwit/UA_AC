C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine subre(dpp)
C-----------------------------------------------------------------------
C  CALCULATION OF RESONANCE- AND SUBRESONANCE-DRIVINGTERMS
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
      dimension t(6,4)
      dimension beta(2),alfa(2),phi(2),phibf(2)
      dimension clo0(2),clop0(2)
      dimension aa(mmul),bb(mmul)
      dimension qw(2),qwc(3),dpr(6)
      dimension nnf(10),ep(2)
      dimension ab1(10),ab2(10),re(10,18),ip(10,18)
      dimension b(10,10),nz2(9),e(10,10)
      dimension chy(9,18),shy(9,18)
      dimension dfac(10),dtu(2,5),dtup(2,5,0:4,0:4)
      dimension cr(mmul),ci(mmul)
      save
C-----------------------------------------------------------------------
      ium=5
      ipl=1
      gtu1=zero
      gtu2=zero
      dfac(1)=one
      dfac(2)=one
      dfac(3)=two
      dfac(4)=6
      dfac(5)=24
      dfac(6)=120
      dfac(7)=720
      dfac(8)=5040
      dfac(9)=40320
      dfac(10)=362880
      if(ipt.eq.1) ipl=3
      do 940 ipcc=1,ipl
        ipc=ipcc-ipl+1
        if(ipt.eq.0) ipc=0
        btc=zero
        bts=zero
        phy=zero
        dt=zero
        del=zero
        ns=0
        ik=0
        do 10 i=1,ium
          dpr(i)=zero
   10   continue
        do 20 i=1,ium
          do 20 j=1,4
            t(i,j)=zero
   20   continue
        do 30 i=1,2
          beta(i)=zero
          alfa(i)=zero
          phi(i)=zero
          phibf(i)=zero
          qw(i)=zero
          qwc(i)=zero
          clo0(i)=zero
          clop0(i)=zero
          ep(i)=zero
   30   continue
        qwc(3)=zero
        do 40 i=1,10
          nnf(i)=0
          do 40 j=1,18
            re(i,j)=zero
            ip(i,j)=0
   40   continue
        do 50 i=1,mmul
          aa(i)=zero
          bb(i)=zero
          cr(i)=zero
          ci(i)=zero
   50   continue
        do 60 i=1,2
          do 60 j=1,5
            dtu(i,j)=zero
   60   continue
        do 70 i=1,5
          do 70 j=0,4
            do 70 k=0,4
              dtup(1,i,j,k)=zero
              dtup(2,i,j,k)=zero
   70   continue
        do 120 i=1,9
          nz2(i)=0
          do 110 j=1,18
            chy(i,j)=zero
            shy(i,j)=zero
            do 100 k=1,10
              do 80 ii=1,10
                e(k,ii)=zero
                b(k,ii)=zero
   80         continue
              do 90 l=1,5
                rtc(i,j,k,l)=zero
                rts(i,j,k,l)=zero
   90         continue
  100       continue
  110     continue
  120   continue
        write(6,10030)
        write(6,10020)
        pie=two*pi
        etl=zero
        radi=totl/pie
        nr=0
        dpr(1)=dpp*c1e3
        dpr(6)=c1e3
        dpp1=dpp+ded
        call clorb(dpp1)
        do 130 l=1,2
          clo0(l)=clo(l)
  130   clop0(l)=clop(l)
        call clorb(dpp)
        do 140 l=1,2
          di0(l)=(clo0(l)-clo(l))/ded
  140   dip0(l)=(clop0(l)-clop(l))/ded
        write(6,10030)
        write(6,10120) (di0(l),dip0(l),l=1,2)
        call betalf(dpp,qw)
        call phasad(dpp,qwc)
        if(ierro.ne.0) call error(22+ierro)
        write(6,10070) dpp,qwc(1),qwc(2)
        call envar(dpp)
C--STARTVALUES OF THE TRAJECTORIES
        do 150 l=1,2
          ll=2*l
          alfa(l)=alf0(l)
          beta(l)=bet0(l)
          t(1,ll-1)=clo(l)
          t(1,ll)=clop(l)
          clo0(l)=clo(l)
  150   clop0(l)=clop(l)
        do 160 i=1,4
          do 160 j=1,4
            t(i+1,j)=ta(j,i)
  160   t(i+1,j)=ta(j,i)
        write(6,10030)
        write(6,10040)
        write(6,10030)
        write(6,10010) nr,'START   ',zero,zero,(beta(l),alfa(l),phi(l),
     +  di0(l),dip0(l),clo0(l),clop0(l),l=1,2)
C--EP=EMITTANCE IN PI*MM*MRAD
        ep(1)=tam1*tam1/beta(1)
        ep(2)=tam2*tam2/beta(2)
        write(6,10050) tam1,ep(1),tam2,ep(2)
        write(6,10030)
C--SINGLE TURN BLOCKLOOP
        izu=0
        do 790 k=1,iu
          do 170 k1=1,10
            ab1(k1)=zero
  170     ab2(k1)=zero
          ix=ic(k)
          if(ix.gt.nblo) goto 250
          jj=0
          dj=1
          if(ix.gt.0) goto 180
          ix=-ix
          jj=mel(ix)+1
          dj=-1
  180     jm=mel(ix)
C--SINGLE TURN BLOCKLOOP
          do 240 j=1,jm
            jj=jj+dj
            jk=mtyp(ix,jj)
            if(ithick.eq.1.and.kz(jk).ne.0) goto 210
            if(ithick.eq.0.and.kz(jk).ne.0) goto 790
C--PURE DRIFTLENGTH
            etl=etl+el(jk)
            do 190 l=1,2
              ll=2*l
              if(abs(t(ll,ll-1)).gt.pieni) then
                phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
              else
                phibf(l)=pi2
              endif
              do 190 i=1,ium
  190       t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
            do 200 l=1,2
              ll=2*l
              beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
              alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
              clo0(l)=t(1,ll-1)
              clop0(l)=t(1,ll)
              if(abs(t(ll,ll-1)).gt.pieni) then
                dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
              else
                dphi=pi2-phibf(l)
              endif
              if(-dphi.gt.pieni) dphi=dphi+pi
  200       phi(l)=phi(l)+dphi/pie
            nr=nr+1
            goto 240
C--MAGNETELEMENT
  210       continue
            if(kz(jk).ne.8) etl=etl+el(jk)
            do 220 l=1,2
              ll=2*l
              if(abs(t(ll,ll-1)).gt.pieni) then
                phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
              else
                phibf(l)=zero
              endif
              do 220 i=1,ium
                puf=t(i,ll-1)
                t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a
     +          (jk,l,5)
  220       t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
            do 230 l=1,2
              ll=2*l
              beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
              alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
              clo0(l)=t(1,ll-1)
              clop0(l)=t(1,ll)
              if(abs(t(ll,ll-1)).gt.pieni) then
                dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
              else
                dphi=-phibf(l)
              endif
              if(-dphi.gt.pieni) dphi=dphi+pi
  230       phi(l)=phi(l)+dphi/pie
            nr=nr+1
  240     continue
          goto 790
C--NL-INSERTION
  250     ix=ix-nblo
          qu=zero
          qv=zero
          kpz=kp(ix)
          if(kpz.eq.6) goto 790
          kzz=kz(ix)
          if(kzz.eq.0) goto 790
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
          if(kzz.lt.0) goto 400
          goto(260,270,280,290,300,310,320,330,340,350,360),kzz
          goto 790
C--HORIZONTAL DIPOLE
  260     ekk=ekk*c1e3
        mpe=20
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
          goto 510
C--NORMAL QUADRUPOLE
  270     continue
        dyy1=ekk*(tiltc(k)*xl+tilts(k)*zl)
        dyy2=ekk*(-tiltc(k)*zl+tilts(k)*xl)
        mpe=20
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        ab1(2)=qu
        ab2(2)=-qv
          goto 510
C--NORMAL SEXTUPOLE
  280     ekk=ekk*c1m3
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
          goto 510
C--NORMAL OCTUPOLE
  290     ekk=ekk*c1m6
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
          call detune(2,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
C--NORMAL DECAPOLE
  300     ekk=ekk*c1m9
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          goto 510
C--NORMAL DODECAPOLE
  310     ekk=ekk*c1m12
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
C--NORMAL 14-POLE
  320     ekk=ekk*c1m15
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          goto 510
C--NORMAL 16-POLE
  330     ekk=ekk*c1m18
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          call detune(4,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
C--NORMAL 18-POLE
  340     ekk=ekk*c1m21
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          call detune(4,ab1(8),ep,beta,dtu,dtup,dfac)
          goto 510
C--NORMAL 20-POLE
  350     ekk=ekk*c1m24
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
          tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
          tiltsk=two*tiltc(k)*tilts(k)
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk4=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck4=tiltckuk
          tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
          tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk6=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck6=tiltckuk
          tiltckuk=tiltck6*tiltc(k)-tiltsk6*tilts(k)
          tiltsk=tiltck6*tilts(k)+tiltsk6*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk8=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck8=tiltckuk
          tiltckuk=tiltck8*tiltc(k)-tiltsk8*tilts(k)
          tiltsk=tiltck8*tilts(k)+tiltsk8*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk10=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck10=tiltckuk
          ekko=ekk
          ekk=ekko*tiltck10
          call detune(5,ekk,ep,beta,dtu,dtup,dfac)
          cxzyr=cxzr*cxzr-cxzi*cxzi
          cxzyi=cxzr*cxzi+cxzi*cxzr
          ekk=36*ekko*(tiltck8*cxzyr+tiltsk8*cxzyi)
          call detune(4,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=126*ekko*(tiltck6*cxzyr+tiltsk6*cxzyi)
          call detune(3,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=84*ekko*(tiltck4*cxzyr+tiltsk4*cxzyi)
          call detune(2,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
  360     r0=ek(ix)
          if(abs(dki(ix,1)).gt.pieni) then
            if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)
     +      +dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)
     +      +dppi*tilts(k)
            do 363 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  363       continue
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
            do 366 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  366       continue
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
          if(abs(r0).le.pieni) goto 790
          nmz=nmu(ix)
          if(nmz.eq.0) then
            izu=izu+2*mmul
            goto 790
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
          do 370 l=lmin,mmul
            aa(l)=zero
            bb(l)=zero
            cr(l)=zero
  370     ci(l)=zero
          do 380 l=1,nmz
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
  380     continue
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
          izu=izu+2*mmul-2*nmz
          do 390 iv=2,5
            tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
            tiltsk=two*tiltc(k)*tilts(k)
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk4=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck4=tiltckuk
            tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
            tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk6=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck6=tiltckuk
            tiltckuk=tiltck6*tiltc(k)-tiltsk6*tilts(k)
            tiltsk=tiltck6*tilts(k)+tiltsk6*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk8=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck8=tiltckuk
            tiltckuk=tiltck8*tiltc(k)-tiltsk8*tilts(k)
            tiltsk=tiltck8*tilts(k)+tiltsk8*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk10=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck10=tiltckuk
            if(iv.eq.2) then
              ekk= tiltck4* bb(4)                     +
     +             tiltsk4*(            -aa(4)       )+
     +         4  *tiltck4*(bb(5) *cr(2)+aa(5) *ci(2))+
     +         4  *tiltsk4*(bb(5) *ci(2)-aa(5) *cr(2))+
     +         10 *tiltck4*(bb(6) *cr(3)+aa(6) *ci(3))+
     +         10 *tiltsk4*(bb(6) *ci(3)-aa(6) *cr(3))+
     +         20 *tiltck4*(bb(7) *cr(4)+aa(7) *ci(4))+
     +         20 *tiltsk4*(bb(7) *ci(4)-aa(7) *cr(4))+
     +         35 *tiltck4*(bb(8) *cr(5)+aa(8) *ci(5))+
     +         35 *tiltsk4*(bb(8) *ci(5)-aa(8) *cr(5))+
     +         56 *tiltck4*(bb(9) *cr(6)+aa(9) *ci(6))+
     +         56 *tiltsk4*(bb(9) *ci(6)-aa(9) *cr(6))+
     +         84 *tiltck4*(bb(10)*cr(7)+aa(10)*ci(7))+
     +         84 *tiltsk4*(bb(10)*ci(7)-aa(10)*cr(7))
            endif
            if(iv.eq.3) then
              ekk= tiltck6* bb(6)                     +
     +             tiltsk6*(            -aa(6)       )+
     +         6  *tiltck6*(bb(7) *cr(2)+aa(7) *ci(2))+
     +         6  *tiltsk6*(bb(7) *ci(2)-aa(7) *cr(2))+
     +         21 *tiltck6*(bb(8) *cr(3)+aa(8) *ci(3))+
     +         21 *tiltsk6*(bb(8) *ci(3)-aa(8) *cr(3))+
     +         56 *tiltck6*(bb(9) *cr(4)+aa(9) *ci(4))+
     +         56 *tiltsk6*(bb(9) *ci(4)-aa(9) *cr(4))+
     +         126*tiltck6*(bb(10)*cr(5)+aa(10)*ci(5))+
     +         126*tiltsk6*(bb(10)*ci(5)-aa(10)*cr(5))
            endif
            if(iv.eq.4) then
              ekk= tiltck8* bb(8)                     +
     +             tiltsk8*(            -aa(8)       )+
     +         8  *tiltck8*(bb(9) *cr(2)+aa(9) *ci(2))+
     +         8  *tiltsk8*(bb(9) *ci(2)-aa(9) *cr(2))+
     +         36 *tiltck8*(bb(10)*cr(3)+aa(10)*ci(3))+
     +         36 *tiltsk8*(bb(10)*ci(3)-aa(10)*cr(3))
            endif
            if(iv.eq.5) then
              ekk= tiltck10*bb(10)-tiltsk10*aa(10)
            endif
            call detune(iv,ekk,ep,beta,dtu,dtup,dfac)
  390     continue
          goto 510
C--SKEW ELEMENTS
  400     kzz=-kzz
          goto(410,420,430,440,450,460,470,480,490,500),kzz
          goto 790
C--VERTICAL DIPOLE
  410     ekk=ekk*c1e3
        mpe=20
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
          goto 510
C--SKEW QUADRUPOLE
  420     continue
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
          goto 510
C--SKEW SEXTUPOLE
  430     ekk=ekk*c1m3
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
          goto 510
C--SKEW OCTUPOLE
  440     ekk=ekk*c1m6
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
          goto 510
C--SKEW DECAPOLE
  450     ekk=ekk*c1m9
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          goto 510
C--SKEW DODECAPOLE
  460     ekk=ekk*c1m12
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          goto 510
C--SKEW 14-POLE
  470     ekk=ekk*c1m15
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          goto 510
C--SKEW 16-POLE
  480     ekk=ekk*c1m18
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          goto 510
C--SKEW 18-POLE
  490     ekk=ekk*c1m21
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
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          call detune(4,ab1(8),ep,beta,dtu,dtup,dfac)
          goto 510
C--SKEW 20-POLE
  500     ekk=ekk*c1m24
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
          tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
          tiltsk=two*tiltc(k)*tilts(k)
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk4=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck4=tiltckuk
          tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
          tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk6=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck6=tiltckuk
          tiltckuk=tiltck6*tiltc(k)-tiltsk6*tilts(k)
          tiltsk=tiltck6*tilts(k)+tiltsk6*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk8=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck8=tiltckuk
          tiltckuk=tiltck8*tiltc(k)-tiltsk8*tilts(k)
          tiltsk=tiltck8*tilts(k)+tiltsk8*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk10=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck10=tiltckuk
          ekko=ekk
          ekk=-ekko*tiltsk10
          call detune(5,ekk,ep,beta,dtu,dtup,dfac)
          cxzyr=cxzr*cxzr-cxzi*cxzi
          cxzyi=cxzr*cxzi+cxzi*cxzr
          ekk=36*ekko*(tiltck8*cxzyi-tiltsk8*cxzyr)
          call detune(4,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=126*ekko*(tiltck6*cxzyi-tiltsk6*cxzyr)
          call detune(3,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=84*ekko*(tiltck4*cxzyi-tiltsk4*cxzyr)
          call detune(2,ekk,ep,beta,dtu,dtup,dfac)
  510     t(1,2)=t(1,2)+dyy1
          t(1,4)=t(1,4)+dyy2
          do 520 i=2,ium
            t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
            t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
  520     continue
          do 530 l=1,2
            ll=2*l
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
  530     clop0(l)=t(1,ll)
          if(mpe.gt.9.or.(mpe.eq.9.and.nmz.le.1)) goto 790
          if(mpe.lt.nta) goto 790
          if(mpe.gt.nte) mpe=nte
          if(nta.gt.2) goto 550
          if(mx.eq.-1.or.mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or
     +    .mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 550
C-----------------------------------------------------------------------
C  SKEW-QUADRUPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
          do 540 l=2,nmz
            l1=l-1
  540     ab2(2)=ab2(2)+l1*(aa(l)*cr(l1)-bb(l)*ci(l1))
  550     b1=beta(1)
          b2=beta(2)
          sb1=sqrt(b1)
          sb2=sqrt(b2)
          b(3,1)=b1
          b(1,3)=b2
          b(2,2)=sb1*sb2
          if(nta.gt.3) goto 570
          if(mpe.eq.2.or.(mpe.eq.9.and.nmz.le.2)) goto 700
          if(mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.
     +    eq.6.or.mx.eq.7) goto 570
C-----------------------------------------------------------------------
C  REGULAR-SEXTUPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
          l2=1
          do 560 l=3,nmz
            l1=l-2
            ab1(3)=ab1(3)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(3)=ab2(3)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  560     l2=l2*l/l1
  570     b(4,1)=b1*sb1
          b(1,4)=b2*sb2
          b(3,2)=b1*sb2
          b(2,3)=b2*sb1
          if(nta.gt.4) goto 590
          if(mpe.eq.3.or.(mpe.eq.9.and.nmz.le.3)) goto 700
          if(mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.
     +    eq.7) goto 590
C-----------------------------------------------------------------------
C  REGULAR-OCTUPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
          l2=1
          do 580 l=4,nmz
            l1=l-3
            ab1(4)=ab1(4)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(4)=ab2(4)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  580     l2=l2*l/l1
  590     b(5,1)=b1*b1
          b(1,5)=b2*b2
          b(4,2)=b(3,2)*sb1
          b(2,4)=b(2,3)*sb2
          b(3,3)=b1*b2
          if(nta.gt.5) goto 610
          if(mpe.eq.4.or.(mpe.eq.9.and.nmz.le.4)) goto 700
          if(mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7)
     +    goto 610
C-----------------------------------------------------------------------
C  REGULAR-DEKAPOLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
          l2=1
          do 600 l=5,nmz
            l1=l-4
            ab1(5)=ab1(5)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(5)=ab2(5)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  600     l2=l2*l/l1
  610     b(6,1)=b(5,1)*sb1
          b(1,6)=b(1,5)*sb2
          b(5,2)=b(4,2)*sb1
          b(2,5)=b(2,4)*sb2
          b(4,3)=b(4,2)*sb2
          b(3,4)=b(2,4)*sb1
          if(nta.gt.6) goto 630
          if(mpe.eq.5.or.(mpe.eq.9.and.nmz.le.5)) goto 700
          if(mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 630
C-----------------------------------------------------------------------
C  REGULAR-12-POLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
          l2=1
          do 620 l=6,nmz
            l1=l-5
            ab1(6)=ab1(6)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(6)=ab2(6)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  620     l2=l2*l/l1
  630     b(7,1)=b(6,1)*sb1
          b(1,7)=b(1,6)*sb2
          b(6,2)=b(5,2)*sb1
          b(2,6)=b(2,5)*sb2
          b(5,3)=b(5,2)*sb2
          b(3,5)=b(2,5)*sb1
          b(4,4)=b(3,4)*sb1
          if(nta.gt.7) goto 650
          if(mpe.eq.6.or.(mpe.eq.9.and.nmz.le.6)) goto 700
          if(mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 650
C-----------------------------------------------------------------------
C  REGULAR-14-POLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
          l2=1
          do 640 l=7,nmz
            l1=l-6
            ab1(7)=ab1(7)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(7)=ab2(7)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  640     l2=l2*l/l1
  650     b(8,1)=b(7,1)*sb1
          b(1,8)=b(1,7)*sb2
          b(7,2)=b(7,1)*sb2
          b(2,7)=b(1,7)*sb1
          b(6,3)=b(5,3)*sb1
          b(3,6)=b(3,5)*sb2
          b(5,4)=b(4,4)*sb1
          b(4,5)=b(4,4)*sb2
          if(nta.gt.8) goto 670
          if(mpe.eq.7.or.(mpe.eq.9.and.nmz.le.7)) goto 700
          if(mx.eq.6.or.mx.eq.7) goto 670
C-----------------------------------------------------------------------
C  REGULAR-16-POLE;MULTIPOLES UP TO 9-TH ORDER
C-----------------------------------------------------------------------
          l2=1
          do 660 l=8,nmz
            l1=l-7
            ab1(8)=ab1(8)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(8)=ab2(8)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  660     l2=l2*l/l1
  670     b(9,1)=b(8,1)*sb1
          b(1,9)=b(1,8)*sb2
          b(8,2)=b(8,1)*sb2
          b(2,8)=b(1,8)*sb1
          b(7,3)=b(7,2)*sb2
          b(3,7)=b(2,7)*sb1
          b(6,4)=b(6,3)*sb2
          b(4,6)=b(3,6)*sb1
          b(5,5)=b(4,5)*sb1
          if(mpe.eq.8.or.(mpe.eq.9.and.nmz.le.8)) goto 700
          if(mx.eq.7) goto 690
C-----------------------------------------------------------------------
C  REGULAR-18-POLE
C-----------------------------------------------------------------------
          l2=1
          do 680 l=9,nmz
            l1=l-8
            ab1(9)=ab1(9)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(9)=ab2(9)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  680     l2=l2*l/l1
  690     b(10,1)=b(9,1)*sb1
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
  700     do 720 np=1,mpe
            n2e=2*np
            do 710 nv=1,n2e
              n2=nv-np
              nn2=abs(n2)
              nn1=np-nn2
              re1=nn1*qxt+n2*qzt
              ip(np,nv)=int(re1+half)+ipc
              if(-re1.gt.pieni) ip(np,nv)=-int(abs(re1)+half)-ipc
C--RE=DISTANCE FROM THE RESONANCE
              re(np,nv)=re1-ip(np,nv)
              res=re(np,nv)/radi
              chy(np,nv)=cos(nn1*pie*phi(1)+n2*pie*phi(2)-res*etl)
              shy(np,nv)=sin(nn1*pie*phi(1)+n2*pie*phi(2)-res*etl)
  710       continue
  720     continue
          do 780 np=nta,mpe
            np2=np
            nkk=0
  730       nkk=nkk+1
            n2e=2*np2
            do 770 i=1,nkk
              do 760 nv=1,n2e
                nn2=abs(nv-np2)
                nv1=np2-nn2+(i-1)*2+1
                nv2=np-nv1+2
                rn2=nn2*half
C--EVENESS OF N2
                mm=0
                gerad=rn2-aint(rn2)
                if(abs(gerad).le.pieni) mm=1
C--MM=0 =>N2 UNEVEN, MM=1 => N2 EVEN
                if (mm.eq.0) goto 740
                btc=ab1(np)*b(nv1,nv2)*chy(np2,nv)
                bts=ab1(np)*b(nv1,nv2)*shy(np2,nv)
                goto 750
  740           btc=ab2(np)*b(nv1,nv2)*chy(np2,nv)
                bts=ab2(np)*b(nv1,nv2)*shy(np2,nv)
  750           rtc(np2,nv,np,i)=rtc(np2,nv,np,i)+btc
                rts(np2,nv,np,i)=rts(np2,nv,np,i)+bts
  760         continue
  770       continue
            np2=np2-2
            if(np2.ge.1) goto 730
  780     continue
          nr=nr+1
  790   continue
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
        write(6,10000)
        write(6,10030)
        write(6,10010) nr,'END     ',etl,zero,(beta(l),alfa(l),phi(l),
     +  di0(l),dip0(l),clo0(l),clop0(l),l=1,2)
        write(6,10030)
        write(6,10110) etl,qwc(1),qwc(2)
        write(6,10030)
        do 800 iv=2,5
          gtu1=gtu1+dtu(1,iv)
          gtu2=gtu2+dtu(2,iv)
  800   continue
        write(6,10150) dtu(1,2),dtu(1,3),dtu(1,4),dtu(1,5),gtu1, dtu
     +  (2,2),dtu(2,3),dtu(2,4),dtu(2,5),gtu2
        do 810 i=1,2
          do 810 j=1,5
            do 810 l=0,4
              do 810 k=0,4
                if(i.eq.2.and.j.eq.1.and.k.eq.1.and.l.eq.1) write
     +          (6,10160)
                if(abs(dtup(i,j,k,l)).gt.pieni) write(6,
     +          '(10X,G16.10,3X,I2,2X,I2)') dtup(i,j,k,l),k,l
  810   continue
        write(6,10060)
        write(6,10030)
        do 880 np=nta,nte
          write(6,10080) np
          write(6,10030)
          vdt1=nnf(np)/(nz2(np)*pi)
          np2=np
          nkk=0
          write(6,10090) np
          goto 830
  820     write(6,10100) np,np2
  830     nkk=nkk+1
          n2e=2*np2
          do 850 i=1,nkk
            do 840 nv=1,n2e
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
              vdt4=vdt3
              if(n2.ge.0) vdt3=n2*nv21*ea+nn1*nv11*eb
              rtc(np2,nv,np,i)=rtc(np2,nv,np,i)*vdt2*vdt3
              rts(np2,nv,np,i)=rts(np2,nv,np,i)*vdt2*vdt3
  840       continue
  850     continue
          do 870 nv=1,n2e
            mis=1
            rc=zero
            rs=zero
            do 860 i=1,nkk
              rc=rc+mis*rtc(np2,nv,np,i)
              rs=rs+mis*rts(np2,nv,np,i)
              mis=-mis
  860       continue
            sdel2=sqrt(rc*rc+rs*rs)
            n22=nv-np2
            write(6,10140) n22,ip(np2,nv),ipc,rc,rs,re(np2,nv),sdel2
  870     continue
          np2=np2-2
          if(np2.ge.1) goto 820
  880   continue
        ntx=nte-2
        write(6,10130)
        do 930 np=1,nte
          write(6,10090) np
          n2e=2*np
          do 920 nv=1,n2e
            n2=nv-np
            nkk=2
            nph=np+2
            min1=-1
  890       min2=min1
            do 900 i=1,nkk
              rtc(np,nv,np,1)=rtc(np,nv,np,1)+min2*rtc(np,nv,nph,i)
              rts(np,nv,np,1)=rts(np,nv,np,1)+min2*rts(np,nv,nph,i)
              min2=-min2
  900       continue
            nph=nph+2
            if(nph.gt.nte) goto 910
            nkk=nkk+1
            min1=-min1
            goto 890
  910       cc=rtc(np,nv,np,1)
            ss=rts(np,nv,np,1)
            sdel=sqrt(cc*cc+ss*ss)
            write(6,10140) n2,ip(np,nv),ipc,cc,ss,re(np,nv),sdel
  920     continue
  930   continue
  940 continue
      call clorb(ded)
      do 950 l=1,2
        clo0(l)=clo(l)
        clop0(l)=clop(l)
  950 continue
      call clorb(zero)
      do 960 l=1,2
        ll=2*l
        di0(l)=(clo0(l)-clo(l))/ded
        dip0(l)=(clop0(l)-clop(l))/ded
  960 continue
C-----------------------------------------------------------------------
      return
10000 format(1x,i4,27x,f7.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6.3,1x,f6.2,1x,
     +f6.3,1x,f7.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6.3,1x,f6.2,1x,f6.3)
10020 format(t5,'---- ENTRY SUBRES ----')
10030 format(131('-'))
10040 format(1h ,'  NR  TYP      L-TOTAL  LENGTH   BETAH  ALFAH  ',
     +' PHIH   DISH  DISPH   CLOH  CLOPH',
     +'   BETAV  ALFAV   PHIV   DISV  DISPV   CLOV  CLOPV'/ 1x,
     +'                 (M)      (M)     (M)           ',
     +'(QE)   (M)   (RAD)   (MM) (MRAD)',
     +'    (M)           (QE)   (M)   (RAD)   (MM) (MRAD)')
10050 format(//7x,'INIT. X-AMPLITUDE=',g15.8,'X-EMITTANCE=',g15.8,/40x,
     +/7x,'INIT. Z-AMPLITUDE=',g15.8,'Z-EMITTANCE=',g15.8,
     +'UNITS ARE (PI X MM X MRAD)'//)
10060 format(//10x,'E=NX*QX+NZ*QZ-P',//10x,'CLOSESET P-VALUE CHANGED ',
     +'BY D-P',//10x,'DELTA-E STANDS FOR THE RESONANCE-WIDTH' //10x)
10070 format(1h /10x,'RELATIVE ENERGY DEVIATION  ',t40,f10.7/ 10x,
     +'TUNES -HORIZONTAL',t40,f10.7/ 10x,'      -VERTICAL  ',t40,f10.7)
10080 format(/10x,'RESONANCE EXCITING MULTIPOLE-ORDER = ',i2)
10090 format(//20x,'RESONANCE-ORDER =',i2/20x,100(1h-)/ 20x,'| NZ |',
     +'   P  | D-P |',2x,'DRIVING-COS ',3x,'|', 2x,'DRIVING-SIN ',3x,'|'
     +, 8x,'E',8x,'|',5x,'DELTA-E',5x,'|')
10100 format(//20x,'RESONANCE-ORDER =',i2,5x,'SUBRESONANCE-ORDER = ',i2,
     +/20x,100(1h-)/ 20x,'| NZ |','   P  | D-P |',2x,'DRIVING-COS ',3x,
     +'|', 2x,'DRIVING-SIN ',3x,'|', 8x,'E',8x,'|',5x,'DELTA-E',5x,'|')
10110 format(/10x,'PRECISE LENGTH OF THE MACHINE : ',f43.33/ /10x,
     +'   PRECISE HORIZONTAL Q-VALUE : ',f43.33/ /10x,
     +'     PRECISE VERTICAL Q-VALUE : ',f43.33/)
10120 format(t8,'  PLANE     DISP(MM)     DISP(MRAD)   '/ t6,'      X  '
     +,2(f12.3,3x)/t10,'  Z  ',2(f12.3,3x)/)
10130 format(//10x,'E=NX*QX+NZ*QZ-P',//10x,'CLOSESET P-VALUE CHANGED ',
     +'BY D-P',//10x,'DELTA-E STANDS FOR THE RESONANCE-WIDTH' //10x,
     +'!!!! ALL SUBRESONANCES ARE INCLUDED !!!! ')
10140 format(20x,'| ',i2,' | ',i4,' | ',i3,' |', g16.8,' |',g16.8,' |',
     +g16.8,' |',g16.8,' |')
10150 format(1h /10x,'NONLINEAR DETUNING  '// 10x,'CHANGE IN QX'/ 10x,
     +' 4. ORDER ',f15.12/ 10x,' 6. ORDER ',f15.12/ 10x,' 8. ORDER ',f15
     +.12/ 10x,'10. ORDER ',f15.12/ 10x,'   TOTAL  ',f15.12/ 10x,
     +'CHANGE IN QZ'/ 10x,' 4. ORDER ',f15.12/ 10x,' 6. ORDER ',f15.12/
     +10x,' 8. ORDER ',f15.12/ 10x,'10. ORDER ',f15.12/ 10x,'   TOTAL  '
     +,f15.12// 10x,'DETUNING ORDER BY ORDER'// 10x,
     +'Qx - COEFFICIENT   Ex  Ez'/ 10x,'-------------------------')
10160 format(1h / 10x,'Qz - COEFFICIENT   Ex  Ez'/ 10x,
     +'-------------------------')
10010 format(1x,i4,1x,a8,1x,f8.2,1x,f7.3,1x, f7.2,1x,f6.2,1x,f6.2,1x,f6.
     +2,1x,f6.3,1x,f6.2,1x,f6.3,1x, f7.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6.
     +3,1x,f6.2,1x,f6.3)
      end

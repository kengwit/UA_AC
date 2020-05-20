C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine decoup
C-----------------------------------------------------------------------
C  DECOUPLING USING MATRIX ELEMENTS
C
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
      dimension aa(6,6),bb(6),dsm(6),sn(6),sen(6),ss(6)
      dimension qwc(3),qw(2),d1(6)
      save
C-----------------------------------------------------------------------
      do 10 i=1,6
        bb(i)=zero
        dsm(i)=zero
        sn(i)=zero
        sen(i)=zero
        ss(i)=zero
        d1(i)=zero
        do 10 j=1,6
          aa(i,j)=zero
   10 continue
      do 20 i=1,3
        qwc(i)=zero
   20 continue
      dpp=zero
      write(6,10000)
      call betalf(dpp,qw)
      call phasad(dpp,qwc)
      sen(1)=ta(3,1)
      sen(2)=ta(3,2)
      sen(3)=ta(4,1)
      sen(4)=ta(4,2)
      if(iskew.eq.1) then
        sen(5)=qwc(1)
        sen(6)=qwc(2)
      endif
      do 30 i=1,6
        if(iskew.eq.2.and.i.gt.4) goto 30
        if(i.le.4) then
          sn(i)=ed(nskew(i))
          dsm(i)=dsm0
          bb(i)=sen(i)
        else
          if (abs(el(nskew(i))).le.pieni) then
            sn(i)=ed(nskew(i))
          else
            sn(i)=ek(nskew(i))
          endif
          dsm(i)=dkq
          bb(i)=sen(i)-qwsk(i-4)
        endif
        ss(i)=sen(i)
   30 continue
      do 100 no=1,itcro
        do 40 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 40
          if(i.le.4) then
            ed(nskew(i))=ed(nskew(i))+dsm(i)
          else
            if (abs(el(nskew(i))).le.pieni) then
              ed(nskew(i))=ed(nskew(i))+dsm(i)
            else
              ek(nskew(i))=ek(nskew(i))+dsm(i)
            endif
          endif
          if(kp(nskew(i)).eq.5) call combel(nskew(i))
          call betalf(dpp,qw)
          call phasad(dpp,qwc)
          aa(i,1)=(ta(3,1)-ss(1))/dsm(i)
          aa(i,2)=(ta(3,2)-ss(2))/dsm(i)
          aa(i,3)=(ta(4,1)-ss(3))/dsm(i)
          aa(i,4)=(ta(4,2)-ss(4))/dsm(i)
          if(iskew.eq.1) then
            aa(i,5)=(qwc(1)-ss(5))/dsm(i)
            aa(i,6)=(qwc(2)-ss(6))/dsm(i)
          endif
          if(i.le.4) then
            ed(nskew(i))=ed(nskew(i))-dsm(i)
          else
            if (abs(el(nskew(i))).le.pieni) then
              ed(nskew(i))=ed(nskew(i))-dsm(i)
            else
              ek(nskew(i))=ek(nskew(i))-dsm(i)
            endif
          endif
          if(kp(nskew(i)).eq.5) call combel(nskew(i))
   40   continue
        if(iskew.eq.1) then
          call loesd(aa,bb,6,6,ierr)
        else if(iskew.eq.2) then
          call loesd(aa,bb,4,4,ierr)
        endif
        if(ierr.eq.1) call error(64)
        do 50 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 50
          if(i.le.4) then
            ed(nskew(i))=ed(nskew(i))-bb(i)
          else
            if (abs(el(nskew(i))).le.pieni) then
              ed(nskew(i))=ed(nskew(i))-bb(i)
            else
              ek(nskew(i))=ek(nskew(i))-bb(i)
            endif
          endif
          if(kp(nskew(i)).eq.5) call combel(nskew(i))
   50   continue
        call betalf(dpp,qw)
        call phasad(dpp,qwc)
        ss(1)=ta(3,1)
        ss(2)=ta(3,2)
        ss(3)=ta(4,1)
        ss(4)=ta(4,2)
        if(iskew.eq.1) then
          ss(5)=qwc(1)
          ss(6)=qwc(2)
        endif
        write(6,10010)
        write(6,10020) no,sen(1),ss(1),sen(2),ss(2),sen(3),ss(3), sen
     +  (4),ss(4)
        write(6,10030) bez(nskew(1)),sn(1),ed(nskew(1)),bez(nskew(2)),sn
     +  (2),ed(nskew(2)),bez(nskew(3)),sn(3),ed(nskew(3)), bez
     +  (nskew(4)),sn(4),ed(nskew(4))
        if(iskew.eq.1) then
          write(6,10010)
          write(6,10040) qwsk(1),qwc(1),qwsk(2),qwc(2)
          if (abs(el(nskew(5))).le.pieni) then
            write(6,10060) sn(5),ed(nskew(5)),nskew(5),sn(6),ed
     +      (nskew(6)), nskew(6)
          else
            write(6,10060) sn(5),ek(nskew(5)),nskew(5),sn(6),ek
     +      (nskew(6)), nskew(6)
          endif
        else if(iskew.eq.2) then
          write(6,10010)
          write(6,10050) qwc(1),qwc(2)
        endif
        do 60 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 60
          if(i.le.4) then
            d1(i)=abs(ss(i))
          else
            d1(i)=abs(ss(i)-qwsk(i-4))
          endif
   60   continue
        do 70 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 70
          if(d1(i).gt.dsi) goto 80
   70   continue
        return
   80   do 90 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 90
          if(i.le.4) then
            bb(i)=ss(i)
          else
            bb(i)=ss(i)-qwsk(i-4)
          endif
   90   continue
  100 continue
C-----------------------------------------------------------------------
      return
10000 format(t5,'---- ENTRY DECOUP ----')
10010 format(1h ,131('-'))
10020 format(/10x,'DECOUPLING ROUTINE  ITERATION #',i2// 30x,
     +'BEFORE         AFTER     DECOUPLING'// 17x,'   M(3,1)      ',2g15
     +.5/ 17x,'   M(3,2)      ',2g15.5/ 17x,'   M(4,1)      ',2g15.5/ 17
     +x,'   M(4,2)      ',2g15.5// 5x,'SKEW QUDRUPOLE STRENGTHS')
10040 format(10x,'Q-VARIATION' / 10x,
     +'Q-VALUE            THEORET.        AFTER     COMPENSATION'/ 10x,
     +'HORIZONTAL     ',2g15.7/ 10x,'VERTICAL       ',2g15.7/)
10050 format(10x,'CURRENT TUNE' / 10x,'Q-VALUE'/ 10x,'HORIZONTAL     ',
     +g15.7/ 10x,'VERTICAL       ',g15.7/)
10060 format(10x,'QUADRU.STRENGTH',2g15.8,'   INDEX ',i3/ 10x,
     +'               ',2g15.8,'         ',i3)
10030 format(14x,a16,2x,g16.10,1x,g16.10/14x,a16,2x,g16.10,1x,
     +g16.10/14x,a16,2x,g16.10,1x,g16.10/14x,a16,2x,g16.10,1x,g16.10)
      end

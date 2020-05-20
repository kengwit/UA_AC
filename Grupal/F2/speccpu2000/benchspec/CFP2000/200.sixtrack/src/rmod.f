C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine rmod(dppr)
C-----------------------------------------------------------------------
C  CALCULATION OF THE STRENGTH OF CORRECTION-ELEMENTS
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
      dimension aa(10,10),bb(10),dsm(10),sn(10),sen(10),ss(10)
      dimension qwc(3),d1(10),irr(12)
      save
C-----------------------------------------------------------------------
      ntao=nta
      nteo=nte
      nta=npp
      nte=npp
      dpp=dppr
      do 10 i=1,10
        bb(i)=zero
        dsm(i)=zero
        sn(i)=zero
        sen(i)=zero
        ss(i)=zero
        d1(i)=zero
        do 10 j=1,10
          aa(i,j)=zero
   10 continue
      do 20 i=1,12
        irr(i)=0
   20 continue
      do 30 i=1,3
        qwc(i)=zero
   30 continue
      k=1
      jj1=0
      jj2=0
      jjr=2*nre
      de2=de0*half
      if(nre.eq.0) goto 50
      write(6,10000)
      write(6,10010) npp,totl,qxt,qzt,tam1
      call resex(dpp)
      do 40 i=1,nre
        i2=2*i
        i1=i2-1
        irr(i1)=ire(i1)
        irr(i2)=ire(i2)
        sn(i1)=ed(irr(i1))
        sn(i2)=ed(irr(i2))
        dsm(i1)=dsm0
        dsm(i2)=dsm0
        write(6,10020) i,nrr(i),ipr(i)
        sen(i1)=dtr(i1)
        bb(i1)=sen(i1)
        sen(i2)=dtr(i2)
        bb(i2)=sen(i2)
        ss(i1)=sen(i1)
        ss(i2)=sen(i2)
   40 continue
      j2=jjr
   50 if(nur.eq.0) goto 70
      write(6,10030) nur
      do 60 i=1,nur
        write(6,10040) nu(i),i
   60 continue
   70 if(nch.eq.0) goto 90
      write(6,10050)
      j1=j2+1
      j2=j2+2
      irr(j1)=ire(7)
      irr(j2)=ire(8)
      sn(j1)=ed(irr(j1))
      sn(j2)=ed(irr(j2))
      dsm(j1)=dsm0
      dsm(j2)=dsm0
      se2=zero
      se11=zero
      se12=zero
      do 80 n=1,5
        dpp=de2*(3-n)
        call clorb2(dpp)
        call phasad(dpp,qwc)
        ox=qwc(1)
        oz=qwc(2)
        se2=se2+dpp*dpp
        se11=se11+ox*dpp
        se12=se12+oz*dpp
   80 continue
      sen(j1)=se11/se2
      sen(j2)=se12/se2
      bb(j1)=sen(j1)
      bb(j2)=sen(j2)
      ss(j1)=sen(j1)
      ss(j2)=sen(j2)
   90 if(nqc.eq.0) goto 100
      write(6,10060)
      j1=j2+1
      j2=j2+2
      jj1=j1
      jj2=j2
      irr(j1)=ire(9)
      irr(j2)=ire(10)
      if (abs(el(irr(j1))).le.pieni.or.abs(el(irr(j2))).le.pieni) then
        sn(j1)=ed(irr(j1))
        sn(j2)=ed(irr(j2))
      else
        sn(j1)=ek(irr(j1))
        sn(j2)=ek(irr(j2))
      endif
      dsm(j1)=dkq
      dsm(j2)=dkq
      dpp=zero
      call clorb2(dpp)
      call phasad(dpp,qwc)
      sen(j1)=qwc(1)
      sen(j2)=qwc(2)
      bb(j1)=sen(j1)-qw0(1)
      bb(j2)=sen(j2)-qw0(2)
      ss(j1)=sen(j1)
      ss(j2)=sen(j2)
  100 do 330 no=1,itcro
        do 160 i=1,j2
          if(i.ne.jj1.and.i.ne.jj2) ed(irr(i))=ed(irr(i))+dsm(i)
          if(i.eq.jj1.or.i.eq.jj2) then
            if (abs(el(irr(i))).le.pieni) then
              ed(irr(i))=ed(irr(i))+dsm(i)
            else
              ek(irr(i))=ek(irr(i))+dsm(i)
            endif
          endif
          if(kp(irr(i)).eq.5) call combel(irr(i))
          if(nre.eq.0) goto 120
          call resex(dpp)
          do 110 j=1,jjr
            aa(i,j)=(dtr(j)-ss(j))/dsm(i)
  110     continue
  120     if(nch.eq.0) goto 140
          j3=jjr+1
          j4=jjr+2
          se2=zero
          se11=zero
          se12=zero
          do 130 n=1,5
            dpp=de2*(3-n)
            call clorb2(dpp)
            call phasad(dpp,qwc)
            ox=qwc(1)
            oz=qwc(2)
            se2=se2+dpp*dpp
            se11=se11+ox*dpp
            se12=se12+oz*dpp
  130     continue
          sen15=se11/se2
          sen16=se12/se2
          aa(i,j3)=(sen15-ss(j3))/dsm(i)
          aa(i,j4)=(sen16-ss(j4))/dsm(i)
  140     if(nqc.eq.0) goto 150
          dpp=zero
          call clorb2(dpp)
          call phasad(dpp,qwc)
          sen17=qwc(1)
          sen18=qwc(2)
          aa(i,j1)=(sen17-ss(j1))/dsm(i)
          aa(i,j2)=(sen18-ss(j2))/dsm(i)
  150     continue
          if(i.eq.jj1.or.i.eq.jj2) then
            if (abs(el(irr(i))).le.pieni) then
              ed(irr(i))=ed(irr(i))-dsm(i)
            else
              ek(irr(i))=ek(irr(i))-dsm(i)
            endif
          endif
          if(i.ne.jj1.and.i.ne.jj2)ed(irr(i))=ed(irr(i))-dsm(i)
          if(kp(irr(i)).eq.5) call combel(irr(i))
  160   continue
        call loesd(aa,bb,j2,10,ierr)
        if(ierr.eq.1) call error(38)
        do 170 i=1,j2
          if(i.eq.jj1.or.i.eq.jj2) then
            if (abs(el(irr(i))).le.pieni) then
              ed(irr(i))=ed(irr(i))-bb(i)
            else
              ek(irr(i))=ek(irr(i))-bb(i)
            endif
          endif
          if(i.ne.jj1.and.i.ne.jj2)ed(irr(i))=ed(irr(i))-bb(i)
          if(kp(irr(i)).eq.5) call combel(irr(i))
  170   continue
        if(nre.eq.0) goto 190
        call resex(dpp)
        do 180 i=1,jjr
          ss(i)=dtr(i)
  180   d1(i)=abs(ss(i))
  190   if(nch.eq.0) goto 210
        se2=zero
        se11=zero
        se12=zero
        do 200 n=1,5
          dpp=de2*(3-n)
          call clorb2(dpp)
          call phasad(dpp,qwc)
          ox=qwc(1)
          oz=qwc(2)
          se2=se2+dpp*dpp
          se11=se11+ox*dpp
          se12=se12+oz*dpp
  200   continue
        ss(j3)=se11/se2
        ss(j4)=se12/se2
        d1(j3)=abs(ss(j3))
        d1(j4)=abs(ss(j4))
  210   if(nqc.eq.0) goto 220
        dpp=zero
        call clorb2(dpp)
        call phasad(dpp,qwc)
        ss(j1)=qwc(1)
        ss(j2)=qwc(2)
        d1(j1)=abs(qwc(1)-qw0(1))
        d1(j2)=abs(qwc(2)-qw0(2))
  220   write(6,10070)
        if(nre.eq.0) goto 270
        write(6,10080) no,nrr(1),sen(1),ss(1),sen(2),ss(2)
        if(nre.eq.1) goto 240
        do 230 i=2,nre
          i2=2*i
          i1=i2-1
  230   write(6,10090) nrr(i),sen(i1),ss(i1),sen(i2),ss(i2)
  240   write(6,10100)
        write(6,10110) bez(irr(1)),sn(1),ed(irr(1)),bez(irr(2)),sn(2),
     +  ed(irr(2))
        if(nre.eq.1) goto 260
        do 250 i=2,nre
          i2=2*i
          i1=i2-1
  250   write(6,10110) bez(irr(i1)),sn(i1),ed(irr(i1)),bez(irr(i2)),sn
     +  (i2), ed(irr(i2))
  260   write(6,10070)
  270   if(nch.eq.0) goto 280
        write(6,10120) sen(j3),ss(j3),sen(j4),ss(j4)
        write(6,10110) bez(irr(j3)),sn(j3),ed(irr(j3)),bez(irr(j4)),sn
     +  (j4), ed(irr(j4))
        write(6,10070)
  280   if(nqc.eq.0) goto 290
        write(6,10130) qw0(1),qwc(1),qw0(2),qwc(2)
        if (abs(el(irr(j1))).le.pieni) then
          write(6,10140) sn(j1),ed(irr(j1)),irr(j1),sn(j2),ed(irr(j2)),
     +    irr(j2)
        else
          write(6,10140) sn(j1),ek(irr(j1)),irr(j1),sn(j2),ek(irr(j2)),
     +    irr(j2)
        endif
  290   do 300 i=1,j2
  300   if(d1(i).gt.dsi) goto 310
        nta=ntao
        nte=nteo
        return
  310   do 320 i=1,j2
  320   bb(i)=ss(i)
        if(nqc.eq.1) bb(j1)=bb(j1)-qw0(1)
        if(nqc.eq.1) bb(j2)=bb(j2)-qw0(2)
  330 continue
      nta=ntao
      nte=nteo
C-----------------------------------------------------------------------
      return
10000 format(t5,'---- ENTRY RMOD ----')
10010 format(/10x,'N=',i1,' IS THE ORDER OF RESONACE, THAT WILL BE',
     +' COMPENSATED'// 10x,'L=',f15.6,'; QX=',f10.5,'; QZ=',f10.5,
     +'; AMAX=',f10.5)
10020 format(/10x,i1,' RESONANCE; NZ=',i2,';CHANGE OF P=',i2)
10030 format(/10x,'NUMBER OF SUBRESONANCES THAT ARE CONSIDERED IS ',i2)
10040 format(/10x,'NU=',i2,' IS THE ',i1,' SUBRESONANCE-MULTIPOLE-ORDER'
     +,i2)
10050 format(/10x,'CHROMATICITY IS COMPENSATED')
10060 format(/10x,'Q-VALUES ARE ADJUSTED')
10070 format(1h ,131('-'))
10080 format(/10x,'RESONANCE-CORRECTION     ITERATION #',i2// 15x,
     +'DRIVING-TERM',13x,'BEFORE         AFTER     COMPENSATION'// 10x,
     +'NZ=',i2,'  COS-COMPONENT  ',2g15.5/ 17x,'SIN-COMPONENT  ',2g15.5/
     +)
10090 format(10x,'NZ=',i2,'  COS-COMPONENT  ',2g15.5/ 17x,
     +'SIN-COMPONENT  ',2g15.5/)
10100 format(10x,'  ELEMENT NAME'/)
10130 format(10x,'Q-VARIATION' / 10x,
     +'Q-VALUE            THEORET.        AFTER     COMPENSATION'/ 10x,
     +'HORIZONTAL     ',2g15.7/ 10x,'VERTICAL       ',2g15.7/)
10140 format(10x,'QUADRU.STRENGTH',2g15.8,'   INDEX ',i3/ 10x,
     +'               ',2g15.8,'         ',i3)
10120 format(10x,'CHROMATICITY-CORRECTION'/ 15x,'CHROMATICITY',13x,
     +'BEFORE         AFTER     COMPENSATION'// 19x,'HORIZONTAL   ',2g15
     +.5/ 19x,'VERTICAL     ',2g15.5/ 10x,'   SEXTUPOLE'/)
10110 format(14x,a16,2x,g16.10,1x,g16.10/14x,a16,2x,g16.10,1x,g16.10)
      end

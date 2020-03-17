C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine qmod
C-----------------------------------------------------------------------
C  ADJUSTMENT OF THE Q-VALUES PLUS AN ADDITIONAL ADJUSTMENT OF A
C  X-PHASEADVANCE BETWEEN 2 POSITIONS IN THE MACHINE
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
      dimension sens(3,5),aa(3,3),bb(3),qx(3),qz(3),sm0(3),qwc(3)
      dimension aa1(2,2)
C    &                  ,qw(2)
      save
C-----------------------------------------------------------------------
      do 10 i=1,3
        bb(i)=zero
        qx(i)=zero
        qz(i)=zero
        sm0(i)=zero
        qwc(i)=zero
        do 10 j=1,3
          aa(i,j)=zero
   10 continue
      do 20 i=1,3
        do 20 j=1,5
          sens(i,j)=zero
   20 continue
      do 30 i=1,2
        do 30 j=1,2
          aa1(i,j)=zero
   30 continue
      write(6,10010)
      sqx=zero
      sqz=zero
      sqxh=zero
      dpp=zero
      iq1=iq(1)
      iq2=iq(2)
      if(kz(iq1).ne.2.or.kz(iq2).ne.2) call error(8)
      if (abs(el(iq1)).le.pieni.or.abs(el(iq2)).le.pieni) then
        sm0(1)=ed(iq1)
        sm0(2)=ed(iq2)
      else
        sm0(1)=ek(iq1)
        sm0(2)=ek(iq2)
      endif
      if(kp(iq1).eq.5) call combel(iq1)
      if(kp(iq2).eq.5) call combel(iq2)
      sens(1,1)=qw0(1)
      sens(2,1)=qw0(2)
      if(abs(qw0(3)).gt.pieni) then
        iq3=iq(3)
        if(kz(iq3).ne.2) call error(8)
        if (abs(el(iq3)).le.pieni) then
          sm0(3)=ed(iq3)
        else
          sm0(3)=ek(iq3)
        endif
        if(kp(iq3).eq.5) call combel(iq3)
        nite=3
      else
        nite=2
      endif
      call clorb(dpp)
      if(ierro.gt.0) call error(9)
      call phasad(dpp,qwc)
      sens(1,5)=qwc(1)
      sens(2,5)=qwc(2)
      if(nite.eq.3) then
        sens(3,1)=qw0(3)
        sens(3,5)=qwc(3)
        write(6,10100)
        write(6,10120) qwc,qw0
      else
        write(6,10110)
        write(6,10130) qwc(1),qwc(2),qw0(1),qw0(2)
      endif
      do 60 ii=1,itqv
        do 40 n=1,nite
          iql=iq(n)
          if (abs(el(iql)).le.pieni) then
            ed(iql)=ed(iql)+dkq
          else
            ek(iql)=ek(iql)+dkq
          endif
          if(kp(iql).eq.5) call combel(iql)
          call clorb(dpp)
          if(ierro.gt.0) call error(9)
          call phasad(dpp,qwc)
          sens(1,n+1)=qwc(1)
          sens(2,n+1)=qwc(2)
          if(nite.eq.3) then
            sens(3,n+1)=qwc(3)
            write(6,10140) ii,n,qwc
          else
            write(6,10150) ii,n,qwc(1),qwc(2)
          endif
          if (abs(el(iql)).le.pieni) then
            ed(iql)=ed(iql)-dkq
          else
            ek(iql)=ek(iql)-dkq
          endif
          if(kp(iql).eq.5) call combel(iql)
   40   continue
C--Q-VALUE ADJUSTMENT
        aa1(1,1)=(sens(1,2)-sens(1,5))/dkq
        aa1(1,2)=(sens(2,2)-sens(2,5))/dkq
        aa1(2,1)=(sens(1,3)-sens(1,5))/dkq
        aa1(2,2)=(sens(2,3)-sens(2,5))/dkq
        a11=aa1(1,1)
        a12=aa1(1,2)
        a21=aa1(2,1)
        a22=aa1(2,2)
        bb(1)=sens(1,5)-sens(1,1)
        bb(2)=sens(2,5)-sens(2,1)
        sqx=sqx+abs(bb(1))
        sqz=sqz+abs(bb(2))
        if(nite.eq.3) then
          aa(1,1)=a11
          aa(1,2)=a12
          aa(1,3)=(sens(3,2)-sens(3,5))/dkq
          aa(2,1)=a21
          aa(2,2)=a22
          aa(2,3)=(sens(3,3)-sens(3,5))/dkq
          aa(3,1)=(sens(1,4)-sens(1,5))/dkq
          aa(3,2)=(sens(2,4)-sens(2,5))/dkq
          aa(3,3)=(sens(3,4)-sens(3,5))/dkq
          a13=aa(1,3)
          a23=aa(2,3)
          a31=aa(3,1)
          a32=aa(3,2)
          a33=aa(3,3)
          bb(3)=sens(3,5)-sens(3,1)
          sqxh=sqxh+abs(bb(3))
          call loesd(aa,bb,nite,nite,ierr)
        else
          call loesd(aa1,bb,nite,nite,ierr)
        endif
        if(ierr.eq.1) call error(35)
        do 50 l=1,nite
          iql=iq(l)
          if (abs(el(iql)).le.pieni) then
            ed(iql)=ed(iql)-bb(l)
          else
            ek(iql)=ek(iql)-bb(l)
          endif
          if(kp(iql).eq.5) call combel(iql)
   50   continue
        call clorb(dpp)
        if(ierro.gt.0) call error(9)
        call phasad(dpp,qwc)
        sens(1,5)=qwc(1)
        sens(2,5)=qwc(2)
        if(nite.eq.3) then
          sens(3,5)=qwc(3)
          write(6,10020) qw0(1),qwc(1),qw0(2),qwc(2),qw0(3),qwc(3)
          if (abs(el(iq1)).le.pieni) then
            write(6,10040) sm0(1),ed(iq1),bez(iq1), sm0(2),ed(iq2),bez
     +      (iq2),sm0(3),ed(iq3),bez(iq3)
          else
            write(6,10040) sm0(1),ek(iq1),bez(iq1), sm0(2),ek(iq2),bez
     +      (iq2),sm0(3),ek(iq3),bez(iq3)
          endif
          write(6,10080) sqx,sqz,sqxh
          write(6,10060) a11,a12,a13,a21,a22,a23,a31,a32,a33
        else
          write(6,10030) qw0(1),qwc(1),qw0(2),qwc(2)
          if (abs(el(iq1)).le.pieni) then
            write(6,10050) sm0(1),ed(iq1),bez(iq1), sm0(2),ed(iq2),bez
     +      (iq2)
          else
            write(6,10050) sm0(1),ek(iq1),bez(iq1), sm0(2),ek(iq2),bez
     +      (iq2)
          endif
          write(6,10090) sqx,sqz
          write(6,10070) a11,a12,a21,a22
        endif
        if (abs(el(iq(1))).le.pieni) then
          sm0(1)=ed(iq(1))
          sm0(2)=ed(iq(2))
        else
          sm0(1)=ek(iq(1))
          sm0(2)=ek(iq(2))
        endif
        dq1=abs(qwc(1)-qw0(1))
        dq2=abs(qwc(2)-qw0(2))
        if(nite.eq.3) then
          if (abs(el(iq(3))).le.pieni) then
            sm0(3)=ed(iq(3))
          else
            sm0(3)=ek(iq(3))
          endif
          dq3=abs(qwc(3)-qw0(3))
          if(dq1.lt.dqq.and.dq2.lt.dqq.and.dq3.lt.dqq) return
        else
          if(dq1.lt.dqq.and.dq2.lt.dqq) return
        endif
   60 continue
      write(6,10000) itqv
C-----------------------------------------------------------------------
      return
10000 format(t5/t10,'TUNE ADJUSTMENT'/ t10,
     +'MAXIMUM NUMBER OF ITERATIONS ACHIEVED--->',2x,i4/ t10,
     +'PROCEDURE MAY NOT HAVE CONVERGED')
10010 format(1h /131('-'))
10020 format(//131('-')//t10,'DATA BLOCK TUNE-VARIATION' / /t10,
     +'TUNE'           ,26x,'THEORET.     AFTER CORRECTION'/ t10,
     +'HORIZONTAL'     ,17x,g16.10,2x,g16.10/ t10,
     +'VERTICAL'       ,19x,g16.10,2x,g16.10/ t10,
     +'PART-HORIZONTAL',12x,g16.10,2x,g16.10/)
10030 format(//131('-')//t10,'DATA BLOCK TUNE-VARIATION' / /t10,
     +'TUNE'           ,26x,'THEORET.      AFTER CORRECTION'/ t10,
     +'HORIZONTAL'     ,17x,g16.10,2x,g16.10/ t10,
     +'VERTICAL'       ,19x,g16.10,2x,g16.10/)
10060 format(t10,'QUADRUPOLE SENSITIVITIES',6x,'D-QX',14x,'D-QZ',14x,
     +'D-QXH'/29x,'QF   ',d15.8,3x,d15.8,3x,d15.8/29x,
     +'QD   ',d15.8,3x,d15.8,3x,d15.8/29x,
     +'QF2  ',d15.8,3x,d15.8,3x,d15.8//131('-')//)
10070 format(t10,'QUADRUPOLE SENSITIVITIES',6x,'D-QX',14x,'D-QZ', /29x,
     +'QF   ',d15.8,3x,d15.8/29x,'QD   ',d15.8,3x,d15.8 //131('-')//)
10080 format(t10,'TOTAL TUNE SHIFT',10x,'QX =',f10.7,'    QZ =',f10.7,
     +'   QXH =',f10.7)
10090 format(t10,'TOTAL TUNE SHIFT',10x,'QX =',f10.7,'    QZ =',f10.7)
10100 format(t5,'---- QMOD FOR SPLIT-Q-VALUES ENTRY ---- ',
     +'(ZERO MOMENTUM-DEVIATION)')
10110 format(t5,'---- QMOD ENTRY ---- (ZERO MOMENTUM-DEVIATION)')
10120 format(t10,'START-QX-QZ-QXH',3f12.7,' END-QX-QZ-QXH',3f12.7)
10130 format(t10,'START-QX-QZ',2f12.7,' END-QX-QZ',2f12.7)
10140 format(t10,'ITER=',i3,'/QUAD=',i3,'/QX-QZ-QXH',3f12.7)
10150 format(t10,'ITER=',i3,'/QUAD=',i3,'/QX-QZ',2f12.7)
10040 format(t10,'QUADRU.STRENGTHS',7x,g16.10,2x,g16.10,'   TYP     ',
     +   a16/t10,                  23x,g16.10,2x,g16.10,'           ',
     +a16)
10050 format(t10,'QUADRU.STRENGTHS',7x,g16.10,2x,g16.10,'   TYP     ',
     +   a16/t10,                  23x,g16.10,2x,g16.10,'           ',
     +a16)
      end

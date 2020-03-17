C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine error(ier)
C-----------------------------------------------------------------------
C  ERROR OUTPUT
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
      save
      write(6,10000)
      goto(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,
     +180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,
     +340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,
     +500,510,520,530,540,550,560,570,580,590,600,610,620,630,640,650,
     +660,670,680,690,700,710,720,730,740,750,760,770,780,790,800,810,
     +820,830,840,850,860,870,880,890,900,910,920,930),ier
   10 write(6,10010)
      goto 1870
   20 write(6,10020) nele
      goto 1870
   30 write(6,10030)
      goto 1870
   40 write(6,10040)
      goto 1870
   50 write(6,10050)
      goto 1870
   60 write(6,10060)
      goto 1870
   70 write(6,10070)
      goto 1870
   80 write(6,10080)
      goto 1870
   90 write(6,10090)
      goto 1870
  100 write(6,10100)
      goto 1870
  110 write(6,10110)
      goto 1870
  120 write(6,10120)
      goto 1870
  130 write(6,10130)
      goto 1870
  140 write(6,10140)
      goto 1870
  150 write(6,10150)
      goto 1870
  160 write(6,10160) nele
      goto 1870
  170 write(6,10170) nper
      goto 1870
  180 write(6,10180) nblo
      goto 1870
  190 write(6,10190) erbez
      goto 1870
  200 write(6,10200) erbez
      goto 1870
  210 write(6,10210)
      goto 1870
  220 write(6,10220)
      goto 1870
  230 write(6,10230)
      goto 1870
  240 write(6,10240)
      goto 1870
  250 write(6,10250)
      goto 1870
  260 write(6,10260) nelb
      goto 1870
  270 write(6,10270)
      goto 1870
  280 write(6,10280)
      goto 1870
  290 write(6,10290)
      goto 1870
  300 write(6,10300) nran
      goto 1870
  310 write(6,10310)
      goto 1870
  320 write(6,10320)
      goto 1870
  330 write(6,10330)
      goto 1870
  340 write(6,10340) mran
      goto 1870
  350 write(6,10350)
      goto 1870
  360 write(6,10360)
      goto 1870
  370 write(6,10370)
      goto 1870
  380 write(6,10380)
      goto 1870
  390 write(6,10390)
      goto 1870
  400 write(6,10400)
      goto 1870
  410 write(6,10410)
      goto 1870
  420 write(6,10420)
      goto 1870
  430 write(6,10430) nzfz
      goto 1870
  440 write(6,10440)
      goto 1870
  450 write(6,10450)
      goto 1870
  460 write(6,10460) nrco
      goto 1870
  470 write(6,10470)
      goto 1870
  480 write(6,10480)
      goto 1870
  490 write(6,10490)
      goto 1870
  500 write(6,10500)
      goto 1870
  510 write(6,10510)
      goto 1870
  520 write(6,10520) nema
      goto 1870
  530 write(6,10530)
      goto 1870
  540 write(6,10540) npart
      goto 1870
  550 write(6,10550) nmac
      goto 1870
  560 write(6,10560) ierro
      goto 1870
  570 write(6,10570) ierro
      goto 1870
  580 write(6,10580) ierro
      goto 1870
  590 write(6,10590) ierro
      goto 1870
  600 write(6,10600) ierro
      goto 1870
  610 write(6,10610) ierro
      goto 1870
  620 write(6,10620)
      goto 1870
  630 write(6,10630)
      goto 1870
  640 write(6,10640)
      goto 1870
  650 write(6,10650) mcor
      goto 1870
  660 write(6,10660)
      goto 1870
  670 write(6,10670)
      goto 1870
  680 write(6,10680)
      goto 1870
  690 write(6,10690)
      goto 1870
  700 write(6,10700)
      goto 1870
  710 write(6,10710)
      goto 1870
  720 write(6,10720)
      goto 1870
  730 write(6,10730)
      goto 1870
  740 write(6,10740)
      goto 1870
  750 write(6,10750)
      goto 1870
  760 write(6,10760)
      goto 1870
  770 write(6,10770)
      goto 1870
  780 write(6,10780)
      goto 1870
  790 write(6,10790)
      goto 1870
  800 write(6,10800)
      goto 1870
  810 write(6,10810)
      goto 1870
  820 write(6,10820)
      goto 1870
  830 write(6,10830)
      goto 1870
  840 write(6,10840)
      goto 1870
  850 write(6,10850) mmul
      goto 1870
  860 write(6,10860)
      goto 1870
  870 write(6,10870)
      goto 1870
  880 write(6,10880)
      goto 1870
  890 write(6,10890)
      goto 1870
  900 write(6,10900)
      goto 1870
  910 write(6,10910)
      goto 1870
  920 write(6,10920)
      goto 1870
  930 write(6,10930)
 1870 continue
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      stop
10000 format(5x///t10,'++++++++++++++++++++++++'/ t10,
     +'+++++ERROR DETECTED+++++'/ t10,'++++++++++++++++++++++++'/ t10,
     +'RUN TERMINATED ABNORMALLY !!!'//)
10010 format(t10,'WRONG MODE DEFINITION')
10020 format(t10,'NOT MORE THAN: ',i4,' POSITIONS FOR RESONANCE-COMPEN',
     +'SATION ALLOWED')
10030 format(t10,'ELEMENT FOR RESONANCE-COMPENSATION IS NOT IN THE ELE',
     +'MENTLIST')
10040 format(t10,'UNSTABLE CLOSED ORBIT DURING INITIAL DISPERSION' ,
     +' CALCULATION'/ t10,
     +'INSTABILITY OCCURED FOR SMALL RELATIVE ENERGY DEVIATION')
10050 format(t10,'UNSTABLE CLOSED ORBIT FOR ZERO ENERGY DEVIATION')
10060 format(t10,'UNSTABLE CLOSED ORBIT DURING DISPERSION CALCULATION' ,
     +' AFTER ORBIT SCALING'/ t10,
     +'INSTABILITY OCCURED FOR SMALL RELATIVE ENERGY DEVIATION')
10070 format(t10,'UNSTABLE CLOSED ORBIT AFTER ORBIT SCALING')
10080 format(t10,'ELEMENTS SPECIFIED FOR TUNE VARIATION ARE NOT' ,
     +' QUADRUPOLES')
10090 format(t10,'UNSTABLE CLOSED ORBIT DURING TUNE VARIATION')
10100 format(t10,'NO OPTICAL SOLUTION DURING TUNE VARIATION')
10110 format(t10,'ELEMENTS SPECIFIED FOR CHROMATICITY CORRECTION ARE' ,
     +' NOT SEXTUPOLES')
10120 format(t10,'UNSTABLE CLOSED ORBIT DURING CHROMATICITY CORRECTION')
10130 format(t10,'NO OPTICAL SOLUTION DURING CHROMATICITY CORRECTION')
10140 format(t10,'ELEMENTS OF DIFFERENT TYPES ARE COMBINED IN DATA' ,
     +' BLOCK COMBINATION OF ELEMENTS')
10150 format(t10,'UNKNOWN BLOCK SPECIFICATION')
10160 format(t10,'NO. OF SINGLE ELEMENTS EXCEEDS THE MAXIMUM ALLOWED' ,
     +' VALUE: ',i4)
10170 format(t10,'NO. OF SUPERPERIODS LARGER THAN : ',i4)
10180 format(t10,'NO. OF DIFFERENT BLOCKS EXCEEDS THE MAXIMUM ALLOWED' ,
     +' VALUE: ',i5)
10190 format(t10,'UNKNOWN SINGLE ELEMENT : ',a16,
     +' IN THE BLOCK DEFINITION')
10200 format(t10,'UNKNOWN BLOCK NAME OR INSERTION NAME : ',a16,' IN THE'
     +,' STRUCTURE INPUT')
10210 format(t10,'MAXIMUM NUMBER OF STRUCTURE ELEMENTS SURPASSED')
10220 format(t10,'NO SOLUTION FOR ORBIT SCALING - POSSIBLE REASONS:'/
     +t10,'--> DIPOLE STRENGTHS OF NON-CORRECTOR ELEMENTS TO HIGH'/ t10,
     +'--> NONLINEARITIES TOO STRONG, TRY TO INCREASE INITIAL'/ t10,
     +'    CORRECTOR STRENGTHS'/ t10,
     +'--> USE ALL DIPOLE ELEMENTS FOR SCALING'/)
10230 format(t10,'NO OPTICAL SOLUTION')
10240 format(t10,'NO SOLUTION FOR DISPERSION')
10250 format(t10,'--> PLEASE INCLUDE LENGTH OF MACHINE IN THE' ,
     +' <SYNCHROTRON>-BLOCK')
10260 format(t10,'ONE BLOCK CAN NOT HAVE MORE THAN ',i4,' ELEMENTS')
10270 format(t10,'KINETIC ENERGY OF THE PARTICLE IS LESS OR EQUAL ZERO')
10280 format(t10,'EITHER YOUR RF-FREQENCY IS SHIFTED BY 180 DEGREES'/ ,
     +t10,'THEN CHANGE THE SIGN OF <ITION> IN THE ',
     +'<SYNCHROTRON>-INPUTBLOCK',/t10,'OR YOUR ALFA-P IS WRONGLY ',
     +'INTRODUCED IN THE SAME INPUTBLOCK')
10290 format(t10,'MULTIPOLECOEFFITIONS CANNOT BE SET EQUAL')
10300 format(t10,'THE RANDOM NUMBER: ',i6,' FOR THE INITIAL',
     +' STRUCTURE IS TOO SMALL')
10310 format(t10,'ELEMENTS THAT NEED RANDOM NUMBERS HAVE A KZ > 0')
10320 format(t10,'THERE ARE NOT ENOUGH RANDOM NUMBERS FOR THE INSERTED',
     +' ELEMENTS')
10330 format(t10,'TO USE THE SAME RANDOMNUMBERS FOR 2 ELEMENTS, THE',
     +' INSERTED ELEMENT MUST NOT NEED MORE OF SUCH NUMBERS THAN THE',
     +' REFERENCE ELEMENT')
10340 format(t10,'NOT MORE THAN',i4,' OF EACH TYP OF INSERTED ELEMENTS',
     +' CAN BE USED')
10350 format(t10,'PROBLEMS DURING MATRIX-INVERSION IN QMOD')
10360 format(t10,'NO CONVERGENCE IN RMOD')
10370 format(t10,'CHOSEN ORDERS OF RESONANCES CAN NOT BE CALCULATED')
10380 format(t10,'PROBLEMS DURING MATRIX-INVERSION IN RMOD')
10390 format(t10,'WITH THE SPECIFIED ELEMENTS THE RESONANCE CANNOT BE',
     +' COMPENSATED - RESONANCEORDER AND ELEMENTTYP # MUST BE THE SAME')
10400 format(t10,'NOT MORE THAN 2 PARTICLES CAN BE TRACKED')
10410 format(t10,'GEOMETRY AND STRENGTH FILE (UNIT 2) IS EMPTY OR ' ,
     +' DISTROYED')
10420 format(t10,'TRACKING PARAMETER FILE (UNIT 3) IS EMPTY OR ' ,
     +' NONEXISTING')
10430 format(t10,'NOT MORE THAN ',i4,' RANDOM NUMBERS CAN BE USED')
10440 format(t10,'FOR THE INPUTBLOCK - ORBIT CORRECTION - ONLY CORRE',
     +'CTORS WITH THE KEYWORDS ( HCOR= ; VCOR= )'/t10,
     +'AND MONITORS WITH THE', ' KEYWORDS ( HMON= ; VMON= ) ARE ALLOWED'
     +)
10450 format(t10,'FOR THE INPUTBLOCK - LINEAR OPTICS - ONLY',
     +' THE KEYWORD ( ELEMENT ) AND ( BLOCK ) ARE ALLOWED')
10460 format(t10,'ORDER OF COMPENSATION CAN NOT BE LARGER THAN : ',i4)
10470 format(t10,'ONLY UP TO 3 RESONANCES CAN BE COMPENSATED')
10480 format(t10,'RESONANCE TYPE IS OUT OF THE RANGE OF THE RESONANCE',
     +' ORDER')
10490 format(t10,'ONLY UP TO 3 SUBRESONANCES CAN BE COMPENSATED')
10500 format(t10,'THE MULTIPOLE ORDER FOR THE SUBRESONANCE COMPENSATION'
     +,' SHOULD NOT EXCEED THE VALUE 9')
10510 format(t10,'TOO MANY RIPPLE ELEMENTS')
10520 format(t10,'MAXIMUM ORDER OF THE ONE TURN MAP IS ',i4, /,
     +' NEMA HAS TO BE LARGER THAN NORD')
10530 format(t10,'# OF VARIABLES -NV- OF THE ONE TURN MAP IS NOT',
     +' IN THE ALLOWED RANGE [2 <= NV <= 5]')
10540 format(t10,'MAXIMUM NUMBER OF PARTICLES FOR VECTORIZATION', ' IS '
     +,i4)
10550 format(t10,'MAXIMUM NUMBER OF DIFFERENT SEEDS FOR VECTORIZATION',
     +' IS ',i4)
10560 format(t10,'PROBLEMS WITH FILE 13 WITH INITIAL COORDINATES ',
     +' - ERROR CODE : ',i10)
10570 format(t10,'PROBLEMS WITH FILE 2 WITH ACCELERATOR STRUCTURE ',
     +' - ERROR CODE : ',i10)
10580 format(t10,'PROBLEMS WITH FILE 3 WITH TRACKING PARAMETERS ',
     +' - ERROR CODE : ',i10)
10590 format(t10,'PROBLEMS WITH FILE 11 FOR DATEN LIST INPUT ',
     +' - ERROR CODE : ',i10)
10600 format(t10,'PROBLEMS WITH FILE 99 FOR BINARY OUTPUT ',
     +' - ERROR CODE : ',i10)
10610 format(t10,'PROBLEMS WITH FILE 12 FOR END COORDINATES',
     +' - ERROR CODE : ',i10)
10620 format(t10,'ELEMENTS SPECIFIED FOR DECOUPLING ARE NOT' ,
     +' SKEW QUADRUPOLES')
10630 format(t10,'THERE ARE THE APPROPRIATE ELEMENTS FOR' ,
     +' THE DECOUPLING OR SIMULTANEOUS TUNE ADJUSTMENT')
10640 format(t10,'PROBLEMS DURING MATRIX-INVERSION IN DECOUP')
10650 format(t10,'MAXIMUM NUMBER OF EXTRA PARAMETERS IS : ',i4)
10660 format(t10,'EXTRA PARAMETERS FOR THE MAP DOES NOT EXIST')
10670 format(t10,'ONLY SINGLE KICK ELEMENTS ALLOWED FOR MAP CALCULATION'
     +)
10680 format(t10,'THE ORDER OF THE NORMAL FORM IS TOO HIGH. CHECK THE' ,
     +' DIFFERENTIAL ALGEBRA PARAMETERS')
10690 format(t10,'TOO MANY VARIABLES SPECIFIED. CHECK THE DIFFERENTIAL'
     +,' ALGEBRA PARAMETERS')
10700 format(t10,'NO CORRECTORS SPECIFIED')
10710 format(t10,'BOTH AMPLITUDE AND MOMENTUM ORDER ARE ZERO!')
10720 format(t10,'BOTH AMPLITUDE AND MOMENTUM ORDER ARE DIFFERENT FROM',
     +' ZERO!')
10730 format(t10,'AMPLITUDE ORDER OUTSIDE RANGE [0,2]')
10740 format(t10,'MOMENTUM ORDER OUTSIDE RANGE [0,3] (ONE EXCLUDED!)')
10750 format(t10,'MINIMUM ORDER OUTSIDE RANGE [2,3]')
10760 format(t10,'MINIMUM ORDER GREATER THAN MAXIMUM!')
10770 format(t10,'MAXIMUM ORDER OUTSIDE RANGE [2,3]')
10780 format(t10,'NORMAL FORMS ANALYSIS IMPOSSIBLE',/ ,t10,
     +'THE TRANSFER MAP DOES NOT EXIST!')
10790 format(t10,'ZERO OR NEGATIVE ENERGY DOES NOT MAKE MUCH SENSE')
10800 format(t10,'PROBLEM READING EXTERNAL MULTIPOLE ERRORS')
10810 format(t10,'TOO MANY ELEMENTS FOR LINEAR OPTICS WRITE-OUT')
10820 format(t10,'FOR CLOSED ORBIT CORRECTORS ONLY DIPOLES OF LEGTH',
     +' ZERO OR MULTIPOLE LENSES ALLOWED')
10830 format(t10,'AN ELEMENT FOR CLOSED ORBIT CORRECTION CAN BE ONLY',
     +' EITHER A HORIZONTAL MONITOR',/,t10, 'OR A VERTICAL MONITOR OR',
     +' A HORIZONTAL CORRECTOR OR A VERTICAL CORRECTOR')
10840 format(t10,'NUMBER OF ORBIT CORRECTORS IS ZERO')
10850 format(t10,'THE ORDER OF MULTIPOLES MMUL: ',i4,' HAS TO BE LARGER'
     +,' THAN 10 BUT SMALLER THAN 20')
10860 format(t10,'PROBLEM READING EXTERNAL MISALIGNMENTS')
10870 format(t10,'PROBLEM READING FROM FILE 30 (SINGLE KICKS AND ',
     +'MISALIGNMENTS')
10880 format(t10,'BEAM_BEAM: EITHER NORMALIZED EMITTANCES OR THE ',
     +'RESULTING SIGMA VALUES EQUAL TO ZERO')
10890 format(t10,'BEAM_BEAM: AT EACH INTERACTION POINT THE BEAM ',
     +'MUST BE EITHER ROUND OR ELLIPTICAL FOR ALL PARTICLES')
10900 format(t10,'QUADRUPOLES ARE NOT SUITED TO ADJUST THE 6D TUNES')
10910 format(t10,'ORDER AND NUMBER OF VARIABLES HAVE TO BE LARGER ',
     +'THAN ZERO TO CALCULATE A DIFFEREENTIAL ALGEBRA MAP')
10920 format(t10,'YOU CANNOT COMBINE AN ELEMENT WITH ITSELF')
10930 format(t10,'INVERTED LINEAR BLOCKS NOT ALLOWED')
      end

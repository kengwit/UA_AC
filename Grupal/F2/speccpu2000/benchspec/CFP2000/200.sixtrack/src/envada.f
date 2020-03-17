C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine envada(ix)
C-----------------------------------------------------------------------
C  CALCULATION OF : MOMENTUM-DEPENDING ELEMENT-MATRICES AND
C                   CHANGE OF PATH LENGTHS FOR EACH PARTICLE.
C      SPECIALLY PREPARED FOR NEW D.A. (SIX-DIMENSIONAL VERSION)
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
      external dapri6
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
      common/daele/alda,asda,aldaq,asdaq,smida,dpda,dpda1,sigmda,ej1,
     &ejf1,rv
      common/dael6/ald6(nele,2,6,nema),asd6(nele,2,6,nema)
C-----------------------------------------------------------------------
*FOX  B D ;
*FOX  D V DA EXT SIGMDA NORD NVAR ; D V DA EXT DPDA NORD NVAR ;
*FOX  D V DA EXT DPDA1 NORD NVAR ; D V DA EXT RV NORD NVAR ;
*FOX  D V DA EXT EJ1 NORD NVAR ; D V DA EXT EJF1 NORD NVAR ;
*FOX  D V DA EXT ALDA NORD NVAR 2 6 ; D V DA EXT ASDA NORD NVAR 2 6 ;
*FOX  D V DA EXT ALDAQ NORD NVAR 2 6 ; D V DA EXT ASDAQ NORD NVAR 2 6 ;
*FOX  D V DA EXT SMIDA NORD NVAR MCOR ;
*FOX  D V DA INT FOKQ NORD NVAR ; D V DA INT WFHI NORD NVAR ;
*FOX  D V DA INT DPD NORD NVAR ; D V DA INT DPSQ NORD NVAR ;
*FOX  D V DA INT FOK NORD NVAR ; D V DA INT RHO NORD NVAR ;
*FOX  D V DA INT FOK1 NORD NVAR ; D V DA INT SM1 NORD NVAR ;
*FOX  D V DA INT SM2 NORD NVAR ; D V DA INT SM3 NORD NVAR ;
*FOX  D V DA INT SM4 NORD NVAR ; D V DA INT SM5 NORD NVAR ;
*FOX  D V DA INT SM6 NORD NVAR ; D V DA INT SM12 NORD NVAR ;
*FOX  D V DA INT SM23 NORD NVAR ; D V DA INT AS3 NORD NVAR ;
*FOX  D V DA INT AS4 NORD NVAR ; D V DA INT AS6 NORD NVAR ;
*FOX  D V DA INT SI NORD NVAR ; D V DA INT CO NORD NVAR ;
*FOX  D V DA INT G NORD NVAR ; D V DA INT GL NORD NVAR ;
*FOX  D V DA INT SIQ NORD NVAR ; D V DA INT RHOC NORD NVAR ;
*FOX  D V DA INT HI NORD NVAR ; D V DA INT FI NORD NVAR ;
*FOX  D V DA INT AEK NORD NVAR ; D V DA INT HI1 NORD NVAR ;
*FOX  D V DA INT HP NORD NVAR ; D V DA INT HM NORD NVAR ;
*FOX  D V DA INT HC NORD NVAR ; D V DA INT HS NORD NVAR ;
*FOX  D V DA INT FOKC NORD NVAR ; D V DA INT WF NORD NVAR ;
*FOX  D V DA INT AFOK NORD NVAR ; D V DA INT RHOI NORD NVAR ;
*FOX  D V DA INT WFA NORD NVAR ; D V RE INT RATIOE NELE ;
*FOX  D V RE INT EL NELE ; D V RE INT EK NELE ; D V RE INT ED NELE ;
*FOX  D V RE INT ONE ; D V RE INT ZERO ; D V RE INT TWO ;
*FOX  D V RE INT HALF ; D V RE INT FOUR ; D V RE INT C1E3 ;
*FOX  D V RE INT C2E3 ; D V RE INT C4E3 ;
*FOX  D V IN INT I ; D V IN INT L ; D V IN INT IH ; D V IN INT NE ;
*FOX  D V IN INT NA ; D V IN INT IP ; D V IN INT IPCH ;
*FOX  D F RE DARE 1 ;
*FOX  E D ;
*FOX{
      INTEGER SIGMDA  
      INTEGER DPDA    
      INTEGER DPDA1   
      INTEGER RV      
      INTEGER EJ1     
      INTEGER EJF1    
      INTEGER ALDA    (2,6)
      INTEGER ASDA    (2,6)
      INTEGER ALDAQ   (2,6)
      INTEGER ASDAQ   (2,6)
      INTEGER SMIDA   (MCOR)
      INTEGER FOKQ    
      INTEGER WFHI    
      INTEGER DPD     
      INTEGER DPSQ    
      INTEGER FOK     
      INTEGER RHO     
      INTEGER FOK1    
      INTEGER SM1     
      INTEGER SM2     
      INTEGER SM3     
      INTEGER SM4     
      INTEGER SM5     
      INTEGER SM6     
      INTEGER SM12    
      INTEGER SM23    
      INTEGER AS3     
      INTEGER AS4     
      INTEGER AS6     
      INTEGER SI      
      INTEGER CO      
      INTEGER G       
      INTEGER GL      
      INTEGER SIQ     
      INTEGER RHOC    
      INTEGER HI      
      INTEGER FI      
      INTEGER AEK     
      INTEGER HI1     
      INTEGER HP      
      INTEGER HM      
      INTEGER HC      
      INTEGER HS      
      INTEGER FOKC    
      INTEGER WF      
      INTEGER AFOK    
      INTEGER RHOI    
      INTEGER WFA     
      INTEGER ISCRDA, ISCRRI,IDAO
C     INTEGER LFOX0, LFOX1
      REAL*8 RSCRRI
      COMMON/DASCR/ISCRDA(100),RSCRRI(100),ISCRRI(100),IDAO
      save
      if((idalloc.eq.1.or.idalloc.eq.3).and.ix.ne.numl*ncy) then      
         CALL DAKEY('FOX V2.1')
         CALL DAALL(FOKQ    ,1,'FOKQ      ',NORD,NVAR)
         CALL DAALL(WFHI    ,1,'WFHI      ',NORD,NVAR)
         CALL DAALL(DPD     ,1,'DPD       ',NORD,NVAR)
         CALL DAALL(DPSQ    ,1,'DPSQ      ',NORD,NVAR)
         CALL DAALL(FOK     ,1,'FOK       ',NORD,NVAR)
         CALL DAALL(RHO     ,1,'RHO       ',NORD,NVAR)
         CALL DAALL(FOK1    ,1,'FOK1      ',NORD,NVAR)
         CALL DAALL(SM1     ,1,'SM1       ',NORD,NVAR)
         CALL DAALL(SM2     ,1,'SM2       ',NORD,NVAR)
         CALL DAALL(SM3     ,1,'SM3       ',NORD,NVAR)
         CALL DAALL(SM4     ,1,'SM4       ',NORD,NVAR)
         CALL DAALL(SM5     ,1,'SM5       ',NORD,NVAR)
         CALL DAALL(SM6     ,1,'SM6       ',NORD,NVAR)
         CALL DAALL(SM12    ,1,'SM12      ',NORD,NVAR)
         CALL DAALL(SM23    ,1,'SM23      ',NORD,NVAR)
         CALL DAALL(AS3     ,1,'AS3       ',NORD,NVAR)
         CALL DAALL(AS4     ,1,'AS4       ',NORD,NVAR)
         CALL DAALL(AS6     ,1,'AS6       ',NORD,NVAR)
         CALL DAALL(SI      ,1,'SI        ',NORD,NVAR)
         CALL DAALL(CO      ,1,'CO        ',NORD,NVAR)
         CALL DAALL(G       ,1,'G         ',NORD,NVAR)
         CALL DAALL(GL      ,1,'GL        ',NORD,NVAR)
         CALL DAALL(SIQ     ,1,'SIQ       ',NORD,NVAR)
         CALL DAALL(RHOC    ,1,'RHOC      ',NORD,NVAR)
         CALL DAALL(HI      ,1,'HI        ',NORD,NVAR)
         CALL DAALL(FI      ,1,'FI        ',NORD,NVAR)
         CALL DAALL(AEK     ,1,'AEK       ',NORD,NVAR)
         CALL DAALL(HI1     ,1,'HI1       ',NORD,NVAR)
         CALL DAALL(HP      ,1,'HP        ',NORD,NVAR)
         CALL DAALL(HM      ,1,'HM        ',NORD,NVAR)
         CALL DAALL(HC      ,1,'HC        ',NORD,NVAR)
         CALL DAALL(HS      ,1,'HS        ',NORD,NVAR)
         CALL DAALL(FOKC    ,1,'FOKC      ',NORD,NVAR)
         CALL DAALL(WF      ,1,'WF        ',NORD,NVAR)
         CALL DAALL(AFOK    ,1,'AFOK      ',NORD,NVAR)
         CALL DAALL(RHOI    ,1,'RHOI      ',NORD,NVAR)
         CALL DAALL(WFA     ,1,'WFA       ',NORD,NVAR)
      ENDIF
      IDAA = IDAO
*FOX}
C-----------------------------------------------------------------------
*FOX  DPD=ONE+DPDA ;                                                    *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),DPD        )                          
*FOX  DPSQ=SQRT(DPD) ;                                                  *FOX
      CALL DAFUN('SQRT  ',DPD        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),DPSQ       )                          
      do 220 i=1,il
        do 10 ih=1,2
          do 10 ip=1,6
*FOX  ALDA(IH,IP)=ZERO ;                                                *FOX
      RSCRRI(100) = ZERO                                                
      CALL DACON(ALDA       (IH         ,IP         ),RSCRRI(100))      
*FOX  ASDA(IH,IP)=ZERO ;                                                *FOX
      RSCRRI(100) = ZERO                                                
      CALL DACON(ASDA       (IH         ,IP         ),RSCRRI(100))      
   10   continue
        if(abs(el(i)).le.pieni) goto 190
        kz1=kz(i)+1
        goto(20,40,100,60,80,90,130,170,180),kz1
C-----------------------------------------------------------------------
C  DRIFTLENGTH
C-----------------------------------------------------------------------
   20   do 30 l=1,2
*FOX  ALDA(L,1)=ONE  ;                                                  *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       (L          ,(1)),RSCRRI(100))              
*FOX  ALDA(L,2)=EL(I) ;                                                 *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      RSCRRI(100) = RSCRRI(  1+IDAA)                                    
      CALL DACON(ALDA       (L          ,(2)),RSCRRI(100))              
*FOX  ALDA(L,3)=ZERO ;                                                  *FOX
      RSCRRI(100) = ZERO                                                
      CALL DACON(ALDA       (L          ,(3)),RSCRRI(100))              
*FOX  ALDA(L,4)=ONE ;                                                   *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       (L          ,(4)),RSCRRI(100))              
*FOX  ASDA(L,6)=-RV*ALDA(L,2)/C2E3 ;                                    *FOX
      CALL DACOP(ALDA       (L          ,(2)),ISCRDA(  1+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  2+IDAA))        
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACDI(ISCRDA(  3+IDAA),ONE*C2E3       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),ASDA       (L          ,(6)))         
   30   continue
*FOX  ASDA(1,1)=EL(I)*(ONE-RV)*C1E3 ;                                   *FOX
      CALL DASUC(RV         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      RSCRRI(  2+IDAA) = EL         (I          )                       
      CALL DACMU(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA(  3+IDAA),ONE*C1E3       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),ASDA       ((1),(1)))                 
        goto 190
C-----------------------------------------------------------------------
C  RECTANGULAR MAGNET
C  HORIZONTAL
C-----------------------------------------------------------------------
   40   ih=1
   50   continue
        if(abs(ed(i)).le.pieni) goto 20
*FOX  FOK=EL(I)*ED(I)/DPSQ ;                                            *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      RSCRRI(  2+IDAA) = ED         (I          )                       
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      CALL DADIC(DPSQ       ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),FOK        )                          
*FOX  RHO=ONE/ED(I)*DPSQ ;                                              *FOX
      RSCRRI(  1+IDAA) = ED         (I          )                       
      RSCRRI(  2+IDAA) = ONE         / RSCRRI(  1+IDAA)                 
      CALL DACMU(DPSQ       ,ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),RHO        )                          
*FOX  FOK1=SIN(FOK*HALF)/COS(FOK*HALF)/RHO ;                            *FOX
      CALL DACMU(FOK        ,ONE*HALF       ,ISCRDA(  1+IDAA))          
      CALL DACMU(FOK        ,ONE*HALF       ,ISCRDA(  2+IDAA))          
      CALL DAFUN('SIN   ',ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))            
      CALL DAFUN('COS   ',ISCRDA(  2+IDAA),ISCRDA(  4+IDAA))            
      CALL DADIV(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DADIV(ISCRDA(  5+IDAA),RHO        ,ISCRDA(  6+IDAA))         
      CALL DACOP(ISCRDA(  6+IDAA),FOK1       )                          
*FOX  SI=SIN(FOK) ;                                                     *FOX
      CALL DAFUN('SIN   ',FOK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),SI         )                          
*FOX  CO=COS(FOK) ;                                                     *FOX
      CALL DAFUN('COS   ',FOK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),CO         )                          
*FOX  ALDA(IH,1)=ONE ;                                                  *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       (IH         ,(1)),RSCRRI(100))              
*FOX  ALDA(IH,2)=RHO*SI ;                                               *FOX
      CALL DAMUL(RHO        ,SI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=ZERO ;                                                 *FOX
      RSCRRI(100) = ZERO                                                
      CALL DACON(ALDA       (IH         ,(3)),RSCRRI(100))              
*FOX  ALDA(IH,4)=ONE ;                                                  *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       (IH         ,(4)),RSCRRI(100))              
*FOX  ALDA(IH,5)=-DPDA*RHO*(ONE-CO)/DPSQ*C1E3 ;                         *FOX
      CALL DASUC(CO         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACMU(DPDA       ,ONE*(-ONE       ),ISCRDA(  2+IDAA))        
      CALL DAMUL(ISCRDA(  2+IDAA),RHO        ,ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  3+IDAA),ISCRDA(  1+IDAA),ISCRDA(  4+IDAA))    
      CALL DADIV(ISCRDA(  4+IDAA),DPSQ       ,ISCRDA(  5+IDAA))         
      CALL DACMU(ISCRDA(  5+IDAA),ONE*C1E3       ,ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),ALDA       (IH         ,(5)))         
*FOX  ALDA(IH,6)=-DPDA*TWO*SIN(FOK*HALF)/COS(FOK*HALF)/DPSQ*C1E3 ;      *FOX
      CALL DACMU(FOK        ,ONE*HALF       ,ISCRDA(  1+IDAA))          
      CALL DACMU(FOK        ,ONE*HALF       ,ISCRDA(  2+IDAA))          
      CALL DAFUN('SIN   ',ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))            
      CALL DAFUN('COS   ',ISCRDA(  2+IDAA),ISCRDA(  4+IDAA))            
      CALL DACMU(DPDA       ,ONE*(-ONE       ),ISCRDA(  5+IDAA))        
      CALL DACMU(ISCRDA(  5+IDAA),ONE*TWO        ,ISCRDA(  6+IDAA))     
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  3+IDAA),ISCRDA(  7+IDAA))    
      CALL DADIV(ISCRDA(  7+IDAA),ISCRDA(  4+IDAA),ISCRDA(  8+IDAA))    
      CALL DADIV(ISCRDA(  8+IDAA),DPSQ       ,ISCRDA(  9+IDAA))         
      CALL DACMU(ISCRDA(  9+IDAA),ONE*C1E3       ,ISCRDA( 10+IDAA))     
      CALL DACOP(ISCRDA( 10+IDAA),ALDA       (IH         ,(6)))         
*FOX  SM1=COS(FOK) ;                                                    *FOX
      CALL DAFUN('COS   ',FOK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),SM1        )                          
*FOX  SM2=SIN(FOK)*RHO ;                                                *FOX
      CALL DAFUN('SIN   ',FOK        ,ISCRDA(  1+IDAA))                 
      CALL DAMUL(ISCRDA(  1+IDAA),RHO        ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),SM2        )                          
*FOX  SM3=-SIN(FOK)/RHO ;                                               *FOX
      CALL DAFUN('SIN   ',FOK        ,ISCRDA(  1+IDAA))                 
      CALL DACMU(ISCRDA(  1+IDAA),ONE*(-ONE       ),ISCRDA(  2+IDAA))   
      CALL DADIV(ISCRDA(  2+IDAA),RHO        ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),SM3        )                          
*FOX  SM5=-RHO*DPSQ*(ONE-SM1) ;                                         *FOX
      CALL DASUC(SM1        ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACMU(RHO        ,ONE*(-ONE       ),ISCRDA(  2+IDAA))        
      CALL DAMUL(ISCRDA(  2+IDAA),DPSQ       ,ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  3+IDAA),ISCRDA(  1+IDAA),ISCRDA(  4+IDAA))    
      CALL DACOP(ISCRDA(  4+IDAA),SM5        )                          
*FOX  SM6=-SM2*DPSQ/RHO ;                                               *FOX
      CALL DACMU(SM2        ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),DPSQ       ,ISCRDA(  2+IDAA))         
      CALL DADIV(ISCRDA(  2+IDAA),RHO        ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),SM6        )                          
*FOX  SM12=EL(I)-SM1*SM2 ;                                              *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DAMUL(SM1        ,SM2        ,ISCRDA(  2+IDAA))              
      CALL DASUC(ISCRDA(  2+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  3+IDAA),SM12       )                          
*FOX  SM23=SM2*SM3 ;                                                    *FOX
      CALL DAMUL(SM2        ,SM3        ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),SM23       )                          
*FOX  AS3=-RV*(DPDA*RHO/(TWO*DPSQ)*SM23+SM5) ;                          *FOX
      CALL DACMU(DPSQ       ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(DPDA       ,RHO        ,ISCRDA(  2+IDAA))              
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DAMUL(ISCRDA(  3+IDAA),SM23       ,ISCRDA(  4+IDAA))         
      CALL DAADD(ISCRDA(  4+IDAA),SM5        ,ISCRDA(  5+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACOP(ISCRDA(  7+IDAA),AS3        )                          
*FOX  AS4=-RV*SM23/C2E3 ;                                               *FOX
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),SM23       ,ISCRDA(  2+IDAA))         
      CALL DACDI(ISCRDA(  2+IDAA),ONE*C2E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),AS4        )                          
*FOX  AS6=-RV*(EL(I)+SM1*SM2)/C4E3 ;                                    *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DAMUL(SM1        ,SM2        ,ISCRDA(  2+IDAA))              
      CALL DACAD(ISCRDA(  2+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  4+IDAA))        
      CALL DAMUL(ISCRDA(  4+IDAA),ISCRDA(  3+IDAA),ISCRDA(  5+IDAA))    
      CALL DACDI(ISCRDA(  5+IDAA),ONE*C4E3       ,ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),AS6        )                          
*FOX  ASDA(IH,1)=(-RV*(DPDA*DPDA/(FOUR*DPD)*SM12+DPDA*(EL(I)-SM2))      *FOX
*FOX  +EL(I)*(ONE-RV))*C1E3 ;                                           *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACMU(DPD        ,ONE*FOUR       ,ISCRDA(  2+IDAA))          
      CALL DASUC(SM2        ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DAMUL(DPDA       ,DPDA       ,ISCRDA(  4+IDAA))              
      CALL DADIV(ISCRDA(  4+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DAMUL(ISCRDA(  5+IDAA),SM12       ,ISCRDA(  6+IDAA))         
      CALL DAMUL(DPDA       ,ISCRDA(  3+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DASUC(RV         ,ONE*ONE        ,ISCRDA(  9+IDAA))          
      RSCRRI( 10+IDAA) = EL         (I          )                       
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA( 11+IDAA))        
      CALL DAMUL(ISCRDA( 11+IDAA),ISCRDA(  8+IDAA),ISCRDA( 12+IDAA))    
      CALL DACMU(ISCRDA(  9+IDAA),ONE*RSCRRI( 10+IDAA),ISCRDA( 13+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA( 12+IDAA),ISCRDA( 13+IDAA),ISCRDA( 14+IDAA))    
      CALL DACMU(ISCRDA( 14+IDAA),ONE*C1E3       ,ISCRDA( 15+IDAA))     
      CALL DACOP(ISCRDA( 15+IDAA),ASDA       (IH         ,(1)))         
*FOX  ASDA(IH,2)=-RV*(DPDA/(TWO*RHO*DPSQ)*SM12+SM6)+FOK1*AS3 ;          *FOX
      CALL DACMU(RHO        ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(ISCRDA(  1+IDAA),DPSQ       ,ISCRDA(  2+IDAA))         
      CALL DADIV(DPDA       ,ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  3+IDAA),SM12       ,ISCRDA(  4+IDAA))         
      CALL DAADD(ISCRDA(  4+IDAA),SM6        ,ISCRDA(  5+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DAMUL(FOK1       ,AS3        ,ISCRDA(  8+IDAA))              
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),ASDA       (IH         ,(2)))         
*FOX  ASDA(IH,3)=AS3 ;                                                  *FOX
      CALL DACOP(AS3        ,ASDA       (IH         ,(3)))              
*FOX  ASDA(IH,4)=AS4+TWO*AS6*FOK1 ;                                     *FOX
      CALL DACMU(AS6        ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(ISCRDA(  1+IDAA),FOK1       ,ISCRDA(  2+IDAA))         
      CALL DAADD(AS4        ,ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=-RV*SM12/(C4E3*RHO*RHO)+AS6*FOK1*FOK1+FOK1*AS4  ;      *FOX
      CALL DACMU(RHO        ,ONE*C4E3       ,ISCRDA(  1+IDAA))          
      CALL DAMUL(ISCRDA(  1+IDAA),RHO        ,ISCRDA(  2+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  3+IDAA))        
      CALL DAMUL(ISCRDA(  3+IDAA),SM12       ,ISCRDA(  4+IDAA))         
      CALL DADIV(ISCRDA(  4+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DAMUL(AS6        ,FOK1       ,ISCRDA(  6+IDAA))              
      CALL DAMUL(ISCRDA(  6+IDAA),FOK1       ,ISCRDA(  7+IDAA))         
      CALL DAMUL(FOK1       ,AS4        ,ISCRDA(  8+IDAA))              
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))    
      CALL DAADD(ISCRDA(  9+IDAA),ISCRDA(  8+IDAA),ISCRDA( 10+IDAA))    
      CALL DACOP(ISCRDA( 10+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=AS6 ;                                                  *FOX
      CALL DACOP(AS6        ,ASDA       (IH         ,(6)))              
C--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
*FOX  G=SIN(FOK*HALF)/COS(FOK*HALF)/RHO ;                               *FOX
      CALL DACMU(FOK        ,ONE*HALF       ,ISCRDA(  1+IDAA))          
      CALL DACMU(FOK        ,ONE*HALF       ,ISCRDA(  2+IDAA))          
      CALL DAFUN('SIN   ',ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))            
      CALL DAFUN('COS   ',ISCRDA(  2+IDAA),ISCRDA(  4+IDAA))            
      CALL DADIV(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DADIV(ISCRDA(  5+IDAA),RHO        ,ISCRDA(  6+IDAA))         
      CALL DACOP(ISCRDA(  6+IDAA),G          )                          
*FOX  GL=EL(I)*G ;                                                      *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACMU(G          ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),GL         )                          
*FOX  ALDA(IH,1)=ONE-GL ;                                               *FOX
      CALL DASUC(GL         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(1)))         
*FOX  ALDA(IH,2)=EL(I) ;                                                *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      RSCRRI(100) = RSCRRI(  1+IDAA)                                    
      CALL DACON(ALDA       (IH         ,(2)),RSCRRI(100))              
*FOX  ALDA(IH,3)=-G*(TWO-GL) ;                                          *FOX
      CALL DASUC(GL         ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DACMU(G          ,ONE*(-ONE       ),ISCRDA(  2+IDAA))        
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=ALDA(IH,1) ;                                           *FOX
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  1+IDAA))         
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(4)))         
*FOX  AS6=-RV*ALDA(IH,2)/C2E3 ;                                         *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  2+IDAA))        
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACDI(ISCRDA(  3+IDAA),ONE*C2E3       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),AS6        )                          
*FOX  ASDA(IH,4)=-TWO*AS6*FOK1 ;                                        *FOX
      RSCRRI(  1+IDAA) = (-ONE       ) * TWO                            
      CALL DACMU(AS6        ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DAMUL(ISCRDA(  2+IDAA),FOK1       ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=AS6*FOK1*FOK1 ;                                        *FOX
      CALL DAMUL(AS6        ,FOK1       ,ISCRDA(  1+IDAA))              
      CALL DAMUL(ISCRDA(  1+IDAA),FOK1       ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=AS6 ;                                                  *FOX
      CALL DACOP(AS6        ,ASDA       (IH         ,(6)))              
        goto 190
C-----------------------------------------------------------------------
C  SEKTORMAGNET
C  HORIZONTAL
C-----------------------------------------------------------------------
   60   ih=1
   70   continue
        if(abs(ed(i)).le.pieni) goto 20
*FOX  FOK=EL(I)*ED(I)/DPSQ ;                                            *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      RSCRRI(  2+IDAA) = ED         (I          )                       
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      CALL DADIC(DPSQ       ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),FOK        )                          
*FOX  RHO=(ONE/ED(I))*DPSQ ;                                            *FOX
      RSCRRI(  1+IDAA) = ED         (I          )                       
      RSCRRI(  2+IDAA) = ONE         / RSCRRI(  1+IDAA)                 
      CALL DACMU(DPSQ       ,ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),RHO        )                          
*FOX  SI=SIN(FOK) ;                                                     *FOX
      CALL DAFUN('SIN   ',FOK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),SI         )                          
*FOX  CO=COS(FOK) ;                                                     *FOX
      CALL DAFUN('COS   ',FOK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),CO         )                          
*FOX  RHOC=RHO*(ONE-CO)/DPSQ ;                                          *FOX
      CALL DASUC(CO         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(RHO        ,ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))         
      CALL DADIV(ISCRDA(  2+IDAA),DPSQ       ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),RHOC       )                          
*FOX  SIQ=SI/DPSQ ;                                                     *FOX
      CALL DADIV(SI         ,DPSQ       ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),SIQ        )                          
*FOX  ALDA(IH,1)=CO ;                                                   *FOX
      CALL DACOP(CO         ,ALDA       (IH         ,(1)))              
*FOX  ALDA(IH,2)=RHO*SI ;                                               *FOX
      CALL DAMUL(RHO        ,SI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=-SI/RHO ;                                              *FOX
      CALL DACMU(SI         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DADIV(ISCRDA(  1+IDAA),RHO        ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=CO ;                                                   *FOX
      CALL DACOP(CO         ,ALDA       (IH         ,(4)))              
*FOX  ALDA(IH,5)=-DPDA*RHOC*C1E3 ;                                      *FOX
      CALL DACMU(DPDA       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),RHOC       ,ISCRDA(  2+IDAA))         
      CALL DACMU(ISCRDA(  2+IDAA),ONE*C1E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ALDA       (IH         ,(5)))         
*FOX  ALDA(IH,6)=-DPDA*SIQ*C1E3 ;                                       *FOX
      CALL DACMU(DPDA       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),SIQ        ,ISCRDA(  2+IDAA))         
      CALL DACMU(ISCRDA(  2+IDAA),ONE*C1E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ALDA       (IH         ,(6)))         
*FOX  SM12=EL(I)-ALDA(IH,1)*ALDA(IH,2) ;                                *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  5+IDAA),SM12       )                          
*FOX  SM23=ALDA(IH,2)*ALDA(IH,3) ;                                      *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (IH         ,(3)),ISCRDA(  2+IDAA))         
      CALL DAMUL(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),SM23       )                          
*FOX  ASDA(IH,1)=(-RV*(DPDA*DPDA/(FOUR*DPD)*SM12                        *FOX
*FOX  +DPDA*(EL(I)-ALDA(IH,2)))+EL(I)*(ONE-RV))*C1E3 ;                  *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  2+IDAA))         
      CALL DACMU(DPD        ,ONE*FOUR       ,ISCRDA(  3+IDAA))          
      CALL DASUC(ISCRDA(  2+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  4+IDAA))
     *                                                                  
      CALL DAMUL(DPDA       ,DPDA       ,ISCRDA(  5+IDAA))              
      CALL DADIV(ISCRDA(  5+IDAA),ISCRDA(  3+IDAA),ISCRDA(  6+IDAA))    
      CALL DAMUL(ISCRDA(  6+IDAA),SM12       ,ISCRDA(  7+IDAA))         
      CALL DAMUL(DPDA       ,ISCRDA(  4+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DASUC(RV         ,ONE*ONE        ,ISCRDA( 10+IDAA))          
      RSCRRI( 11+IDAA) = EL         (I          )                       
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA( 12+IDAA))        
      CALL DAMUL(ISCRDA( 12+IDAA),ISCRDA(  9+IDAA),ISCRDA( 13+IDAA))    
      CALL DACMU(ISCRDA( 10+IDAA),ONE*RSCRRI( 11+IDAA),ISCRDA( 14+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA( 13+IDAA),ISCRDA( 14+IDAA),ISCRDA( 15+IDAA))    
      CALL DACMU(ISCRDA( 15+IDAA),ONE*C1E3       ,ISCRDA( 16+IDAA))     
      CALL DACOP(ISCRDA( 16+IDAA),ASDA       (IH         ,(1)))         
*FOX  ASDA(IH,2)=-RV*(DPDA/(TWO*RHO*DPSQ)*SM12-DPD*SIQ) ;               *FOX
      CALL DACMU(RHO        ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(ISCRDA(  1+IDAA),DPSQ       ,ISCRDA(  2+IDAA))         
      CALL DADIV(DPDA       ,ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  3+IDAA),SM12       ,ISCRDA(  4+IDAA))         
      CALL DAMUL(DPD        ,SIQ        ,ISCRDA(  5+IDAA))              
      CALL DASUB(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  7+IDAA))        
      CALL DAMUL(ISCRDA(  7+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(2)))         
*FOX  ASDA(IH,3)=-RV*(DPDA*RHO/(TWO*DPSQ)*SM23-DPD*RHOC) ;              *FOX
      CALL DACMU(DPSQ       ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(DPDA       ,RHO        ,ISCRDA(  2+IDAA))              
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DAMUL(ISCRDA(  3+IDAA),SM23       ,ISCRDA(  4+IDAA))         
      CALL DAMUL(DPD        ,RHOC       ,ISCRDA(  5+IDAA))              
      CALL DASUB(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  7+IDAA))        
      CALL DAMUL(ISCRDA(  7+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(3)))         
*FOX  ASDA(IH,4)=-RV*SM23/C2E3 ;                                        *FOX
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),SM23       ,ISCRDA(  2+IDAA))         
      CALL DACDI(ISCRDA(  2+IDAA),ONE*C2E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=-RV*SM12/(C4E3*RHO*RHO) ;                              *FOX
      CALL DACMU(RHO        ,ONE*C4E3       ,ISCRDA(  1+IDAA))          
      CALL DAMUL(ISCRDA(  1+IDAA),RHO        ,ISCRDA(  2+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  3+IDAA))        
      CALL DAMUL(ISCRDA(  3+IDAA),SM12       ,ISCRDA(  4+IDAA))         
      CALL DADIV(ISCRDA(  4+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(ISCRDA(  5+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=-RV*(EL(I)+ALDA(IH,1)*ALDA(IH,2))/C4E3 ;               *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(6)))         
C--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
*FOX  ALDA(IH,1)=ONE ;                                                  *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       (IH         ,(1)),RSCRRI(100))              
*FOX  ALDA(IH,2)=EL(I) ;                                                *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      RSCRRI(100) = RSCRRI(  1+IDAA)                                    
      CALL DACON(ALDA       (IH         ,(2)),RSCRRI(100))              
*FOX  ALDA(IH,3)=ZERO ;                                                 *FOX
      RSCRRI(100) = ZERO                                                
      CALL DACON(ALDA       (IH         ,(3)),RSCRRI(100))              
*FOX  ALDA(IH,4)=ONE ;                                                  *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       (IH         ,(4)),RSCRRI(100))              
*FOX  ASDA(IH,6)=-RV*ALDA(IH,2)/C2E3 ;                                  *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  2+IDAA))        
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACDI(ISCRDA(  3+IDAA),ONE*C2E3       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),ASDA       (IH         ,(6)))         
        goto 190
C-----------------------------------------------------------------------
C  RECTANGULAR MAGNET VERTIKAL
C-----------------------------------------------------------------------
   80   ih=2
        goto 50
C-----------------------------------------------------------------------
C  SEKTORMAGNET VERTIKAL
C-----------------------------------------------------------------------
   90   ih=2
        goto 70
C-----------------------------------------------------------------------
C  QUADRUPOLE
C  FOCUSSING
C-----------------------------------------------------------------------
  100   continue
        if(abs(ek(i)).le.pieni) goto 20
*FOX  FOK=EK(I)/(ONE+DPDA) ;                                            *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      RSCRRI(  2+IDAA) = EK         (I          )                       
      CALL DADIC(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  3+IDAA),FOK        )                          
c*FOX  AEK=ABS(FOK) ;                                                   *FOX
*FOX  AEK=FOK ;                                                         *FOX
      CALL DACOP(FOK        ,AEK        )                               
        if(dare(AEK).lt.zero) then                                      *FOX
*FOX  AEK=-AEK ;                                                        *FOX
      CALL DACMU(AEK        ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),AEK        )                          
        endif 
        ih=0
*FOX  HI=SQRT(AEK) ;                                                    *FOX
      CALL DAFUN('SQRT  ',AEK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HI         )                          
*FOX  FI=EL(I)*HI ;                                                     *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACMU(HI         ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),FI         )                          
        if(ek(i).gt.zero) goto 120
  110   ih=ih+1
*FOX  ALDA(IH,1)=COS(FI) ;                                              *FOX
      CALL DAFUN('COS   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(1)))         
*FOX  HI1=SIN(FI) ;                                                     *FOX
      CALL DAFUN('SIN   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HI1        )                          
*FOX  ALDA(IH,2)=HI1/HI ;                                               *FOX
      CALL DADIV(HI1        ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=-HI1*HI ;                                              *FOX
      CALL DACMU(HI1        ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),HI         ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=ALDA(IH,1) ;                                           *FOX
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  1+IDAA))         
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(4)))         
*FOX  ASDA(IH,1)=EL(I)*(ONE-RV)*C1E3 ;                                  *FOX
      CALL DASUC(RV         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      RSCRRI(  2+IDAA) = EL         (I          )                       
      CALL DACMU(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA(  3+IDAA),ONE*C1E3       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),ASDA       (IH         ,(1)))         
*FOX  ASDA(IH,4)=-RV*ALDA(IH,2)*ALDA(IH,3)/C2E3 ;                       *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (IH         ,(3)),ISCRDA(  2+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  3+IDAA))        
      CALL DAMUL(ISCRDA(  3+IDAA),ISCRDA(  1+IDAA),ISCRDA(  4+IDAA))    
      CALL DAMUL(ISCRDA(  4+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DACDI(ISCRDA(  5+IDAA),ONE*C2E3       ,ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=-RV*(EL(I)-ALDA(IH,1)*ALDA(IH,2))*AEK/C4E3 ;           *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DAMUL(ISCRDA(  7+IDAA),AEK        ,ISCRDA(  8+IDAA))         
      CALL DACDI(ISCRDA(  8+IDAA),ONE*C4E3       ,ISCRDA(  9+IDAA))     
      CALL DACOP(ISCRDA(  9+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=-RV*(EL(I)+ALDA(IH,1)*ALDA(IH,2))/C4E3 ;               *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(6)))         
        if(ih.eq.2) goto 190
C--DEFOCUSSING
  120   ih=ih+1
*FOX  HP=EXP(FI) ;                                                      *FOX
      CALL DAFUN('EXP   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HP         )                          
*FOX  HM=ONE/HP ;                                                       *FOX
      CALL DADIC(HP         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),HM         )                          
*FOX  HC=(HP+HM)*HALF ;                                                 *FOX
      CALL DAADD(HP         ,HM         ,ISCRDA(  1+IDAA))              
      CALL DACMU(ISCRDA(  1+IDAA),ONE*HALF       ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),HC         )                          
*FOX  HS=(HP-HM)*HALF ;                                                 *FOX
      CALL DASUB(HP         ,HM         ,ISCRDA(  1+IDAA))              
      CALL DACMU(ISCRDA(  1+IDAA),ONE*HALF       ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),HS         )                          
*FOX  ALDA(IH,1)=HC ;                                                   *FOX
      CALL DACOP(HC         ,ALDA       (IH         ,(1)))              
*FOX  ALDA(IH,2)=HS/HI ;                                                *FOX
      CALL DADIV(HS         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=HS*HI ;                                                *FOX
      CALL DAMUL(HS         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=HC ;                                                   *FOX
      CALL DACOP(HC         ,ALDA       (IH         ,(4)))              
*FOX  ASDA(IH,4)=-RV*ALDA(IH,2)*ALDA(IH,3)/C2E3 ;                       *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (IH         ,(3)),ISCRDA(  2+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  3+IDAA))        
      CALL DAMUL(ISCRDA(  3+IDAA),ISCRDA(  1+IDAA),ISCRDA(  4+IDAA))    
      CALL DAMUL(ISCRDA(  4+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DACDI(ISCRDA(  5+IDAA),ONE*C2E3       ,ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=+RV*(EL(I)-ALDA(IH,1)*ALDA(IH,2))*AEK/C4E3 ;           *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DAMUL(RV         ,ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))         
      CALL DAMUL(ISCRDA(  6+IDAA),AEK        ,ISCRDA(  7+IDAA))         
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=-RV*(EL(I)+ALDA(IH,1)*ALDA(IH,2))/C4E3 ;               *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(6)))         
        if(ih.eq.1) goto 110
        goto 190
C-----------------------------------------------------------------------
C  COMBINED FUNCTION MAGNET HORIZONTAL
C  FOCUSSING
C-----------------------------------------------------------------------
  130   ih=0
*FOX  FOKQ=EK(I) ;                                                      *FOX
      RSCRRI(  1+IDAA) = EK         (I          )                       
      RSCRRI(100) = RSCRRI(  1+IDAA)                                    
      CALL DACON(FOKQ       ,RSCRRI(100))                               
  140   continue
        if(abs(ek(i)).le.pieni) goto 60
        if(abs(ed(i)).le.pieni) goto 100
        if(abs(ek(i)-ed(i)*ed(i)).le.pieni) goto 20
*FOX  WF=ED(I)/DPSQ ;                                                   *FOX
      RSCRRI(  1+IDAA) = ED         (I          )                       
      CALL DADIC(DPSQ       ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),WF         )                          
*FOX  FOK=FOKQ/DPD-WF*WF ;                                              *FOX
      CALL DADIV(FOKQ       ,DPD        ,ISCRDA(  1+IDAA))              
      CALL DAMUL(WF         ,WF         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),FOK        )                          
c*FOX  AFOK=ABS(FOK) ;                                                  *FOX
*FOX  AFOK=FOK ;                                                        *FOX
      CALL DACOP(FOK        ,AFOK       )                               
      if(dare(AFOK).lt.zero) then                                       *FOX
*FOX  AFOK=-AFOK ;                                                      *FOX
      CALL DACMU(AFOK       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),AFOK       )                          
      endif
*FOX  HI=SQRT(AFOK) ;                                                   *FOX
      CALL DAFUN('SQRT  ',AFOK       ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HI         )                          
*FOX  FI=HI*EL(I) ;                                                     *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACMU(HI         ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),FI         )                          
        if(dare(fok).gt.zero) goto 160
  150   ih=ih+1
*FOX  SI=SIN(FI) ;                                                      *FOX
      CALL DAFUN('SIN   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),SI         )                          
*FOX  CO=COS(FI) ;                                                      *FOX
      CALL DAFUN('COS   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),CO         )                          
*FOX  WFA=WF/AFOK*(ONE-CO)/DPSQ ;                                       *FOX
      CALL DASUC(CO         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DADIV(WF         ,AFOK       ,ISCRDA(  2+IDAA))              
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DADIV(ISCRDA(  3+IDAA),DPSQ       ,ISCRDA(  4+IDAA))         
      CALL DACOP(ISCRDA(  4+IDAA),WFA        )                          
*FOX  WFHI=WF/HI*SI/DPSQ ;                                              *FOX
      CALL DADIV(WF         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(ISCRDA(  1+IDAA),SI         ,ISCRDA(  2+IDAA))         
      CALL DADIV(ISCRDA(  2+IDAA),DPSQ       ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),WFHI       )                          
*FOX  ALDA(IH,1)=CO ;                                                   *FOX
      CALL DACOP(CO         ,ALDA       (IH         ,(1)))              
*FOX  ALDA(IH,2)=SI/HI ;                                                *FOX
      CALL DADIV(SI         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=-SI*HI ;                                               *FOX
      CALL DACMU(SI         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),HI         ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=CO ;                                                   *FOX
      CALL DACOP(CO         ,ALDA       (IH         ,(4)))              
*FOX  ALDA(IH,5)=-WFA*DPDA*C1E3 ;                                       *FOX
      CALL DACMU(WFA        ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),DPDA       ,ISCRDA(  2+IDAA))         
      CALL DACMU(ISCRDA(  2+IDAA),ONE*C1E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ALDA       (IH         ,(5)))         
*FOX  ALDA(IH,6)=-WFHI*DPDA*C1E3 ;                                      *FOX
      CALL DACMU(WFHI       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),DPDA       ,ISCRDA(  2+IDAA))         
      CALL DACMU(ISCRDA(  2+IDAA),ONE*C1E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ALDA       (IH         ,(6)))         
*FOX  SM12=EL(I)-ALDA(IH,1)*ALDA(IH,2) ;                                *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  5+IDAA),SM12       )                          
*FOX  SM23=ALDA(IH,2)*ALDA(IH,3) ;                                      *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (IH         ,(3)),ISCRDA(  2+IDAA))         
      CALL DAMUL(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),SM23       )                          
*FOX  ASDA(IH,1)=(-RV*(DPDA*DPDA/(FOUR*DPD)*SM12+DPDA                   *FOX
*FOX  *(EL(I)-ALDA(IH,2)))/AFOK*WF*WF+EL(I)*(ONE-RV))*C1E3 ;            *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  2+IDAA))         
      CALL DACMU(DPD        ,ONE*FOUR       ,ISCRDA(  3+IDAA))          
      CALL DASUC(ISCRDA(  2+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  4+IDAA))
     *                                                                  
      CALL DAMUL(DPDA       ,DPDA       ,ISCRDA(  5+IDAA))              
      CALL DADIV(ISCRDA(  5+IDAA),ISCRDA(  3+IDAA),ISCRDA(  6+IDAA))    
      CALL DAMUL(ISCRDA(  6+IDAA),SM12       ,ISCRDA(  7+IDAA))         
      CALL DAMUL(DPDA       ,ISCRDA(  4+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DASUC(RV         ,ONE*ONE        ,ISCRDA( 10+IDAA))          
      RSCRRI( 11+IDAA) = EL         (I          )                       
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA( 12+IDAA))        
      CALL DAMUL(ISCRDA( 12+IDAA),ISCRDA(  9+IDAA),ISCRDA( 13+IDAA))    
      CALL DADIV(ISCRDA( 13+IDAA),AFOK       ,ISCRDA( 14+IDAA))         
      CALL DAMUL(ISCRDA( 14+IDAA),WF         ,ISCRDA( 15+IDAA))         
      CALL DAMUL(ISCRDA( 15+IDAA),WF         ,ISCRDA( 16+IDAA))         
      CALL DACMU(ISCRDA( 10+IDAA),ONE*RSCRRI( 11+IDAA),ISCRDA( 17+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA( 16+IDAA),ISCRDA( 17+IDAA),ISCRDA( 18+IDAA))    
      CALL DACMU(ISCRDA( 18+IDAA),ONE*C1E3       ,ISCRDA( 19+IDAA))     
      CALL DACOP(ISCRDA( 19+IDAA),ASDA       (IH         ,(1)))         
*FOX  ASDA(IH,2)=-RV*(DPDA*WF/(TWO*DPSQ)*SM12-DPD*WFHI) ;               *FOX
      CALL DACMU(DPSQ       ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(DPDA       ,WF         ,ISCRDA(  2+IDAA))              
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DAMUL(ISCRDA(  3+IDAA),SM12       ,ISCRDA(  4+IDAA))         
      CALL DAMUL(DPD        ,WFHI       ,ISCRDA(  5+IDAA))              
      CALL DASUB(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  7+IDAA))        
      CALL DAMUL(ISCRDA(  7+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(2)))         
*FOX  ASDA(IH,3)=-RV*(DPDA*HALF/AFOK/DPD*ED(I)*SM23-DPD*WFA) ;          *FOX
      RSCRRI(  1+IDAA) = ED         (I          )                       
      CALL DACMU(DPDA       ,ONE*HALF       ,ISCRDA(  2+IDAA))          
      CALL DADIV(ISCRDA(  2+IDAA),AFOK       ,ISCRDA(  3+IDAA))         
      CALL DADIV(ISCRDA(  3+IDAA),DPD        ,ISCRDA(  4+IDAA))         
      CALL DACMU(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DAMUL(ISCRDA(  5+IDAA),SM23       ,ISCRDA(  6+IDAA))         
      CALL DAMUL(DPD        ,WFA        ,ISCRDA(  7+IDAA))              
      CALL DASUB(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  9+IDAA))        
      CALL DAMUL(ISCRDA(  9+IDAA),ISCRDA(  8+IDAA),ISCRDA( 10+IDAA))    
      CALL DACOP(ISCRDA( 10+IDAA),ASDA       (IH         ,(3)))         
*FOX  ASDA(IH,4)=-RV*SM23/C2E3 ;                                        *FOX
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),SM23       ,ISCRDA(  2+IDAA))         
      CALL DACDI(ISCRDA(  2+IDAA),ONE*C2E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=-RV*SM12*AFOK/C4E3 ;                                   *FOX
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),SM12       ,ISCRDA(  2+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),AFOK       ,ISCRDA(  3+IDAA))         
      CALL DACDI(ISCRDA(  3+IDAA),ONE*C4E3       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=-RV*(EL(I)+ALDA(IH,1)*ALDA(IH,2))/C4E3 ;               *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(6)))         
        ih=ih+1
        if(ih.gt.2) ih=1
c*FOX  AEK=ABS(EK(I)/DPD) ;                                             *FOX
*FOX  AEK=EK(I)/DPD ;                                                   *FOX
      RSCRRI(  1+IDAA) = EK         (I          )                       
      CALL DADIC(DPD        ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),AEK        )                          
      if(dare(AEK).lt.zero) then                                        *FOX
*FOX  AEK=-AEK ;                                                        *FOX
      CALL DACMU(AEK        ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),AEK        )                          
      endif
*FOX  HI=SQRT(AEK) ;                                                    *FOX
      CALL DAFUN('SQRT  ',AEK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HI         )                          
*FOX  FI=HI*EL(I) ;                                                     *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACMU(HI         ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),FI         )                          
*FOX  HP=EXP(FI) ;                                                      *FOX
      CALL DAFUN('EXP   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HP         )                          
*FOX  HM=ONE/HP ;                                                       *FOX
      CALL DADIC(HP         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),HM         )                          
*FOX  HC=(HP+HM)*HALF ;                                                 *FOX
      CALL DAADD(HP         ,HM         ,ISCRDA(  1+IDAA))              
      CALL DACMU(ISCRDA(  1+IDAA),ONE*HALF       ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),HC         )                          
*FOX  HS=(HP-HM)*HALF ;                                                 *FOX
      CALL DASUB(HP         ,HM         ,ISCRDA(  1+IDAA))              
      CALL DACMU(ISCRDA(  1+IDAA),ONE*HALF       ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),HS         )                          
*FOX  ALDA(IH,1)=HC ;                                                   *FOX
      CALL DACOP(HC         ,ALDA       (IH         ,(1)))              
*FOX  ALDA(IH,2)=EL(I) ;                                                *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      RSCRRI(100) = RSCRRI(  1+IDAA)                                    
      CALL DACON(ALDA       (IH         ,(2)),RSCRRI(100))              
*FOX  ALDA(IH,2)=HS/HI ;                                                *FOX
      CALL DADIV(HS         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=HS*HI ;                                                *FOX
      CALL DAMUL(HS         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=HC ;                                                   *FOX
      CALL DACOP(HC         ,ALDA       (IH         ,(4)))              
*FOX  ASDA(IH,4)=-RV*ALDA(IH,2)*ALDA(IH,3)/C2E3 ;                       *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (IH         ,(3)),ISCRDA(  2+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  3+IDAA))        
      CALL DAMUL(ISCRDA(  3+IDAA),ISCRDA(  1+IDAA),ISCRDA(  4+IDAA))    
      CALL DAMUL(ISCRDA(  4+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DACDI(ISCRDA(  5+IDAA),ONE*C2E3       ,ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=+RV*(EL(I)-ALDA(IH,1)*ALDA(IH,2))*AEK/C4E3 ;           *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DAMUL(RV         ,ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))         
      CALL DAMUL(ISCRDA(  6+IDAA),AEK        ,ISCRDA(  7+IDAA))         
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=-RV*(EL(I)+ALDA(IH,1)*ALDA(IH,2))/C4E3 ;               *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(6)))         
        goto 190
C--DEFOCUSSING
  160   ih=ih+1
*FOX  HP=EXP(FI) ;                                                      *FOX
      CALL DAFUN('EXP   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HP         )                          
*FOX  HM=ONE/HP ;                                                       *FOX
      CALL DADIC(HP         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),HM         )                          
*FOX  HC=(HP+HM)*HALF ;                                                 *FOX
      CALL DAADD(HP         ,HM         ,ISCRDA(  1+IDAA))              
      CALL DACMU(ISCRDA(  1+IDAA),ONE*HALF       ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),HC         )                          
*FOX  HS=(HP-HM)*HALF ;                                                 *FOX
      CALL DASUB(HP         ,HM         ,ISCRDA(  1+IDAA))              
      CALL DACMU(ISCRDA(  1+IDAA),ONE*HALF       ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),HS         )                          
*FOX  ALDA(IH,1)=HC ;                                                   *FOX
      CALL DACOP(HC         ,ALDA       (IH         ,(1)))              
*FOX  ALDA(IH,2)=HS/HI ;                                                *FOX
      CALL DADIV(HS         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=HS*HI ;                                                *FOX
      CALL DAMUL(HS         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=HC ;                                                   *FOX
      CALL DACOP(HC         ,ALDA       (IH         ,(4)))              
*FOX  WFA=WF/AFOK*(ONE-HC)/DPSQ ;                                       *FOX
      CALL DASUC(HC         ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DADIV(WF         ,AFOK       ,ISCRDA(  2+IDAA))              
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DADIV(ISCRDA(  3+IDAA),DPSQ       ,ISCRDA(  4+IDAA))         
      CALL DACOP(ISCRDA(  4+IDAA),WFA        )                          
*FOX  WFHI=WF/HI*HS/DPSQ ;                                              *FOX
      CALL DADIV(WF         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(ISCRDA(  1+IDAA),HS         ,ISCRDA(  2+IDAA))         
      CALL DADIV(ISCRDA(  2+IDAA),DPSQ       ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),WFHI       )                          
*FOX  ALDA(IH,5)= WFA*DPDA*C1E3 ;                                       *FOX
      CALL DAMUL(WFA        ,DPDA       ,ISCRDA(  1+IDAA))              
      CALL DACMU(ISCRDA(  1+IDAA),ONE*C1E3       ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),ALDA       (IH         ,(5)))         
*FOX  ALDA(IH,6)=-WFHI*DPDA*C1E3 ;                                      *FOX
      CALL DACMU(WFHI       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),DPDA       ,ISCRDA(  2+IDAA))         
      CALL DACMU(ISCRDA(  2+IDAA),ONE*C1E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ALDA       (IH         ,(6)))         
*FOX  SM12=EL(I)-ALDA(IH,1)*ALDA(IH,2) ;                                *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  5+IDAA),SM12       )                          
*FOX  SM23=ALDA(IH,2)*ALDA(IH,3) ;                                      *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (IH         ,(3)),ISCRDA(  2+IDAA))         
      CALL DAMUL(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),SM23       )                          
*FOX  ASDA(IH,1)=(RV*(DPDA*DPDA/(FOUR*DPD)*SM12                         *FOX
*FOX  +DPDA*(EL(I)-ALDA(IH,2)))/AFOK*WF*WF+EL(I)*(ONE-RV))*C1E3 ;       *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  2+IDAA))         
      CALL DACMU(DPD        ,ONE*FOUR       ,ISCRDA(  3+IDAA))          
      CALL DASUC(ISCRDA(  2+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  4+IDAA))
     *                                                                  
      CALL DAMUL(DPDA       ,DPDA       ,ISCRDA(  5+IDAA))              
      CALL DADIV(ISCRDA(  5+IDAA),ISCRDA(  3+IDAA),ISCRDA(  6+IDAA))    
      CALL DAMUL(ISCRDA(  6+IDAA),SM12       ,ISCRDA(  7+IDAA))         
      CALL DAMUL(DPDA       ,ISCRDA(  4+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DASUC(RV         ,ONE*ONE        ,ISCRDA( 10+IDAA))          
      RSCRRI( 11+IDAA) = EL         (I          )                       
      CALL DAMUL(RV         ,ISCRDA(  9+IDAA),ISCRDA( 12+IDAA))         
      CALL DADIV(ISCRDA( 12+IDAA),AFOK       ,ISCRDA( 13+IDAA))         
      CALL DAMUL(ISCRDA( 13+IDAA),WF         ,ISCRDA( 14+IDAA))         
      CALL DAMUL(ISCRDA( 14+IDAA),WF         ,ISCRDA( 15+IDAA))         
      CALL DACMU(ISCRDA( 10+IDAA),ONE*RSCRRI( 11+IDAA),ISCRDA( 16+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA( 15+IDAA),ISCRDA( 16+IDAA),ISCRDA( 17+IDAA))    
      CALL DACMU(ISCRDA( 17+IDAA),ONE*C1E3       ,ISCRDA( 18+IDAA))     
      CALL DACOP(ISCRDA( 18+IDAA),ASDA       (IH         ,(1)))         
*FOX  ASDA(IH,2)=-RV*(DPDA*WF/(TWO*DPSQ)*SM12-DPD*WFHI) ;               *FOX
      CALL DACMU(DPSQ       ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(DPDA       ,WF         ,ISCRDA(  2+IDAA))              
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DAMUL(ISCRDA(  3+IDAA),SM12       ,ISCRDA(  4+IDAA))         
      CALL DAMUL(DPD        ,WFHI       ,ISCRDA(  5+IDAA))              
      CALL DASUB(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  7+IDAA))        
      CALL DAMUL(ISCRDA(  7+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(2)))         
*FOX  ASDA(IH,3)=RV*(DPDA*HALF/AFOK/DPD*ED(I)*SM23-DPD*WFA) ;           *FOX
      RSCRRI(  1+IDAA) = ED         (I          )                       
      CALL DACMU(DPDA       ,ONE*HALF       ,ISCRDA(  2+IDAA))          
      CALL DADIV(ISCRDA(  2+IDAA),AFOK       ,ISCRDA(  3+IDAA))         
      CALL DADIV(ISCRDA(  3+IDAA),DPD        ,ISCRDA(  4+IDAA))         
      CALL DACMU(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DAMUL(ISCRDA(  5+IDAA),SM23       ,ISCRDA(  6+IDAA))         
      CALL DAMUL(DPD        ,WFA        ,ISCRDA(  7+IDAA))              
      CALL DASUB(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DAMUL(RV         ,ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))         
      CALL DACOP(ISCRDA(  9+IDAA),ASDA       (IH         ,(3)))         
*FOX  ASDA(IH,4)=-RV*SM23/C2E3 ;                                        *FOX
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),SM23       ,ISCRDA(  2+IDAA))         
      CALL DACDI(ISCRDA(  2+IDAA),ONE*C2E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=+RV*SM12*AFOK/C4E3 ;                                   *FOX
      CALL DAMUL(RV         ,SM12       ,ISCRDA(  1+IDAA))              
      CALL DAMUL(ISCRDA(  1+IDAA),AFOK       ,ISCRDA(  2+IDAA))         
      CALL DACDI(ISCRDA(  2+IDAA),ONE*C4E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=-RV*(EL(I)+ALDA(IH,1)*ALDA(IH,2))/C4E3 ;               *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(6)))         
        ih=ih+1
        if(ih.gt.2) ih=1
c*FOX  AEK=ABS(EK(I)/DPD) ;                                             *FOX
*FOX  AEK=EK(I)/DPD ;                                                   *FOX
      RSCRRI(  1+IDAA) = EK         (I          )                       
      CALL DADIC(DPD        ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),AEK        )                          
      if(dare(AEK).lt.zero) then                                        *FOX
*FOX  AEK=-AEK ;                                                        *FOX
      CALL DACMU(AEK        ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),AEK        )                          
      endif
*FOX  HI=SQRT(AEK) ;                                                    *FOX
      CALL DAFUN('SQRT  ',AEK        ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),HI         )                          
*FOX  FI=HI*EL(I) ;                                                     *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACMU(HI         ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),FI         )                          
*FOX  SI=SIN(FI) ;                                                      *FOX
      CALL DAFUN('SIN   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),SI         )                          
*FOX  CO=COS(FI) ;                                                      *FOX
      CALL DAFUN('COS   ',FI         ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),CO         )                          
*FOX  ALDA(IH,1)=CO ;                                                   *FOX
      CALL DACOP(CO         ,ALDA       (IH         ,(1)))              
*FOX  ALDA(IH,2)=SI/HI ;                                                *FOX
      CALL DADIV(SI         ,HI         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       (IH         ,(2)))         
*FOX  ALDA(IH,3)=-SI*HI ;                                               *FOX
      CALL DACMU(SI         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),HI         ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),ALDA       (IH         ,(3)))         
*FOX  ALDA(IH,4)=CO ;                                                   *FOX
      CALL DACOP(CO         ,ALDA       (IH         ,(4)))              
*FOX  ASDA(IH,4)=-RV*ALDA(IH,2)*ALDA(IH,3)/C2E3 ;                       *FOX
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (IH         ,(3)),ISCRDA(  2+IDAA))         
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  3+IDAA))        
      CALL DAMUL(ISCRDA(  3+IDAA),ISCRDA(  1+IDAA),ISCRDA(  4+IDAA))    
      CALL DAMUL(ISCRDA(  4+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DACDI(ISCRDA(  5+IDAA),ONE*C2E3       ,ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),ASDA       (IH         ,(4)))         
*FOX  ASDA(IH,5)=-RV*(EL(I)-ALDA(IH,1)*ALDA(IH,2))*AEK/C4E3 ;           *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DAMUL(ISCRDA(  7+IDAA),AEK        ,ISCRDA(  8+IDAA))         
      CALL DACDI(ISCRDA(  8+IDAA),ONE*C4E3       ,ISCRDA(  9+IDAA))     
      CALL DACOP(ISCRDA(  9+IDAA),ASDA       (IH         ,(5)))         
*FOX  ASDA(IH,6)=-RV*(EL(I)+ALDA(IH,1)*ALDA(IH,2))/C4E3 ;               *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      CALL DACOP(ALDA       (IH         ,(1)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (IH         ,(2)),ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))
     *                                                                  
      CALL DACMU(RV         ,ONE*(-ONE       ),ISCRDA(  6+IDAA))        
      CALL DAMUL(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACDI(ISCRDA(  7+IDAA),ONE*C4E3       ,ISCRDA(  8+IDAA))     
      CALL DACOP(ISCRDA(  8+IDAA),ASDA       (IH         ,(6)))         
        goto 190
C-----------------------------------------------------------------------
C  COMBINED FUNCTION MAGNET VERTICAL
C-----------------------------------------------------------------------
  170   ih=1
*FOX  FOKQ=-EK(I) ;                                                     *FOX
      RSCRRI(  1+IDAA) = EK         (I          )                       
      RSCRRI(  2+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      RSCRRI(100) = RSCRRI(  2+IDAA)                                    
      CALL DACON(FOKQ       ,RSCRRI(100))                               
        goto 140
C-----------------------------------------------------------------------
C  EDGE FOCUSSING
C-----------------------------------------------------------------------
  180   continue
*FOX  RHOI=ED(I)/DPSQ ;                                                 *FOX
      RSCRRI(  1+IDAA) = ED         (I          )                       
      CALL DADIC(DPSQ       ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),RHOI       )                          
*FOX  FOK=RHOI*SIN(EL(I)*RHOI*HALF)/COS(EL(I)*RHOI*HALF) ;              *FOX
      RSCRRI(  1+IDAA) = EL         (I          )                       
      RSCRRI(  2+IDAA) = EL         (I          )                       
      CALL DACMU(RHOI       ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(ISCRDA(  3+IDAA),ONE*HALF       ,ISCRDA(  4+IDAA))     
      CALL DACMU(RHOI       ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DACMU(ISCRDA(  5+IDAA),ONE*HALF       ,ISCRDA(  6+IDAA))     
      CALL DAFUN('SIN   ',ISCRDA(  4+IDAA),ISCRDA(  7+IDAA))            
      CALL DAFUN('COS   ',ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))            
      CALL DAMUL(RHOI       ,ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))         
      CALL DADIV(ISCRDA(  9+IDAA),ISCRDA(  8+IDAA),ISCRDA( 10+IDAA))    
      CALL DACOP(ISCRDA( 10+IDAA),FOK        )                          
*FOX  ALDA(1,1)=ONE ;                                                   *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       ((1),(1)),RSCRRI(100))                      
*FOX  ALDA(1,2)=ZERO ;                                                  *FOX
      RSCRRI(100) = ZERO                                                
      CALL DACON(ALDA       ((1),(2)),RSCRRI(100))                      
*FOX  ALDA(1,3)=FOK ;                                                   *FOX
      CALL DACOP(FOK        ,ALDA       ((1),(3)))                      
*FOX  ALDA(1,4)=ONE ;                                                   *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       ((1),(4)),RSCRRI(100))                      
*FOX  ALDA(2,1)=ONE ;                                                   *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       ((2),(1)),RSCRRI(100))                      
*FOX  ALDA(2,2)=ZERO ;                                                  *FOX
      RSCRRI(100) = ZERO                                                
      CALL DACON(ALDA       ((2),(2)),RSCRRI(100))                      
*FOX  ALDA(2,3)=-FOK ;                                                  *FOX
      CALL DACMU(FOK        ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),ALDA       ((2),(3)))                 
*FOX  ALDA(2,4)=ONE ;                                                   *FOX
      RSCRRI(100) = ONE                                                 
      CALL DACON(ALDA       ((2),(4)),RSCRRI(100))                      
        goto 190
C-----------------------------------------------------------------------
C   NONLINEAR INSERTION
C-----------------------------------------------------------------------
  190   continue
        do 210 ih=1,2
          do 210 ip=1,6
            do 200 ien=1,nord+1
              if (nvar2.eq.5) then
                call dapri6(alda(ih,ip),result,ien,5)
                ald6(i,ih,ip,ien) = result
                call dapri6(asda(ih,ip),result,ien,5)
                asd6(i,ih,ip,ien) = result
              else if (nvar2.eq.6) then
                call dapri6(alda(ih,ip),result,ien,6)
                ald6(i,ih,ip,ien) = result
                call dapri6(asda(ih,ip),result,ien,6)
                asd6(i,ih,ip,ien) = result
              endif
  200       continue
  210   continue
  220 continue
      if((IDALLOC.eq.2.or.IDALLOC.eq.3).and.ix.eq.numl*ncy) then
        CALL DADAL(WFA     ,1)                                                  
        CALL DADAL(RHOI    ,1)                                                  
        CALL DADAL(AFOK    ,1)                                                  
        CALL DADAL(WF      ,1)                                                  
        CALL DADAL(FOKC    ,1)                                                  
        CALL DADAL(HS      ,1)                                                  
        CALL DADAL(HC      ,1)                                                  
        CALL DADAL(HM      ,1)                                                  
        CALL DADAL(HP      ,1)                                                  
        CALL DADAL(HI1     ,1)                                                  
        CALL DADAL(AEK     ,1)                                                  
        CALL DADAL(FI      ,1)                                                  
        CALL DADAL(HI      ,1)                                                  
        CALL DADAL(RHOC    ,1)                                                  
        CALL DADAL(SIQ     ,1)                                                  
        CALL DADAL(GL      ,1)                                                  
        CALL DADAL(G       ,1)                                                  
        CALL DADAL(CO      ,1)                                                  
        CALL DADAL(SI      ,1)                                                  
        CALL DADAL(AS6     ,1)                                                  
        CALL DADAL(AS4     ,1)                                                  
        CALL DADAL(AS3     ,1)                                                  
        CALL DADAL(SM23    ,1)                                                  
        CALL DADAL(SM12    ,1)                                                  
        CALL DADAL(SM6     ,1)                                                  
        CALL DADAL(SM5     ,1)                                                  
        CALL DADAL(SM4     ,1)                                                  
        CALL DADAL(SM3     ,1)                                                  
        CALL DADAL(SM2     ,1)                                                  
        CALL DADAL(SM1     ,1)                                                  
        CALL DADAL(FOK1    ,1)                                                  
        CALL DADAL(RHO     ,1)                                                  
        CALL DADAL(FOK     ,1)                                                  
        CALL DADAL(DPSQ    ,1)                                                  
        CALL DADAL(DPD     ,1)                                                  
        CALL DADAL(WFHI    ,1)                                                  
        CALL DADAL(FOKQ    ,1)                                                  
C     DADAL AUTOMATIC INCLUSION
      endif
      return
      end

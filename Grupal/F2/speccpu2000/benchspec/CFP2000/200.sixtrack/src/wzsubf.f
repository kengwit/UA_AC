      SUBROUTINE WZSUBF(X,Y,U,V,IX,NBEAM)                               *FOX
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
      parameter ( xcut=7.77d0, ycut=7.46d0 )
      parameter ( h=1.d0/63.d0 )
      parameter ( nx=490, ny=470 )
      parameter ( idim = (nx+2)*(ny+2) )
      parameter ( half=0.5d0, one=1.d0 )
C     character*16 bez,bezb,bezr,erbez,coel,bezl
      character*16 coel
      common /wzcom1/ hrecip, kstep
      common /wzcom2/ wtreal(idim), wtimag(idim)
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor),
     &idalloc
      common/norf/nordf,nvarf,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax
      common/tcorrc/coel(10)
      parameter ( a1=0.5124242248d0, a2=0.0517653588d0 )
      parameter ( b1=0.2752551286d0, b2=2.7247448714d0 )
C-----------------------------------------------------------------------
*FOX  B D ;
*FOX  D V DA EXT X NORD NVAR ; D V DA EXT Y NORD NVAR ;
*FOX  D V DA EXT U NORD NVAR ; D V DA EXT V NORD NVAR ;
*FOX  D V DA INT XH NORD NVAR ; D V DA INT YH NORD NVAR ;               *FOX
*FOX  D V DA INT XHREL NORD NVAR ; D V DA INT YHREL NORD NVAR ;         *FOX
*FOX  D V DA INT USUM3 NORD NVAR ; D V DA INT VSUM3 NORD NVAR ;         *FOX
*FOX  D V DA INT USUM NORD NVAR ; D V DA INT VSUM NORD NVAR ;           *FOX
*FOX  D V DA INT P NORD NVAR ; D V DA INT Q NORD NVAR ;                 *FOX
*FOX  D V DA INT QSQ NORD NVAR ; D V DA INT T NORD NVAR ;               *FOX
*FOX  D V DA INT R NORD NVAR ; D V DA INT SREAL NORD NVAR ;             *FOX
*FOX  D V DA INT SIMAG NORD NVAR ;                                      *FOX
*FOX  D V RE INT ONE ; D V RE INT HALF ; D V RE INT TWO ;               *FOX
*FOX  D V RE INT HRECIP ; D V RE INT DMU ; D V RE INT DNU ;             *FOX
*FOX  D V RE INT TDD13R ; D V RE INT TDD13I ; D V RE INT TDDDR ;        *FOX
*FOX  D V RE INT TDDDI ;                                                *FOX
*FOX  D V RE INT D12R ; D V RE INT D12I ;                               *FOX
*FOX  D V RE INT W1R ; D V RE INT W1I ;                                 *FOX
*FOX  D V RE INT A1 ; D V RE INT A2 ; D V RE INT B1 ; D V RE INT B2 ;   *FOX
*FOX  D V IN INT K ; D V IN INT MU ; D V IN INT NU ;                    *FOX
*FOX  D F RE DARE 1 ;
*FOX  E D ;
*FOX{
      INTEGER X       
      INTEGER Y       
      INTEGER U       
      INTEGER V       
      INTEGER XH      
      INTEGER YH      
      INTEGER XHREL   
      INTEGER YHREL   
      INTEGER USUM3   
      INTEGER VSUM3   
      INTEGER USUM    
      INTEGER VSUM    
      INTEGER P       
      INTEGER Q       
      INTEGER QSQ     
      INTEGER T       
      INTEGER R       
      INTEGER SREAL   
      INTEGER SIMAG   
      INTEGER ISCRDA, ISCRRI,IDAO
C     INTEGER LFOX0, LFOX1
      REAL*8 RSCRRI
      COMMON/DASCR/ISCRDA(100),RSCRRI(100),ISCRRI(100),IDAO
      save
      if(idalloc.eq.1.or.idalloc.eq.3) then                           
         CALL DAKEY('FOX V2.1')
         CALL DAALL(XH      ,1,'XH        ',NORD,NVAR)
         CALL DAALL(YH      ,1,'YH        ',NORD,NVAR)
         CALL DAALL(XHREL   ,1,'XHREL     ',NORD,NVAR)
         CALL DAALL(YHREL   ,1,'YHREL     ',NORD,NVAR)
         CALL DAALL(USUM3   ,1,'USUM3     ',NORD,NVAR)
         CALL DAALL(VSUM3   ,1,'VSUM3     ',NORD,NVAR)
         CALL DAALL(USUM    ,1,'USUM      ',NORD,NVAR)
         CALL DAALL(VSUM    ,1,'VSUM      ',NORD,NVAR)
         CALL DAALL(P       ,1,'P         ',NORD,NVAR)
         CALL DAALL(Q       ,1,'Q         ',NORD,NVAR)
         CALL DAALL(QSQ     ,1,'QSQ       ',NORD,NVAR)
         CALL DAALL(T       ,1,'T         ',NORD,NVAR)
         CALL DAALL(R       ,1,'R         ',NORD,NVAR)
         CALL DAALL(SREAL   ,1,'SREAL     ',NORD,NVAR)
         CALL DAALL(SIMAG   ,1,'SIMAG     ',NORD,NVAR)
      ENDIF
      IDAA = IDAO
*FOX}
C-----------------------------------------------------------------------
      if(idalloc.eq.1) idalloc=0
      if(idalloc.eq.3) idalloc=2
C-----------------------------------------------------------------------
c      
c  *********************************************************************
c      
c  This subroutine sets u=real(w(z)) and v=imag(w(z)), where z=x+i*y and
c  where w(z) is the complex error function defined by formula 7.1.3 in
c  "Handbook of Mathematical functions [eds. M.Abramowitz & I.A.Stegun,
c  Washington, 1966].  The absolute error of the computed value is less *FOX
c  than 1E-8.
c      
c  *** Note.  Subroutine WZSET must have been called before this sub-
c  routine can be used.
c      
c  For (x,y) inside the rectangle with opposite corners (xcut,0) and
c  (0,ycut), where xcut and ycut have been set by WZSET, an interpo-
c  lation formula is used.  For (x,y) outside this rectangle, a two-
c  term rational approximation is used.
c      
c  (G.A.Erskine, 29.09.1997)
c      
c  *********************************************************************
c      
c  Start.
      if ( dare(x).ge.xcut .or. dare(y).ge.ycut ) go to 1000
c      
c  Third-order divided-difference interpolation over the corners of a
c  square [e.g. formula (2.5.1) in "Introduction to Numerical Analysis"
c  (F.B.Hildebrand New York, 1957), but with complex nodes and          *FOX
c  function values].
c      
c  In the interpolation formula the corners of the grid square contain-
c  ing (x,y) are numbered (0,0)=3, (h,0)=4, (h,h)=1, (0,h)=2.
c  Identifiers d, dd and ddd denote divided-differences of orders 1, 2
c  and 3 respectively, and a preceding 't' indicates twice the value.
c      
c  Turned into a DA routine by F.Schmidt 17.02.1998
c      
c      
      TWO=2.D0
*FOX  XH = HRECIP*X ;                                                   *FOX
      CALL DACMU(X          ,ONE*HRECIP     ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),XH         )                          
*FOX  YH = HRECIP*Y ;                                                   *FOX
      CALL DACMU(Y          ,ONE*HRECIP     ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),YH         )                          
      MU = INT(DARE(XH))                                                *FOX
      NU = INT(DARE(YH))                                                *FOX
c  Compute divided differences.
      K = 2 + MU + NU*KSTEP
      W4R = WTREAL(K)
      W4I = WTIMAG(K)
      K = K - 1
      W3R = WTREAL(K)
      W3I = WTIMAG(K)
      D34R = W4R - W3R
      D34I = W4I - W3I
      K = K + KSTEP
      W2R = WTREAL(K)
      W2I = WTIMAG(K)
      D23R = W2I - W3I
      D23I = W3R - W2R
      TR = D23R - D34R
      TI = D23I - D34I
      TDD24R = TI - TR
      TDD24I = - ( TR + TI )
      K = K + 1
      W1R = WTREAL(K)
      W1I = WTIMAG(K)
      D12R = W1R - W2R
      D12I = W1I - W2I
      TR = D12R - D23R
      TI = D12I - D23I
      TDD13R = TR + TI
      TDD13I = TI - TR
      TDDDR = TDD13I - TDD24I
      TDDDI = TDD24R - TDD13R
c  Evaluate polynomial.
      DMU=DBLE(MU)
      DNU=DBLE(NU)
*FOX  XHREL = XH - DMU ;                                                *FOX
      CALL DACSU(XH         ,ONE*DMU        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),XHREL      )                          
*FOX  YHREL = YH - DNU ;                                                *FOX
      CALL DACSU(YH         ,ONE*DNU        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),YHREL      )                          
*FOX  USUM3 = HALF*( TDD13R + ( XHREL*TDDDR - YHREL*TDDDI ) ) ;         *FOX
      CALL DACMU(XHREL      ,ONE*TDDDR      ,ISCRDA(  1+IDAA))          
      CALL DACMU(YHREL      ,ONE*TDDDI      ,ISCRDA(  2+IDAA))          
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACAD(ISCRDA(  3+IDAA),ONE*TDD13R     ,ISCRDA(  4+IDAA))     
      CALL DACMU(ISCRDA(  4+IDAA),ONE*HALF       ,ISCRDA(  5+IDAA))     
      CALL DACOP(ISCRDA(  5+IDAA),USUM3      )                          
*FOX  VSUM3 = HALF*( TDD13I + ( XHREL*TDDDI + YHREL*TDDDR ) ) ;         *FOX
      CALL DACMU(XHREL      ,ONE*TDDDI      ,ISCRDA(  1+IDAA))          
      CALL DACMU(YHREL      ,ONE*TDDDR      ,ISCRDA(  2+IDAA))          
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACAD(ISCRDA(  3+IDAA),ONE*TDD13I     ,ISCRDA(  4+IDAA))     
      CALL DACMU(ISCRDA(  4+IDAA),ONE*HALF       ,ISCRDA(  5+IDAA))     
      CALL DACOP(ISCRDA(  5+IDAA),VSUM3      )                          
*FOX  YHREL = YHREL - ONE ;                                             *FOX
      CALL DACSU(YHREL      ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),YHREL      )                          
*FOX  USUM = D12R + ( XHREL*USUM3 - YHREL*VSUM3 ) ;                     *FOX
      CALL DAMUL(XHREL      ,USUM3      ,ISCRDA(  1+IDAA))              
      CALL DAMUL(YHREL      ,VSUM3      ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACAD(ISCRDA(  3+IDAA),ONE*D12R       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),USUM       )                          
*FOX  VSUM = D12I + ( XHREL*VSUM3 + YHREL*USUM3 ) ;                     *FOX
      CALL DAMUL(XHREL      ,VSUM3      ,ISCRDA(  1+IDAA))              
      CALL DAMUL(YHREL      ,USUM3      ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACAD(ISCRDA(  3+IDAA),ONE*D12I       ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),VSUM       )                          
*FOX  XHREL = XHREL - ONE ;                                             *FOX
      CALL DACSU(XHREL      ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),XHREL      )                          
*FOX  U = W1R + ( XHREL*USUM - YHREL*VSUM ) ;                           *FOX
      CALL DAMUL(XHREL      ,USUM       ,ISCRDA(  1+IDAA))              
      CALL DAMUL(YHREL      ,VSUM       ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACAD(ISCRDA(  3+IDAA),ONE*W1R        ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),U          )                          
*FOX  V = W1I + ( XHREL*VSUM + YHREL*USUM ) ;                           *FOX
      CALL DAMUL(XHREL      ,VSUM       ,ISCRDA(  1+IDAA))              
      CALL DAMUL(YHREL      ,USUM       ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACAD(ISCRDA(  3+IDAA),ONE*W1I        ,ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),V          )                          
      goto 2000
c      
c  Two-term rational approximation to w(z) [Footnote to Table 7.9       *FOX
c  in "Handbook of Mathematical Functions (eds. M.Abramowitz &
c  I.A.Stegun, Washington, 1966), but with additional digits in
c  the constants]:
c              u+i*v = i*z*( a1/(z**2-b1) + a2/(z**2-b2) ).
c  Maximum absolute error:
c        <1.E-6  for  x>=4.9  or  y>=4.4
c        <1.E-7  for  x>=6.1  or  y>=5.7
c        <1.E-8  for  x>=7.8  or  y>=7.5
c      
 1000 continue
*FOX  P=X*X-Y*Y ;                                                       *FOX
      CALL DAMUL(X          ,X          ,ISCRDA(  1+IDAA))              
      CALL DAMUL(Y          ,Y          ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),P          )                          
*FOX  Q=TWO*X*Y ;                                                       *FOX
      CALL DACMU(X          ,ONE*TWO        ,ISCRDA(  1+IDAA))          
      CALL DAMUL(ISCRDA(  1+IDAA),Y          ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),Q          )                          
*FOX  QSQ=Q*Q ;                                                         *FOX
      CALL DAMUL(Q          ,Q          ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),QSQ        )                          
c  First term.
*FOX  T=P-B1 ;                                                          *FOX
      CALL DACSU(P          ,ONE*B1         ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),T          )                          
*FOX  R=A1/(T*T+QSQ) ;                                                  *FOX
      CALL DAMUL(T          ,T          ,ISCRDA(  1+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),QSQ        ,ISCRDA(  2+IDAA))         
      CALL DADIC(ISCRDA(  2+IDAA),ONE*A1         ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),R          )                          
*FOX  SREAL=R*T ;                                                       *FOX
      CALL DAMUL(R          ,T          ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),SREAL      )                          
*FOX  SIMAG=-R*Q ;                                                      *FOX
      CALL DACMU(R          ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),Q          ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),SIMAG      )                          
c  Second term
*FOX  T=P-B2 ;                                                          *FOX
      CALL DACSU(P          ,ONE*B2         ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),T          )                          
*FOX  R=A2/(T*T+QSQ) ;                                                  *FOX
      CALL DAMUL(T          ,T          ,ISCRDA(  1+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),QSQ        ,ISCRDA(  2+IDAA))         
      CALL DADIC(ISCRDA(  2+IDAA),ONE*A2         ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),R          )                          
*FOX  SREAL=SREAL+R*T ;                                                 *FOX
      CALL DAMUL(R          ,T          ,ISCRDA(  1+IDAA))              
      CALL DAADD(SREAL      ,ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),SREAL      )                          
*FOX  SIMAG=SIMAG-R*Q ;                                                 *FOX
      CALL DAMUL(R          ,Q          ,ISCRDA(  1+IDAA))              
      CALL DASUB(SIMAG      ,ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),SIMAG      )                          
c  Multiply by i*z.
*FOX  U=-(Y*SREAL+X*SIMAG) ;                                            *FOX
      CALL DAMUL(Y          ,SREAL      ,ISCRDA(  1+IDAA))              
      CALL DAMUL(X          ,SIMAG      ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACMU(ISCRDA(  3+IDAA),ONE*(-ONE       ),ISCRDA(  4+IDAA))   
      CALL DACOP(ISCRDA(  4+IDAA),U          )                          
*FOX  V=X*SREAL-Y*SIMAG ;                                               *FOX
      CALL DAMUL(X          ,SREAL      ,ISCRDA(  1+IDAA))              
      CALL DAMUL(Y          ,SIMAG      ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),V          )                          
 2000 continue
      if(IDALLOC.eq.2.and.ix.eq.nbeam) then
        CALL DADAL(SIMAG   ,1)                                                  
        CALL DADAL(SREAL   ,1)                                                  
        CALL DADAL(R       ,1)                                                  
        CALL DADAL(T       ,1)                                                  
        CALL DADAL(QSQ     ,1)                                                  
        CALL DADAL(Q       ,1)                                                  
        CALL DADAL(P       ,1)                                                  
        CALL DADAL(VSUM    ,1)                                                  
        CALL DADAL(USUM    ,1)                                                  
        CALL DADAL(VSUM3   ,1)                                                  
        CALL DADAL(USUM3   ,1)                                                  
        CALL DADAL(YHREL   ,1)                                                  
        CALL DADAL(XHREL   ,1)                                                  
        CALL DADAL(YH      ,1)                                                  
        CALL DADAL(XH      ,1)                                                  
C     DADAL AUTOMATIC INCLUSION
      endif
      return
      end

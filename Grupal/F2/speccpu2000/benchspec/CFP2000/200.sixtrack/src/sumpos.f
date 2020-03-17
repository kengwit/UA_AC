C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine sumpos
C-----------------------------------------------------------------------
C  SUBROUTINE TO SUMMARIZE THE RESULTS OF THE POSTPROCESSING
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
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
      character*4 ch
      dimension d(60)
      save
      rewind 10
      do 10 i=1,1000
        ch=' '
        read(10,*,end=20,iostat=ierro) (d(j),j=1,60)
        if(ierro.gt.0) then
          write(6,*) '**ERROR**'
          write(6,*) 'CORRUPTED INPUT FILE FOR SUMMARY OF THE',
     +    ' POSTPROCESSING ERROR # : ',ierro
          return
        endif
        if(i.eq.1) write(6,10000)
        if(abs(d(2)).gt.pieni) ch='LOST'
        if(d(22).ge.d(23)) then
          dlost=d(23)
        else
          dlost=d(22)
        endif
        write(6,10010) nint(dlost),d(3),d(5),d(7),d(9),d(10),d(11),
     +  d(12),nint(d(16)),nint(d(18)),d(19),d(21),ch,d(4),d(6),d(8),
     +  d(13),nint(d(17)),d(20),d(25),d(14),d(15)
   10 continue
   20 rewind 10
      write(6,10020)
      do 30 i=1,1000
        read(10,*,end=40,iostat=ierro) (d(j),j=1,60)
        if(ierro.gt.0) then
          write(6,*) '**ERROR**'
          write(6,*) 'CORRUPTED INPUT FILE FOR SUMMARY OF THE',
     +    ' POSTPROCESSING ERROR # : ',ierro
          return
        endif
        write(6,10030) i,nint(d(60)),nint(d(59)),nint(d(60))*nint(d(24))
   30 continue
   40 continue
      write(6,10040)
C-----------------------------------------------------------------------
      return
10000 format(1h /131('-')/t10,'SUMMARY OF THE POSTPROCESSING' //t1,125(
     +'-'), /t1,'|',8x,'|',11x,'|',11x,'|',12x,'|',10x,
     +'|NORMALIZED| SLOPE  |',13x,'|',10x,'|',21x,'|', /t1,
     +'|  TURN  |   LINEAR  |   BETA-   | AMPLITUDES | MOMENTUM |',
     +'PHASESPACE| OF THE |  NONLINEAR  |  NEAREST |',7x,'SMEAR OF',6x,
     +'|', /t1,
     +'| NUMBER |   TUNES   | FUNCTIONS |            | DEVIATION|',
     +' DISTANCE |DISTANCE|  DETUNING   | RESONANCE|    THE EMITTANCES'
     +,3x,'|',/t1,125('-'), /t1,
     +'|        |           |     [M]   |     [MM]   |          |',
     +'          |        |             |     |ORD.|',
     +'    [%]  |      [%]  |'/t1,125('-'))
10010 format(t1,'|',i8,'|X ',f9.5,'|X ',f9.4,'|X ',f10.6,'|',d10.4, '|',
     +d10.4,'|',f8.4,'|X ',d11.5,'|X ',i3,'| ',i2,' |X ', f7.3,'|X+Z ',
     +f7.3,'|' /t1,'|  ',a4,'  |Z ',f9.5,'|Z ',f9.4,'|Z ',f10.6,'|',10x,
     +'|',10x,'|',8x,'|+/- ',d9.3,'|Z ',i3,'|    |Z ', f7.3,'|    ',7x,
     +'|' /t1,'|',8x,'|QS ',f8.6,'|  ',9x,'|  ',10x,'|',10x, '|',10x,'|'
     +,8x,'|Z ',d11.5,'|  ',3x,'|    |  ', 7x,'|    ',7x,'|' /t1,'|',8x,
     +'|  ',9x,'|  ',9x,'|  ',10x,'|',10x, '|',10x,'|',8x,'|+/- ',d9.3,
     +'|  ',3x,'|    |  ', 7x,'|    ',7x,'|'/t1,125('-'))
10020 format(1h /131('-')/t10,'RANDOM SETS USED' //
     +'  CASE  |  # OF RANDOM SET  |  MAX. POSSIBLE SETS   |    ',
     +' SEED'/65('-'))
10030 format(3x,i2,13x,i2,19x,i2,13x,i8)
10040 format(65('-')//131('-'))
      end

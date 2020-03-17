C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine intepr(i,j,ch,ch1)
C-----------------------------------------------------------------------
C     SUBROUTINE TO INTEPRET INPUT WITH CHARACTERS AND NUMBERS MIXED
C
C     I ... TYPE OF COMBINATION
C
C         1  LINE WITH 1 CHARACTERSTRING FOLLOWED BY NUMBERS
C         2  LINE WITH CHARACTERSTRINGS, IF THE FIRST 5 CHARACTERS
C            ARE BLANKS THIS IS INTERPRETED AS A BLANK CHARACTER
C         3  LINE WITH CHARACTERSTRINGS
C         4  LINE WITH 2 CHARACTERSTRINGS
C         5  LINE WITH 1 CHARACTERSTRING AND N*(NUMBER,CHA.STRING)
C         6  LINE WITH 1 NUMBER AND 2 CHARACTERSTRINGS
C         7  LINE WITH 1 NUMBER, 2 CHARACTERSTRINGS AND NUMBERS
C         8  LINE WITH 2 CHARACTERSTRINGS AND NUMBERS
C
C     J ... SKIP THE FIRST (J-1) CHARACTERS OF CHARACTERSTRING CH
C    CH ... INPUT CHARACTERSTRING
C   CH1 ... OUTPUT CHARACTERSTRING
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      character*80 ch
      character*160 ch1
      save
      i0=0
      i1=j
      i2=1
      i4=0
      do 10 ii=j,80
        if(i0.eq.0.and.ch(ii:ii).eq.' ') then
          if(i.eq.2.and.ii.eq.5.and.ch(:5).eq.'     ') then
            ch1(:4)=''' '' '
            i2=5
          endif
          i1=ii+1
          goto 10
        endif
        i0=1
        if(ch(ii:ii).eq.' ') then
          i4=i4+1
          iev=1
          if(mod(i4,2).eq.0) iev=0
          if(i.eq.1) goto 20
          if(i.eq.2.or.i.eq.3.or.i.eq.4.or. (i.eq.5.and.iev.eq.1.).or.
     +    (i.eq.6.and.i4.ge.2).or. (i.eq.7.and.(i4.eq.2.or.i4.eq.3)).or.
     +    (i.eq.8.and.i4.lt.3)) then
            i3=i2+ii-i1+2
            ch1(i2:i3)=''''//ch(i1:ii-1)//''' '
            if(i.eq.4.and.i4.eq.2) goto 30
            i2=i3+1
          endif
          if((i.eq.5.and.iev.eq.0).or. (i4.eq.1.and.(i.eq.6.or.i.eq.7)))
     +    then
            i3=i2+ii-i1
            ch1(i2:i3)=ch(i1:ii)
            i2=i3+1
          endif
          if((i.eq.7.and.i4.gt.3).or.(i.eq.8.and.i4.eq.3)) goto 40
          i0=0
          i1=ii+1
        endif
   10 continue
      goto 30
   20 ch1(1:85)=''''//ch(i1:ii-1)//''''//ch(ii:80)//' / '
      go to 50
   30 i3=i3+1
      ch1(i3:i3+2)=' / '
      go to 50
   40 i3=i2+83-i1
      ch1(i2:i3)=ch(i1:80)//' / '
   50 rewind 11
      write(11,1000,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      return
 1000 format(a160)
      end

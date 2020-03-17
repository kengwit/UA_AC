!
!	Copyright 1998 by Jan C. Vorbrueggen.
!
	Module FFT2D 
		Public :: FFT2DF, FFT2DB
	Contains

	Subroutine FFT2DF (Space, Freq)
!
!	Compute a forward two-dimensional FFT
!
	Implicit NONE
!
!	Included files
!
	Include 'cfft.int'
!
!	Declarations
!
!	  Arguments
!
	Complex(4), Intent(In) :: Space (:, :)
	Complex(4), Pointer    :: Freq  (:, :)
!
!	  Coefficient arrays for FFT routines
!
	Real(4),    Allocatable, Save :: WSaveRow (:), WSaveCol (:)
!
!	  Temporary array
!
	Complex(4), Allocatable, Save :: STemp (:, :)
!
!	  Local variables: array sizes
!
	Integer, Save :: RowLen = 0, ColLen = 0
!
!	  Local variables: counters and such
!
	Integer :: I, SpaceShape (2)
!
!	Determine Space size, (re)allocate Freq if neccessary
!
	SpaceShape = SHAPE (Space)
	If (ASSOCIATED (Freq)) Then
	  If (SpaceShape (1) /= SIZE (Freq, 2) .OR. &
	&     SpaceShape (2) /= SIZE (Freq, 1)) Then
	    Deallocate (Freq)
	    Allocate (Freq (SpaceShape (2), SpaceShape (1)))
	  End If
	Else
	  Allocate (Freq (SpaceShape (2), SpaceShape (1)))
	End If
!
!	Allocate and initialize variables, if required
!
	If (RowLen /= SpaceShape (1)) Then
	  If (ALLOCATED (WSaveRow)) Deallocate (WSaveRow)
	  RowLen = SpaceShape (1)
	  Allocate (WSaveRow (4*RowLen+15))
	  Call CFFTI (RowLen, WSaveRow)
	End If

	If (ColLen /= SpaceShape (2)) Then
	  If (ALLOCATED (WSaveCol)) Deallocate (WSaveCol)
	  ColLen = SpaceShape (2)
	  Allocate (WSaveCol (4*ColLen+15))
	  Call CFFTI (ColLen, WSaveCol)
	End If

        If (ALLOCATED (STemp)) Then
          If (ANY (SpaceShape /= SHAPE (STemp))) Then
            Deallocate (STemp)
            Allocate (STemp (RowLen, ColLen))
          End If
        Else
	  Allocate (STemp (RowLen, ColLen))
	End If
!
!	Compute forward FFT
!
	STemp = Space
!
!	  Loop over rows
!
	Do I = 1, ColLen
	  Call CFFTF (RowLen, STemp (:, I), WSaveRow)
	End Do
!
!	  Copy transposed into result array
!
	Freq = TRANSPOSE (STemp)
!
!	  Loop over columns
!
	Do I = 1, RowLen
	  Call CFFTF (ColLen, Freq (:, I), WSaveCol)
	End Do

	Return

	End Subroutine FFT2DF
!
	Subroutine FFT2DB (Freq, Space)
!
!	Compute an inverse two-dimensional FFT
!
	Implicit NONE
!
!	Included files
!
	Include 'cfft.int'
!
!	Declarations
!
!	  Arguments
!
	Complex(4), Intent(In)  :: Freq  (:, :)
	Complex(4), Pointer     :: Space (:, :)
!
!	  Coefficient arrays for FFT routines
!
	Real(4),    Allocatable, Save :: WSaveRow (:), WSaveCol (:)
!
!	  Temporary array
!
	Complex(4), Allocatable, Save :: FTemp (:, :)
!
!	  Local variables: array sizes
!
	Integer, Save :: RowLen = 0, ColLen = 0
!
!	  Local variables: counters and such
!
	Integer :: I, FreqShape (2)
!
!	Determine Freq size, (re)allocate Space if necessary
!
	FreqShape = SHAPE (Freq)
	If (ASSOCIATED (Space)) Then
	  If (FreqShape (1) /= SIZE (Space, 2) .OR. &
	&     FreqShape (2) /= SIZE (Space, 1)) Then
	    Deallocate (Space)
	    Allocate (Space (FreqShape (2), FreqShape (1)))
	  End If
	Else
	  Allocate (Space (FreqShape (2), FreqShape (1)))
	End If
!
!	Allocate and initialize variables, if required
!
	If (RowLen /= FreqShape (1)) Then
	  If (ALLOCATED (WSaveRow)) Deallocate (WSaveRow)
	  RowLen = FreqShape (1)
	  Allocate (WSaveRow (4*RowLen+15))
	  Call CFFTI (RowLen, WSaveRow)
	End If

	If (ColLen /= FreqShape (2)) Then
	  If (ALLOCATED (WSaveCol)) Deallocate (WSaveCol)
	  ColLen = FreqShape (2)
	  Allocate (WSaveCol (4*ColLen+15))
	  Call CFFTI (ColLen, WSaveCol)
	End If

	If ( ALLOCATED (FTemp) ) Then
	  If (ANY (FreqShape /= SHAPE (FTemp))) Then
	    Deallocate (FTemp)
	    Allocate (FTemp (RowLen, ColLen))
	  End If
	Else
	  Allocate (FTemp (RowLen, ColLen))
	End If
!
!	Compute inverse FFT
!
	FTemp = Freq
!
!	  Loop over rows
!
	Do I = 1, ColLen
	  Call CFFTB (RowLen, FTemp (:, I), WSaveRow)
	End Do
!
!	  Copy transposed into result array
!
	Space = TRANSPOSE (FTemp)
!
!	  Loop over columns
!
	Do I = 1, RowLen
	  Call CFFTB (ColLen, Space (:, I), WSaveCol)
	End Do

	Return

	End Subroutine FFT2DB

	End Module FFT2D 

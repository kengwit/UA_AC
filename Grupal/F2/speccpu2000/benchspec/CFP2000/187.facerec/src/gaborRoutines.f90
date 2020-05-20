!
!	Copyright 1998  by Jan C. Vorbrueggen.
!
	Module GaborRoutines
		Public :: GaborTrafo
	Contains

	Subroutine GaborTrafo (TrafoPars, Image, TrafoResult)
!
!	This implements the Gabor transform.
!
	Use FaceRecTypes, Only: TrafoP
	Use FFT2D

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Type(TrafoP), Intent(In) :: TrafoPars
	Integer,      Intent(In) :: Image (:, :)
	Real(4),      Pointer    :: TrafoResult (:, :, :, :)
!
!	  Local arrays in space domain
!
	Complex(4), Allocatable, Save :: CImage (:, :)
	Complex(4), Pointer,     Save :: CTemp (:, :)
!
!	  Local arrays in Fourier domain
!
	Complex(4), Pointer,     Save :: FCImage (:, :)
	Complex(4), Allocatable, Save :: FCTemp (:, :)
	Real(4),    Pointer,     Save :: Kernel (:, :, :)
!
!	  Local variables: array sizes and parameters
!
	Type(TrafoP), Save :: CurTrafoPars = &
	&                       TrafoP (0.0, 0.0, 0.0, 0, 0, .TRUE.)

	Integer,      Save :: IShape (2) = 0
	Integer            :: RowLen, ColLen
	Equivalence (Ishape(1), RowLen)
	Equivalence (IShape(2), ColLen)
!
!	  Local variables: loop counters and such
!
	Integer :: Level, Orient
	Real(4) :: DC, K, KRow, KCol
	Integer :: ShiftRow, ShiftCol
	Integer :: Row, Col
	Real(4) :: InvNorm
	Real(4), Parameter :: PI = 3.1415926535897931
!
!	(Re)Allocate variables, if required
!
	If (ANY (SHAPE (Image) /= IShape)) Then
		If (RowLen == 0) Then	! indicator of first time through
			Nullify (CTemp, FCImage, Kernel)
		Else 
			Deallocate (CImage, CTemp, FCImage, FCTemp)
		End If

		IShape = SHAPE (Image)
		Allocate (CImage (RowLen, ColLen))
		Allocate (FCTemp (ColLen, RowLen))
	End If

	If (ASSOCIATED (TrafoResult)) Then
		If (ANY (SHAPE (TrafoResult) /= &
	&           (/TrafoPars%NOrient, TrafoPars%NLevel, RowLen, ColLen/))) &
	&	    Deallocate (TrafoResult)
	End If

	If (.NOT. ASSOCIATED (TrafoResult)) &
	    Allocate (TrafoResult (TrafoPars%NOrient, TrafoPars%NLevel, &
	                           RowLen, ColLen))

	If (.NOT. TrafoEqual (CurTrafoPars, TrafoPars)) Then
		CurTrafoPars = TrafoPars
		Call ComputeKernel (CurTrafoPars, ColLen, RowLen, Kernel)
	End If
!
!	Copy image to complex array
!
	CImage = CMPLX (Image)
!
!	Compute forward FFT of image
!
	Call FFT2DF (CImage, FCImage)
!
!	Loop over all kernels
!
	DC = EXP (-CurTrafoPars%Sigma**2/2.)
	Do Level = 1, CurTrafoPars%NLevel

		K = CurTrafoPars%KMax * CurTrafoPars%KStep**(1-Level)
		Do Orient = 1, CurTrafoPars%NOrient
!
!	    Convolve with relevant kernel
!
		KRow = K * SIN ((Orient-CurTrafoPars%NOrient/2)*PI/CurTrafoPars%NOrient)
		ShiftRow = INT (KRow/PI*(RowLen/2) + SIGN (0.5_4, KRow))

		KCol = K * COS ((Orient-CurTrafoPars%NOrient/2)*PI/CurTrafoPars%NOrient)
		ShiftCol = INT (KCol/PI*(ColLen/2) + SIGN (0.5_4, KCol))

	    FCTemp = FCImage &
	&	         * (CSHIFT(CSHIFT(Kernel (:, :, Level),ShiftRow,1),ShiftCol,2) &
	&	            - DC * Kernel (:, :, Level))
!
!	    Compute inverse FFT of image
!
	    Call FFT2DB (FCTemp, CTemp)
!
!	    Store results in trafo (note transpose)
!
	    TrafoResult (Orient, Level, :, :) = ABS (CTemp)
	  End Do
	End Do
!
!	If requested, normalize the jets
!
	If (CurTrafoPars%NormalizeJets) Then
		Do Col = 1, ColLen
			Do Row = 1, RowLen
				InvNorm = 1.0 / SQRT (SUM (TrafoResult (:, :, Row, Col) * &
	&			                           TrafoResult (:, :, Row, Col)))
				TrafoResult (:, :, Row, Col) = &
				     TrafoResult (:, :, Row, Col) * InvNorm
			End Do
		End Do
	End If

	Contains

		Logical Function TrafoEqual (A, B)
!
!		Function to check two variables of type TrafoP for equality
!
		Type(TrafoP), Intent(In) :: A, B

		If (A%KMax    == B%KMax   .AND. &
	&	    A%KStep   == B%KStep  .AND. &
	&	    A%Sigma   == B%Sigma  .AND. &
	&	    A%NLevel  == B%NLevel .AND. &
	&	    A%NOrient == B%NOrient) Then
			TrafoEqual = .TRUE.
		Else
			TrafoEqual = .FALSE.
		Endif

		End Function TrafoEqual

	End Subroutine GaborTrafo

	Subroutine ComputeKernel (TrafoPars, ColLen, RowLen, Kernel)
!
!	This computes the kernels required by the Gabor transform.
!
	Use FaceRecTypes, Only: TrafoP

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Type(TrafoP), Intent(In)  :: TrafoPars
	Integer,      Intent(In)  :: ColLen, RowLen
	Real(4),      Pointer     :: Kernel (:, :, :)
!
!	  Local variables: wave vector values, PI, loop counters
!
	Real(4) :: K, KRow, KCol
	Real(4), Parameter :: PI = 3.1415926535897931

	Integer :: Level, Row, Col
!
!	(Re)Allocate variables, if required
!
	If (ASSOCIATED (Kernel)) Then
		If  (ANY (SHAPE (Kernel) /= (/ColLen, RowLen, TrafoPars%NLevel/))) &
	&	     Deallocate (Kernel)
	End If

	If (.NOT. ASSOCIATED (Kernel)) &
	&	Allocate (Kernel (ColLen, RowLen, TrafoPars%NLevel))

	Do Level = 1, TrafoPars%NLevel
		K = TrafoPars%KMax * TrafoPars%KStep**(1-Level)
		Do Row = 0, RowLen-1
			KRow = (Row-RowLen/2)*PI/(RowLen/2)
			Do Col = 0, ColLen-1
				KCol = (Col-ColLen/2)*PI/(ColLen/2)
				Kernel (MOD (Col+ColLen/2, ColLen)+1, &
	&			        MOD (Row+RowLen/2, RowLen)+1, &
	&					Level) = &
	&			          EXP (-TrafoPars%Sigma**2*(KRow**2+KCol**2)/(2.*K**2))
			End Do
		End Do
	End Do

	End Subroutine ComputeKernel

	End Module GaborRoutines

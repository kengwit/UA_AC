!
!	Copyright 1998  by Jan C. Vorbrueggen.
!
	Module ImageRoutines
		Public :: ReadImage
	Contains

	Subroutine ReadImage (PathName, FileName, ImagePars, Image)
!
!	Read an image from the specified file; size is implicit in input array
!
	Use FaceRecTypes
	Use FFT2D

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Character(*), Intent(In) :: PathName, FileName
	Type(ImageP), Intent(In) :: ImagePars
	Integer,      Pointer    :: Image (:, :)
!
!	  Local variables
!
	Real(4) :: InvScale
	Integer :: ReadRow, ReadCol, ActualRow, ActualCol

	Integer,    Allocatable, Save :: Input  (:, :)
	Complex(4), Allocatable, Save :: FImage (:, :)
	Complex(4), Pointer           :: FInput (:, :), CImage (:, :)

	Integer, Parameter :: imageLUN = 21
!
!	Initialize pointers
!
	Nullify (FInput, CImage)
!
!	(Re)allocate Image
!
	ReadRow = ImagePars%RowLen
	ReadCol = ImagePars%ColLen

	ActualRow = ReadRow * ImagePars%UpSample
	ActualCol = ReadCol * ImagePars%UpSample

	If (ASSOCIATED (Image)) Then
		IF (ANY (SHAPE (Image) /= (/ActualRow, ActualCol/))) Deallocate (Image)
	End If
	If (.NOT. ASSOCIATED (Image)) Allocate (Image (ActualRow, ActualCol))
!
!	Open image file
!	
	Open (Unit = imageLUN, File = PathName//FileName, Action = 'READ', &
	&     Recl = 130, Form = 'FORMATTED', Access = 'SEQUENTIAL', Status = 'OLD')
!
!	If UpSample == 1, read image directly.
!
	If (ImagePars%UpSample == 1) Then
		Read (Unit = imageLUN, FMT = '(64(Z2.2))') Image
!
!	If UpSample > 1, read image into a temporary array and do proper
!	upsampling by using the FFT and padding with zeros in frequency space.
!
	Else
		If (ALLOCATED (Input)) Then
			IF (ANY (SHAPE (Input) /= (/ReadRow, ReadCol/))) &
	&			Deallocate (Input)
		End If
		If (.NOT. ALLOCATED (Input)) Allocate (Input (ReadRow, ReadCol))

		If (ALLOCATED (FImage)) Then
			IF (ANY (SHAPE (FImage) /= (/ActualCol, ActualRow/))) &
	&			Deallocate (FImage)
		End If
		If (.NOT. ALLOCATED (FImage)) Then
			Allocate (FImage (ActualCol, ActualRow))
			FImage = 0.0
		End If

		Read (Unit = imageLUN, FMT = '(64(Z2.2))') Input

		Call FFT2DF (CMPLX (Input, KIND=4), FInput)

		FImage (1:ReadCol/2, 1:ReadRow/2) = &
	&	  FInput (1:ReadCol/2, 1:ReadRow/2)
		FImage (1:ReadCol/2, ActualRow-ReadRow/2+1:ActualRow) = &
	&	  FInput (1:ReadCol/2, ReadRow/2+1:ReadRow)
		FImage (ActualCol-ReadCol/2+1:ActualCol, 1:ReadRow/2) = &
	&	  FInput (ReadCol/2+1:ReadCol, 1:ReadRow/2)
		FImage (ActualCol-ReadCol/2+1:ActualCol, &
	&	        ActualRow-ReadRow/2+1:ActualRow) = &
	&	  FInput (ReadCol/2+1:ReadCol, ReadRow/2+1:ReadRow)

		Call FFT2DB (FImage, CImage)

		InvScale = 1.0 / (ReadRow * ReadCol)
		Image = INT (InvScale * ABS (CImage))

		Deallocate (FInput, CImage)
	End If

	Close (Unit = imageLUN)

	End Subroutine ReadImage

	End Module ImageRoutines

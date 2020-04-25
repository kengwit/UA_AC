!
!	Copyright 1998  by Jan C. Vorbrueggen.
!
	Module ParameterRoutines
		Public :: ReadParameters, PrintParameters
	Contains

	Subroutine ReadParameters (Params)
!
!	Read in all parameters from files for FaceRec
!
	Use FaceRecTypes

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Type(FaceRecP), Intent(Out) :: Params
!
!	  Local parameters: LUN and names of parameter files
!
	Integer,      Parameter :: paramsLUN = 20
	Character(*), Parameter :: imageParFile = 'imagePars.dat'
	Character(*), Parameter :: trafoParFile = 'trafoPars.dat'
	Character(*), Parameter :: matchParFile = 'matchPars.dat'
	Character(*), Parameter :: graphParFile = 'graphPars.dat'
	Character(*), Parameter :: albumParFile = 'albumPars.dat'
	Character(*), Parameter :: probeParFile = 'probePars.dat'
!
!	  Local variables: Loop counter and such
!
	Integer      :: Mult, NodeCount
	Integer      :: I, RunLen
	Character(6) :: RunType
!
!	Determine run type and data directory by asking user
!
	Print *, 'Enter run type (test, train, ref):'
	Read *, RunType
	Select Case (RunType)
		Case ('test', 'TEST', 'Test')
			RunType = 'test-'
			RunLen = 5
		Case ('train', 'TRAIN', 'Train')
			RunType = 'train-'
			RunLen = 6
		Case ('ref', 'REF', 'Ref')
			RunType = 'ref-'
			RunLen = 4
		Case DEFAULT
			Print *, 'ReadParameters: Run type not recognized'
			STOP
	End Select
!
!	Open and read image parameters
!
	Open (Unit = paramsLUN, File = imageParFile, Status = 'OLD', &
	&     Action = 'READ', Access = 'SEQUENTIAL', Form = 'FORMATTED')

	Read (paramsLUN, *) Params%Image%RowLen
	Read (paramsLUN, *) Params%Image%ColLen
	Read (paramsLUN, *) Params%Image%UpSample

	Mult = Params%Image%UpSample

	Close (Unit = paramsLUN)
!
!	Open and read transformation parameters
!
	Open (Unit = paramsLUN, File = trafoParFile, Status = 'OLD', &
	&     Action = 'READ', Access = 'SEQUENTIAL', Form = 'FORMATTED')

	Read (paramsLUN, *) Params%Trafo%KMax
	Params%Trafo%KMax = Params%Trafo%KMax / Mult
	Read (paramsLUN, *) Params%Trafo%KStep
	Read (paramsLUN, *) Params%Trafo%Sigma
	Read (paramsLUN, *) Params%Trafo%NLevel
	Read (paramsLUN, *) Params%Trafo%NOrient
	Read (paramsLUN, *) Params%Trafo%NormalizeJets

	Close (Unit = paramsLUN)
!
!	Open and read match parameters
!
	Open (Unit = paramsLUN, File = matchParFile, Status = 'OLD', &
	&     Action = 'READ', Access = 'SEQUENTIAL', Form = 'FORMATTED')

	Read (paramsLUN, *) Params%Match%GlobalStep
	Params%Match%GlobalStep = Params%Match%GlobalStep * Mult
	Read (paramsLUN, *) Params%Match%DoLocalMove
	Read (paramsLUN, *) Params%Match%MaxSweeps
	Read (paramsLUN, *) Params%Match%MaxDev
	Params%Match%MaxDev = Params%Match%MaxDev * Mult
	Read (paramsLUN, *) Params%Match%Lambda
	Params%Match%Lambda = Params%Match%Lambda / (Mult**2)
	Read (paramsLUN, *) Params%Match%SimThresh

	Close (Unit = paramsLUN)
!
!	Open and read graph parameters
!
	Open (Unit = paramsLUN, File = graphParFile, Status = 'OLD', &
	&     Action = 'READ', Access = 'SEQUENTIAL', Form = 'FORMATTED')

	Read (paramsLUN, *) Params%Graph%NodesX
	Read (paramsLUN, *) Params%Graph%NodesY
	Read (paramsLUN, *) Params%Graph%StepX
	Read (paramsLUN, *) Params%Graph%StepY
	Params%Graph%StepX = Params%Graph%StepX * Mult
	Params%Graph%StepY = Params%Graph%StepY * Mult
	Read (paramsLUN, *) Params%Graph%ProtoFileName
	Params%Graph%ProtoNameLength = &
	&  LEN_TRIM (Params%Graph%ProtoFileName)
	Read (paramsLUN, *) Params%Graph%ProtoLLX
	Read (paramsLUN, *) Params%Graph%ProtoLLY
	Params%Graph%ProtoLLX = Params%Graph%ProtoLLX * Mult
	Params%Graph%ProtoLLY = Params%Graph%ProtoLLY * Mult

	NodeCount = Params%Graph%NodesX * Params%Graph%NodesY
	Allocate (Params%Graph%RandomIndex (NodeCount))
	Read (paramsLUN, *) Params%Graph%RandomIndex

	Close (Unit = paramsLUN)
!
!	Open and read album gallery
!
	Open (Unit = paramsLUN, Action = 'READ', Access = 'SEQUENTIAL', &
	&     Form = 'FORMATTED', Status = 'OLD', &
	&     File = RunType(:RunLen)//albumParFile)

	Read (paramsLUN, *) Params%Album%Path
	Params%Album%PathLength = LEN_TRIM (Params%Album%Path)
	Read (paramsLUN, *) Params%Album%NoOfEntries
	Allocate (Params%Album%FileName   (Params%Album%NoOfEntries))
	Allocate (Params%Album%NameLength (Params%Album%NoOfEntries))
	Do I = 1, Params%Album%NoOfEntries
		Read (paramsLUN, *) Params%Album%FileName (I)
		Params%Album%NameLength (I) = &
		  LEN_TRIM (Params%Album%FileName (I))
	End Do

	Close (Unit = paramsLUN)
!
!	Open and read probe gallery
!
	Open (Unit = paramsLUN, Action = 'READ', Access = 'SEQUENTIAL', &
	&     Form = 'FORMATTED', Status = 'OLD', &
	&     File = RunType(:RunLen)//probeParFile)

	Read (paramsLUN, *) Params%Probe%Path
	Params%Probe%PathLength = LEN_TRIM (Params%Probe%Path)
	Read (paramsLUN, *) Params%Probe%NoOfEntries
	Allocate (Params%Probe%FileName   (Params%Probe%NoOfEntries))
	Allocate (Params%Probe%NameLength (Params%Probe%NoOfEntries))
	Do I = 1, Params%Probe%NoOfEntries
		Read (paramsLUN, *) Params%Probe%FileName (I)
		Params%Probe%NameLength (I) = &
		  LEN_TRIM (Params%Probe%FileName (I))
	End Do

	Close (Unit = paramsLUN)

	End Subroutine ReadParameters

	Subroutine PrintParameters (Params)
!
!	Print all parameters for FaceRec
!
	Use FaceRecTypes

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Type(FaceRecP), Intent(In) :: Params
!
!	Print out all parameters
!
	Print *
	Print *, 'This run uses the following parameter settings:'
	Print *, '==============================================='

	Print '(A, 2I4, I2)', ' Image parameters:', Params%Image
	Print '(A, 3F10.7, 2I2, 1X, L1)', ' Trafo parameters:', Params%Trafo
	Print '(A, I3, 1X, L1, I4, I3, 2ES8.1)', ' Match parameters:', Params%Match
	Print '(A, 6I3, 3A)', ' Graph parameters:', &
	&        Params%Graph%NodesX, Params%Graph%NodesY, &
	&        Params%Graph%StepX, Params%Graph%StepY, &
	&        Params%Graph%ProtoLLX, Params%Graph%ProtoLLY, ' "', &
	&        Params%Graph%ProtoFileName (:Params%Graph%ProtoNameLength), '"'

	Print '(A, I4, 3A)', ' Album gallery contains', Params%Album%NoOfEntries, &
	&        ' images from "', Params%Album%Path(:Params%Album%PathLength), '".'

	Print '(A, I4, 3A)', ' Probe gallery contains', Params%Probe%NoOfEntries, &
	&        ' images from "', Params%Probe%Path(:Params%Probe%PathLength), '".'

	End Subroutine PrintParameters

	End Module ParameterRoutines 

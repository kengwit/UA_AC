!
!	Copyright 1999  by Jan C. Vorbrueggen.
!
	Module FaceRecTypes
!
!	Derived type containing image parameters
!
	Type ImageP
		Integer :: RowLen, ColLen, UpSample
	End Type ImageP
!
!	Derived type containing parameters for Gabor transformation
!
	Type TrafoP
		Real(4) :: KMax, KStep, Sigma
		Integer :: NLevel, NOrient
		Logical :: NormalizeJets
	End Type TrafoP
!
!	Derived type containing parameters for graph matching
!
	Type MatchP
		Integer :: GlobalStep
		Logical :: DoLocalMove
		Integer :: MaxSweeps
		Integer :: MaxDev
		Real(4) :: Lambda
		Real(4) :: SimThresh
	End Type MatchP
!
!	Derived type containing parameters for graph layout and prototype
!
	Type GraphP
		Integer, Pointer :: RandomIndex (:)
		Integer          :: NodesX,   NodesY
		Integer          :: StepX,    StepY
		Integer          :: ProtoLLX, ProtoLLY
		Integer          :: ProtoNameLength
		Character(80)    :: ProtoFileName
	End Type GraphP
!
!	Derived type containing gallery parameters
!
	Type GalleryP
		Integer       :: NoOfEntries
		Character(80) :: Path
		Integer       :: PathLength
		Character(80), Pointer :: FileName (:)
		Integer,       Pointer :: NameLength (:)
	End Type GalleryP
!
!	Derived type containing all parameters for FaceRec
!
	Type FaceRecP
		Type(ImageP)   :: Image
		Type(TrafoP)   :: Trafo
		Type(MatchP)   :: Match
		Type(GraphP)   :: Graph
		Type(GalleryP) :: Album
		Type(GalleryP) :: Probe
	End Type FaceRecP

	End Module FaceRecTypes
        

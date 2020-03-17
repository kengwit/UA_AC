!
!	Copyright 1998  by Jan C. Vorbrueggen.
!
	Program FaceRec
!
!	This is FaceRec, a version of the face recognition application documented
!	in M. Lades et al., IEEE Trans. Comp., vol. 42(3), pp. 300-311 (1993),
!	for use as a SPEC CPU FP2000 benchmark.
!
	Use FaceRecTypes
	Use GaborRoutines
	Use GraphRoutines
	Use ImageRoutines
	Use ParameterRoutines

	Implicit NONE
!
!	Declarations
!
!	  Local variables: arrays
!
	Integer, Pointer :: Image (:, :)
	Real(4), Pointer :: GaborImage (:, :, :, :)

	Real(4), Pointer :: Prototype  (:, :, :)
	Real(4), Pointer :: Graphs     (:, :, :, :)

	Integer :: Best (1)
	Integer :: Position (2)
!
!	  Local variables: parameter set
!
	Type(faceRecP) :: Params
!
!	  Local variables: loop counters and such
!
	Real(4)              :: ProtoSim
	Real(4), Allocatable :: ProbeSim (:)
	Integer, Allocatable :: Hops (:), Sweeps (:)

	Integer :: NodeCount, Scale
	Integer :: TotalHops = 0, TotalSweeps = 0
	Integer :: Correct = 0, Incorrect = 0 
	Integer :: I, J
	Integer,   Parameter :: hopLUN = 26
!
!	Initialize pointers
!
	Nullify (Image, GaborImage, Prototype, Graphs)
!
!	Open output file for detailed info about hops/sweeps (we will
!	use a less stringent tolerance validating against this file)
!
	Open (Unit = hopLUN, File = 'hops.out', Action = 'WRITE', &
	&     Status = 'REPLACE')
!
!	Read in all parameters; allocate arrays
!
	Call  ReadParameters (Params)
	Call PrintParameters (Params)

	Allocate (ProbeSim (Params%Album%NoOfEntries))
	Allocate (Hops     (Params%Album%NoOfEntries))
	Allocate (Sweeps   (Params%Album%NoOfEntries))
!
!	Generate graphs for album entries and prototype graph for global move
!
	Print *
	Print *, 'Generating graphs for album entries'
	Call GenerateGraphs (Params, Prototype, Graphs)
	NodeCount = SIZE (Prototype, DIM = 3)
	Scale = Params%Image%UpSample
!
!	Loop over all probe entries and perform comparison
!
	Print *
	Print *, 'Performing comparison with probe entries'
	Do I = 1, Params%Probe%NoOfEntries
!
!		Do global move, using prototype graph
!
		Call ReadImage (Params%Probe%Path        (:Params%Probe%PathLength), &
	&	                Params%Probe%FileName(I) (:Params%Probe%NameLength(I)),&
	&	                Params%Image, Image)
		Call GaborTrafo (Params%Trafo, Image, GaborImage)
		Call GlobalMove (Params%Match%GlobalStep, Params%Image%UpSample, & 
	&                    Params%Graph, Prototype, GaborImage, &
	&                    Position, ProtoSim)

		Print '(I3, A, A8, A, 2I4, A, F8.5)', I, ' ', &
	&	      Params%Probe%FileName(I) (:Params%Probe%NameLength(I)-4), &
	&	      ':    global move determines position', Position / Scale, &
	&	      ',  similarity =', ProtoSim / NodeCount
!
!		Loop over all album entries
!
		Do J = 1, Params%Album%NoOfEntries
			ProbeSim (J) = GraphSimFct (Position (1), Position (2), &
	&		                            Params%Graph, &
	&		                            Graphs (:, :, :, J), GaborImage)
			If (Params%Match%DoLocalMove) Then
				Call LocalMove (Params, Position, Graphs (:, :, :, J), &
				                GaborImage, ProbeSim (J), Hops (J), Sweeps (J))
				TotalHops   = TotalHops   + Hops (J)
				TotalSweeps = TotalSweeps + Sweeps (J)
			End If
		End Do
!
!		Determine best match, print result
!
		Best = MAXLOC (ProbeSim)

		If (Params%Match%DoLocalMove) Then
			Print '(I3, A, 2(A8, A), F8.5)', &
	&		        I, ' ', &
	&		        Params%Probe%FileName(I) (:Params%Probe%NameLength(I)-4), &
	&		        ': best match ', &
	&	  Params%Album%FileName(Best(1))(:Params%Album%NameLength(Best(1))-4),&
	&		       ' sim. =', ProbeSim (Best(1)) / NodeCount

			Write (hopLUN, '(A8, 1X, I4, A, I5, A, I2, A, I3, A)') &
	&		       Params%Probe%FileName(I) (:Params%Probe%NameLength(I)-4), &
	&		         Hops (Best(1)),  ' / ', SUM (Hops),   ' hops,', &
	&		       Sweeps (Best(1)),  ' / ', SUM (Sweeps), ' sweeps'
		Else
			Print '(I3, A, 2(A8, A), 2I4, A, F8.5)', I, ' ', &
	&		      Params%Probe%FileName(I) (:Params%Probe%NameLength(I)-4), &
	&		      ': best match ', &
	&	   Params%Album%FileName(Best(1))(:Params%Album%NameLength(Best(1))-4),&
	&		      ' at position', Position / Scale, &
	&		      ',    similarity =', ProbeSim (Best(1)) / NodeCount
		End If
!
!		Check for correct match, update counters
!
		If (Params%Probe%FileName(I) (:Params%Probe%NameLength(I)-5) == &
	&	   Params%Album%FileName(Best(1))(:Params%Album%NameLength(Best(1))-5))&
	&	Then
			Correct = Correct + 1
		Else
			Incorrect = Incorrect + 1
		End If
	End Do
!
!	Print correct/incorrect counters
!
	Print *
	Print '(A, I3, A, I3, A, I3, A)', 'Performed ', Params%Probe%NoOfEntries, &
	&     ' comparisons, of which ', Correct, ' were correct and ', &
	&     Incorrect, ' were incorrect.'
	Print '(A, I8, A, I6, A)', 'A total of', TotalHops, ' hops and', &
	&     TotalSweeps, ' sweeps were performed.'

	Close (Unit = hopLUN)

	End Program FaceRec

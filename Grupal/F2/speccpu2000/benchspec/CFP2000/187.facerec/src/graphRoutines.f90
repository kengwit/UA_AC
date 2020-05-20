!
!	Copyright 1998  by Jan C. Vorbrueggen.
!
	Module GraphRoutines
		Public :: GenerateGraphs, GlobalMove, LocalMove
	Contains

	Subroutine GenerateGraphs (Params, ProtoGraph, GraphResult)
!
!	Extract graphs for all entries in the album gallery.
!
	Use FaceRecTypes
	Use GaborRoutines
	Use ImageRoutines

	Implicit NONE
!
!       Declarations
!
!         Arguments
!
	Type(faceRecP)   :: Params
	Real(4), Pointer :: ProtoGraph  (:, :, :)
	Real(4), Pointer :: GraphResult (:, :, :, :)
!
!	  Local variables: arrays
!
	Integer, Pointer :: Image (:, :)
	Real(4), Pointer :: GaborImage (:, :, :, :)
!
!	  Local variables: loop counter and such
!
	Integer :: I
	Integer :: NodeCount
	Integer :: Position (2)
	Real(4) :: Similarity
!
!	Initialize pointers
!
	Nullify (Image, GaborImage)
!
!	Allocate space for graphs
!
	NodeCount = Params%Graph%NodesX *Params%Graph%NodesY

	If (ASSOCIATED (ProtoGraph)) Deallocate (ProtoGraph)
	Allocate (ProtoGraph (Params%Trafo%NOrient, Params%Trafo%NLevel, NodeCount))

	If (ASSOCIATED (GraphResult)) Deallocate (GraphResult)
	Allocate (GraphResult (Params%Trafo%NOrient, Params%Trafo%NLevel, &
	&         NodeCount, Params%Album%NoOfEntries))
!
!	Read prototye image from album, transform it and extract protoype graph
!
	Call ReadImage (Params%Album%Path          (:Params%Album%PathLength), &
	&               Params%Graph%ProtoFileName (:Params%Graph%ProtoNameLength),&
	&               Params%Image, Image)
 	Call GaborTrafo (Params%Trafo, Image, GaborImage)
	Position = (/ Params%Graph%ProtoLLX, Params%Graph%ProtoLLY /)
	Call ExtractGraph (Position, Params%Graph, GaborImage, ProtoGraph)
!
!	Loop over all entries in album gallery, extracting graphs 
!	via global move comparison to prototype graph
!
	Do I = 1, Params%Album%NoOfEntries
		Call ReadImage (Params%Album%Path        (:Params%Album%PathLength), &
	&	                Params%Album%FileName(I) (:Params%Album%NameLength(I)),&
	&	                Params%Image, Image)
		Call GaborTrafo (Params%Trafo, Image, GaborImage)
		Call GlobalMove (Params%Match%GlobalStep, Params%Image%UpSample, & 
	&                    Params%Graph, ProtoGraph, GaborImage, &
	&                    Position, Similarity)
		Call ExtractGraph (Position, Params%Graph, GaborImage, &
	&	                   GraphResult (:, :, :, I))
		Print '(A8, A, 2I3, A, F8.5)', &
	&	      Params%Album%FileName(I) (:Params%Album%NameLength(I)-4), &
	&	      ': generated graph at ', Position/Params%Image%UpSample, &
	&	      ', similarity =', Similarity / NodeCount
	End Do
!
!	Deallocate arrays allocated here and no longer required
!
	Deallocate (Image, GaborImage)

	Return

	End Subroutine GenerateGraphs

	Subroutine GlobalMove (Step, Scale, GraphPars, Graph, GaborTrafo, &
	&                      Position, Similarity)
!
!	Perform global move step, returning best-fit position and its similarity.
!
	Use FaceRecTypes, Only: GraphP

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Integer,      Intent(In)  :: Step, Scale
	Type(GraphP), Intent(In)  :: GraphPars
	Real(4),      Intent(In)  :: Graph (:, :, :)
	Real(4),      Intent(In)  :: GaborTrafo (:, :, :, :)
	Integer,      Intent(Out) :: Position (2)
	Real(4),      Intent(Out) :: Similarity
!
!	  Local variables
!
	Integer :: RowLen, ColLen, SecondStep
	Integer :: StartX, StartY, EndX, EndY, CountX, CountY
	Integer :: IX, IY, LLX, LLY
	Real(4) :: CurSimilarity
!
!	Compute area in which to do graph comparison
!
	RowLen = SIZE (GaborTrafo, DIM = 3)
	ColLen = SIZE (GaborTrafo, DIM = 4)

	EndX = RowLen - (GraphPars%StepX * (GraphPars%NodesX - 1) + Step / 2)
	EndY = ColLen - (GraphPars%StepY * (GraphPars%NodesY - 1) + Step / 2)
!
!	Determine start position and loop counts
!
	CountX = (EndX / Step) + 1
	StartX = Step / 2
	CountY = (EndY / Step) + 1
	StartY = Step / 2
!
!	Do first scan with grid spacing given by Step
!
	Position = (/0, 0/)	! Illegal
	Similarity = 0.0
	LLY = StartY
	Do IY = 1, CountY
		LLX = StartX
		Do IX = 1, CountX
			CurSimilarity = GraphSimFct (LLX, LLY, GraphPars, Graph, GaborTrafo)
			If (CurSimilarity > Similarity) Then
				Similarity = CurSimilarity
				Position = (/ LLX, LLY /)
			End If
			LLX = LLX + Step
		End Do
		LLY = LLY + Step
	End Do
!
!	Do second scan around best position just found
!   Note: for scaled images, this is done on a subgrid with steps of Scale
!
	SecondStep = Step / Scale

	StartX = Position (1) - ((Step+1) / 2)
	CountX = SecondStep + 1
	If ( StartX            <= 0)    StartX = 1
	If ((StartX+Step) > EndX) StartX = EndX - Step

	StartY = Position (2) - ((Step+1) / 2)
	CountY = SecondStep + 1
	If ( StartY      <= 0)    StartY = 1
	If ((StartY+Step) > EndY) StartY = EndY - Step

	LLY = StartY
	Do IY = 1, CountY
		LLX = StartX
		Do IX = 1, CountX
			CurSimilarity = GraphSimFct (LLX, LLY, GraphPars, Graph, GaborTrafo)
			If (CurSimilarity > Similarity) Then
				Similarity = CurSimilarity
				Position = (/LLX, LLY/)
			End If
			LLX = LLX + Scale
		End Do
		LLY = LLY + Scale
	End Do

	Return

	End Subroutine GlobalMove

	Real(4) Function GraphSimFct (LLX, LLY, GraphPars, Graph, GaborTrafo)
!
!	Function to compute graph similarity for rigid graph
!
	Use FaceRecTypes, Only: GraphP

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Integer,      Intent (In) :: LLX, LLY
	Type(GraphP), Intent (In) :: GraphPars
	Real(4),      Intent (In) :: Graph (:, :, :)
	Real(4),      Intent (In) :: GaborTrafo (:, :, :, :)
!
!	  Local variables
!
	Integer :: IX, IY, VertexX, VertexY, Index
!
!	Scan the grid positioned at (LLX, LLY) and compute jet similarity
!
	GraphSimFct = 0.0
	Index = 1
	VertexY = LLY
	Do IY = 1, GraphPars%NodesY
		VertexX = LLX
		Do IX = 1, GraphPars%NodesX
			GraphSimFct = GraphSimFct + &
	&		              SUM (Graph (:, :, Index) * &
	&		                   GaborTrafo (:, :, VertexX, VertexY))
			Index = Index + 1
			VertexX = VertexX + GraphPars%StepX
		End Do
		VertexY = VertexY + GraphPars%StepY
	End Do

	End Function GraphSimFct

	Subroutine ExtractGraph (Position, GraphPars, GaborTrafo, GraphResult)
!
!	Extract rectangular graph from Gabor transform.
!
	Use FaceRecTypes, Only: GraphP

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Integer,      Intent(In)  :: Position (2)
	Type(GraphP), Intent(In)  :: GraphPars
	Real(4),      Intent(In)  :: GaborTrafo (:, :, :, :)
	Real(4),      Intent(Out) :: GraphResult (:, :, :)
!
!	  Local variables
!
	Integer :: RowLen, ColLen
	Integer :: IX, IY, LLX, LLY, Index
!
!	Check specified graph lies in the image
!
	RowLen = SIZE (GaborTrafo, DIM = 3)
	ColLen = SIZE (GaborTrafo, DIM = 4)
	If (Position (1) <= 0 .OR. Position (2) <= 0 .OR. &
	&   (Position (1)+(GraphPars%StepX*(GraphPars%NodesX-1))) > RowLen .OR. &
	&   (Position (2)+(GraphPars%StepY*(GraphPars%NodesY-1))) > ColLen) Then
		Print *, 'ExtractGraph: Graph not totally inside image'
		Print *, Position, GraphPars%NodesX, GraphPars%NodesY, &
	&	         GraphPars%StepX, GraphPars%StepY, RowLen, ColLen
		STOP
	End If
!
!	Loop over all graph positions and copy jets
!
	Index = 1
	LLY = Position (2)
	Do IY = 0, GraphPars%NodesY - 1
		LLX = Position (1)
		Do IX = 0, GraphPars%NodesX - 1
			GraphResult (:, :, Index) = GaborTrafo (:, :, LLX, LLY)
			Index = Index + 1
			LLX = LLX + GraphPars%StepX
		End Do
		LLY = LLY + GraphPars%StepY
	End Do
	Return

	End Subroutine ExtractGraph

	Subroutine LocalMove (Params, Position, Graph, GaborTrafo, &
				          Similarity, Hops, Sweeps)
!
!	Perform local move of Graph on GaborTrafo, starting from Position.
!
	Use FaceRecTypes

	Implicit NONE
!
!	Declarations
!
!	  Arguments
!
	Type(FaceRecP), Intent(In)    :: Params
	Integer,        Intent(In)    :: Position (2)
	Real(4),        Intent(In)    :: Graph (:, :, :)
	Real(4),        Intent(In)    :: GaborTrafo (:, :, :, :)
	Real(4),        Intent(InOut) :: Similarity
	Integer,        Intent(Out)   :: Hops, Sweeps
!
!	  Local variables: arrays
!
	Integer, Allocatable, Save :: VertexX (:), VertexY (:)
	Integer, Allocatable, Save :: CoordX  (:, :), CoordY  (:, :)
	Real(4), Allocatable, Save :: JetSim  (:)
!
!	  Local variables: abbreviations, counters and such
!
	Logical :: Improved

	Real(4) :: OldSim, NewJetSim, NewSim
	Integer :: TopCost, OldTopCost, NewTopCost

	Integer :: MaxDev
	Real(4) :: Lambda, SimThresh

	Integer :: RowLen, ColLen
	Integer :: StepX, StepY, NodesX, NodesY, NodeCount

	Integer :: Step, NewX, NewY
	Integer :: Index, IX, IY, IG
!
!	Set the abbreviations
!
	MaxDev = Params%Match%MaxDev
	Lambda = Params%Match%Lambda
	SimThresh = Params%Match%SimThresh

	StepX  = Params%Graph%StepX
	StepY  = Params%Graph%StepY
	NodesX = Params%Graph%NodesX
	NodesY = Params%Graph%NodesY
	NodeCount = NodesX * NodesY

	RowLen = SIZE (GaborTrafo, DIM = 3)
	ColLen = SIZE (GaborTrafo, DIM = 4)
!
!	(Re)allocate arrays
!
	If (ALLOCATED (VertexX)) Then
		If (ANY (SHAPE (VertexX) /= (/NodeCount/))) Then
			Deallocate (VertexX, VertexY, JetSim)
		End If
	End If
	If (.NOT. ALLOCATED (VertexX)) Then
		Allocate (VertexX (NodeCount), VertexY (NodeCount), JetSim (NodeCount))
	End If

	If (ALLOCATED (CoordX)) Then
		If (ANY (SHAPE (CoordX) /= (/NodesX, NodesY/))) Then
			Deallocate (CoordX, CoordY)
		End If
	End If
	If (.NOT. ALLOCATED (CoordX)) Then
		Allocate (CoordX (NodesX, NodesY), CoordY (NodesX, NodesY))
	End If
!
!	Initialize vertex, similarity and coordinate arrays
!
	Do Index = 1, NodeCount
		IX = MOD (Index - 1, NodesX) + 1
		IY = ((Index - 1) / NodesX) + 1
		CoordX (IX, IY) = Position (1) + (IX - 1) * StepX
		CoordY (IX, IY) = Position (2) + (IY - 1) * StepY
		JetSim (Index)  = SUM (Graph (:, :, Index) * &
	&		                   GaborTrafo (:, :, CoordX(IX,IY), CoordY(IX,IY)))
		VertexX (Index) = MOD (Params%Graph%RandomIndex (Index) - 1, NodesX) + 1
		VertexY (Index) = ((Params%Graph%RandomIndex (Index) - 1) / NodesX) + 1
	End Do

	If (ABS (Similarity - SUM (JetSim)) > 0.1) &
	&	Print *, 'LocalMove: old = ', Similarity, ', new = ', SUM (JetSim)
!
!	Initialize other local variables
!
	Hops = 0
	Sweeps = 0
	TopCost = 0
!
!	Perform sweeps over the graph, stopping when either no improvement
!	is obtained for one sweep or when the maximum number of sweeps has been
!	executed.
!
	Do
		Improved = .FALSE.
		Step = 2 * Params%Image%UpSample
!
!		Loop over all nodes (one sweep)
!
		Do Index = 1, NodeCount
			IX = VertexX (Index)
			IY = VertexY (Index)
			IG = Params%Graph%RandomIndex (Index)

			Do NewY = CoordY (IX, IY)-MaxDev, CoordY (IX, IY)+MaxDev, Step
				If ((NewY > 0) .AND. (NewY <= ColLen)) Then
					Do NewX = CoordX(IX, IY)-MaxDev, CoordX(IX, IY)+MaxDev, Step
						If ((NewX > 0) .AND. (NewX <= RowLen)) Then
							NewJetSim = SUM (Graph (:, :, IG) * &
	&		                                 GaborTrafo (:, :, NewX, NewY))
							Call TopCostFct (OldTopCost, NewTopCost)

							OldSim = JetSim (IG) - (Lambda * OldTopCost)
							NewSim = NewJetSim   - (Lambda * NewTopCost)

							If ((NewSim - OldSim) > SimThresh) Then
								CoordX (IX, IY) = NewX
								CoordY (IX, IY) = NewY
								JetSim (IG) = NewJetSim
								TopCost = TopCost - OldTopCost + NewTopCost
								If (TopCost < 0) &
	&								Print '(A, 4I4, I6)', &
	&								      'LocalMove: negative top. cost at', &
	&								      Sweeps, Hops, IX, IY, TopCost
								Hops = Hops + 1
								Improved = .TRUE.
							End If
						End If
					End Do
				End If
			End Do
		End Do
!
!		Check termination criterion
!
		Sweeps = Sweeps + 1
		If ((.NOT. Improved) .OR. (Sweeps >= Params%Match%MaxSweeps)) Exit
	End Do

	Similarity = SUM (JetSim) - (Lambda * TopCost)

	Return

	Contains

		Subroutine TopCostFct (OC, NC)
!
!		Computes the topological costs associated with a vertex move
!
!		Declarations
!
!		  Arguments
!
		Integer, Intent(Out) :: OC, NC
!
!		  Statement functions to compute contribution of one edge
!
		Integer :: EdgeXP, EdgeXM, EdgeYP, EdgeYM
		Integer :: V1X, V1Y, V2X, V2Y
		EdgeXP (V1X, V1Y, V2X, V2Y) = (V2X - V1X - StepX)**2 + (V2Y - V1Y)**2
		EdgeXM (V1X, V1Y, V2X, V2Y) = (V2X - V1X + StepX)**2 + (V2Y - V1Y)**2
		EdgeYP (V1X, V1Y, V2X, V2Y) = (V2X - V1X)**2 + (V2Y - V1Y - StepY)**2
		EdgeYM (V1X, V1Y, V2X, V2Y) = (V2X - V1X)**2 + (V2Y - V1Y + StepY)**2
!
!		Select on IY: are we on top or bottom row, or somewhere in between
!		In each case, a similar selection occurs on IX.
!
		Select Case (IY)
			Case (1)
				Select Case (IX)
					Case (1)
!
!						5: On bottom edge of graph, left corner
!
						OC = 0  + EdgeXP (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						NC = 0  + EdgeXP (NewX,              NewY,             &
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						OC = OC + EdgeYP (CoordX (IX, IY),   CoordY (IX, IY),  &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						NC = NC + EdgeYP (NewX,              NewY,             &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
					Case DEFAULT
						If ((IX > 1) .AND. (IX < NodesX)) Then
!
!						4: On bottom edge of graph, not a corner
!
						OC = 0  + EdgeXM (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						NC = 0  + EdgeXM (NewX,              NewY,             &
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						OC = OC + EdgeXP (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						NC = NC + EdgeXP (NewX,              NewY,             &
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						OC = OC + EdgeYP (CoordX (IX, IY),   CoordY (IX, IY),  &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						NC = NC + EdgeYP (NewX,              NewY,             &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						Else If (IX == NodesX) Then 
!
!						6: On bottom edge of graph, right corner
!
						OC = 0  + EdgeXM (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						NC = 0  + EdgeXM (NewX,              NewY,             &
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						OC = OC + EdgeYP (CoordX (IX, IY),   CoordY (IX, IY),  &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						NC = NC + EdgeYP (NewX,              NewY,             &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						Else
							Print *, 'LocalMove: IX (', IX, &
	&					             ') out of range (1,', NodesX,').'
							STOP
						End If
		        End Select
			Case DEFAULT
				If ((IY > 1) .AND. (IY < NodesY)) Then
				Select Case (IX)
					Case (1)
!
!						2: On left edge of graph, not a corner
!
						OC = 0  + EdgeYM (CoordX (IX, IY),   CoordY (IX, IY),  &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						NC = 0  + EdgeYM (NewX,              NewY,             &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						OC = OC + EdgeXP (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						NC = NC + EdgeXP (NewX,              NewY,             &
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						OC = OC + EdgeYP (CoordX (IX, IY),   CoordY (IX, IY), &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						NC = NC + EdgeYP (NewX,              NewY,             &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
					Case DEFAULT
						If ((IX > 1) .AND. (IX < NodesX)) Then
!
!						1: In the central part of the graph (likeliest case)
!
						OC = 0  + EdgeXM (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						NC = 0  + EdgeXM (NewX,              NewY,             &
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						OC = OC + EdgeYM (CoordX (IX, IY),   CoordY (IX, IY), &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						NC = NC + EdgeYM (NewX,              NewY,             &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						OC = OC + EdgeXP (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						NC = NC + EdgeXP (NewX,              NewY,             &
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						OC = OC + EdgeYP (CoordX (IX, IY),   CoordY (IX, IY), &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						NC = NC + EdgeYP (NewX,              NewY,             &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						Else If (IX == NodesX) Then 
!
!						3: On right edge of graph, not a corner
!
						OC = 0  + EdgeXM (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						NC = 0  + EdgeXM (NewX,              NewY,             &
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						OC = OC + EdgeYM (CoordX (IX, IY),   CoordY (IX, IY), &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						NC = NC + EdgeYM (NewX,              NewY,             &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						OC = OC + EdgeYP (CoordX (IX, IY),   CoordY (IX, IY), &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						NC = NC + EdgeYP (NewX,              NewY,             &
	&					                  CoordX (IX, IY+1), CoordY (IX, IY+1))
						Else
							Print *, 'LocalMove: IX (', IX, &
	&					             ') out of range (1,', NodesX,').'
							STOP
						End If
		        End Select
			Else If (IY == NodesY) Then 
				Select Case (IX)
					Case (1)
!
!						8: On top edge of graph, left corner
!
						OC = 0  + EdgeYM (CoordX (IX, IY),   CoordY (IX, IY),  &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						NC = 0  + EdgeYM (NewX,              NewY,             &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						OC = OC + EdgeXP (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						NC = NC + EdgeXP (NewX,              NewY,             &
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
					Case DEFAULT
						If ((IX > 1) .AND. (IX < NodesX)) Then
!
!						7: On top edge of graph, not a corner
!
						OC = 0  + EdgeXM (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						NC = 0  + EdgeXM (NewX,              NewY,             &
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						OC = OC + EdgeYM (CoordX (IX, IY),   CoordY (IX, IY), &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						NC = NC + EdgeYM (NewX,              NewY,             &
						                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						OC = OC + EdgeXP (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						NC = NC + EdgeXP (NewX,              NewY,             &
	&					                  CoordX (IX+1, IY), CoordY (IX+1, IY))
						Else If (IX == NodesX) Then 
!
!						9: On top edge of graph, right corner
!
						OC = 0  + EdgeXM (CoordX (IX,   IY), CoordY (IX,   IY),&
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						NC = 0  + EdgeXM (NewX,              NewY,             &
	&					                  CoordX (IX-1, IY), CoordY (IX-1, IY))
						OC = OC + EdgeYM (CoordX (IX, IY),   CoordY (IX, IY),  &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						NC = NC + EdgeYM (NewX,              NewY,             &
	&					                  CoordX (IX, IY-1), CoordY (IX, IY-1))
						Else
							Print *, 'LocalMove: IX (', IX, &
	&					             ') out of range (1,', NodesX,').'
							STOP
						End If
		        End Select
			Else
				Print *, 'LocalMove: IY (',IY,') out of range (1,',NodesY,').'
				STOP
			End If
        End Select

		If ((OC < 0) .OR. (NC < 0)) &
	&		Print '(A, 4I4, 2I8)', 'LocalMove: negative inc. top. cost at', &
	&		      IX, IY, NewX, NewY, OC, NC
		End Subroutine TopCostFct

	End Subroutine LocalMove

	End Module GraphRoutines

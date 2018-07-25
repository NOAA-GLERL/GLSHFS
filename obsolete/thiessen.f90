!------------------------------------------------------------------------------
!  This module contains routines used to compute thiessen weights.
!  Note that this code is a simplified brute-force method that, in truth,
!  has little resemblance to thiessen polygons. It's a simple 
!  nearest-neighbor weighting scheme that emulates the result from the
!  thiessen polygon method.
!
!  Efficiency of this code is not great. The methodology used in the
!  original AHPS package was a good deal more efficient, but the design of
!  the code made it difficult or impossible to build corollary outputs such
!  as maps of the result. These corollary outputs were an oft-requested thing,
!  so I am switching to this less efficient method in hopes that it will still
!  be "efficient enough" for our needs.
!  We will see.
!
!  Tim Hunter
!  January 11, 2017
!------------------------------------------------------------------------------

MODULE Thiessen
      USE GLSHFS_Global
      USE ErrorProcess
      USE GLSHFS_Util
      USE GlerlMaps
      

CONTAINS


!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Given a bunch of station locations and a GLERL map, compute 
      !  "thiessen weights" for each station for each subbasin.
      !
      !  Input:
      !    Lats = 1d array of REALs with latitude for each station
      !    Lons = 1d array of REALs with longitude for each station
      !    GMap = TGlerlMap structure
      !  Output:
      !    Weights = 2d array of REALs with weight for each station, per subbasin.
      !              This array must already be sized correctly by the 
      !              calling routine, and is indexed (1:NumStations, 0:NumSubbasins)
      !              The weights will sum to 1.0 for each subbasin
      !  *** IMPORTANT NOTE ***
      !  When the Weights array is passed in as an argument, the indexing within
      !  this subroutine becomes (1:NumStations, 1:NumSubbasins+1).  i.e. the lower 
      !  bound of each index is automatically set to 1.  This is a "feature" of 
      !  Fortran, and I don't know of any way to change that.  Thus, when assigning
      !  the values, we have to adjust the index values for that offset.
      !------------------------------------------------------------------------
      SUBROUTINE ComputeThsnWeights(Lats, Lons, GMap, Weights)
      IMPLICIT NONE
      REAL, DIMENSION(:),   INTENT(IN)  :: Lats, Lons
      TYPE (TGlerlMap),     INTENT(IN)  :: GMap
      REAL, DIMENSION(:,:), INTENT(OUT) :: Weights
      
      INTEGER :: I, NumS, MC, MR, TotCount, Closest, SubP1
      REAL    :: Lt, Ln, X, Y, DistSq, MinDistSq
      
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: CellCount       ! indexed (1:station, 0:subbasins)
      REAL, DIMENSION(:), ALLOCATABLE :: StnX, StnY
      
      !
      !  Initialize the Weights array
      !
      Weights(:,:) = 0.0
      
      !
      !  How many valid stations, based on contents of Lats array?
      !
      NumS = 0
      DO I = UBOUND(Lats, 1)
         IF ((Lats(I) .GT. -91.0) .AND. (LatS(I) .LT. 91.0)) NumS = NumS + 1
      END DO         
      
      !
      !  Handle the zero-station situation
      !  Just leave the weights to all 0.0
      !
      IF (NumS .EQ. 0) RETURN
      
      !
      !  Handle the trivial one station situation.
      !  Just set the weights to all 1.0
      !
      IF (NumS .EQ. 1) THEN
         Weights(:,:) = 1.0
         RETURN
      END IF
      
      !
      !  Allocate local working arrays
      !
      Subs = GMap%Subbasins + 2
      ALLOCATE(StnX(NumS), StnY(NumS), CellCount(NumS,0:Subs), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for local arrays.'; CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Convert all lat/long coordinates to x/y
      !
      DO I = 1, NumS
         Lt = Lats(I)
         Ln = Lons(I)
         CALL GMap%LatLong2XY(Lt, Ln, X, Y); IF (ErrorLevel .NE. 0) GOTO 899
         StnX(I) = X
         StnY(I) = Y
      END DO
      
      !
      !  Step through the entire map, cell-by-cell. 
      !  For each cell:
      !    1) Determine which station is closest.
      !    2) Increment the count of times that station was the closest
      !       station to a cell of this subbasin.
      !
      DO MR = 0, GMap%MapHeight-1             ! row (south to north)
         DO MC = 0, GMap%MapWidth-1           ! column (west to east)
            MinDistSq = 9.9e29
            Closest = -1                      ! index of the closest station to this cell
            DO I = 1, NumS
               DistSq = (StnX(I)-MC)**2 + (StnY(I)-MR)**2       
               IF (DistSQ .LT. MinDistSq) THEN
                  Closest = I
                  MinDistSq = DistSq
               END IF
            END DO
            Sub = ICHAR(GMap%Map2D(MC,MR))      ! includes the OOB code
            CellCount(Closest, Sub) = CellCount(Closest, Sub) + 1
         END DO
      END DO

      !
      !  Now compute the weights for each station, for each subbasin.
      !  Remember to adjust the subbasin index for Weights() in order
      !  to account for the modified indexing 
      !  (0:n in caller becomes 1:n+1 here).
      !
      DO Sub = 0, GMap%Subbasins
         TotCount = 0
         SubP1 = Sub + 1      ! Index into Weights array for subbasin Sub
         DO I = 1, NumS
            TotCount = TotCount + CellCount(I,Sub)
         END DO
         IF (TotCount .GT. 0) THEN
            DO I = 1, NumS
               Weights(I,SubP1) = (CellCount * 1.0) / TotCount     ! value between 0..1
            END DO
         END IF         
      END DO
      GOTO 999
      
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] ComputeThsnWeights...'; CALL PassMsg
      
 999  DEALLOCATE(StnX, StnY, CellCount, STAT=IOS)
      RETURN
      
      END SUBROUTINE ComputeThsnWeights
      
      
      !------------------------------------------------------------------------
      !  Given a bunch of station locations and a GLERL map, compute 
      !  "thiessen weights" for each station.  Only do this for the 
      !  requested subbasin.
      !
      !  Input:
      !    Subbasin = The subbasin of interest (0..n)
      !    Lats = 1d array of REALs with latitude for each station
      !    Lons = 1d array of REALs with longitude for each station
      !    GMap = TGlerlMap structure
      !  Output:
      !    Weights = 1d array of REALs with weight for each station.
      !              This array must already be sized correctly by the 
      !              calling routine.
      !              The weights will sum to 1.0
      !------------------------------------------------------------------------
      SUBROUTINE ComputeThsnWeights_OneSubbasin(Subbasin, Lats, Lons, GMap, Weights)
      IMPLICIT NONE
      INTEGER,            INTENT(IN)  :: Subbasin
      REAL, DIMENSION(:), INTENT(IN)  :: Lats, Lons
      TYPE (TGlerlMap),   INTENT(IN)  :: GMap
      REAL, DIMENSION(:), INTENT(OUT) :: Weights
      
      INTEGER :: I, NumS, MC, MR, TotCount, Closest
      REAL    :: Lt, Ln, X, Y, DistSq, MinDistSq
      
      INTEGER, DIMENSION(:), ALLOCATABLE :: CellCount       ! indexed (1:stations)
      REAL,    DIMENSION(:), ALLOCATABLE :: StnX, StnY
      
      !
      !  Initialize the Weights array
      !
      Weights(:) = 0.0
      
      !
      !  How many valid stations, based on contents of Lats array?
      !
      NumS = 0
      DO I = UBOUND(Lats, 1)
         IF ((Lats(I) .GT. -91.0) .AND. (LatS(I) .LT. 91.0)) NumS = NumS + 1
      END DO         
      
      !
      !  Allocate local working arrays
      !
      Subs = GMap%Subbasins + 2
      ALLOCATE(StnX(NumS), StnY(NumS), CellCount(NumS), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for local arrays.'; CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Convert all lat/long coordinates to x/y
      !
      DO I = 1, NumS
         Lt = Lats(I)
         Ln = Lons(I)
         CALL GMap%LatLong2XY(Lt, Ln, X, Y); IF (ErrorLevel .NE. 0) GOTO 899
         StnX(I) = X
         StnY(I) = Y
      END DO
      
      !
      !  Step through the entire map, cell-by-cell. 
      !  For each cell:
      !    1) Determine which station is closest.
      !    2) Increment the count of times that station was the closest
      !       station to a cell of this subbasin.
      !
      DO MR = 0, GMap%MapHeight-1             ! row (south to north)
         DO MC = 0, GMap%MapWidth-1           ! column (west to east)
            Sub = ICHAR(GMap%Map2D(MC,MR))    ! includes the OOB code
            IF (Sub .EQ. Subbasin) THEN
               MinDistSq = 9.9e29
               Closest = -1                      ! index of the closest station to this cell
               DO I = 1, NumS
                  DistSq = (StnX(I)-MC)**2 + (StnY(I)-MR)**2       
                  IF (DistSQ .LT. MinDistSq) THEN
                     Closest = I
                     MinDistSq = DistSq
                  END IF
               END DO
               CellCount(Closest) = CellCount(Closest) + 1
            END IF
         END DO
      END DO

      !
      !  Now compute the weights for each station
      !
      TotCount = 0
      DO I = 1, NumS
         TotCount = TotCount + CellCount(I)
      END DO
      IF (TotCount .GT. 0) THEN
         DO I = 1, NumS
            Weights(I) = (CellCount * 1.0) / TotCount     ! value between 0..1
         END DO
      END IF
      GOTO 999
      
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] ComputeThsnWeights...'; CALL PassMsg
      
 999  DEALLOCATE(StnX, StnY, CellCount, STAT=IOS)
      RETURN
      
      END SUBROUTINE ComputeThsnWeights_OneSubbasin
      

END MODULE Thiessen
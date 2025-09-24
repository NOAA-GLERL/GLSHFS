!-------------------------------------------------------------------------------
!  Define an object for the 2-d subbasin map.
!  This is a little bit of overkill for the map as it exists now, but it will
!  allow for easy future improvements/changes, particularly once I have new 
!  raster maps built with a consistent projection across all lakes.
!  
!  Tim Hunter, 2016Oct17
!-------------------------------------------------------------------------------

MODULE GlerlMaps
      USE ErrorProcess
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GlerlDataTypesAndUnits

      PRIVATE

      PUBLIC :: ReadGlerlMap_Old
      PUBLIC :: GlerlPoly_LatLong2XY
      PUBLIC :: GlerlPoly_XY2LatLong
      PUBLIC :: DistanceFromNearestSubbasin
      PUBLIC :: DistanceCellXY
      
      TYPE, PUBLIC :: TGlerlMap
         CHARACTER (LEN=3) :: Bsn
         INTEGER           :: MapWidth             ! number of cells
         INTEGER           :: MapHeight            ! number of cells
         INTEGER           :: Subbasins            !
         INTEGER           :: OutOfBasin           ! Code for cells that are outside the basin
         REAL              :: CellSize             ! meters, cells assumed to be square
         REAL              :: XOffset              !
         REAL              :: YOffset              !
         REAL              :: RefMeridian          ! degrees EAST of Prime meridian (G.L. ~ -65 to -95)
         REAL              :: Radius               ! radius of the earth (km)
         CHARACTER (LEN=1), DIMENSION(:,:), ALLOCATABLE  :: Map2d     ! (0:MapWidth-1, 0:MapHeight-1)
      CONTAINS
         PROCEDURE, PRIVATE, PASS :: Initialize    => InitializeMap
         PROCEDURE, PUBLIC        :: Clear         => ClearMap
         PROCEDURE, PUBLIC        :: SetUp         => SetUpMap
         PROCEDURE, PUBLIC        :: ReadMap       => ReadMap_OldFormat
         PROCEDURE, PUBLIC        :: LatLong2XY    => LatLong2XY
         PROCEDURE, PUBLIC        :: XY2LatLong    => XY2LatLong
      END TYPE TGlerlMap
      
      INTERFACE TGlerlMap
         PROCEDURE CreateMap
      END INTERFACE TGlerlMap

      !
      !  These are the values corresponding to GLERL's historical maps.
      !  Included here for now so I can use old map files.  But the
      !  new map file format will have these values in the file itself, and
      !  these arrays will be vestigial/useless.
      !
      !  Arrays are in the standard order:
      !  Sup, Mic, Hur, Geo, Stc, Eri, Ont
      !  (Note that these are private to the module)
      !
      !  Note that for purposes of coordinate transformation (lat,long <-> x,y)
      !  the rows and columns are numbered (0..n-1), with (0,0) being the SW corner.
      !
      INTEGER, DIMENSION(7), PARAMETER :: MapWideArr =                     &
            (/ 760, 472, 411, 552, 240, 584, 456 /)
            
      INTEGER, DIMENSION(7), PARAMETER :: MapHighArr =                     &
            (/ 516, 608, 436, 426, 184, 424, 392 /)
            
      REAL, DIMENSION(7), PARAMETER :: XOffsetArr =                        &
            (/ 406.039150,  227.973530,  279.796362,  279.796362,          &
                64.526407,  298.661220,  262.877366 /)

      REAL, DIMENSION(7), PARAMETER :: YOffsetArr =                        &
            (/ -5121.245120, -4586.637700, -4709.846822, -4859.846822,     &
               -4648.658407, -4473.268070, -4632.988223 /)

      REAL, DIMENSION(7), PARAMETER ::  RMArr =                            &
            (/ -87.85, -86.87, -81.79, -81.79, -82.71, -81.74, -76.90 /)

      REAL, DIMENSION(7), PARAMETER ::  RadiusArr =                        &
            (/ 6365.2, 6363.7, 6346.2, 6346.2, 6328.6, 6338.3, 6341.8 /)

      
      
CONTAINS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      FUNCTION CreateMap() RESULT(M)
         IMPLICIT NONE
         TYPE (TGlerlMap) :: M
         CALL M%Initialize()
      END FUNCTION CreateMap

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE InitializeMap(this)
         IMPLICIT NONE
         CLASS (TGlerlMap), INTENT(INOUT) :: this
         INTEGER :: IOS
         this%Bsn = ''
         this%MapWidth    = -1
         this%MapHeight   = -1
         this%CellSize    = -1
         this%Subbasins   = -1
         this%OutofBasin  = -1
         this%XOffset     = -1
         this%YOffset     = -1
         this%RefMeridian = -1
         this%Radius      = -1
         DEALLOCATE(this%Map2D, STAT=IOS)
      END SUBROUTINE InitializeMap
      
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE ClearMap(this)
         IMPLICIT NONE
         CLASS (TGlerlMap), INTENT(INOUT) :: this
         CALL this%Initialize()
      END SUBROUTINE ClearMap

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE SetupMap(this, Bsn)
         IMPLICIT NONE
         CLASS (TGlerlMap), INTENT(INOUT) :: this
         CHARACTER(LEN=3),  INTENT(IN)    :: Bsn
         INTEGER :: Lk
         
         Lk = LakeNumberFromName3(Bsn)
         IF (Lk .LT. 1) THEN
            ErrorMessage = 'Error: Invalid basin name ['//TRIM(Bsn)//']'; CALL PassMsg
            ErrorLevel = 1
            RETURN
         END IF
         this%Bsn         = Bsn
         this%MapWidth    = MapWideArr(Lk)
         this%MapHeight   = MapHighArr(Lk)
         this%CellSize    = 1000.0
         this%Subbasins   = NumSubbasins(Lk)
         this%OutofBasin  = NumSubbasins(Lk) + 2
         this%XOffset     = XOffsetArr(Lk)
         this%YOffset     = YOffsetArr(Lk)
         this%RefMeridian = RMArr(Lk)
         this%Radius      = RadiusArr(Lk)
         
      END SUBROUTINE SetupMap

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE ReadMap_OldFormat(this, Filename)
         IMPLICIT NONE
         CLASS (TGlerlMap), INTENT(INOUT) :: this
         CHARACTER(LEN=*) :: Filename
         CALL ReadGlerlMap_Old(Filename, this%Bsn, this)
      END SUBROUTINE ReadMap_OldFormat

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE LatLong2XY(this, Lt, Ln, X, Y)
         IMPLICIT NONE
         CLASS (TGlerlMap), INTENT(INOUT) :: this
         REAL,              INTENT(IN)    :: Lt, Ln
         REAL,              INTENT(OUT)   :: X, Y
         REAL :: RM, XOff, YOff, Rad
         RM   = this%RefMeridian
         XOff = this%XOffset
         YOff = this%YOffset
         Rad  = this%Radius
         CALL GlerlPoly_LatLong2XY(Lt, Ln, RM, XOff, YOff, Rad, X, Y)
      END SUBROUTINE LatLong2XY

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE XY2LatLong(this, X, Y, Lt, Ln)
         IMPLICIT NONE
         CLASS (TGlerlMap), INTENT(INOUT) :: this
         REAL,              INTENT(IN)    :: X, Y
         REAL,              INTENT(OUT)   :: Lt, Ln
         REAL :: RM, XOff, YOff, Rad
         RM   = this%RefMeridian
         XOff = this%XOffset
         YOff = this%YOffset
         Rad  = this%Radius
         CALL GlerlPoly_XY2LatLong(X, Y, RM, XOff, YOff, Rad, Lt, Ln)
      END SUBROUTINE XY2LatLong


!-------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------
      SUBROUTINE ReadGlerlMap_Old(Filename, BsnIn, GMap)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN)    :: Filename, BsnIn
         TYPE (TGlerlMap), INTENT(INOUT) :: GMap

         INTEGER :: I, J, IOS, U1, LkNum
         LOGICAL :: FExist
         CHARACTER(LEN=1)  :: BridgeCode 
         CHARACTER(LEN=3)  :: Bsn
         CHARACTER(LEN=20) :: S

         CHARACTER (LEN=1), DIMENSION(:),   ALLOCATABLE :: MapRow        ! (0:MapWidth-1)
         
         !
         !  Does the map file exist?
         !
         INQUIRE(FILE=TRIM(Filename), EXIST=FExist)
         IF (.NOT. FExist) THEN
            ErrorMessage = 'Specified map file not found. ['//TRIM(Filename)//']'; CALL PassMsg
            GOTO 898
         END IF
         
         !
         !  Use BsnIn to determine which lake, and then set various parameters
         !
         S = TRIM(ADJUSTL(BsnIn))
         I = LEN_TRIM(S)
         IF (I .GT. 3) S = S(1:3)
         Bsn = GetLowercase(TRIM(S))
         LkNum = LakeNumberFromName3(Bsn)
         IF (LkNum .LT. 1) THEN
            ErrorMessage = 'Unable to determine the lake number from lake name ['//Bsn//']'; CALL PassMsg
            GOTO 898
         END IF

         GMap%Bsn         = Bsn
         GMap%Subbasins   = NumSubbasins(LkNum)        ! from GLSHFS_Global
         GMap%CellSize    = 1000.0                     ! by definition, cells are 1 sq km
         GMap%OutOfBasin  = GMap%Subbasins + 2
         GMap%MapWidth    = MapWideArr(LkNum)
         GMap%MapHeight   = MapHighArr(LkNum)
         GMap%XOffset     = XOffsetArr(LkNum)
         GMap%YOffset     = YOffsetArr(LkNum)
         GMap%RefMeridian = RMArr(LkNum)
         GMap%Radius      = RadiusArr(LkNum)

         
         !
         !  Allocate the array(s) for the map file
         !
         IF (ALLOCATED(GMap%Map2D)) DEALLOCATE(GMap%Map2d, STAT=IOS)
         ALLOCATE(GMap%Map2d(0:GMap%MapWidth-1, 0:GMap%MapHeight-1),             &
                  MapRow(0:GMap%MapWidth-1), STAT=IOS)
         IF (IOS .NE. 0) THEN
            ErrorMessage = 'Error allocating temporary arrays for reading map file.'; CALL PassMsg
            GOTO 898
         END IF

         !
         !  Input the code map for the subbasins within the basin.
         !  Read one row at a time into zMapRow, then assign the row
         !  into the zMap array.
         !
         U1 = GetFreeUnitNumber()
         OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='OLD', RECL=GMap%MapWidth,          &
              ACCESS='DIRECT', FORM='UNFORMATTED', ERR=811)
         CALL FileWasOpened(U1)
         DO J = 0, GMap%MapHeight-1
           READ(U1, REC=J+1, ERR=812) MapRow
           DO I = 0, GMap%MapWidth-1
              GMap%Map2d(I,J) = MapRow(I)
           END DO
         END DO
         CLOSE (U1)
         CALL FileWasClosed(U1)
         
         !
         !  The historical map file contains cells with a "bridge" code. These are water cells
         !  that were assigned this bridge code to connect them to the mainland subbasins.
         !  This scheme is only relevant when trying to draw the boundaries of the "subbasin"
         !  (so that the islands will be shown as part of the subbasin). However, I have no
         !  interest in maintaining that scheme. There are other ways to graphically represent
         !  the subbasin boundaries without doing this, and it tends to confuse the issue of 
         !  what constitutes the subbasin areas and the lake area. Simple iteration and 
         !  counting won't work. And the scheme is not necessary for computation of thiessen
         !  weights. Thus, for GLSHFS, I am eliminating bridge codes. They still exist in 
         !  the data files; I just reassign them to subbasin 0 (lake surface).
         !
         BridgeCode = CHAR(GMap%Subbasins + 1)
         DO J = 0, GMap%MapHeight-1
           DO I = 0, GMap%MapWidth-1
              IF (GMap%Map2d(I,J) .EQ. BridgeCode) GMap%Map2d(I,J) = CHAR(0)
           END DO
         END DO

         RETURN
         
         811 ErrorMessage = 'Error opening map file ' // TRIM(Filename); CALL PassMsg
             GOTO 898
         812 ErrorMessage = 'Error reading map file ' // TRIM(Filename); CALL PassMsg
             GOTO 898

         898 ErrorLevel = 1
             ErrorMessage = '[traceback] ReadGlerlMap_Old()...'; CALL PassMsg
             DEALLOCATE(GMap%Map2D, MapRow, STAT=IOS)
         RETURN


      END SUBROUTINE ReadGlerlMap_Old


!-------------------------------------------------------------------------
!  GlerlPoly_LatLong2XY:
!       This routine accepts the latitude and longitude of a point and
!       returns the x and y map coordinates of the point representing
!       the 'horizontal' and 'vertical' distances, respectively, from
!       an 'origin' on the map of (0.5, 0.5).
!
!       The map is a polyconic projection about a central meridian
!       (e.g. 88.0000 degrees west of the prime meridian for Superior).
!
!       On entry,
!       Lat is latitude in degrees and decimal degrees, positive from the
!                equator north.
!       Long is longitude in degrees and decimal degrees, positive from the
!                prime meridian east (negative west of the p. m.).
!
!       Calibrated map parameters:
!          RM  = the reference meridian (e.g. 88.00 for Superior)
!          Rad = calibrated radius of the earth in km
!          XOff  = x offset
!          YOff  = y offset
!
!       On exit,
!          x is distance in kilometers from point (0.5, 0.5) right.
!          y is distance in kilometers from point (0.5, 0.5) up.
!-------------------------------------------------------------------------
      SUBROUTINE GlerlPoly_LatLong2XY(Lat, Long, RM, XOff, YOff, Rad, X, Y)
         IMPLICIT NONE
         REAL, INTENT(IN)  :: Lat, Long, RM, XOff, YOff, Rad
         REAL, INTENT(OUT) :: X, Y
         REAL              :: Lt, Ln, Tp, Snpl

         !
         !  Verify lat/long inputs are reasonable
         !
         IF (Lat .LT.   -90) GOTO 850
         IF (Lat .GT.    90) GOTO 850
         IF (Long .LT. -180) GOTO 850
         IF (Long .GT.  180) GOTO 850
         
         !
         !  Get latitude in radians and longitude in radians from reference meridian.
         !
         Lt = Lat / 57.29578
         Ln = (Long - RM) / 57.29578

         Tp = TAN(Lt)
         Snpl = SIN(Lt) * Ln
         X = Rad * SIN(Snpl) / Tp
         Y = Rad * (Lt + (1. - COS(Snpl)) / Tp)
         X = X + XOff
         Y = Y + YOff
         RETURN

         850 ErrorMessage = 'Invalid latitude or longitude value.'; CALL PassMsg
             WRITE(ErrorMessage, 1001) Lat, Long; CALL PassMsg; GOTO 898
            
         898 ErrorLevel = 1
             ErrorMessage = '[traceback] GlerlPoly_LatLong2XY()...'; CALL PassMsg
             RETURN
            
         1001 FORMAT('Latitude=(', F0.2, ')  Longitude = (', F0.2, ')')
         END SUBROUTINE GlerlPoly_LatLong2XY

!-------------------------------------------------------------------------
!  GlerlPoly_XY2LatLong:
!       This routine accepts the x and y map coordinates of a point representing
!       the 'horizontal' and 'vertical' distances, respectively, from
!       an 'origin' on the map of (0.5, 0.5).  It returns the latitude and 
!       longitude of the point.
!
!       The map is a polyconic projection about a central meridian
!       (e.g. 88.0000 degrees west of the prime meridian for Superior).
!
!       On entry,
!          x is distance in kilometers from point (0.5, 0.5) right.
!          y is distance in kilometers from point (0.5, 0.5) up.
!
!       Calibrated map parameters:
!          RM  = the reference meridian (e.g. 88.00 for Superior)
!          Rad = calibrated radius of the earth in km
!          XOff  = x offset
!          YOff  = y offset
!
!       On exit,
!          Lat is latitude in degrees and decimal degrees, positive from the
!                 equator north.
!          Long is longitude in degrees and decimal degrees, positive from the
!                 prime meridian east (negative west of the p. m.).
!-------------------------------------------------------------------------
      SUBROUTINE GlerlPoly_XY2LatLong(X, Y, RM, XOff, YOff, Rad, Lat, Long)
         IMPLICIT NONE
         REAL, INTENT(IN)   :: X, Y, RM, XOff, YOff, Rad
         REAL, INTENT(OUT)  :: Lat, Long
         REAL, PARAMETER    :: Tolerance = 1.0e-6
         INTEGER :: Iterations
         REAL    :: XA, YA, XB, YB
         REAL    :: LtA, LnA, LtB, LnB, DeltaLat, DeltaLon
         REAL    :: SX, SY
         REAL    :: Diff, XDiffA, YDiffA

         INTEGER, PARAMETER :: MaxIterations = 50
         
         !
         !  Choose an arbitrary Lat/Long point as a first guess...
         !  (Gotta use Ann Arbor, of course.  ha-ha)
         !  Also set an initial delta in each direction (with repect to lat/long)
         !
         LtA =  42.25
         LnA = -83.75
         DeltaLat = 1.0
         DeltaLon = 1.0

         !
         !  Compute the X,Y coordinates of our first guess
         !
         CALL GlerlPoly_LatLong2XY(LtA, LnA, RM, XOff, YOff, Rad, XA, YA); IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  How far off target is that guess?
         !  Note that I intentionally skip the square root for a tiny bit 
         !  of computational efficiency. I simply set Tolerance to the
         !  square of the desired actual tolerance.
         !
         XDiffA = XA - X
         YDiffA = YA - Y
         Diff = XDiffA**2 + YDiffA**2
         
         !
         !  If we got real lucky and hit the mark, assign final value and return
         !
         IF (Diff .LE. Tolerance) THEN
            Lat  = LtA
            Long = LnA
            RETURN
         END IF
            
         !
         !  Assign an initial (arbitrary) move amount.
         !  I will set it to 1 degree positive in both the lat and long.
         !
         DeltaLat = 1.0
         DeltaLon = 1.0
            
         !
         !  Iterate to try to find a solution.
         !  Basic idea is this...
         !    A is our starting (incorrect) guess. B is a trial guess.
         !    B will likely be wrong as well.
         !    Assume the difference from A to B in the X/Y values is proportional
         !       to the difference between them in Lat/Long.
         !
         Iterations = 1
         DO WHILE (Iterations .LE. MaxIterations)
            
            !
            !  Set a trial point B and compute the resulting X,Y
            !
            LtB = LtA + DeltaLat
            LnB = LnA + DeltaLon
            CALL GlerlPoly_LatLong2XY(LtB, LnB, RM, XOff, YOff, Rad, XB, YB); IF (ErrorLevel .NE. 0) GOTO 899
            
            !
            !  Compute a scaled directional factor to apply to the delta.
            !  
            SX = (X-XA) / (XB-XA)
            SY = (Y-YA) / (YB-YA)
            
            !
            !  Compute a revised delta
            !
            DeltaLat = DeltaLat * SY
            DeltaLon = DeltaLon * SX
            
            !
            !  Apply that revised delta to get a new point A.
            !
            LtA = LtA + DeltaLat
            LnA = LnA + DeltaLon
            CALL GlerlPoly_LatLong2XY(LtA, LnA, RM, XOff, YOff, Rad, XA, YA); IF (ErrorLevel .NE. 0) GOTO 899
            
            !
            !  Did we hit the mark with the new point A?
            !
            XDiffA = XA - X
            YDiffA = YA - Y
            Diff = XDiffA**2 + YDiffA**2
            IF (Diff .LE. Tolerance) THEN
               Lat  = LtA
               Long = LnA
               RETURN
            END IF

            !
            !  Getting here means we were still too far away, but presumably point
            !  A is a lot closer than our previous guess. And that means we 
            !  shouldn't need to move as far away for our next trial guess (B).
            !  So we want to reduce the delta values. How much??? That's a
            !  bit of a guess. Let's arbitrarily say 25% of the move we just made.
            !
            DeltaLat = DeltaLat * 0.25
            DeltaLon = DeltaLon * 0.25
            
            Iterations = Iterations + 1
         END DO 
         
         !
         !  If we get to here, it means we never got close enough to the target.
         !
         WRITE(ErrorMessage, 1001) X, Y; CALL PassMsg
         GOTO 898      

         
         898 ErrorLevel = 1
         899 ErrorMessage = '[traceback] GlerlPoly_XY2LatLong()...'; CALL PassMsg
             RETURN
            
         1001 FORMAT('Unable to converge on a (lat,long) solution for (X,Y) coordinates: (', I0, ',', I0, ')')
         
      END SUBROUTINE GlerlPoly_XY2LatLong


!============ The rest of these routines use the map to do something useful ===========================

!------------------------------------------------------------------------------------      
      !------------------------------------------------------------------------------
      !  Compute the distance (in meters) between two cells (center of each cell)
      !------------------------------------------------------------------------------
      FUNCTION DistanceCellXY(X1, Y1, X2, Y2, GMap)   Result(Dist)
      IMPLICIT NONE
      INTEGER,          INTENT(IN) :: X1, Y1, X2, Y2
      TYPE (TGlerlMap), INTENT(IN) :: GMap
      REAL :: Dist, CDist, CDSq

      !
      !  First, compute CDist, which is the "cell count" distance.
      !  Then convert that to meters for the final result.
      !
      CDSq = (X2-X1)*(X2-X1)  +  (Y2-Y1)*(Y2-Y1)
      CDist = SQRT(CDSq)
      Dist  = CDist * GMap%CellSize
      
      END FUNCTION DistanceCellXY

!------------------------------------------------------------------------------------      
      !------------------------------------------------------------------------------
      !  Compute the distance (in meters) between two (Lat,Long) points
      !------------------------------------------------------------------------------
      FUNCTION DistanceLatLong(Lat1, Lon1, Lat2, Lon2, GMap)   Result(Dist)
      IMPLICIT NONE
      REAL,             INTENT(IN) :: Lat1, Lon1, Lat2, Lon2
      TYPE (TGlerlMap), INTENT(IN) :: GMap
      REAL :: Dist, X1, Y1, X2, Y2, RM, XOff, YOff, Rad, CDist
      
      RM   = GMap%RefMeridian
      Rad  = GMap%Radius
      XOff = GMap%XOffset
      YOff = GMap%YOffset
      
      !
      !  Convert Lat,Long to X,Y. 
      !
      CALL GlerlPoly_LatLong2XY(Lat1, Lon1, RM, XOff, YOff, Rad, X1, Y1)
      CALL GlerlPoly_LatLong2XY(Lat2, Lon2, RM, XOff, YOff, Rad, X2, Y2)
      CDist = SQRT((X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1))
      Dist  = CDist * GMap%CellSize
      
      END FUNCTION DistanceLatLong

!------------------------------------------------------------------------------------      
      !------------------------------------------------------------------------------
      !  Compute the distance to the nearest cell of the specified subbasin.
      !  If subbasin = 0..NumSubbasins, compute to nearest cell of that subbasin.
      !  If Subbasin = SubNum_Land,  then compute distance to the nearest LAND cell
      !  If Subbasin = SubNum_Basin, then compute distance to the nearest BASIN cell (lake or land)
      !
      !------------------------------------------------------------------------------
      FUNCTION DistanceFromNearestSubbasin(Lat, Long, GMap, Subbasin) Result(Dist)
      IMPLICIT NONE
      REAL,             INTENT(IN) :: Lat, Long
      TYPE (TGlerlMap), INTENT(IN) :: GMap
      INTEGER,          INTENT(IN) :: Subbasin        ! 0..NumSubbasins; 98=any land; 99=basin
      REAL                         :: Dist

      INTEGER :: Col, Row, Sub
      LOGICAL :: Done
      REAL    :: RM, Rad, XOff, YOff, X, Y
      REAL    :: DistSq, MinDistSq, CX, RY

      !
      !  Set Dist to a default very large value, greater than the 
      !  circumference of the Earth, eliminating the compiler 
      !  warning for possible uninitialized value.
      !
      Dist = 99999.9 
      
      !
      !  Compute the X,Y coordinates of the specified Lat,Long
      !  Then use the X,Y values to get the corresponding cell coordinates.
      !
      RM   = GMap%RefMeridian
      Rad  = GMap%Radius
      XOff = GMap%XOffset
      YOff = GMap%YOffset
      CALL GlerlPoly_LatLong2XY(Lat, Long, RM, XOff, YOff, Rad, X, Y); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Let's do a real quick check that might save a lot of extra processing...
      !  If the specified lat,long falls inside the subbasin of interest, then
      !  the distance is 0 and we don't need to do any more testing.
      !
      !  Hey Tim.... Remember that the array is indexed (0..width-1, 0..height-1)
      !  (I get so tired of forgetting that part, and coding for 0..n like the
      !   old routines.)  
      !
      Col = INT(X)   ! truncate 
      Row = INT(Y)   ! truncate 
      Dist = 9.9e9
      IF (((Col .GE. 0) .AND. (Col .LE. GMap%MapWidth-1)) .AND.       &
          ((Row .GE. 0) .AND. (Row .LE. GMap%MapHeight-1))) THEN
         Sub = ICHAR(GMap%Map2D(Col,Row))     ! subbasin code (INTEGER) for this cell
         IF (Sub .EQ. Subbasin) Dist = 0.0    ! simple case where a single subbasin was specified
         IF (Sub .EQ. SubNum_Overland) THEN 
            IF ((Sub .GE. 1) .AND. (Sub .LE. GMap%Subbasins)) Dist = 0.0
         END IF
         IF (Sub .EQ. SubNum_Overbasin) THEN 
            IF ((Sub .GE. 0) .AND. (Sub .LE. GMap%Subbasins)) Dist = 0.0
         END IF
         IF (Dist .LT. 1.0)  RETURN
      END IF
      
      !
      !  Initialize the minimum distance to a ridiculously large value
      !  This is actually the SQUARE of the distance, since we will always
      !  skip doing the square root part of the distance calculations.
      !  No point in doing unnecessary SQRT() calcs.
      !
      MinDistSq = 9.9e29
      
      !
      !  Main loop. 
      !  Any time we find a distance of 0, we know we can call it quits. In order
      !  to take advantage of that, I will structure the loop to be a WHILE loop
      !  rather than just a simple double nest.  I am assuming that this will
      !  result in better efficiency by eliminating a bunch of distance calcs.
      !  The trade-off, of course, is the added IF tests in each loop step.
      ! 
      !  Note that the test is for (MinDistSq > 0.7). That is to deal with
      !  any rounding issues. 
      !
      Done = .FALSE.
      CX = 0.5        ! column, or X   (center of cell 0,0)
      RY = 0.5        ! row,    or Y   (center of cell 0,0)
      DO WHILE ((.NOT. Done) .AND. (MinDistSq .GT. 0.7))
         Col = INT(CX)   ! truncate 
         Row = INT(RY)   ! truncate 
         Sub = ICHAR(GMap%Map2D(Col,Row))     ! subbasin code for the cell of interest
         
         IF (Sub .EQ. Subbasin) THEN
            DistSq = (CX-X)*(CX-X) + (RY-Y)*(RY-Y)
            MinDistSq = MIN(MinDistSq, DistSq)
         END IF
         IF (Subbasin .EQ. SubNum_Overland) THEN
            IF ((Sub .GE. 1) .AND. (Sub .LE. GMap%Subbasins)) THEN
               DistSq = (CX-X)*(CX-X) + (RY-Y)*(RY-Y)
               MinDistSq = MIN(MinDistSq, DistSq)
            END IF
         END IF
         IF (Subbasin .EQ. SubNum_Overbasin) THEN
            IF ((Sub .GE. 0) .AND. (Sub .LE. GMap%Subbasins)) THEN
               DistSq = (CX-X)*(CX-X) + (RY-Y)*(RY-Y)
               MinDistSq = MIN(MinDistSq, DistSq)
            END IF
         END IF
         
         CX = CX + 1
         IF (CX .GE. GMap%MapWidth) THEN                   ! indexing is 0..mapwidth-1
            CX = 0
            RY = RY + 1
            IF (RY .GE. GMap%MapHeight) Done = .TRUE.      ! indexing is 0..mapheight-1
         END IF
      END DO      
      
      Dist = SQRT(MinDistSq)    ! for the final value, do the SQRT()
      RETURN

      !
      !  Error Handlers
      !
  899 ErrorMessage = '[traceback] DistanceFromNearestSubbasin()...'; CALL PassMsg
      RETURN
      END FUNCTION DistanceFromNearestSubbasin
      
      
      
      
      
END MODULE GlerlMaps
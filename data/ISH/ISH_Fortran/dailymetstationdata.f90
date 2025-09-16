!-----------------------------------------------------------------------
!  This module provides a structured storage container for a set of daily 
!  data such as would be read from a single station.  It does NOT provide
!  any read/write procedures, etc. 
!
!  Data Structure:
!  The data for read/write will be stored in the TMetCsvFile type.
!  This includes all of the relevant info including the file name, 
!  station information, and the data. Upon reading the data, I will be
!  converting it to my preferred units (the same as used for output) and
!  the same pre-defined order. When the write procedure is called, there 
!  is no guarantee that the data will be in this structure, since the user
!  may have populated the TMetCsvFile with any code. Thus, the output 
!  procedure will first do necessary conversions and re-ordering.
!
!  Fields:
!    DataValid  = a logical(boolean) variable that indicates if we think the
!                 data in the 2-d data array is valid.
!    HeaderInfo = several sub-fields
!                 ID           = station ID (max 25 characters)
!                 Name         = station name (max 50 characters)
!                 Country      = 2-character country code (US, CA)
!                 Lat          = latitude of the station, degrees North of equator
!                 Long         = longitude of the station, degrees East of Prime Meridian
!                                (note that this means G.L. region stations are 
!                                 typically -75 to -95 degrees)
!                 SDate        = sequence number for the first day's data
!                 EDate        = sequence number for the last day's data
!                 NumDataTypes = number of data types stored. Typically 7.
!                 DataTypes    = array (typically size 7) of integer values. Each entry
!                                is a defined constant for a data type (e.g. DT_AirtempMax)
!                                and they correspond with the entries in the data array
!                 DataUnits    = array (typically size 7) of integer values. Each entry
!                                is a defined constant for a data unit (e.g. DU_Celsius)
!                                and they correspond with the entries in the data array
!    MetData    = 2-d array of REAL data values. Missing data is indicated by an entry of
!                 MissingData_Real. The indexing of the array is (1:NumDataTypes, 1:NumDays).
!
!-----------------------------------------------------------------------

MODULE DailyMetStationDataset
   USE MetDataTypesAndUnits
   USE ErrorProcess
   USE Glshfs_Util


   TYPE TDlyMetStnDataset
      LOGICAL            :: DataValid
      CHARACTER(LEN=25)  :: ID               ! Station ID number, left-justified
      CHARACTER(LEN=50)  :: Name             ! Station name
      CHARACTER(LEN=2)   :: Country          ! US, CA, etc
      REAL               :: Lat, Long, Elevation
      INTEGER            :: SDate, EDate
      INTEGER            :: NumDataTypes
      INTEGER, DIMENSION(:),   ALLOCATABLE :: DataTypes
      INTEGER, DIMENSION(:),   ALLOCATABLE :: DataUnits
      REAL,    DIMENSION(:,:), ALLOCATABLE :: MetData        !  (DType, DayNum)
   END TYPE TDlyMetStnDataset
   
CONTAINS

   !--------------------------------------------------------------
   SUBROUTINE InitializeDlyMetStnDataset(DMSD)
   IMPLICIT NONE
   TYPE (TDlyMetStnDataset), INTENT(INOUT) :: DMSD
   INTEGER :: IOS
   
   CALL ClearDlyMetStnDatasetHeader(DMSD)
   IF (ALLOCATED(DMSD%MetData)) DEALLOCATE(DMSD%MetData, STAT=IOS)
   
   END SUBROUTINE InitializeDlyMetStnDataset

   !--------------------------------------------------------------
   SUBROUTINE ClearDlyMetStnDatasetHeader(DMSD)
   IMPLICIT NONE
   TYPE (TDlyMetStnDataset), INTENT(INOUT) :: DMSD
   INTEGER :: IOS

   DMSD%DataValid    = .FALSE.
   DMSD%ID           = ''
   DMSD%Name         = ''
   DMSD%Country      = ''
   DMSD%Lat          = -999.99
   DMSD%Long         = -999.99
   DMSD%Elevation    = -999.99
   DMSD%SDate        = MissingData_Date
   DMSD%EDate        = MissingData_Date
   DMSD%NumDataTypes = 0
   IF (ALLOCATED(DMSD%DataTypes)) DEALLOCATE(DMSD%DataTypes, STAT=IOS)
   IF (ALLOCATED(DMSD%DataUnits)) DEALLOCATE(DMSD%DataUnits, STAT=IOS)
   
   END SUBROUTINE ClearDlyMetStnDatasetHeader
   
   !--------------------------------------------------------------
   SUBROUTINE CopyDlyMetStnDatasetHeader(Src, Dest)
   IMPLICIT NONE
   TYPE (TDlyMetStnDataset), INTENT(IN)    :: Src
   TYPE (TDlyMetStnDataset), INTENT(INOUT) :: Dest

   INTEGER :: I, NumDT, IOS1, IOS2, IOS
   
   IF (ALLOCATED(Dest%DataTypes)) DEALLOCATE(Dest%DataTypes, STAT=IOS)
   IF (ALLOCATED(Dest%DataUnits)) DEALLOCATE(Dest%DataUnits, STAT=IOS)

   Dest%DataValid    = Src%DataValid
   Dest%ID           = Src%ID
   Dest%Name         = Src%Name
   Dest%Country      = Src%Country
   Dest%Lat          = Src%Lat
   Dest%Long         = Src%Long
   Dest%Elevation    = Src%Elevation
   Dest%SDate        = Src%SDate
   Dest%EDate        = Src%EDate
   Dest%NumDataTypes = Src%NumDataTypes

   NumDT = Src%NumDataTypes
   ALLOCATE(Dest%DataTypes(NumDT), STAT=IOS1)
   ALLOCATE(Dest%DataUnits(NumDT), STAT=IOS2)
   IOS = IOR(IOS1,IOS2)
   IF (IOS .NE. 0) THEN
      CALL ClearDlyMetStnDatasetHeader(Dest)
      RETURN
   END IF
   
   DO I = 1, NumDT
      Dest%DataTypes(I) = Src%DataTypes(I)
      Dest%DataUnits(I) = Src%DataUnits(I)
   END DO

   END SUBROUTINE CopyDlyMetStnDatasetHeader

END MODULE DailyMetStationDataset
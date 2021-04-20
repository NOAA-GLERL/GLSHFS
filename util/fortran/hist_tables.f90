!-------------------------------------------------------------------------
!   Author:   Tim Hunter
!   Date:     May, 2017
!   Purpose:  Read the data prior to a specified date and output a file of
!             values (at the specified time-step) for analysis or creating plots.
!
!   Input Files:  lbrm_outputs_xxxnn.csv
!                 lltm_outputs_xxx.csv
!                 (xxx = SUP, MIC, ...;   nn = 01, 02, 03, ...)
!
!   Output File:  hist_xxxx.csv      (xxxx = year, mnth, qmon, wkly, daly)
!
!      Format of output file is shown below:
!-------------------------------------------------------------------------
!

MODULE HistTable_Subs
      USE GLSHFS_Global
      USE GLSHFS_Util
      USE GlerlDataTypesAndUnits

      INTEGER, PARAMETER :: MaxYearsDim = 100
      INTEGER, PARAMETER :: MaxMonsDim  = MaxYearsDim * 12
      INTEGER, PARAMETER :: MaxDaysDim  = MaxMonsDim * 31
      INTEGER, PARAMETER :: MaxWeeksDim = MaxDaysDim / 7
      INTEGER, PARAMETER :: MaxQMonsDim = MaxMonsDim * 4
      INTEGER, PARAMETER :: NumDataTypes = 30

      LOGICAL, PARAMETER :: PrintTraceMessages = .FALSE.     ! TRUE for debugging
      LOGICAL, PARAMETER :: DoDebug            = .FALSE.     ! TRUE for debugging

      INTEGER  :: Subbasins             ! # of subbasins for this lake
      INTEGER  :: LakeNumber
      LOGICAL  :: DoCMS
      CHARACTER (LEN=3)  :: Bsn

      REAL, DIMENSION(MaxMonsDim, DT) :: MonData
      REAL, DIMENSION(-1:30) :: Area     ! area of subbasins for this lake.
                                         ! Declared too big so it can be static.
                                         ! index '-1' used for the overland area

      !
      !  Data types
      !
      INTEGER, PARAMETER, DIMENSION(NumDataTypes) :: DataTypes =            &
         (/ GDT_Runoff,         GDT_Precipitation,  GDT_Evaporation,        &
            GDT_NetBasinSupply, GDT_AirtempMean,    GDT_WaterTemp,          &             ! this air temp is overLAKE
            GDT_AirtempMean,    GDT_DewpointMean,   GDT_Windspeed,          &             ! this air temp is overLAND
            GDT_CloudCover,     GDT_IncidentRad,    GDT_ReflectedRad,       &
            GDT_NetLongWaveRad, GDT_LatentRad,      GDT_SensibleRad,        &
            GDT_Advection,      GDT_TotalHeat,      GDT_IceTemp,            &
            GDT_IceArea,        GDT_UpperSoil,      GDT_LowerSoil,          &
            GDT_GroundWater,    GDT_SurfaceZone,    GDT_SnowWater,          &
            GDT_TotalMoisture,  GDT_MeanLakeLevel,  GDT_EndingLakeLevel,    &
            GDT_Inflow,         GDT_Outflow,        GDT_TotalBasinSupply /)
                                         
                                         
      !
      !  What type of value is output for each data type?  
      !  Total         = 1
      !  Average       = 2
      !  End-of-period = 3
      !
      INTEGER, PARAMETER, DIMENSION(NumDataTypes) :: OutputType =     &
          (/ 1, 1, 1,   1, 2, 2,   2, 2, 2,   2, 2, 2,   2, 2, 2,     &
             2, 2, 2,   2, 3, 3,   3, 3, 3,   3, 2, 3,   2, 2, 2/)

      CHARACTER (LEN=60), DIMENSION(NumDataTypes) :: Hdr
      CHARACTER (LEN=20) :: TotHdr, AveHdr, EndHdr


CONTAINS

!----------------------------------------------------------------------------
      !----------------------------------------------------------------------
      !  Find the earliest date that is common to all of the input files.
      !     BsnDir = The lake directory. Must have the trailing file path 
      !              separator character.
      !              e.g.   /mydrive/glshfs/sup/      (linux)
      !                     c:\glshfs\sup\            (windows)
      !     LkCode = 3-character basin name (sup, mic, hur, etc)
      !----------------------------------------------------------------------
      FUNCTION EarliestCommonDate(BsnDir, LkCode)     RESULT(SeqNum)
      IMPLICIT   NONE
      CHARACTER(LEN=*) :: BsnDir, LkCode
      INTEGER :: SeqNum

      INTEGER :: U1, SSeq, ESeq
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER (LEN=200) :: FileName

      TYPE (THeaderInfoType), INTENT(INOUT) :: HdrInfo

      !
      !  
      Bsn = GetLowerCase(LkCode)
      HdrInfo = THeaderInfoType()
      
      !
      FileName = TRIM(BsnDir) // 'lbrm_output_' // Bsn // '00.csv'
      CALL ReadJustHeaderInfo(TRIM(FileName), HFT_LBRM, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      
      
      
      
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', RECL=6,            &
     &     ACCESS='DIRECT', FORM='UNFORMATTED', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, REC=1, ERR=812) I2, Clm00_SSeq
      READ(U1, REC=2, ERR=812) Clm00_ESeq
      CLOSE(U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      FileName = TRIM(ThisLakeDir) // Bsn // 'MD.CLM'
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', RECL=8,            &
     &     ACCESS='DIRECT', FORM='UNFORMATTED', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, REC=1, ERR=812) I2, ClmMD_SSeq
      READ(U1, REC=2, ERR=812) ClmMD_ESeq
      CLOSE(U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      FileName = TRIM(ThisLakeDir) // Bsn // 'ALL.SUM'
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', RECL=12,           &
     &     ACCESS='DIRECT', FORM='UNFORMATTED', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, REC=1, ERR=812) I2, Sum_SSeq
      READ(U1, REC=2, ERR=812) Sum_ESeq
      CLOSE(U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      FileName = TRIM(ThisLakeDir) // 'ZSEVAP.' // Bsn
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', RECL=64,           &
     &     ACCESS='DIRECT', FORM='UNFORMATTED', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, REC=1, ERR=812) ZS_SSeq, ZS_ESeq
      CLOSE(U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      SSeq = Clm00_SSeq
      IF (ClmMD_SSeq .GT. SSeq) SSeq = ClmMD_SSeq
      IF (Sum_SSeq   .GT. SSeq) SSeq = Sum_SSeq
      IF (ZS_SSeq    .GT. SSeq) SSeq = ZS_SSeq

      ESeq = Clm00_ESeq
      IF (ClmMD_ESeq .LT. ESeq) ESeq = ClmMD_ESeq
      IF (Sum_ESeq   .LT. ESeq) ESeq = Sum_ESeq
      IF (ZS_ESeq    .LT. ESeq) ESeq = ZS_ESeq

      IF (ESeq .LT. SSeq) THEN
         ErrorMessage = 'Files for historical data do not contain a common period of record.'
         CALL PassMsg
         ErrorLevel = 1
         RETURN
      END IF

      SeqNum = SSeq
      RETURN

!
!  Handlers for I/O errors
!
  811 ErrorMessage = 'PHISTALL: Error opening file '//TRIM(FileName)
      CALL PassMsg; GOTO 898
  812 ErrorMessage = 'PHISTALL: Error reading file '//TRIM(FileName)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : EarliestCommonDate...'
      CALL PassMsg
      RETURN
      END SUBROUTINE EarliestCommonDate

!----------------------------------------------------------------------
!     DI   = Data Type index
!     BSeq = "Base" sequence -- corresponds to start of data in DlyData array
!     SSeq = "Start" sequence -- start day for computing values
!     TVal = total for the period
!     MVal = mean value for the period
!     LVal = last value for the period (the value on last day of period)
!----------------------------------------------------------------------
      SUBROUTINE ComputeWeeklyValues(DI, BSeq, SSeq, DlyData, TVal, MVal, LVal)
      IMPLICIT   NONE
      INTEGER, INTENT(IN)  :: DI, BSeq, SSeq
      REAL,    INTENT(IN)  :: DlyData(:,:)
      REAL,    INTENT(OUT) :: TVal, MVal, LVal

      INTEGER :: I, Seq1, Seq2

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'Computing Weekly Values...'; CALL PassMsg
      END IF

      Seq1 = SSeq
      Seq2 = SSeq + 6

      CALL DoTheComputation(DI,BSeq,Seq1,Seq2,DlyData,TVal,MVal,LVal)
      IF (ErrorLevel .NE. 0) GOTO 899
      RETURN

  899 ErrorMessage = '[traceback] : ComputeWeeklyValues...'
      CALL PassMsg
      RETURN

      END SUBROUTINE ComputeWeeklyValues

!----------------------------------------------------------------------
!     DI   = Data Type index
!     BSeq = "Base" sequence -- corresponds to start of data in DlyData array
!     SSeq = "Start" sequence -- start day for computing values
!     TVal = total for the period
!     MVal = mean value for the period
!     LVal = last value for the period (the value on last day of period)
!----------------------------------------------------------------------
      SUBROUTINE ComputeQMonValues(DI, BSeq, SSeq, DlyData, TVal, MVal, LVal)
      IMPLICIT   NONE
      INTEGER, INTENT(IN)    :: DI, BSeq, SSeq
      REAL,    INTENT(IN)    :: DlyData(:,:)
      REAL,    INTENT(INOUT) :: TVal, MVal, LVal

      INTEGER :: Day, Mon, Year, Q, I, J, Seq1, Seq2

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'Computing Qtr-Month Values...'; CALL PassMsg
      END IF

!
!  Set the start/end for computations to the start/end of the qtr-month.
!  Note that SSeq should already be the 1st of the qtr-month, but this makes
!  it certain that Seq1 is set correctly.
!
      Seq1 = SSeq
      J = FirstDayOfQtrMon(Seq1)
      CALL SequenceDate(Day, Mon, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL DateSequence(  J, Mon, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
      Seq2 = SSeq
      J = LastDayOfQtrMon(Seq2)
      CALL SequenceDate(Day, Mon, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL DateSequence(  J, Mon, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899

      CALL DoTheComputation(DI,BSeq,Seq1,Seq2,DlyData,TVal,MVal,LVal)
      IF (ErrorLevel .NE. 0) GOTO 899
      RETURN

  899 ErrorMessage = '[traceback] : ComputeQMonValues...'
      CALL PassMsg
      RETURN

      END SUBROUTINE ComputeQMonValues

!----------------------------------------------------------------------
!     DI   = Data Type index
!     BSeq = "Base" sequence -- corresponds to start of data in DlyData array
!     SSeq = "Start" sequence -- start day for computing values
!     TVal = total for the period
!     MVal = mean value for the period
!     LVal = last value for the period (the value on last day of period)
!----------------------------------------------------------------------
      SUBROUTINE ComputeMonthValues(DI, BSeq, SSeq, DlyData, TVal, MVal, LVal)
      IMPLICIT   NONE
      INTEGER, INTENT(IN)  :: DI, BSeq, SSeq
      REAL,    INTENT(IN)  :: DlyData(:,:)
      REAL,    INTENT(OUT) :: TVal, MVal, LVal

      INTEGER :: Seq1, Seq2

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'Computing Monthly Values...'; CALL PassMsg
      END IF

!
!  Set the start/end for computations to the start/end of the month.
!  Note that SSeq should already be the 1st of the month, but this makes
!  it certain that Seq1 is set to the 1st.
!
      Seq1 = SSeq
      Seq2 = SSeq
      CALL AdjustToStartOfMonth(Seq1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL AdjustToEndOfMonth(Seq1);   IF (ErrorLevel .NE. 0) GOTO 899

      CALL DoTheComputation(DI,BSeq,Seq1,Seq2,DlyData,TVal,MVal,LVal)
      IF (ErrorLevel .NE. 0) GOTO 899
      RETURN

  899 ErrorMessage = '[traceback] : ComputeMonthValues...'
      CALL PassMsg
      RETURN

      END SUBROUTINE ComputeMonthValues
          
!----------------------------------------------------------------------
!     DI   = Data Type index
!     BSeq = "Base" sequence -- corresponds to start of data in DlyData array
!     SSeq = "Start" sequence -- start day for computing values
!     TVal = total for the period
!     MVal = mean value for the period
!     LVal = last value for the period (the value on last day of period)
!----------------------------------------------------------------------
      SUBROUTINE ComputeAnnualValues(DI, BSeq, SSeq, DlyData, TVal, MVal, LVal)
      IMPLICIT   NONE
      INTEGER, INTENT(IN)  :: DI, BSeq, SSeq
      REAL,    INTENT(IN)  :: DlyData(:,:)
      REAL,    INTENT(OUT) :: TVal, MVal, LVal

      INTEGER :: Day, Mon, Year, Seq1, Seq2

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'Computing Annual Values...'; CALL PassMsg
      END IF

!
!  Set the start/end for computations to the start/end of the year.
!  Note that SSeq should already be the 1st of the year, but this makes
!  it certain that Seq1 is set to January 1st.
!
      Seq1 = SSeq
      CALL SequenceDate(Day, Mon, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL DateSequence(  1,   1, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899

      Seq2 = SSeq
      CALL SequenceDate(Day, Mon, Year, Seq2); IF (ErrorLevel .NE. 0) GOTO 899
      CALL DateSequence( 31,  12, Year, Seq2); IF (ErrorLevel .NE. 0) GOTO 899

      CALL DoTheComputation(DI,BSeq,Seq1,Seq2,DlyData,TVal,MVal,LVal)
      IF (ErrorLevel .NE. 0) GOTO 899
      RETURN

  899 ErrorMessage = '[traceback] : ComputeAnnualValues...'
      CALL PassMsg
      RETURN

      END SUBROUTINE ComputeAnnualValues

!----------------------------------------------------------------------
!  DI      = data index (which type of data?)
!  BSeq    = seq number for start of the data in the array
!  Seq1    = seq number for start of the data of interest
!  Seq2    = seq number for end of the data of interest
!  DlyData = array of data values
!  TVal    = total of the valid values in the specified range (Seq1 to Seq2)
!  MVal    = mean of the valid values in the specified range (Seq1 to Seq2)
!  LVal    = last value in the specified range (value on Seq2 date)
!
      SUBROUTINE DoTheComputation(DI, BSeq, Seq1, Seq2, DlyData, TVal, MVal, LVal)
      IMPLICIT   NONE
      INTEGER, INTENT(IN)  :: DI, BSeq, Seq1, Seq2
      REAL,    INTENT(IN)  :: DlyData(:,:)
      REAL,    INTENT(OUT) :: TVal, MVal, LVal

      INTEGER :: I, Kount, Seq
      REAL    :: X

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'trace: DoTheComputation...'; CALL PassMsg
      END IF

      LVal  = MissingData
      MVal  = MissingData
      TVal  = 0.0
      Kount = 0
      DO Seq = Seq1, Seq2
         I = Seq - BSeq + 1
         X = DlyData(I,DI)
         IF (X .GT. MissingDataTest) THEN
            TVal = TVal + X
            Kount = Kount + 1
         END IF
      END DO
      LVal = DlyData(Seq2-BSeq+1, DI)
      IF (Kount .GT. 0) THEN
         MVal = TVal / Kount
      ELSE
         MVal = MissingData
         TVal = MissingData
         LVal = MissingData
      END IF

      RETURN

      END SUBROUTINE DoTheComputation

!----------------------------------------------------------------------
      SUBROUTINE WriteHistOut(TStep, FcStart, NumVals, SSeq, ESeq, OutData)
      IMPLICIT  NONE
      INTEGER,              INTENT(IN) :: TStep, FcStart, NumVals, SSeq, ESeq
      REAL, DIMENSION(:,:), INTENT(IN) :: OutData      ! dimensioned (NumVals, 24)

      INTEGER :: I, M, U1, FcDay, FcMon, FcYear
      REAL    :: MissVal
      REAL, DIMENSION(NumVals) :: ODat
      CHARACTER (LEN=99) :: FileName

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'trace: WriteHistOut...'; CALL PassMsg
      END IF

!
!  Assign header lines to a single array
!
      IF (TStep .EQ. 1) THEN
         TotHdr = 'Daily'
         AveHdr = 'Daily'
         EndHdr = 'Daily'
      ELSE
         TotHdr = 'Total'
         AveHdr = 'Average'
         IF (TStep .EQ. 2) EndHdr = 'End-of-Week'
         IF (TStep .EQ. 3) EndHdr = 'End-of-QtrMonth'
         IF (TStep .EQ. 4) EndHdr = 'End-of-Month'
         IF (TStep .EQ. 5) EndHdr = 'End-of-Year'
      END IF

      DO I = 1, DT
        SELECT CASE (I)
          CASE (1:4)
               Hdr(I) = TRIM(TotHdr) // ' ' // TRIM(HeaderLines(I))
               IF (DoCMS)  Hdr(I) = TRIM(AveHdr) // ' ' // TRIM(CMSHdrLines(I))
          CASE (5:18, 26:29)
               Hdr(I) = TRIM(AveHdr) // ' ' // TRIM(HeaderLines(I))
          CASE (19:25)
               Hdr(I) = TRIM(EndHdr) // ' ' // TRIM(HeaderLines(I))
        END SELECT
      END DO

!
!  Open the output file
!
      IF (TStep .EQ. 1) FileName = TRIM(ThisLakeDir) // 'HISTDALY.PRN'
      IF (TStep .EQ. 2) FileName = TRIM(ThisLakeDir) // 'HISTWKLY.PRN'
      IF (TStep .EQ. 3) FileName = TRIM(ThisLakeDir) // 'HISTQMON.PRN'
      IF (TStep .EQ. 4) FileName = TRIM(ThisLakeDir) // 'HISTMNTH.PRN'
      IF (TStep .EQ. 5) FileName = TRIM(ThisLakeDir) // 'HISTYEAR.PRN'

      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

!
!  Write the header info.
!
      CALL SequenceDate(FcDay, FcMon, FcYear, FcStart)
      SELECT CASE (TStep)
          CASE (1)
             WRITE(U1, 2101, ERR=813) TRIM(FullLakeName(LakeNumber))
             WRITE(U1, 2102, ERR=813) NumVals, FcDay, MonthName3(FcMon), FcYear
             WRITE(U1,    *, ERR=813) ' '
             WRITE(U1, 2104, ERR=813) (I, I = 1, 31)
          CASE (2)
             WRITE(U1, 2201, ERR=813) TRIM(FullLakeName(LakeNumber))
             WRITE(U1, 2202, ERR=813) NumVals, FcDay, MonthName3(FcMon), FcYear
             WRITE(U1,    *, ERR=813) ' '
             WRITE(U1, 2204, ERR=813) 
          CASE (3)
             WRITE(U1, 2301, ERR=813) TRIM(FullLakeName(LakeNumber))
             WRITE(U1, 2302, ERR=813) NumVals, FcDay, MonthName3(FcMon), FcYear
             WRITE(U1,    *, ERR=813) ' '
             WRITE(U1, 2304, ERR=813) (MonthName3(M), M=1,12)
          CASE (4)
             WRITE(U1, 2401, ERR=813) TRIM(FullLakeName(LakeNumber))
             WRITE(U1, 2402, ERR=813) NumVals, FcDay, MonthName3(FcMon), FcYear
             WRITE(U1,    *, ERR=813) ' '
             WRITE(U1, 2404, ERR=813) (MonthName3(M), M=1,12)
          CASE (5)
             WRITE(U1, 2501, ERR=813) TRIM(FullLakeName(LakeNumber))
             WRITE(U1, 2502, ERR=813) NumVals, FcDay, MonthName3(FcMon), FcYear
             WRITE(U1,    *, ERR=813) ' '
             WRITE(U1, 2504, ERR=813) 
      END SELECT

!
!  Write the data lines  
!
      DO I = 1, DT - LevelDT               ! for now, do not output level stuff
         WRITE(U1, 3001, ERR=813) Hdr(I)
         ODat(:) = OutData(:,I)
         SELECT CASE (TStep)
             CASE (1)
                CALL WriteOneSetDaily(U1, SSeq, ESeq, ODat, FileName)
                IF (ErrorLevel .NE. 0) GOTO 899
             CASE (2)
                CALL WriteOneSetWeekly(U1, SSeq, ESeq, ODat, FileName)
                IF (ErrorLevel .NE. 0) GOTO 899
             CASE (3)
                CALL WriteOneSetQMon(U1, SSeq, ESeq, ODat, FileName)
                IF (ErrorLevel .NE. 0) GOTO 899
             CASE (4)
                MissVal = -999.8
                IF (DoCMS .AND. (I .LE. 4)) MissVal = MissingDataTest
                CALL WriteOneSetMon(U1, SSeq, ESeq, ODat, MissVal, FileName)
                IF (ErrorLevel .NE. 0) GOTO 899
             CASE (5)
                CALL WriteOneSetAnn(U1, SSeq, ESeq, ODat, FileName)
                IF (ErrorLevel .NE. 0) GOTO 899
         END SELECT
      END DO

      RETURN

!
!  Handlers for I/O errors
!
  811 ErrorMessage = 'PHISTALL: Error opening file '//TRIM(FileName)
      CALL PassMsg; GOTO 898
  813 ErrorMessage = 'PHISTALL: Error writing file '//TRIM(FileName)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteHistOut...'
      CALL PassMsg
      RETURN

!
!  FORMATs
!  
 2101 FORMAT(1X, 'Historical and modeled daily values for ', A, ' for the')
 2102 FORMAT(1X, I0, ' days prior to the forecast outlooks starting ', I2.2, A3, I4)
 2104 FORMAT(1X, 'Year Mn', 31(6X, I2))

 2201 FORMAT(1X, 'Historical and modeled weekly values for ', A, ' for the')
 2202 FORMAT(1X, I0, ' weeks prior to the forecast outlooks starting ', I2.2, A3, I4)
 2204 FORMAT(1X, 'DD/MM/YYYY-DD/MM/YYYY   Value')

 2301 FORMAT(1X, 'Historical and modeled quarter-monthly values for ', A, ' for the')
 2302 FORMAT(1X, I0, ' qtr-months prior to the forecast outlooks starting ', I2.2, A3, I4)
 2304 FORMAT(1X, 'Year-Q', 12(5X, A3))

 2401 FORMAT(1X, 'Historical and modeled monthly values for ', A, ' for the')
 2402 FORMAT(1X, I0, ' months prior to the forecast outlooks starting ', I2.2, A3, I4)
 2404 FORMAT(1X, 'Year', 12(5X, A3))

 2501 FORMAT(1X, 'Historical and modeled annual values for ', A, ' for the')
 2502 FORMAT(1X, I0, ' years prior to the forecast outlooks starting ', I2.2, A3, I4)
 2504 FORMAT(1X, 'Year   Value')

 3001 FORMAT(A)

      END SUBROUTINE WriteHistOut

!----------------------------------------------------------------------
!  Data is output with one month's data on each line (28-31 values)
!
!  SSeq, ESeq = start/end dates of the data to be output
!  S1, S2     = start/end dates of a single line of output (i.e. first/last of month)
!  Seq1, Seq2 = start/end dates of actual data in a single line of output 
!
      SUBROUTINE WriteOneSetDaily(U1, SSeq, ESeq, ODat, FileName)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: U1, SSeq, ESeq
      REAL,    INTENT(IN) :: ODat(:)
      CHARACTER (LEN=*), INTENT(IN) :: FileName

      INTEGER :: I, J, D, M, Y, Seq1, Seq2, S1, S2, Seq
      REAL    :: Vals(31)
      CHARACTER (LEN=8) :: CVals(31)

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'trace: WriteOneSetDaily...'; CALL PassMsg
      END IF

      Seq1 = SSeq
      DO WHILE (Seq1 .LE. ESeq)
         Seq2 = Seq1
         CALL AdjustToEndOfMonth(Seq2); IF (ErrorLevel .NE. 0) GOTO 899
         IF (Seq2 .GT. ESeq) Seq2 = ESeq
         S1 = Seq1
         CALL AdjustToStartOfMonth(S1); IF (ErrorLevel .NE. 0) GOTO 899
         S2 = Seq2
         CALL AdjustToEndOfMonth(S2); IF (ErrorLevel .NE. 0) GOTO 899
         
         DO Seq = S1, Seq1-1
            J = Seq - S1 + 1      ! index (1..31) in Vals array
            Vals(J) = MissingData
         END DO
         DO Seq = Seq2+1, S2
            J = Seq - S1 + 1      ! index (1..31) in Vals array
            Vals(J) = MissingData
         END DO
         DO Seq = Seq1, Seq2
            I = Seq - SSeq + 1    ! index into ODat array
            J = Seq - S1 + 1      ! index (1..31) in Vals array
            Vals(J) = ODat(I)
         END DO
         CVals(:) = '        '
         DO Seq = S1, S2
            J = Seq - S1 + 1      ! index (1..31) in Vals array
            WRITE(CVals(J), '(F8.2)') Vals(J)
            IF (Vals(J) .LT. MissingDataTest) CVals(J) = '       -'
         END DO
         CALL SequenceDate(D, M, Y, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(U1, 3001, ERR=813) Y, M, (CVals(J), J=1,S2-S1+1)
         Seq1 = Seq2 + 1
      END DO
      RETURN

  813 ErrorMessage = 'PHISTALL: Error writing file '//TRIM(FileName)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteOneSetDaily...'
      CALL PassMsg
      RETURN

 3001 FORMAT(1X, I4, I3, 31A8)

      END SUBROUTINE WriteOneSetDaily

!----------------------------------------------------------------------
!  Data is output with one week's data on each line.
!
!  SSeq, ESeq = start/end dates of the data to be output
!  S1, S2     = start/end dates of a single line of output (i.e. first/last of week)
!  Seq1, Seq2 = start/end dates of actual data in a single line of output 
!
      SUBROUTINE WriteOneSetWeekly(U1, SSeq, ESeq, ODat, FileName)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: U1, SSeq, ESeq
      REAL,    INTENT(IN) :: ODat(:)
      CHARACTER (LEN=*), INTENT(IN) :: FileName

      INTEGER :: I, J, D1, M1, Y1, D2, M2, Y2, Seq1, Seq2

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'trace: WriteOneSetWeekly...'; CALL PassMsg
      END IF

      Seq1 = SSeq
      I = 0
      DO WHILE (Seq1 .LE. ESeq)
         Seq2 = Seq1 + 6
         CALL SequenceDate(D1, M1, Y1, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
         CALL SequenceDate(D2, M2, Y2, Seq2); IF (ErrorLevel .NE. 0) GOTO 899
         I = I + 1
         WRITE(U1, 3002, ERR=813) D1, M1, Y1, D2, M2, Y2, ODat(I)
         Seq1 = Seq2 + 1
      END DO
      RETURN

  813 ErrorMessage = 'PHISTALL: Error writing file '//TRIM(FileName)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteOneSetWeekly...'
      CALL PassMsg
      RETURN

 3002 FORMAT(1X, I2.2, '/', I2.2, '/', I4, '-', I2.2, '/', I2.2, '/', I4, F8.2)

      END SUBROUTINE WriteOneSetWeekly

!----------------------------------------------------------------------
!  Data is output with 12 values per line.
!
!  SSeq, ESeq = start/end dates of the data to be output
!  S1, S2     = start/end dates of a single line of output (i.e. first/last of month)
!  Seq1, Seq2 = start/end dates of actual data in a single line of output 
!
      SUBROUTINE WriteOneSetQMon(U1, SSeq, ESeq, ODat, FileName)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: U1, SSeq, ESeq
      REAL,    INTENT(IN) :: ODat(:)
      CHARACTER (LEN=*), INTENT(IN) :: FileName

      INTEGER :: I, D, M, Y, Q, Seq1, Seq2, ValIndex
      REAL, DIMENSION(12, 4) :: QMonData
      CHARACTER (LEN=8) :: CVal(12)

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'trace: WriteOneSetQMon...'; CALL PassMsg
      END IF

      QMonData(:,:) = MissingData
      Seq1 = SSeq
      ValIndex = 0
      DO WHILE (Seq1 .LT. ESeq)
         Seq2 = Seq1 + 1
         CALL AdjustToEndOfQtrMonth(Seq2); IF (ErrorLevel .NE. 0) GOTO 899
         CALL SequenceDate(D, M, Y, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
         Q = QtrMonNumber(D, M, Y);        IF (ErrorLevel .NE. 0) GOTO 899
         ValIndex = ValIndex + 1
         QMonData(M, Q) = ODat(ValIndex)
         IF (((M .EQ. 12) .AND. (Q .EQ. 4)) .OR. (Seq2 .GE. ESeq)) THEN
            DO Q = 1, 4
               DO M = 1, 12
                  WRITE(CVal(M), '(F8.2)') QMonData(M,Q)
                  IF (QMonData(M,Q) .LT. MissingDataTest) CVal(M) = '       -'
               END DO
               WRITE(U1, 3003, ERR=813) Y, Q, (CVal(M), M=1,12)
            END DO
            QMonData(:,:) = MissingData
         END IF
         Seq1 = Seq2 + 1
      END DO

      RETURN

  813 ErrorMessage = 'PHISTALL: Error writing file '//TRIM(FileName)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteOneSetQMon...'
      CALL PassMsg
      RETURN

 3003 FORMAT(1X, I4, '-', I1, 12A8)

      END SUBROUTINE WriteOneSetQMon

!----------------------------------------------------------------------
!  Data is output with 12 values per line.
!
!  SSeq, ESeq = start/end dates of the data to be output
!
      SUBROUTINE WriteOneSetMon(U1, SSeq, ESeq, ODat, MissVal, FileName)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: U1, SSeq, ESeq
      REAL,    INTENT(IN) :: ODat(:), MissVal
      CHARACTER (LEN=*), INTENT(IN) :: FileName

      INTEGER :: D, M, Y, Q, Seq1, Seq2, ValIndex
      REAL, DIMENSION(12) :: MonData
      CHARACTER (LEN=8)   :: CVal(12)

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'trace: WriteOneSetMon...'; CALL PassMsg
      END IF

      MonData(:) = MissVal
      Seq1 = SSeq
      ValIndex = 0
      DO WHILE (Seq1 .LE. ESeq)
         Seq2 = Seq1
         CALL AdjustToEndOfMonth(Seq2); IF (ErrorLevel .NE. 0) GOTO 899
         CALL SequenceDate(D, M, Y, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
         ValIndex = ValIndex + 1
         MonData(M) = ODat(ValIndex)
         IF ((M .EQ. 12) .OR. (Seq2 .GE. ESeq)) THEN
            DO M = 1, 12
               WRITE(CVal(M), '(F8.2)') MonData(M)
               IF (MonData(M) .LT. MissVal) CVal(M) = '       -'
               IF (CVal(M)(1:1) .EQ. '*')   CVal(M) = '       -'
            END DO
            WRITE(U1, 3004, ERR=813) Y, (CVal(M), M=1,12)
            MonData(:) = MissingData
         END IF
         Seq1 = Seq2 + 1
      END DO

      RETURN

  813 ErrorMessage = 'PHISTALL: Error writing file '//TRIM(FileName)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteOneSetMon...'
      CALL PassMsg
      RETURN

 3004 FORMAT(1X, I4, 12A8)

      END SUBROUTINE WriteOneSetMon

!----------------------------------------------------------------------
!  Data is output with 1 value per line.
!
!  SSeq, ESeq = start/end dates of the data to be output
!
      SUBROUTINE WriteOneSetAnn(U1, SSeq, ESeq, ODat, FileName)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: U1, SSeq, ESeq
      REAL,    INTENT(IN) :: ODat(:)
      CHARACTER (LEN=*), INTENT(IN) :: FileName

      INTEGER :: I, D, M, Y, Seq1, Seq2
      CHARACTER (LEN=8) :: CVal

      IF (PrintTraceMessages) THEN
         ErrorMessage = 'trace: WriteOneSetAnn...'; CALL PassMsg
      END IF

      Seq1 = SSeq
      I = 0
      DO WHILE (Seq1 .LE. ESeq)
         CALL SequenceDate( D,  M, Y, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
         CALL DateSequence(31, 12, Y, Seq2); IF (ErrorLevel .NE. 0) GOTO 899
         I = I + 1
         WRITE(CVal, '(F8.2)') ODat(I)
         IF (ODat(I) .LT. MissingDataTest) CVal = '       -'
         WRITE(U1, 3005, ERR=813) Y, CVal
         Seq1 = Seq2 + 1
      END DO

      RETURN

  813 ErrorMessage = 'PHISTALL: Error writing file '//TRIM(FileName)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteOneSetAnn...'
      CALL PassMsg
      RETURN

 3005 FORMAT(1X, I4, A8)

      END SUBROUTINE WriteOneSetAnn

!----------------------------------------------------------------------
      REAL FUNCTION MM2CMS(Value, Area, Seconds)
      IMPLICIT  NONE
      INTEGER, INTENT(IN) :: Seconds
      REAL,    INTENT(IN) :: Value, Area
      REAL :: Val

      Val = Value / 1000.0                 ! mm -> meters
      Val = Val * Area / (Seconds * 1.0)   ! meters -> cu meters/sec
      MM2CMS = Val

      RETURN
      END FUNCTION MM2CMS


END MODULE PHIST_Subs

!======================================================================
!======================================================================

MODULE PROVHIST_Main

      USE PHIST_Subs

CONTAINS

      SUBROUTINE DO_PROVHIST(ErrStatus, HandleIn, ReqVals, StringArray)
      IMPLICIT NONE

      INTEGER,           INTENT(INOUT) :: ErrStatus, HandleIn
      INTEGER,           INTENT(INOUT) :: ReqVals              ! # of values to output
      CHARACTER (LEN=*), INTENT(IN)    :: StringArray(:)       ! timestep 3-char-lake [CMS] [QUIET]

      INTEGER :: I, J, K, IOS, Ndx, Q, Sub, Seq, Seq1, Seq2, Mon
      INTEGER :: NumMons, MonIndex, Days, Kount, TimeInt, Day, Month, Year
      INTEGER :: FcDay, FcMon, FcYear, FStartSeq
      INTEGER :: PSDay, PSMon, PSYear, PStartSeq
      INTEGER :: PEDay, PEMon, PEYear, PEndSeq
      INTEGER :: NumOutVals, PercentDone, EarliestDate
      INTEGER :: MaxDays, MaxWeeks, MaxQMons, MaxMons, MaxYears
      INTEGER :: DD, MM, YY

      LOGICAL :: Flag

      REAL :: LkArea, LdArea, LandToLake, Total, TArea, Tmn, Tmx, Prc
      REAL :: TVal, MVal, LVal

      REAL :: Dat(4), EDat(16), RDat(6)
      REAL, DIMENSION(:,:,:), ALLOCATABLE :: DlyMet         ! days, 4, subbasins
      REAL, DIMENSION(:,:),   ALLOCATABLE :: Met            ! days, 4 
      REAL, DIMENSION(:,:),   ALLOCATABLE :: DlyEvp         ! days, 16
      REAL, DIMENSION(:,:),   ALLOCATABLE :: DlyRun         ! days, 6
      REAL, DIMENSION(:,:),   ALLOCATABLE :: DlyData        ! days, 24
      REAL, DIMENSION(:),     ALLOCATABLE :: DataArray      ! days

      REAL, DIMENSION(:,:), ALLOCATABLE :: OutData      ! NumOutVals, 24

      CHARACTER (LEN=10)  :: LakeName
      CHARACTER (LEN=20)  :: S, TimeStep
      CHARACTER (LEN=60)  :: OutputHeaderLines(DT)
      CHARACTER (LEN=99)  :: File11, File21, FName
      CHARACTER (LEN=280) :: Line

      REAL              :: DbgVal(12,31)
      CHARACTER (LEN=8) :: DbgOut(12,31)

      WinHandle = HandleIn
      ErrorLevel = 0
      CALL ClearFilesOpen()

      ErrorMessage = '~%~0';  CALL PassMsg

!
!     Parse StringArray(1) for the time-interval designation. 
!     Look for: 'YEAR', 'MONTH', 'QTRMONTH', 'WEEK', or 'DAY'
!     (case insensitive).  That will tell us the time-interval.
!     If we fail to find any of these then we cannot continue.
!
!     p.s.  Note that 'MONTH' is a substring of 'QTRMONTH'.  We
!           will structure the test so that is handled correctly.
!
      TimeInt = 0
      I = UBOUND(StringArray, 1)
      IF (I >= 1) THEN
         Line = TRIM(StringArray(1))
         CALL Caps(Line); IF (ErrorLevel .NE. 0) GOTO 899
         IF (INDEX(Line, 'ANNUAL'  ) .GT. 0) TimeInt = 5
         IF (INDEX(Line, 'YEAR'    ) .GT. 0) TimeInt = 5
         IF (INDEX(Line, 'MONTH'   ) .GT. 0) TimeInt = 4
         IF (INDEX(Line, 'QTRMONTH') .GT. 0) TimeInt = 3     ! must follow test for "MONTH"
         IF (INDEX(Line, 'WEEK'    ) .GT. 0) TimeInt = 2
         IF (INDEX(Line, 'DAY'     ) .GT. 0) TimeInt = 1
         IF (INDEX(Line, 'DAILY'   ) .GT. 0) TimeInt = 1
      END IF
      IF (TimeInt .EQ. 0) THEN
         ErrorMessage = 'PHISTALL: No time interval was specified!'
         CALL PassMsg
         GOTO 898
      END IF

!
!     Parse StringArray(3) for 'CMS'.
!     If we find it, process the first 4 data types in cubic meters per second 
!     rather than as a depth.
!
      DoCMS = .FALSE.
      I = UBOUND(StringArray, 1)
      IF (I >= 3) THEN
         Line = TRIM(StringArray(3))
         CALL Caps(Line); IF (ErrorLevel .NE. 0) GOTO 899
         IF (INDEX(Line, 'CMS') .GT. 0) DoCMS = .TRUE.
      END IF

      DO I = 1, DT
         OutputHeaderLines(I) = HeaderLines(I)
      END DO
      IF (DoCMS) THEN
         DO I = 1, 4
            OutputHeaderLines(I) = CMSHdrLines(I)
         END DO
      END IF

!
!     Did the user ask for QUIET MODE?
!     (QuietMode is a global variable in module FcstUtil)
!
      QuietMode = .FALSE.
      I = UBOUND(StringArray, 1)
      IF (I >= 4) THEN
         S = TRIM(ADJUSTL(StringArray(4)))
         CALL Caps(S); IF (ErrorLevel .NE. 0) GOTO 899
         IF (S .EQ. 'YES')   QuietMode = .TRUE.
         IF (S .EQ. 'QUIET') QuietMode = .TRUE.
      END IF

!
!  Write info message to user.  While we are putting the text in variable
!  ErrorMessage, it is not an error.  This is just a convenient way
!  to handle writing out the text message.
!
      IF (TimeInt .EQ. 5) ErrorMessage = 'Computing historical annual values.'
      IF (TimeInt .EQ. 4) ErrorMessage = 'Computing historical monthly values.'
      IF (TimeInt .EQ. 3) ErrorMessage = 'Computing historical qtr-monthy values.'
      IF (TimeInt .EQ. 2) ErrorMessage = 'Computing historical weekly values.'
      IF (TimeInt .EQ. 1) ErrorMessage = 'Computing historical daily values.'
      CALL PassMsg

!
!  Handle the requested values specification.  If it is outside the
!  allowable range it will be shifted to that edge of the range.
!  Default value and allowed range are dependent on TimeInt.
!
      IF (TimeInt .EQ. 1) THEN
         IF (ReqVals .LT.         28) ReqVals = 28
         IF (ReqVals .GT. MaxDaysDim) ReqVals = MaxDaysDim
      END IF
      IF (TimeInt .EQ. 2) THEN
         IF (ReqVals .LT.           8) ReqVals = 8 
         IF (ReqVals .GT. MaxWeeksDim) ReqVals = MaxWeeksDim
      END IF
      IF (TimeInt .EQ. 3) THEN
         IF (ReqVals .LT.           8) ReqVals = 8 
         IF (ReqVals .GT. MaxQMonsDim) ReqVals = MaxQMonsDim
      END IF
      IF (TimeInt .EQ. 4) THEN
         IF (ReqVals .LT.         24) ReqVals = 24
         IF (ReqVals .GT. MaxMonsDim) ReqVals = MaxMonsDim
      END IF
      IF (TimeInt .EQ. 5) THEN
         IF (ReqVals .LT.           3) ReqVals = 3
         IF (ReqVals .GT. MaxYearsDim) ReqVals = MaxYearsDim
      END IF

!
!  Find out which directory each lake is in.  Values are assigned to
!  the module array CfgDirs -- a 9-element character array -- as follows:
!    CfgDirs(1) = Lake Superior
!    CfgDirs(2) = Lake Michigan
!    CfgDirs(3) = Lake Huron
!    CfgDirs(4) = Georgian Bay
!    CfgDirs(5) = Lake StClair
!    CfgDirs(6) = Lake Erie
!    CfgDirs(7) = Lake Ontario
!    CfgDirs(8) = Lake Michigan-Huron
!    CfgDirs(9) = Level (CGLRRM stuff)
!
!  Each valid entry will end with the requisite (for directories) backslash.
!
      CALL ReadCfgFile('..\FORECAST.CFG')
      IF (ErrorLevel .NE. 0) GOTO 899

!
!  Make sure that there was an entry in FORECAST.CFG for the specified lake.
!
      S = TRIM(StringArray(2))
      CALL Caps(S); IF (ErrorLevel .NE. 0) GOTO 899
      ThisLakeDir = ''
      DO I = 1, 8
         IF (LakeName3(I) .EQ. S(1:3)) THEN
            ThisLakeDir = TRIM(CfgDirs(I))
            LakeNumber = I
         END IF
      END DO
      IF (ThisLakeDir .EQ. '') THEN
         ErrorMessage = ' No entry in FORECAST.CFG for '//S//'!'
         CALL PassMsg
         GOTO 898
      ENDIF

!
!  Read the lake name and pertinent dates from BSNNM and EBSNNM.
!
      File11 = TRIM(ThisLakeDir) // 'BSNNM'
      OPEN(UNIT=11, FILE=TRIM(File11), STATUS='OLD', RECL=12,              &
     &     ACCESS='DIRECT', FORM='FORMATTED', ERR=811)
      CALL FileWasOpened(11); IF (ErrorLevel .NE. 0) GOTO 899
      READ(11, 1101, REC=1, ERR=812) LakeName
      CALL Caps(LakeName); IF (ErrorLevel .NE. 0) GOTO 899
      Bsn = LakeName(1:3)
      READ(11, 1102, REC=2, ERR=812) Subbasins, OverTheLake
      CALL Caps(OverTheLake); IF (ErrorLevel .NE. 0) GOTO 899
      READ(11, 1103, REC=5, ERR=812) FcDay, FcMon, FcYear
      CALL DateSequence(FcDay, FcMon, FcYear, FStartSeq); IF (ErrorLevel .NE. 0) GOTO 899
      CLOSE(11)
      CALL FileWasClosed(11); IF (ErrorLevel .NE. 0) GOTO 899

      PercentDone = 3
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

!
!     Get the earliest date for output based on the contents of
!     the input files we will be using.
!
      CALL EarliestCommonDate(EarliestDate)
      IF (ErrorLevel .NE. 0) GOTO 899

!                                     
!  Compute dates to pull daily data. Again, this is dependent upon the time-step.
!  We will always go back at least as far as requested, but often we will also
!  go back a little further in order to "even up" the output.  e.g. add enough
!  months or quartermonths to get back to starting on January 1.
!                  
      PEndSeq = FStartSeq - 1     ! will be adjusted backwards (earlier) as required

      IF (TimeInt .EQ. 1) THEN                     ! daily  -  adjust to beginning of a month
         PStartSeq = FStartSeq - ReqVals + 1
!         IF (PStartSeq .LT. EarliestDate) THEN
!            I = PStartSeq
!            PStartSeq = EarliestDate
!            CALL AdjustToEndOfMonth(PStartSeq);        IF (ErrorLevel .NE. 0) GOTO 899
!            PStartSeq = PStartSeq + 1
!            ReqVals = ReqVals - (PStartSeq - I)
!         END IF
         CALL SequenceDate(Day, Mon, Year, PStartSeq); IF (ErrorLevel .NE. 0) GOTO 899
         IF (Day .NE. 1) THEN
            I = ReqVals + Day - 1
            IF (I .LE. MaxDaysDim) THEN
               ReqVals = I
               PStartSeq = FStartSeq - ReqVals + 1
            END IF
         END IF
         NumOutVals = ReqVals
      END IF

      IF (TimeInt .EQ. 2) THEN                      ! weekly
         PStartSeq = FStartSeq - ReqVals * 7
!         DO WHILE (PStartSeq .LT. EarliestDate)
!            PStartSeq = PStartSeq + 7
!            ReqVals = ReqVals - 1
!         END DO
         NumOutVals = ReqVals
      END IF

      IF (TimeInt .EQ. 3) THEN                      ! qtr-monthly
         CALL SequenceDate(Day, Mon, Year, PEndSeq);     IF (ErrorLevel .NE. 0) GOTO 899
         Seq2 = PEndSeq + 1
         CALL AdjustToStartOfQtrMonth(Seq2);             IF (ErrorLevel .NE. 0) GOTO 899
         PEndSeq = Seq2 - 1
         Seq2 = PEndSeq
         Seq1 = PEndSeq - ReqVals * 7                    ! initial "guess" -- will be too close to end
         I = GetNumQtrMons(Seq1, Seq2);                  IF (ErrorLevel .NE. 0) GOTO 899
         DO WHILE (I .LT. ReqVals)     
            Seq1 = Seq1 - 1                              ! back up 1 day at a time
            I = GetNumQtrMons(Seq1, Seq2);               IF (ErrorLevel .NE. 0) GOTO 899
         END DO
         CALL SequenceDate(Day, Mon, Year, Seq1);        IF (ErrorLevel .NE. 0) GOTO 899
         CALL DateSequence(  1,   1, Year, PStartSeq);   IF (ErrorLevel .NE. 0) GOTO 899
!         DO WHILE (PStartSeq .LT. EarliestDate)
!            CALL AdjustToEndOfQtrMonth(PStartSeq);       IF (ErrorLevel .NE. 0) GOTO 899
!            PStartSeq = PStartSeq + 1
!         END DO
         CALL SequenceDate(Day, Mon, Year, PStartSeq);   IF (ErrorLevel .NE. 0) GOTO 899
         IF ((Day .NE. 1) .OR. (Mon .NE. 1)) THEN
            CALL DateSequence(31, 12, Year, PStartSeq);  IF (ErrorLevel .NE. 0) GOTO 899
            PStartSeq = PStartSeq + 1
         END IF
         NumOutVals = GetNumQtrMons(PStartSeq, PEndSeq); IF (ErrorLevel .NE. 0) GOTO 899
      END IF

      IF (TimeInt .EQ. 4) THEN     ! monthly
         CALL SequenceDate(Day, Mon, Year, PEndSeq);     IF (ErrorLevel .NE. 0) GOTO 899
         Seq2 = PEndSeq + 1
         CALL AdjustToStartOfMonth(Seq2);                IF (ErrorLevel .NE. 0) GOTO 899
         PEndSeq = Seq2 - 1
         Seq2 = PEndSeq
         Seq1 = FStartSeq - ReqVals * 28                 ! initial "guess" -- interval should be too small
         I = GetNumMonths(Seq1, Seq2);                   IF (ErrorLevel .NE. 0) GOTO 899
         DO WHILE (I .LT. ReqVals)     
            Seq1 = Seq1 - 1                              ! back up 1 day at a time
            I = GetNumMonths(Seq1, Seq2);                IF (ErrorLevel .NE. 0) GOTO 899
         END DO
         CALL SequenceDate(Day, Mon, Year, Seq1);        IF (ErrorLevel .NE. 0) GOTO 899
         CALL DateSequence(  1,   1, Year, PStartSeq);   IF (ErrorLevel .NE. 0) GOTO 899
!         DO WHILE (PStartSeq .LT. EarliestDate)
!            CALL AdjustToEndOfMonth(PStartSeq);          IF (ErrorLevel .NE. 0) GOTO 899
!            PStartSeq = PStartSeq + 1
!         END DO
         CALL SequenceDate(Day, Mon, Year, PStartSeq);   IF (ErrorLevel .NE. 0) GOTO 899
         IF ((Day .NE. 1) .OR. (Mon .NE. 1)) THEN
            CALL DateSequence(31, 12, Year, PStartSeq);  IF (ErrorLevel .NE. 0) GOTO 899
            PStartSeq = PStartSeq + 1
         END IF
         NumOutVals = GetNumMonths(PStartSeq, PEndSeq);  IF (ErrorLevel .NE. 0) GOTO 899
      END IF

      IF (TimeInt .EQ. 5) THEN     ! annual/yearly
         CALL SequenceDate(Day, Mon, Year, PEndSeq);       IF (ErrorLevel .NE. 0) GOTO 899
         Seq2 = PEndSeq + 1
         CALL AdjustToStartOfYear(Seq2);                   IF (ErrorLevel .NE. 0) GOTO 899
         PEndSeq = Seq2 - 1
         Seq2 = PEndSeq
         Seq1 = FStartSeq - (ReqVals * 365)                ! initial "guess" -- should be too close to Seq2
         I = GetNumYears(Seq1, Seq2);                      IF (ErrorLevel .NE. 0) GOTO 899
         DO WHILE (I .LT. ReqVals)     
            Seq1 = Seq1 - 1                                ! back up 1 day at a time
            I = GetNumYears(Seq1, Seq2);                   IF (ErrorLevel .NE. 0) GOTO 899
         END DO
         CALL SequenceDate(Day, Mon, Year, Seq1);          IF (ErrorLevel .NE. 0) GOTO 899
         CALL DateSequence(  1,   1, Year, PStartSeq);     IF (ErrorLevel .NE. 0) GOTO 899
!         DO WHILE (PStartSeq .LT. EarliestDate)
!            CALL DateSequence(31, 12, Year, PStartSeq);    IF (ErrorLevel .NE. 0) GOTO 899
!            PStartSeq = PStartSeq + 1
!            CALL SequenceDate(Day, Mon, Year, PStartSeq);  IF (ErrorLevel .NE. 0) GOTO 899
!         END DO
         NumOutVals = GetNumYears(PStartSeq, PEndSeq);     IF (ErrorLevel .NE. 0) GOTO 899
      END IF

      CALL SequenceDate(PSDay, PSMon, PSYear, PStartSeq);  IF (ErrorLevel .NE. 0) GOTO 899
      CALL SequenceDate(PEDay, PEMon, PEYear, PEndSeq);    IF (ErrorLevel .NE. 0) GOTO 899

      PercentDone = 5
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

!
!  Allocate the RAM needed for the daily data arrays
!
      I = PEndSeq - PStartSeq + 1
      IF (I .GT. MaxDaysDim) THEN
         ErrorMessage = 'PHISTALL: Data period exceeds maximum allowed number of days!'
         CALL PassMsg
         GOTO 898
      END IF
      IF (I .LT. 1) THEN
         ErrorMessage = 'PHISTALL: Data period less than 1 day (??) !'
         CALL PassMsg
         GOTO 898
      END IF
      ALLOCATE(DlyMet(I,4,-1:30), Met(I,4), DlyEvp(I,16), DlyRun(I,6),    &
               DlyData(I,DT), DataArray(I), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'PHISTALL: Error allocating RAM!'
         CALL PassMsg
         GOTO 898
      END IF

      PercentDone = 7
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

!
!  Read the AREA.xxx file
!
      File21 = TRIM(ThisLakeDir) // 'AREA.' // Bsn
      OPEN(UNIT=21, FILE=TRIM(File21), STATUS='OLD', ERR=821)
      CALL FileWasOpened(21); IF (ErrorLevel .NE. 0) GOTO 899
      LdArea = 0.0
      DO 25, Sub = 1, Subbasins
        READ(21, 2101, ERR=822) Area(Sub)
        LdArea = LdArea + Area(Sub)
  25  CONTINUE
      READ(21, 2101, ERR=822) Area(0)
      CLOSE(21)
      CALL FileWasClosed(21); IF (ErrorLevel .NE. 0) GOTO 899
      Area(-1) = LdArea
      LkArea   = Area(0)
      LandToLake = LdArea / LkArea

!
!  Read the daily data from the xxxnn.eee files.
!  The data units in array Met are:
!    Tmin (:,1) & Tmax (:,2) are in degrees C
!    Precipitation (:,3) is in millimeters
!
!  Arbitrarily assume we're 30% done at end of this step 
!  (with this step being 20% of the total).
!
      RD_FillMetMissing = .FALSE.     ! do NOT fill in missing data
      DO Sub = 0, Subbasins
         CALL ReadMetData(PStartSeq, PEndSeq, Met, Bsn, Sub, ThisLakeDir)
         IF (ErrorLevel .NE. 0) GOTO 899
         DO Seq = PStartSeq, PEndSeq
            I = Seq - PStartSeq + 1
            DO J = 1, 3
               DlyMet(I,J,Sub) = Met(I,J)
            END DO
         END DO
         PercentDone = ((Sub*100.0/Subbasins) * 0.2) + 10    ! 10..30
         WRITE(ErrorMessage, 9011) PercentDone
         CALL PassMsg
      END DO

!
!  Compute overland or overlake temp and precip (dependent on the value of OverTheLake
!  read from the BSNNM file) from the subbasin data just read.
!    DlyData(:,2) = precipitation as a depth (mm)
!    DlyData(:,5) = air temperature (C) -- average of tmin and tmax
!
      IF (OverTheLake .EQ. 'LAKE') THEN
         DlyData(:,2) = DlyMet(:,3,0)
         DlyData(:,5) = (DlyMet(:,1,0) + DlyMet(:,2,0)) / 2.0
      ELSE
         DO Seq = PStartSeq, PEndSeq
           I = Seq - PStartSeq + 1 
           Total = 0.0
           TArea = 0.0
           DO J = 1, Subbasins
              Prc = DlyMet(I,3,J)
              IF (Prc .GT. -999.8) THEN
                 Total = Total + (Prc * Area(J))
                 TArea = TArea + Area(J)
              ENDIF
           END DO
           DlyData(I,2) = MissingData
           IF (TArea .GT. 0) DlyData(I,2) = Total / TArea

           Total = 0.0
           TArea = 0.0
           DO J = 1, Subbasins
              Tmn = DlyMet(I, 1, J)
              Tmx = DlyMet(I, 2, J)
              IF ((Tmn .GT. -998.9) .AND. (Tmx .GT. -998.9)) THEN
                 Total = Total + (((Tmn+Tmx)/2.0) * Area(J))
                 TArea = TArea + Area(J)
              END IF
           END DO
           DlyData(I,5) = MissingData
           IF (TArea .GT. 0) DlyData(I,5) = Total / TArea
         END DO
      END IF

!
!  Read the daily data from ZSEVAP.xxx and/or ZSEVAPPR.xxx
!    DlyData(:, 3) = evaporation as depth over the lake              mm
!    DlyData(:, 6) = water (Lake Surface) temp                       C
!    DlyData(:, 7) = Air Temp over lake (w/overwater adjustments)    C
!    DlyData(:, 8) = Vapor Pressure (w/overwater adjustments)        mb
!    DlyData(:, 9) = Wind Speed over lake (w/overwater adjustments)  m/s
!    DlyData(:,10) = Cloud Cover (w/overwater adjustments)           %   (0..100)
!    DlyData(:,11) = Incident Short-wave Radiation                   watts/m2  
!    DlyData(:,12) = Reflected Radiation                             watts/m2  
!    DlyData(:,13) = Net Long-Wave Radiation Exchange                watts/m2  
!    DlyData(:,14) = Latent Heat Flux                                watts/m2  
!    DlyData(:,15) = Sensible Heat Flux                              watts/m2  
!    DlyData(:,16) = Net Surface Flux (11+12+13+14+15)               watts/m2  
!    DlyData(:,17) = Ice Temperature                                 C         
!    DlyData(:,18) = Ice Area                                        %   (0..100)
!    DlyData(:,19) = Stored Heat                                     calories * 1.0e17 
!
!    (Note that ice depth is skipped)
!
      CALL ReadEvapData(PStartSeq, PEndSeq, DlyEvp, Bsn, ThisLakeDir)
      IF (ErrorLevel .NE. 0) GOTO 899
      DlyData(:, 3) = DlyEvp(:, 1) * 10.0    ! cm -> mm
      DlyData(:, 6) = DlyEvp(:, 2)
      DlyData(:, 7) = DlyEvp(:, 3)
      DlyData(:, 8) = DlyEvp(:, 4)
      DlyData(:, 9) = DlyEvp(:, 5)
      DlyData(:,10) = DlyEvp(:, 6)
      DlyData(:,11) = DlyEvp(:, 7)
      DlyData(:,12) = DlyEvp(:, 8)
      DlyData(:,13) = DlyEvp(:, 9)
      DlyData(:,14) = DlyEvp(:,10)
      DlyData(:,15) = DlyEvp(:,11)
      DlyData(:,16) = DlyEvp(:,12)
      DlyData(:,17) = DlyEvp(:,13)
      DlyData(:,18) = DlyEvp(:,14)
      DlyData(:,19) = DlyEvp(:,16)    ! DlyEvp(:,15) -- ice depth -- is skipped
      
      PercentDone = 50
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

!
!  Read the daily data from xxxALL.SUM
!    DlyData(:, 1) = runoff as depth over the lake surface (mm)
!    DlyData(:,20) = upper + lower soil zone moisture (cm)
!    DlyData(:,21) = ground zone moisture    (cm)
!    DlyData(:,22) = surface zone moisture   (cm)
!    DlyData(:,23) = snowpack zone moisture  (cm)
!    DlyData(:,24) = total moisture storage  (cm)
!
      FName = Bsn // 'ALL.SUM'
      CALL ReadRunData(PStartSeq, PEndSeq, DlyRun, FName, ThisLakeDir)
      IF (ErrorLevel .NE. 0) GOTO 899
      DlyData(:, 1) = DlyRun(:,1) * LandToLake * 10.0   ! cm over land -> mm over lake
      DlyData(:,20) = DlyRun(:,2) + DlyRun(:,3)
      DlyData(:,21) = DlyRun(:,4)
      DlyData(:,22) = DlyRun(:,5)
      DlyData(:,23) = DlyRun(:,6)
      
      Days = PEndSeq - PStartSeq + 1
      DO I = 1, Days
         DlyData(I,24) = SUM(DlyRun(I,2:6))
      END DO

      PercentDone = 70
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

!
!  Fill in the "LEVELS" values with the missing data code
!
      DlyData(:,25) = MissingData
      DlyData(:,26) = MissingData
      DlyData(:,27) = MissingData
      DlyData(:,28) = MissingData
      DlyData(:,29) = MissingData

!
!  Compute daily NBS values
!    DlyData(:,4) = NBS as a depth over the lake surface (mm)
!
      DlyData(:, 4) = MissingData
      Days = PEndSeq - PStartSeq + 1
      DO I = 1, Days
         IF (DlyData(I,1) .GT. MissingData) THEN
           IF (DlyData(I,2) .GT. MissingData) THEN
             IF (DlyData(I,3) .GT. MissingData) THEN
                DlyData(I,4) = DlyData(I,1) + DlyData(I,2) - DlyData(I,3)   ! NBS = R + P - E
             ELSE
                DlyData(I,4) = MissingData
             END IF
           END IF
         END IF
      END DO

!
!  For debugging purposes, output the daily values to a file
!
      IF (DoDebug) THEN
        IF (TimeInt .EQ. 1) File11 = TRIM(ThisLakeDir) // 'H_DLY.INT'
        IF (TimeInt .EQ. 2) File11 = TRIM(ThisLakeDir) // 'H_WKL.INT'
        IF (TimeInt .EQ. 3) File11 = TRIM(ThisLakeDir) // 'H_QMN.INT'
        IF (TimeInt .EQ. 4) File11 = TRIM(ThisLakeDir) // 'H_MON.INT'
        IF (TimeInt .EQ. 5) File11 = TRIM(ThisLakeDir) // 'H_ANN.INT'
        OPEN(UNIT=11, FILE=TRIM(File11), STATUS='REPLACE', ERR=811)
        CALL FileWasOpened(11); IF (ErrorLevel .NE. 0) GOTO 899
        WRITE(11, 3901, ERR=813) (I, I=1,31)
        DO K = 1, 24
          WRITE(11, 3904, ERR=813) HeaderLines(K)
          DbgVal(:,:) = MissingData
          DO Seq = PStartSeq, PEndSeq
            I = Seq - PStartSeq + 1
            CALL SequenceDate(Day, Month, Year, Seq); IF (ErrorLevel .NE. 0) GOTO 899
            DbgVal(Month,Day) = DlyData(I,K)
            IF (((Day .EQ. 31) .AND. (Month .EQ. 12)) .OR. (Seq .EQ. PEndSeq)) THEN
               DO Month = 1, 12
                 DO Day = 1, 31
                    DbgOut(Month,Day) = '      --'
                    TVal = DbgVal(Month,Day)
                    IF ((TVal .GT. -999.998) .AND. (TVal .LT. 9999.999)) THEN
                       WRITE(DbgOut(Month,Day), '(F8.3)') TVal
                    END IF
                 END DO
               END DO
               DO Month = 1, 12
                  WRITE(11, 3905, ERR=813) Year, Month, (DbgOut(Month,Day), Day=1,31)
               END DO
               DbgVal(:,:) = MissingData
            END IF
          END DO
        END DO
        CLOSE(11)
        CALL FileWasClosed(11); IF (ErrorLevel .NE. 0) GOTO 899
      END IF

!
!  If user wants output of the NBS components (and NBS) in CMS, then
!  do that conversion here.
!
      IF (DoCMS) THEN
         DO I = 1, Days
            DO J = 1, 4
               IF (DlyData(I,J) .GT. MissingDataTest) THEN
                  TVal = DlyData(I,J)
                  DlyData(I,J) = MM2CMS(TVal, LkArea, 86400)
               ELSE
                  DlyData(I,J) = MissingData
               END IF
            END DO
         END DO
      END IF

!
!  Allocate the RAM for the output values
!
      ALLOCATE(OutData(NumOutVals,DT), STAT=I)
      IF (I .NE. 0) THEN
         ErrorMessage = 'PHISTALL: Error allocating RAM!'
         CALL PassMsg
         GOTO 898
      END IF

      PercentDone = 75
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

!
!  Now compute the time-step values
!
      DO K = 1, DT
        Seq1 = PStartSeq
        DO NDX = 1, NumOutVals
           IF (TimeInt .EQ. 1) Seq2 = Seq1
           IF (TimeInt .EQ. 2) Seq2 = Seq1 + 6
           IF (TimeInt .EQ. 3) THEN
              CALL SequenceDate(Day, Mon, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
              Seq2 = Seq1 + 1
              CALL AdjustToEndOfQtrMonth(Seq2);        IF (ErrorLevel .NE. 0) GOTO 899
           END IF
           IF (TimeInt .EQ. 4) THEN
              Seq2 = Seq1
              CALL AdjustToEndOfMonth(Seq2); IF (ErrorLevel .NE. 0) GOTO 899
           END IF
           IF (TimeInt .EQ. 5) THEN
              CALL SequenceDate(Day, Mon, Year, Seq1); IF (ErrorLevel .NE. 0) GOTO 899
              CALL DateSequence( 31,  12, Year, Seq2); IF (ErrorLevel .NE. 0) GOTO 899
           END IF

           CALL DoTheComputation(K,PStartSeq,Seq1,Seq2,DlyData,TVal,MVal,LVal)
           IF (ErrorLevel .NE. 0) GOTO 899
           SELECT CASE (K)
              CASE (1:4)
                 IF (DoCMS) THEN
                    OutData(NDX,K) = MVal      ! mean (average)
                 ELSE
                    OutData(NDX,K) = TVal      ! total
                 END IF
              CASE (5:18)
                 OutData(NDX,K) = MVal         ! mean (average)
              CASE (19:24)
                 OutData(NDX,K) = LVal         ! last (end-of-period)
           END SELECT
           Seq1 = Seq2 + 1
        END DO
      END DO

      PercentDone = 90
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

!
!  Write the output values
!
      CALL WriteHistOut(TimeInt, FStartSeq, NumOutVals, PStartSeq, PEndSeq, OutData)
      IF (ErrorLevel .NE. 0) GOTO 899

!
!  Clean up allocated RAM and get outta here
!
      IF (ALLOCATED(OutData))   DEALLOCATE(OutData)
      IF (ALLOCATED(DataArray)) DEALLOCATE(DataArray)
      IF (ALLOCATED(DlyData))   DEALLOCATE(DlyData)
      IF (ALLOCATED(DlyRun))    DEALLOCATE(DlyRun)
      IF (ALLOCATED(DlyEvp))    DEALLOCATE(DlyEvp)
      IF (ALLOCATED(DlyMet))    DEALLOCATE(DlyMet)
      IF (ALLOCATED(Met))       DEALLOCATE(Met)

      PercentDone = 99
      WRITE(ErrorMessage, 9011) PercentDone
      CALL PassMsg

      GOTO 999

!
!  Handlers for I/O errors
!
  811 ErrorMessage = 'PHISTALL: Error opening file ' // TRIM(File11)
      CALL PassMsg; GOTO 898
  812 ErrorMessage = 'PHISTALL: Error reading file ' // TRIM(File11)
      CALL PassMsg; GOTO 898
  813 ErrorMessage = 'PHISTALL: Error writing file ' // TRIM(File11)
      CALL PassMsg; GOTO 898
  821 ErrorMessage = 'PHISTALL: Error opening file ' // TRIM(File21)
      CALL PassMsg; GOTO 898
  822 ErrorMessage = 'PHISTALL: Error reading file ' // TRIM(File21)
      CALL PassMsg; GOTO 898
  823 ErrorMessage = 'PHISTALL: Error writing file ' // TRIM(File21)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 CALL MakeErrorInt
      ErrorMessage = '[traceback] : DO_PROVHIST...'
      CALL PassMsg
      IF (ALLOCATED(OutData))   DEALLOCATE(OutData)
      IF (ALLOCATED(DataArray)) DEALLOCATE(DataArray)
      IF (ALLOCATED(DlyData))   DEALLOCATE(DlyData)
      IF (ALLOCATED(DlyRun))    DEALLOCATE(DlyRun)
      IF (ALLOCATED(DlyEvp))    DEALLOCATE(DlyEvp)
      IF (ALLOCATED(DlyMet))    DEALLOCATE(DlyMet)
      IF (ALLOCATED(Met))       DEALLOCATE(Met)
      GOTO 999

!  -----------------------

  999 CALL CloseAllFiles()
      ErrStatus = ErrorLevel
      RETURN

!
!  FORMAT statements
!
 1101 FORMAT(A10)
 1102 FORMAT(I3, 3X, A4)
 1103 FORMAT(2I3, I5)
 2101 FORMAT(E13.6E2)

 3901 FORMAT(' Daily data used in PHISTALL when computing values', /,     &
     &       ' Year-Mn', 31(6X,I2))
 3904 FORMAT(A)
 3905 FORMAT(I5, '-', I2, 31A8)
 9011 FORMAT('~%~', I3.3)

      END SUBROUTINE DO_PROVHIST

END MODULE PROVHIST_Main



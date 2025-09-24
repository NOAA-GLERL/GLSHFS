!------------------------------------------------------------------------------
!  This data structure (and associated file) is a replacement for the 
!  BSNNM, EBSNNM and AREA.xxx files from AHPS 1.  The BSNNM/EBSNNM files were 
!  an integral part of the old AHPS 1.x structure. The purpose behind them 
!  in the pre-cursor versions was to provide information flow from one program
!  to the next and help prevent the programs from being run in the wrong 
!  sequence. When the code was ported into a GUI environment (version 1) the 
!  following considerations were in play:
!      1) We no longer needed the sequence control, as that was now 
!         controlled by the GUI program.
!      2) We still needed the information flow to ensure that dates were 
!         handled correctly, etc.
!      3) The use of these files was *DEEPLY* imbedded into the programming
!         logic of almost every "sub-program" of AHPS. Removing it would have
!         required a lot of work, and I would still have needed to replace
!         a lot of the date-handling and information passing in some way.
!  It was just a lot easier to retain them as-is, so we did.
!
!  With the total re-design and re-coding of AHPS into the GLSHFS, I am 
!  changing things yet again.  I no longer will need to track dates like
!  in AHPS. So I am taking the essential data from BSNNM (lake name, number
!  of subbasins) and adding the lake & subbasin areas from AREA.xxx.  This new
!  file will never change. It's purpose is essentially to identify the lake 
!  in the current directory and provide basic geographic-type info.
!
!  Note that there is no WRITE procedure here. The file is intended to be read-only.

!  Tim Hunter  -  27 Sep 2016
!------------------------------------------------------------------------------

   MODULE BasinInfo
      USE ErrorProcess
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GL_Constants

      
      !
      !  Define a type for storing the info from the file BasinInfo.txt
      !  This file contains information about the lake basin that is stored 
      !  in the current directory, and combines information from a few of 
      !  the old AHPS files: 
      !    BSNNM
      !    AREA.xxx
      !  Also remember that subbasin 0 = lake surface.
      !
      TYPE, PUBLIC :: BasinInfoData
         CHARACTER(LEN=10)     :: LakeName             !  'superior', 'michigan', etc
         CHARACTER(LEN=3)      :: Basin3               !  'sup', 'mic', etc
         INTEGER               :: NumSubbasins         !
         REAL                  :: LakeArea             !  square meters.  Same value as SubbasinArea(0)
         REAL, DIMENSION(0:30) :: SubbasinArea         !  Statically sized to 30 for convenience
      END TYPE BasinInfoData

     
   CONTAINS
!-----------------------------------------------------------------------------
      !-----------------------------------------------------------------------------
      SUBROUTINE InitializeBasinInfoData(BData)
      IMPLICIT NONE
      TYPE (BasinInfoData), INTENT(OUT) :: BData
      
      BData%LakeName = '----------'
      BData%Basin3   = '---'
      BData%NumSubbasins     = -99
      BData%LakeArea         = MissingData_Real
      BData%SubbasinArea(:)  = MissingData_Real
      
      END SUBROUTINE InitializeBasinInfoData


!-----------------------------------------------------------------------------
      !----------------------------------------------------------------------------      
      SUBROUTINE ReadBasinInfoFile(FName, BData)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN)  :: FName
      TYPE (BasinInfoData),  INTENT(OUT) :: BData
      
      INTEGER :: I, N, U1, IOS, LineNum
      CHARACTER(LEN=200) :: Line, Strings(10), S

      !
      !  Initialize all fields
      !
      CALL InitializeBasinInfoData(BData)
      
      !
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)

      !
      !  Line 1 = Lake name (superior, michigan, huron, georgian, stclair, erie, ontario)
      !  After reading the name, determine the 3-char basin code.
      !
      LineNum = 1
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, N);    IF (N .LE. 1) GOTO 880
      BData%LakeName = GetLowerCase(Strings(2))
      DO I = 1, GLShfsNumBasins
         IF (TRIM(BData%LakeName) .EQ. LakeName10(I)) BData%Basin3 = LakeName3(I)
      END DO
      
      !
      !  Line 2 = number of subbasins.  This COULD be retrieved by using the
      !     array defined in GL_Constants, but I want it to also be in the
      !     BasinInfo.txt file so that programs beyond GLSHFS have easy access 
      !     to the info (stuff like utility programs, etc).
      !
      LineNum = 2
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, N);     IF (N .LE. 1) GOTO 880
      S = TRIM(Strings(2))
      READ(S, *, ERR=831) BData%NumSubbasins
      
      !
      !  Line 3 is the lake area (in square meters)
      !  Assign it to both the LakeArea field and SubbasinArea(0)
      !
      LineNum = 3
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, N);      IF (N .LE. 1) GOTO 880
      READ(Strings(2), *, ERR=832) BData%LakeArea
      BData%SubbasinArea(0) = BData%LakeArea
      
      !
      !  Lines 4-n are the subbasin areas (in square meters)
      !
      DO I = 1, BData%NumSubbasins
         LineNum = LineNum + 1
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, Strings, N);   IF (N .LE. 1) GOTO 880
         READ(Strings(2), *, ERR=833) BData%SubbasinArea(I)
      END DO

      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN      
      
      !
      !  Error Handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(FName);  CALL PassMsg;  GOTO 898
  812 ErrorMessage = 'Error reading file ' // TRIM(FName);  CALL PassMsg;  GOTO 898
 
  831 ErrorMessage = 'Error parsing the number of subbasins in ' // TRIM(FName);  CALL PassMsg;  GOTO 898
  832 ErrorMessage = 'Error parsing the lake area in '           // TRIM(FName);  CALL PassMsg;  GOTO 898
  833 ErrorMessage = 'Error parsing the subbasin areas in '      // TRIM(FName);  CALL PassMsg;  
      WRITE(ErrorMessage, *) 'LineNumber = ', LineNum;                            CALL PassMsg;  GOTO 898
  
  880 WRITE(ErrorMessage, 1050) LineNum;   CALL PassMsg;   GOTO 898
 
  898 ErrorLevel = 1 
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      CALL InitializeBasinInfoData(BData)
      ErrorMessage = '[traceback] ReadBasinInfoFile'; CALL PassMsg
      RETURN

 1001 FORMAT(A200)
 1050 FORMAT('Too few entries on line ', I0, ' of ', A)

      END SUBROUTINE ReadBasinInfoFile
 

!-----------------------------------------------------------------------------
      !----------------------------------------------------------------------------      
      SUBROUTINE WriteBasinInfoFile(FName, BData)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN)  :: FName
      TYPE (BasinInfoData),  INTENT(OUT) :: BData
      
      INTEGER :: I, U1, IOS
      CHARACTER(LEN=200) :: S
      
      !
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)

      !
      !  Line 1 = Lake name (superior, michigan, huron, georgian, stclair, erie, ontario)
      !
      S = TRIM(BData%LakeName)
      CALL LowerCase(S)
      WRITE(U1, 1001, ERR=813) TRIM(S)

      !
      !  Line 2 = number of subbasins.
      !
      WRITE(U1, 1002, ERR=813) BData%NumSubbasins
      
      !
      !  Line 3 is the lake area (in square meters)
      !  
      WRITE(U1, 1003, ERR=813) BData%LakeArea
      
      !
      !  Lines 4-n are the subbasin areas (in square meters)
      !
      DO I = 1, BData%NumSubbasins
         WRITE(U1, 1004, ERR=813) I, BData%SubbasinArea(I)
      END DO


      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN      
      
      !
      !  Error Handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(FName);  CALL PassMsg;  GOTO 898
  813 ErrorMessage = 'Error writing file ' // TRIM(FName);  CALL PassMsg;  GOTO 898
 
 
  898 ErrorLevel = 1 
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      ErrorMessage = '[traceback] WriteBasinInfoFile'; CALL PassMsg
      RETURN

 1001 FORMAT('Lake name:,', A)      
 1002 FORMAT('Number of subbasins:, ', I2)
 1003 FORMAT('Lake area (sq meters):,', E13.6E2)
 1004 FORMAT('Area of subbasin ', I2.2, ' (sq meters):,', E13.6E2)

      END SUBROUTINE WriteBasinInfoFile
 


   END MODULE BasinInfo
   
   
   

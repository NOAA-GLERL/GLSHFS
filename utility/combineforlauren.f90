MODULE CFL_Subs
      USE GLSHFS_Util
      USE GLSHFS_Files

      CHARACTER(LEN=50)  :: ForecastName
      CHARACTER(LEN=200) :: BaseDir
      
CONTAINS
      !--------------------------------------------------------------
      !  Combine the daily nbs files (CGLRRM format) from Michigan, Huron, and 
      !  Georgian Bay into a single file for the combined lake.
      !  Values are supplied as a rate (tens of CMS) so combining them is
      !  simple addition.
      !--------------------------------------------------------------
      SUBROUTINE Combine_MHG_For_CGLRRM(SCode)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: SCode
      
      INTEGER :: I, IOS, IM, IH, IG
      INTEGER :: Seq, SSeq, ESeq, NumDays
      INTEGER :: SSeqMic, ESeqMic, SSeqHur, ESeqHur, SSeqGeo, ESeqGeo
!      INTEGER :: SSeqMHG, ESeqMHG
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=70)  :: Description
      CHARACTER(LEN=200) :: OtlkDir, FName

      REAL, DIMENSION(:), ALLOCATABLE :: NbsMic, NbsHur, NbsGeo, NbsMHG

      !
      OtlkDir = './'

      
      !
      !  Read each of the files
      !
      WRITE(FName, 1105) TRIM(OtlkDir), 'mi', TRIM(SCode)
      CALL ReadFile_CGLRRM(FName, Bsn, NbsMic, SSeqMic, ESeqMic, Description)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      WRITE(FName, 1105) TRIM(OtlkDir), 'hu', TRIM(SCode)
      CALL ReadFile_CGLRRM(FName, Bsn, NbsHur, SSeqHur, ESeqHur, Description)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      WRITE(FName, 1105) TRIM(OtlkDir), 'gb', TRIM(SCode)
      CALL ReadFile_CGLRRM(FName, Bsn, NbsGeo, SSeqGeo, ESeqGeo, Description)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Combine the data values
      !
      SSeq = MAX(SSeqMic, SSeqHur, SSeqGeo)
      ESeq = MIN(ESeqMic, ESeqHur, ESeqGeo)
      NumDays = ESeq - SSeq + 1
      ALLOCATE(NbsMHG(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5002) NumDays;  CALL PassMsg
         GOTO 898
      END IF

      DO Seq = SSeq, ESeq
         IM = Seq - SSeqMic + 1
         IH = Seq - SSeqHur + 1
         IG = Seq - SSeqGeo + 1
         I  = Seq - SSeq    + 1
         NbsMhg(I) = NbsMic(IM) + NbsHur(IH) + NbsGeo(IG)
         IF (NbsMic(IM) .LT. -99998) NbsMHG(I) = MissingData_Real
         IF (NbsHur(IH) .LT. -99998) NbsMHG(I) = MissingData_Real
         IF (NbsGeo(IG) .LT. -99998) NbsMHG(I) = MissingData_Real
      END DO      
      
      !
      !  Write an output NBS file that is in the format compatible with the historical CGLRRM
      !
      Description = 'Outlook NBS values based on meteorology from the ' // TRIM(SCode) //' scenario'
      WRITE(FName, 1105) TRIM(OtlkDir), 'mh', TRIM(SCode)
      Bsn = 'mhg'
      CALL WriteFile_CGLRRM(TRIM(FName), Bsn, NbsMHG, SSeq, ESeq, Description);  IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      ! go to final cleanup
      !
      GOTO 999
      
      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] Combine_MHG_For_CGLRRM...';   CALL PassMsg
      GOTO 999
  
      !
      !  Final cleanup
      !
  999 DEALLOCATE(NbsMic, STAT=IOS)
      DEALLOCATE(NbsHur, STAT=IOS)
      DEALLOCATE(NbsGeo, STAT=IOS)
      DEALLOCATE(NbsMHG, STAT=IOS)
  
  
      !
      !  FORMATs
      !      
 1105 FORMAT(A, A2, 'dnbs_', A, '.txt')
 5002 FORMAT('Error allocating memory for ', I0, ' days of combined NBS data')

      END SUBROUTINE Combine_MHG_For_CGLRRM
   
END MODULE CFL_Subs
   
!------------------------------------------------------------------------------
   
   PROGRAM CombineForLauren
      USE CFL_Subs
      USE GLSHFS_Util
      USE GLSHFS_Files
      USE GL_Constants
   
      INTEGER :: I, J, IOS, NumArgs, U1, U2
      CHARACTER(LEN=10)  :: SCode
      CHARACTER(LEN=80)  :: ArgStrings(5), FName, S1, S2, FName2
      CHARACTER(LEN=100) :: CLine


      CALL GET_COMMAND(CLine)
      CALL ParseCmdLine(CLine, ArgStrings, NumArgs); IF (ErrorLevel .NE. 0) GOTO 898
      IF (NumArgs .EQ. 1) THEN
         FName = 'temp_listfile_tmp.tmp'
         CALL BuildDirFileList_SYSTEM(FName, 'midnbs_*.txt')
         
         U1 = GetFreeUnitNumber()
         OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
         CALL FileWasOpened(U1)
         
         U2 = GetFreeUnitNumber()
         FName2 = 'scenario_names.txt'
         OPEN(UNIT=U2, FILE=TRIM(FName2), STATUS='REPLACE', ERR=821)
         CALL FileWasOpened(U2)
         
         READ(U1, 1101, IOSTAT=IOS) S1
         DO WHILE (IOS .EQ. 0) 
            READ(S1, 1102, IOSTAT=I) S2
            IF (I .EQ. 0) THEN
               J = INDEX(S2, '.txt')
               SCode = S2(1:J-1)
               CALL Combine_MHG_For_CGLRRM(TRIM(SCode));  IF (ErrorLevel .NE. 0) GOTO 899
               WRITE(U2, 1201, ERR=823) TRIM(SCode)
            END IF
            READ(U1, 1101, IOSTAT=IOS) S1
         END DO
         CLOSE(U2)
         CALL FileWasClosed(U2)
         CLOSE(U1, STATUS='DELETE')
         CALL FileWasClosed(U1)
      ELSE
         U1 = GetFreeUnitNumber()
         FName = TRIM(ArgStrings(2))
         OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
         CALL FileWasOpened(U1)
         READ(U1, 1001, IOSTAT=IOS) SCode
         DO WHILE (IOS .EQ. 0) 
            CALL Combine_MHG_For_CGLRRM(TRIM(SCode));  IF (ErrorLevel .NE. 0) GOTO 899
            READ(U1, 1001, IOSTAT=IOS) SCode
         END DO
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      
      GOTO 999
      
      !
      !  Handlers for I/O errors
      !
 811  ErrorMessage = 'Error opening file ' // TRIM(FName);  CALL PassMsg; GOTO 898
! 812  ErrorMessage = 'Error reading file ' // TRIM(FName);  CALL PassMsg; GOTO 898
 821  ErrorMessage = 'Error opening file ' // TRIM(FName2);  CALL PassMsg; GOTO 898
 823  ErrorMessage = 'Error writing file ' // TRIM(FName2);  CALL PassMsg; GOTO 898
      
      !
      !  Help for user on how to use program
      !
! 855  PRINT*, 'USAGE: combineforlauren <listfile>'
!      PRINT*, '       listfile contains the scenario names; 1 per line'
!      GOTO 898

      !
      !  Clean up and exit
      !
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] : combineforlauren...'
      CALL PassMsg
      GOTO 999

      !
      !  Final cleanup
      !
  999 CALL CloseAllFiles()
      CALL EXIT(ErrorLevel)

      !
      !  FORMAT statements
      !
 1001 FORMAT(A10)
 1101 FORMAT(A)
 1102 FORMAT(7X, A)
 1201 FORMAT(A)

   END PROGRAM   
   

   
   PROGRAM PrepCmipFiles
      USE GL_Constants
      USE MyKinds
      USE ErrorProcess
      USE GLSHFS_Util
      IMPLICIT NONE

      CHARACTER(LEN=15), DIMENSION(19), PARAMETER :: ModelName15 =    &
       (/  'bcc-csm1-1     ', 'CanESM2        ', 'CCSM4          ',   &
           'CNRM-CM5       ', 'CSIRO-Mk3-6-0  ', 'GFDL-CM3       ',   &
           'GFDL-ESM2G     ', 'GFDL-ESM2M     ', 'GISS-E2-R      ',   &
           'HadGEM2-CC     ', 'HadGEM2-ES     ', 'inmcm4         ',   &
           'IPSL-CM5A-LR   ', 'IPSL-CM5A-MR   ', 'MIROC5         ',   &
           'MIROC-ESM      ', 'MPI-ESM-MR     ', 'MRI-CGCM3      ',   &
           'NorESM1-M      '  /)

      CHARACTER(LEN=6), DIMENSION(19), PARAMETER :: ModelName6  =  &
       (/  'bcccsm', 'canesm', 'ccsm4 ', 'cnrmc5', 'csiro6',       &
           'gfdlc3', 'esm2g ', 'esm2m ', 'gisse2', 'gem2cc',       &
           'gem2es', 'inmcm4', 'cm5alr', 'cm5amr', 'miroc5',       &
           'miresm', 'mpiesm', 'cgcm3 ', 'noresm'    /)


      CHARACTER(LEN=4), DIMENSION(19), PARAMETER :: ModelName4  =  &
       (/  'bccc', 'caes', 'ccs4', 'cnr5', 'csir',                 &
           'gcm3', 'ge2g', 'ge2m', 'gie2', 'hgcc',                 &
           'hges', 'inm4', 'iplr', 'ipmr', 'mir5',                 &
           'mire', 'mpim', 'mric', 'ne1m'    /)

      INTEGER :: SNum, Lk, Sub
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=100) :: FName1, FName2


      DO SNum = 1, 19
         DO Lk = 1, 7
            Bsn = LakeName3(Lk)
            PRINT*, 'Scen=', TRIM(ADJUSTL(ModelName6(SNum))), '; lake=', Bsn
            DO Sub = 0, NumSubbasins(Lk)
               WRITE(FName1, 1001) TRIM(ModelName15(SNum)), Bsn, Sub, TRIM(ADJUSTL(ModelName6(SNum)))
               WRITE(FName2, 1002) Bsn, Sub, TRIM(ModelName4(SNum))
               CALL CopyFile(TRIM(FName1), TRIM(FName2))
               IF (Errorlevel .NE. 0) THEN
                  print*, ' -- Error copying '//TRIM(FName1)//' to '//TRIM(FName2)
               END IF
            END DO
         END DO
      END DO
      CALL EXIT(0)   
      
   
 1001 FORMAT('\glshfs\cmip_data\', A, '\otlkdata_', A3, I2.2, '_', A, '.csv')
 1002 FORMAT('\glshfs\cmip_otlk\otlkdata_', A3, I2.2, '_', A, '.csv')
   
   END PROGRAM PrepCmipFiles
   
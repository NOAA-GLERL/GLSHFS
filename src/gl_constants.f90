
MODULE GL_Constants

      !
      !  How many "lakes" are we defining?
      !
      INTEGER, PARAMETER, PRIVATE :: GLCNumLakes = 9
      
      !
      !  Lake basin names
      !
      CHARACTER (LEN=2), PARAMETER :: LakeName2(GLCNumLakes) =       &
            (/'sp', 'mi', 'hu', 'gb', 'sc',                          &
                'er', 'on', 'mh', 'hg'/)

      CHARACTER (LEN=3), PARAMETER :: LakeName3(GLCNumLakes) =       &
            (/'sup', 'mic', 'hur', 'geo', 'stc',                     &
              'eri', 'ont', 'mhg', 'hgb'/)

      CHARACTER(LEN=10), PARAMETER :: LakeName10(GLCNumLakes) =      &
            (/'Superior  ', 'Michigan  ', 'Huron     ',              &
              'Georgian  ', 'StClair   ', 'Erie      ',              &
              'Ontario   ', 'Mich-Huron', 'HurAndGeoB'/)

      CHARACTER (LEN=20), PARAMETER :: LakeName20(GLCNumLakes) =     &
            (/'Lake Superior       ', 'Lake Michigan       ',        &
              'Lake Huron (no GB)  ', 'Georgian Bay        ',        &
              'Lake St. Clair      ', 'Lake Erie           ',        &
              'Lake Ontario        ', 'Lake Michigan-Huron ',        &
              'Lake Huron (incl GB)'/)

      !
      !  How many subbasins are there for each lake basin?
      !  The order of entries is consistent with the name arrays:
      !   Sup, Mic, Hur, Geo, Stc, Eri, Ont, Mhg, Hgb
      !
      INTEGER, PARAMETER :: NumSubbasins(GLCNumLakes) =              &
            (/22, 27, 16, 13, 7, 21, 15, 56, 29/)

                        
      !
      !  These coordinated values come from the 1977 publication "Coordinated
      !  Great Lakes Physical Data".  To split the Lake Huron values into
      !  Huron and Georgian as we've done here I looked at our digital values
      !  for lake and land areas.  The ration for each portion was applied to 
      !  the coordinated value for Lake Huron (w/Georgian Bay) and the result
      !  is what is given here as the "coordinated" value.   (TSH  13sep2000)
      !
      !  The Michigan-Huron area is the sum of the component areas AFTER they
      !  were rounded.  This allows internal consistency for our calculations.
      !
      !  The order of entries is consistent with the name arrays:
      !   Sup, Mic, Hur, Geo, Stc, Eri, Ont, Mhg, Hgb
      !
      REAL, PARAMETER, DIMENSION(GLCNumLakes) :: CoordLakeArea =             &
            (/  0.8210e+11,   0.5780e+11,   0.4064e+11,   0.1896e+11,        &
                0.1114e+10,   0.2570e+11,   0.1896e+11,   1.1740e+11,        &
                0.5960e+11/)

      REAL, PARAMETER, DIMENSION(GLCNumLakes) :: CoordLandArea =             &
            (/  0.1280e+12,   0.1180e+12,   0.5120e+11,   0.8280e+11,        &
                0.1570e+11,   0.6100e+11,   0.6400e+11,   0.2520e+12,        &
                0.1340e+12/)
                
      !
      !  Digital areas are based on our digital maps.
      !
      !  The order of entries is consistent with the name arrays:
      !   Sup, Mic, Hur, Geo, Stc, Eri, Ont, Mhg, Hgb
      !
      REAL, PARAMETER, DIMENSION(GLCNumLakes) :: DigitLakeArea =             &
            (/ 8.19250e+10,  5.72910e+10,  4.06110e+10,  1.89490e+10,        &
               1.10900e+09,  2.54040e+10,  1.91210e+10,  1.16851e+11,        &
                5.9580e+10/)

      REAL, PARAMETER, DIMENSION(GLCNumLakes) :: DigitLandArea =             &
            (/ 1.28084e+11,  1.15804e+11,  5.04880e+10,  8.17200e+10,        &
               1.57370e+10,  6.06020e+10,  6.51180e+10,  2.48012e+11,        &
               1.32208e+11/)

      !
      !  LakeVolume comes from Croley code. SUP, MIC, ERI, ONT and HGB match
      !  the 1977 Coordinating Committee report. I'm not sure how he arrived
      !  at the HUR/GEO values. I computed the MHG quite simply by summing MIC
      !  and HGB.
      !
      !  The order of entries is consistent with the name arrays:
      !   Sup, Mic, Hur, Geo, Stc, Eri, Ont, Mhg, Hgb
      !
      REAL, PARAMETER, DIMENSION(GLCNumLakes) :: CoordLakeVolume =          &
            (/ 0.1210e+14,   0.4920e+13,   0.2761e+13,   0.0779e+13,        &
               0.3400e+10,   0.4840e+12,   0.1640e+13,   0.8460e+13,        &
               0.3540e+13/)

      !
      !  Max depths were taken from the 1977 Coordinating Committee
      !  document for SUP, MIC, HUR, STC, ERI, ONT. Georgian Bay came from
      !  Encyclopaedia Brittanica (online). Mich-Hur and HGB are just max
      !  depth of the deepest component.  Note that the STC max depth is
      !  exclusive of the dredged navigation channel, which is slightly deeper.
      !
      !  The order of entries is consistent with the name arrays:
      !   Sup, Mic, Hur, Geo, Stc, Eri, Ont, Mhg, Hgb
      !
      REAL, PARAMETER, DIMENSION(GLCNumLakes) :: LakeDepthMaxMeters =        &
            (/ 405.0,  281.0,  229.0,  165.0,    6.0,   64.0,  244.0,        &
               281.0,  229.0/)
               

      PRIVATE :: MakeLowerCase

CONTAINS
     
      !------------------------------------------------------------------
      !  Given a 3-character lake name, retrieve the lake number for that lake.
      !------------------------------------------------------------------
      INTEGER FUNCTION LakeNumberFromName3(Bsn)
      IMPLICIT  NONE
      CHARACTER (LEN=*), INTENT(IN) :: Bsn

      INTEGER :: I
      CHARACTER (LEN=3)  :: S3

      LakeNumberFromName3 = -1       ! default bad value

      S3 = 'xxx'
      IF (LEN(Bsn) .GE. 3) S3 = ADJUSTL(Bsn(1:3))
      IF (S3 .EQ. 'xxx') RETURN

      CALL MakeLowerCase(S3)
      DO I = 1, GLCNumLakes
         IF (S3 .EQ. LakeName3(I)) LakeNumberFromName3 = I
      END DO
      RETURN
     
      END FUNCTION LakeNumberFromName3

      !------------------------------------------------------------------
      !  Given a 10-character lake name, retrieve the lake number for that lake.
      !------------------------------------------------------------------
      INTEGER FUNCTION LakeNumberFromName10(LkName)
      IMPLICIT  NONE
      CHARACTER (LEN=*), INTENT(IN) :: LkName

      INTEGER :: I
      CHARACTER (LEN=10)  :: S1, S2

      LakeNumberFromName10 = -1       ! default bad value

      S1 = 'xxxxxxxxxx'
      IF (LEN_TRIM(LkName) .GT. 10) RETURN
      S1 = TRIM(ADJUSTL(LkName))
      IF (S1 .EQ. 'xxxxxxxxxx') RETURN

      CALL MakeLowerCase(S1)
      DO I = 1, GLCNumLakes
         S2 = LakeName10(I)
         CALL MakeLowerCase(S2)
         IF (TRIM(S1) .EQ. TRIM(S2)) LakeNumberFromName10 = I
      END DO
      RETURN
     
      END FUNCTION LakeNumberFromName10

      !------------------------------------------------------------------
      !  Convert a string to all lowercase
      !  Keep this subroutine private to the module.
      !------------------------------------------------------------------
      SUBROUTINE MakeLowerCase(S)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(INOUT) :: S
      INTEGER   :: I, ChOff
      ChOff = ICHAR('a') - ICHAR('A')
      DO I=1, LEN_TRIM(S)
         IF ( (S(I:I) .GE. 'A') .AND. (S(I:I) .LE. 'Z') ) THEN
            S(I:I) = CHAR(ICHAR(S(I:I)) + ChOff)
         ENDIF
      END DO
      RETURN
      END SUBROUTINE MakeLowerCase

END MODULE GL_Constants
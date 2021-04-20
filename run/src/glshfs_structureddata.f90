   MODULE GLSHFS_StructuredData
      USE GlerlDataTypesAndUnits
      
      !
      !   Data type that contains the meteorological forcing data for LBRM.
      !   There are 3 types of data that can be stored here.
      !     1 = TMin   (min daily airtemp value, deg C)
      !     2 = TMax   (max daily airtemp value, deg C)
      !     3 = Prec   (total daily precip value, mm)
      !   
      TYPE Met_LBRM
         INTEGER :: SSeq, ESeq
         INTEGER, DIMENSION(3)  :: Types
         INTEGER, DIMENSION(3)  :: Units
         REAL,    DIMENSION(:,:), ALLOCATABLE :: Values      ! dimension = (3, numdays)
      END TYPE Met_LBRM
   
      !
      !   Data type that contains the meteorological forcing data for LLTM.
      !   There are 6 types of data that can be stored here.
      !     1 = airtemp   (mean daily value, deg C)
      !     2 = dewpoint  (mean daily value, deg C)
      !     3 = windspeed (mean daily value, m/s)
      !     4 = cloud     (mean daily value, %)
      !     5 = net longwave radiation (daily total value, watts/m2)
      !     6 = incident radiation     (daily total value, watts/m2)
      !
      TYPE Met_LLTM
         INTEGER :: SSeq, ESeq
         LOGICAL :: CloudIsValid, RadIsValid
         INTEGER, DIMENSION(6)   :: Types
         INTEGER, DIMENSION(6)   :: Units
         REAL,    DIMENSION(:,:), ALLOCATABLE :: Values      ! dimension = (6, numdays)
      END TYPE Met_LLTM
      
      !
      !  Data type that contains the meteorological data for a subbasin.
      !  There are 9 types of data stored here.
      !     1 = TMin   (min daily airtemp value, deg C)
      !     2 = TMax   (max daily airtemp value, deg C)
      !     3 = Prec   (total daily precip value, mm)
      !     4 = airtemp   (mean daily value, deg C)
      !     5 = dewpoint  (mean daily value, deg C)
      !     6 = windspeed (mean daily value, m/s)
      !     7 = cloud     (mean daily value, %)
      !     8 = net longwave radiation (daily total value, watts/m2)
      !     9 = incident radiation     (daily total value, watts/m2)
      !
      TYPE Met_Subbasin
         INTEGER :: SSeq, ESeq
         INTEGER, DIMENSION(9)   :: Types
         INTEGER, DIMENSION(9)   :: Units
         REAL,    DIMENSION(:,:), ALLOCATABLE :: Values      ! dimension = (9, numdays)
      END TYPE Met_Subbasin
   
      !
      !  Type that contains a collection of multiple time-series of a single
      !  data type (e.g. precipitation).
      !  The values for DataType and DataUnit come from the GDT_* and GDU_* definitions.
      !  
      TYPE MultiSeries
         INTEGER :: SSeq, ESeq
         INTEGER :: DataType, DataUnit
         CHARACTER(LEN=20), DIMENSION(:),   ALLOCATABLE :: SeriesName
         REAL,              DIMENSION(:,:), ALLOCATABLE :: Values      ! dimension = (series, numdays)
      END TYPE MultiSeries
   
   
   
   
   
   
   
   END MODULE GLSHFS_StructuredData
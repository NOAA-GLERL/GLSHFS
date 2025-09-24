
#  Detect the O/S and set a flag accordingly. This flag is used for
#  conditional compilation of some files
#  This is a simplification of stuff I found here:
#    http://stackoverflow.com/questions/714100/os-detecting-makefile
#
ifeq ($(OS),Windows_NT)
   OSFLAG = -D __windows__
	ThisOS := Windows
   MyRM = del
else
	UNAME_S := $(shell uname -s)
   ifeq ($(UNAME_S),linux)
      OSFLAG = -D __linux__
	endif
   MyRM = rm
endif

#
#  Specify the compiler and any flags I want.
#
#  Notice that I am turning on all warnings (-Wall), but then turning OFF
#  a few categories. I hate seeing extra warnings that make it seem there might
#  be a problem when there is not.  The ones I turn off are:
#   1) unused-function
#      I have defined a few subroutines/functions that are currently unused.
#      That is ok.
#   2) target-lifetime
#      A few of my functions create pointer variables that are passed back to
#      the caller. Obviously, those variables in the caller could potentially
#      remain assigned even after the object gets destroyed. But I don't need
#      an ugly warning message about that.
#   3) surprising
#      This one is a bit more complicated.  When "surprising" is enabled,
#      a large number of warnings are generated that refer to the destructor
#      routines of the various data objects (TDlyData, TDlyDataOneLocation, etc).
#
#      The specific message is
#        Warning: Only array FINAL procedures declared for derived type 'tdlydata' defined at (1), suggest also scalar one [-Wsurprising]
#
#      I have researched this and tried several things to make the compiler "happy", 
#      but have had no success. Best I can tell, my code is valid and correct, though
#      I am certainly willing to accept additional correction if a smarter programmer
#      can provide it.  Given that my tests with memory leak detection software (valgrind)
#      have all come back clean, I am willing to proceed on the assumption that this issue
#      is either:
#        1) A lingering bug in the gfortran compiler (version 5.3.0)
#        2) Weak understanding of the issue on my part, that may "come back to bite me"
#           at some point in the future. But for now it seems to work.
#        3) Only an issue if we were to try using parallelization (this seems to be the
#           focus of the warning, from what I researched).
#      Whatever the case may be, I am moving on with the assumption that I am ok.  I am
#      turning off that class of warning messages.
#
#
CC = gfortran
ifeq ($(ThisOS), Windows)
   # CFLAGS = -Wall -Wno-unused-function -Wno-target-lifetime -Wno-surprising -fcheck=all -static
   CFLAGS = -Wall -Wno-unused-function -Wno-target-lifetime -Wno-surprising -static
else
   # for using valgrind... compile with -g and -O0
  # CFLAGS = -g -O0
  # CFLAGS = -g -O1
   CFLAGS = -fcheck=all -g -O1
endif


#
#  Standard make target stuff
#
default: glshfs
	$(info Check run dir to see if GLSHFS binary was created)


#
#  Default setup for programs that just need glshfs_util stuff
#  Anything that needs more (most stuff in the GLSHFS project) will need a
#  specific code block for it.
#
%: %.o  glshfs_util.o cpp_util.o
	$(CC) $(CFLAGS) -o $@ $< glshfs_util.o cpp_util.o

%.o: %.f90 glshfs_util.mod
	$(CC) -c -o $@ $< $(CFLAGS)
	

#
#  OS and compiler testing
#
os_test: os_test.cpp
	gcc $(OSFLAG) os_test.cpp


#
#   Specific blocks for executables
#
glshfs:    glshfs.o buildsubbasinmet.o gl_constants.o glshfs_util.o glshfs_global.o cpp_util.o    \
           glerldatatypesandunits.o  dailydatastructures.o dailydatacollections.o                 \
           basininfo.o glshfs_configfile.o glshfs_files.o stndata_rangecheck.o                    \
           buildsubbasinmet.o                                                                     \
           stndata_aggregation.o glerlmaps.o subbasin_met.o glshfs_structureddata.o               \
           outlook_met.o glshfs_lbrm.o lbrm.o glshfs_lltm.o lltm.o  summaryfiles.o 
	$(CC) $(CFLAGS) -o ../run/glshfs glshfs.o glshfs_util.o gl_constants.o cpp_util.o glshfs_global.o       \
	                   glerldatatypesandunits.o dailydatastructures.o glshfs_files.o                 \
							 basininfo.o glshfs_configfile.o stndata_rangecheck.o stndata_aggregation.o	 \
							 dailydatacollections.o glerlmaps.o subbasin_met.o buildsubbasinmet.o          \
                      glshfs_structureddata.o outlook_met.o                                         \
                      glshfs_lbrm.o lbrm.o glshfs_lltm.o lltm.o summaryfiles.o

glshfs2:   glshfs2.o buildsubbasinmet.o gl_constants.o glshfs_util.o glshfs_global.o cpp_util.o   \
           glerldatatypesandunits.o  dailydatastructures.o dailydatacollections.o                 \
           basininfo.o glshfs_configfile.o glshfs_files_newer.o stndata_rangecheck.o              \
           buildsubbasinmet.o                                                                     \
           stndata_aggregation.o glerlmaps.o subbasin_met.o glshfs_structureddata.o               \
           outlook_met.o glshfs_lbrm.o lbrm.o glshfs_lltm.o lltm.o  summaryfiles.o 
	$(CC) $(CFLAGS) -o glshfs2 glshfs2.o glshfs_util.o gl_constants.o cpp_util.o glshfs_global.o       \
	                   glerldatatypesandunits.o dailydatastructures.o glshfs_files_newer.o           \
							 basininfo.o glshfs_configfile.o stndata_rangecheck.o stndata_aggregation.o	 \
							 dailydatacollections.o glerlmaps.o subbasin_met.o buildsubbasinmet.o          \
                      glshfs_structureddata.o outlook_met.o                                         \
                      glshfs_lbrm.o lbrm.o glshfs_lltm.o lltm.o summaryfiles.o

                      
combineforlauren: combineforlauren.o  glshfs_util.o cpp_util.o gl_constants.o glshfs_files.o         \
                  dailydatastructures.o glerldatatypesandunits.o dailydatacollections.o
	$(CC) $(CFLAGS) -o combineforlauren combineforlauren.o glshfs_util.o cpp_util.o gl_constants.o    \
                      glshfs_files.o dailydatastructures.o glerldatatypesandunits.o dailydatacollections.o

                      
                      
                      
prep_cmip_files: prep_cmip_files.o glshfs_util.o glshfs_global.o cpp_util.o gl_constants.o 
	$(CC) $(CFLAGS) -o prep_cmip_files prep_cmip_files.o glshfs_util.o glshfs_global.o cpp_util.o gl_constants.o 
   
test_lltm: test_lltm.o lltm.o gl_constants.o glshfs_util.o glshfs_files.o glshfs_structureddata.o         \
            glerldatatypesandunits.o cpp_util.o dailydatastructures.o dailydatacollections.o
	$(CC) $(CFLAGS) -o test_lltm test_lltm.o lltm.o gl_constants.o glshfs_util.o glshfs_files.o            \
                      glshfs_structureddata.o glerldatatypesandunits.o cpp_util.o dailydatastructures.o   \
                      dailydatacollections.o

setup_forecast_cmip:  setup_forecast_cmip.o glshfs_util.o cpp_util.o glshfs_global.o gl_constants.o     \
                        glerldatatypesandunits.o dailydatastructures.o glshfs_files.o glshfs_configfile.o dailydatacollections.o
	$(CC) $(CFLAGS) -o setup_forecast_cmip setup_forecast_cmip.o glshfs_util.o cpp_util.o glshfs_global.o gl_constants.o     \
                        glerldatatypesandunits.o dailydatastructures.o glshfs_files.o glshfs_configfile.o dailydatacollections.o
                        
                        
setup_hindcast: setup_hindcast.o  glshfs_util.o cpp_util.o gl_constants.o
	$(CC) $(CFLAGS) -o setup_hindcast setup_hindcast.o glshfs_util.o cpp_util.o gl_constants.o

setup_hindcast_cmip: setup_hindcast_cmip.o  glshfs_util.o cpp_util.o gl_constants.o        \
                     glerldatatypesandunits.o glshfs_files.o dailydatastructures.o dailydatacollections.o
	$(CC) $(CFLAGS) -o setup_hindcast_cmip setup_hindcast_cmip.o glshfs_util.o cpp_util.o   \
                      gl_constants.o glerldatatypesandunits.o glshfs_files.o dailydatastructures.o dailydatacollections.o

z_test:  z_test.o glshfs_util.o cpp_util.o
	$(CC) $(CFLAGS) -o z_test z_test.o glshfs_util.o cpp_util.o
                      
                      
#
#   Specific blocks for object code
#

glshfs.o:  glshfs.f90 glshfs_util.o glshfs_global.o glerldatatypesandunits.o gl_constants.o      \
           dailydatastructures.o dailydatacollections.o basininfo.o                              \
           glshfs_files.o stndata_rangecheck.o stndata_aggregation.o                             \
           glerlmaps.o subbasin_met.o buildsubbasinmet.o outlook_met.o                           \
           glshfs_lbrm.o lbrm.o glshfs_lltm.o lltm.o summaryfiles.o cpp_util.o
	$(CC) $(CFLAGS) -c glshfs.f90
						
glshfs2.o: glshfs.f90 glshfs_util.o glshfs_global.o glerldatatypesandunits.o gl_constants.o      \
           dailydatastructures.o dailydatacollections.o basininfo.o                              \
           glshfs_files_newer.o stndata_rangecheck.o stndata_aggregation.o                       \
           glerlmaps.o subbasin_met.o buildsubbasinmet.o outlook_met.o                           \
           glshfs_lbrm.o lbrm.o glshfs_lltm.o lltm.o summaryfiles.o cpp_util.o
	$(CC) $(CFLAGS) -c glshfs.f90 -o glshfs2.o
						

						
basininfo.o basininfo.mod: basininfo.f90 glshfs_util.o cpp_util.o
	$(CC) $(CFLAGS) -c basininfo.f90

buildsubbasinmet.o: buildsubbasinmet.f90 glshfs_util.o glshfs_global.o glerldatatypesandunits.o     \
                    dailydatastructures.o dailydatacollections.o basininfo.o glshfs_files.o         \
                    stndata_rangecheck.o stndata_aggregation.o glerlmaps.o subbasin_met.o cpp_util.o
	$(CC) $(CFLAGS) -c buildsubbasinmet.f90

combineforlauren.o: combineforlauren.f90  glshfs_util.o glshfs_global.o glshfs_files.o cpp_util.o   \
                    dailydatastructures.o glerldatatypesandunits.o dailydatacollections.o
	$(CC) $(CFLAGS) -c combineforlauren.f90

glshfs_configfile.o: glshfs_configfile.f90 glshfs_util.o glshfs_global.o cpp_util.o
	$(CC) $(CFLAGS) -c glshfs_configfile.f90

glshfs_lbrm.o: glshfs_lbrm.f90  glshfs_util.o glshfs_global.o glshfs_files.o lbrm.o cpp_util.o
	$(CC) $(CFLAGS) -c glshfs_lbrm.f90

glshfs_lltm.o: glshfs_lltm.f90  glshfs_global.o glshfs_util.o glshfs_files.o lltm.o dailydatastructures.o cpp_util.o
	$(CC) $(CFLAGS) -c glshfs_lltm.f90

glshfs_structureddata.o: glshfs_structureddata.f90  glerldatatypesandunits.o
	$(CC) $(CFLAGS) -c glshfs_structureddata.f90

lbrm.o: lbrm.f90  glshfs_util.o glshfs_files.o cpp_util.o
	$(CC) $(CFLAGS) -c lbrm.f90
   
lltm.o: lltm.f90  gl_constants.o glshfs_util.o glshfs_files.o glshfs_structureddata.o    \
                  glerldatatypesandunits.o cpp_util.o
	$(CC) $(CFLAGS) -c lltm.f90
  
outlook_met.o: outlook_met.f90 glshfs_util.o glshfs_global.o glerldatatypesandunits.o dailydatastructures.o   \
						glshfs_files.o glshfs_structureddata.o cpp_util.o
	$(CC) $(CFLAGS) -c outlook_met.f90
   
prep_cmip_files.o: prep_cmip_files.f90 glshfs_util.o glshfs_global.o cpp_util.o gl_constants.o
	$(CC) $(CFLAGS) -c prep_cmip_files.f90
   
setup_forecast_cmip.o:  setup_forecast_cmip.f90 glshfs_util.o cpp_util.o glshfs_global.o gl_constants.o     \
                        glerldatatypesandunits.o dailydatastructures.o glshfs_files.o glshfs_configfile.o   \
                        dailydatacollections.o
	$(CC) $(CFLAGS) -c setup_forecast_cmip.f90

stndata_aggregation.o:  stndata_aggregation.f90 glshfs_util.o glshfs_global.o glerldatatypesandunits.o basininfo.o   \
                        glshfs_files.o glshfs_configfile.o cpp_util.o dailydatastructures.o dailydatacollections.o
	$(CC) $(CFLAGS) -c stndata_aggregation.f90

stndata_rangecheck.o:  stndata_rangecheck.f90 glshfs_util.o glshfs_global.o   \
                       dailydatastructures.o glshfs_files.o cpp_util.o
	$(CC) $(CFLAGS) -c stndata_rangecheck.f90

subbasin_met.o:   subbasin_met.f90 glshfs_util.o glshfs_global.o cpp_util.o                       \
                  glerldatatypesandunits.o dailydatastructures.o dailydatacollections.o basininfo.o    \
						glshfs_files.o glerlmaps.o glshfs_structureddata.o
	$(CC) $(CFLAGS) -c subbasin_met.f90

summaryfiles.o:  summaryfiles.f90 glshfs_util.o glshfs_global.o gl_constants.o glshfs_files.o      \
                        dailydatastructures.o glerldatatypesandunits.o basininfo.o cpp_util.o
	$(CC) $(CFLAGS) -c summaryfiles.f90

thiessen.o thiessen.mod:  thiessen.f90 glerlmaps.o glshfs_util.o glshfs_global.o cpp_util.o
	$(CC) $(CFLAGS) -c thiessen.f90
   
test_lltm.o:  test_lltm.f90 lltm.o gl_constants.o glshfs_util.o glshfs_files.o structured_data.o     \
              glerldatatypesandunits.o  cpp_util.o
	$(CC) $(CFLAGS) -c test_lltm.f90
   
setup_hindcast.o:  setup_hindcast.f90 glshfs_util.o cpp_util.o gl_constants.o
	$(CC) $(CFLAGS) -c setup_hindcast.f90

setup_hindcast_cmip.o:  setup_hindcast_cmip.f90 glshfs_util.o cpp_util.o gl_constants.o      \
                        glerldatatypesandunits.o glshfs_files.o dailydatastructures.o dailydatacollections.o
	$(CC) $(CFLAGS) -c setup_hindcast_cmip.f90

	
#
#  The common/utility stuff (data structures, file read/write, etc.)
#
dailydatacollections.o: dailydatacollections.f90 glshfs_util.o cpp_util.o glshfs_global.o glerldatatypesandunits.o   \
                        gl_constants.o dailydatastructures.o
	$(CC) $(CFLAGS) -c dailydatacollections.f90

dailydatastructures.o: dailydatastructures.f90  glshfs_util.o cpp_util.o glshfs_global.o glerldatatypesandunits.o
	$(CC) $(CFLAGS) -c dailydatastructures.f90

gl_constants.o: gl_constants.f90
	$(CC) $(CFLAGS) $(OSFLAG) -c gl_constants.f90

glerldatatypesandunits.o: glerldatatypesandunits.f90
	$(CC) $(CFLAGS) -c glerldatatypesandunits.f90						 
						 
glerlmaps.o: glerlmaps.f90  glshfs_util.o cpp_util.o glshfs_global.o glerldatatypesandunits.o
	$(CC) $(CFLAGS) -c glerlmaps.f90

glshfs_files.o: glshfs_files.f90 glshfs_util.o cpp_util.o glshfs_global.o glerldatatypesandunits.o      \
                dailydatastructures.o dailydatacollections.o
	$(CC) $(CFLAGS) -c glshfs_files.f90
	
	
#
#  Miscellaneous utility routines
#
#  Note that the source code for glshfs_util and glshfs_global include 
#  pre-processor directives that are conditional on the current O/S.  
#  The gfortran compiler will only respond to pre-processor directives if 
#  the both the file extension is F90 and the compile command specifies 
#  the file with extension of F90 (UPPERCASE F IS REQUIRED). 
#  I have seen discussions on the web that indicate this is a common
#  convention (Intel compiler, etc).
#
glshfs_util.o:  glshfs_util.F90 cpp_util.o
	$(CC) $(CFLAGS) $(OSFLAG) -c glshfs_util.F90

glshfs_global.o: glshfs_global.F90 glshfs_util.o cpp_util.o gl_constants.o
	$(CC) $(CFLAGS) $(OSFLAG) -c glshfs_global.F90

   
#
#  Build special C++ routines that fill gaps in Fortran functionality.
#
cpp_util.o: cpp_util.cpp
	gcc $(OSFLAG) -c -g -O1 cpp_util.cpp

#
#  clean everything up
#         
clean:
ifeq ($(OS),Windows_NT)
	del *.mod
	del *.o
else
	rm -f glshfs
	rm -f *.mod
	rm -f *.o
endif
 

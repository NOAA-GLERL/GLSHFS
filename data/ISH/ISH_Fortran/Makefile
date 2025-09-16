#  Build process_ish stuff from the various pieces
#
#  Note that gfortran modules cause issues for make. For some discussion see:
#      http://lagrange.mechse.illinois.edu/f90_mod_deps/
#  I tried to implement some of the suggestions there, but they did not work,
#  maybe because I have not restricted source code files to only containing a
#  single module. I suspect, though, that a bigger problem is the bug that 
#  exists in my current version of gfortran (on Windows, anyway).  When 
#  compiling a module, the current gfortran always chokes if there is an
#  existing .mod file. It ought to simply overwrite it, but it does not. 
#  Instead, it fails and stops. That is why there are all of the $(RM) .mod
#  statements in here. I added the step to remove the .old mod file.
#  The net result is that we end up recompiling everything, every time.
#  Not ideal, but not a huge penalty for a relatively small project like this.
#  - Tim Hunter, 02 October 2015
#

#
#  Specify the compiler and any flags I want
#
CC = gfortran
CFLAGS = -Wall -fcheck=all

#  Detect the O/S and set a flag accordingly. This flag is used for
#  conditional compilation of glshfs_util.F90
#  This is a simplification of stuff I found here:
#    http://stackoverflow.com/questions/714100/os-detecting-makefile
#
ifeq ($(OS),Windows_NT)
   OSFLAG = -D __windows__
	ThisOS := Windows
else
	UNAME_S := $(shell uname -s)
   ifeq ($(UNAME_S),linux)
      OSFLAG = -D __linux__
	endif
endif


#
#  Standard make target stuff
#
default: process_ish

debug: proc_ish_dbg.o readwritemetcsv.o  dailymetstndata.o \
             metdatatypesandunits.o glshfs_util.o cpp_util.o
	$(CC) $(CFLAGS) -o proc_ish_dbg proc_ish_dbg.o readwritemetcsv.o \
             dailymetstndata.o  metdatatypesandunits.o glshfs_util.o  cpp_util.o

cloud: get_ish_cloud.o readwritemetcsv.o  dailymetstndata.o \
             metdatatypesandunits.o glshfs_util.o
	$(CC) $(CFLAGS) -o get_ish_cloud get_ish_cloud.o readwritemetcsv.o \
             dailymetstndata.o  metdatatypesandunits.o glshfs_util.o
				 
process_ish: process_ish.o readwritemetcsv.o  dailymetstationdata.o \
             metdatatypesandunits.o glshfs_util.o cpp_util.o
	$(CC) $(CFLAGS) -o process_ish process_ish.o readwritemetcsv.o \
             dailymetstationdata.o  metdatatypesandunits.o glshfs_util.o cpp_util.o





proc_ish_dbg.o: proc_ish_dbg.f90 glshfs_util.o dailymetstationdata.o        \
            metdatatypesandunits.o readwritemetcsv.o cpp_util.o
	$(CC) $(CFLAGS) $(OSFLAG) -c proc_ish_dbg.f90

process_ish.o: process_ish.f90 glshfs_util.o dailymetstationdata.o          \
            metdatatypesandunits.o readwritemetcsv.o cpp_util.o
	$(CC) $(CFLAGS) $(OSFLAG) -c process_ish.f90

get_ish_cloud.o: get_ish_cloud.f90 glshfs_util.o dailymetstationdataset.o   \
            metdatatypesandunits.o readwritemetcsv.o cpp_util.o
	$(CC) $(CFLAGS) $(OSFLAG) -c get_ish_cloud.f90

readwritemetcsv.o: readwritemetcsv.f90 glshfs_util.o                        \
            metdatatypesandunits.o dailymetstationdata.o cpp_util.o
	$(CC) $(CFLAGS) -c readwritemetcsv.f90
							 							 
dailymetstationdata.o: dailymetstationdata.f90 glshfs_util.o metdatatypesandunits.o 
	$(CC) $(CFLAGS) -c dailymetstationdata.f90
							 
metdatatypesandunits.o: metdatatypesandunits.f90
	$(CC) $(CFLAGS) -c metdatatypesandunits.f90
							 


#
#  The source code for glshfs_util needs to know which O/S is being used in order to
#  do some conditional compilation. But the gfortran compiler will only handle it
#  properly if the file extension is F90 (UPPERCASE F IS REQUIRED).  So do some
#  fancy (i.e. ugly) stuff to make that work.
#
glshfs_util.o:  glshfs_util.F90 cpp_util.o
	$(CC) $(CFLAGS) $(OSFLAG) -c glshfs_util.F90

#
#  Build special C++ routines that fill gaps in Fortran functionality.
#
cpp_util.o: cpp_util.cpp
	gcc $(OSFLAG) -c -g -O1 cpp_util.cpp

#
#  clean everything up
#         
clean:
	$(RM) *.mod
	$(RM) *.o
	$(RM) process_ish.exe
	$(RM) process_ish
 

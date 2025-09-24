//-----------------------------------------------------------------------------
//
//  This C++ source file includes small utility functions that fill holes
//  in what Fortran can do. In terms of implementing portable system-level
//  functionality, Fortran has come a long way since Fortran 1977, but it
//  still has gaps.
//
//  Specifically, the routines here provide the functionality to:
//     1) Get the current working directory and assign that value
//        to a character string.
//        
//     2) Build a text file containing the names of all files meeting 
//        a particular file specification.  
//        
//
//  Note that the #ifdef tests require that a variable __windows__ be passed
//  to the compiler.  This is in the format, for example:
//    gcc -D __windows__ cpp_util.cpp
//
//  The "-D __windows__" is the relevant directive in the example.
//  Note that it is case-sensitive and must have the double-underscore 
//  characters at the beginning and end.
//-----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#define MAX_PATH 260
// #define GETCWD getcwd

#ifdef __windows__     /* Windows */
   #include <windows.h>
   #include <tchar.h>
#else             /* Unix/Linux */
   #include <unistd.h>
   #include <fnmatch.h>
   char *uglyGlobalStringForDList;
#endif

//---------------------------------------------------------------- 
//  I also had to add the nuance of the extern "C" bit, which is explained here:
//    https://people.sc.fsu.edu/~jburkardt/f_src/f90_calls_c++/f90_calls_c++.html
//---------------------------------------------------------------- 

extern "C"
{ 
 void getWorkingDirectory(char *cwd);
 void getEnvironmentVariable(const char *name, char *value);
 void makefilelist(int *modeR, char *fspec, char *outname);
 void file_info(const char* filename, int* mode, int* exist, int* time);
 void file_is_directory(char *filename, int *isdir);
 void create_directory(char *dirname);
}
 

//-------------------------------------------------
//  Get the current working directory:
//  
//  Shamelessly stolen/modified from this post:
//  http://stackoverflow.com/questions/30279228/is-there-an-alternative-to-getcwd-in-fortran-2003-2008
//-------------------------------------------------
void getWorkingDirectory(char *cwd)
{
   char d[MAX_PATH];
   getcwd(d, MAX_PATH);
   strncpy(cwd, d, strlen(d));
}

//-------------------------------------------------
//  Get the value of a defined environment variable
//  
//  Shamelessly stolen/modified from this post:
//  http://stackoverflow.com/questions/30279228/is-there-an-alternative-to-getcwd-in-fortran-2003-2008
//-------------------------------------------------
void getEnvironmentVariable(const char *name, char *value)
{
   const char* e = getenv(name);
   if (e != NULL) {
      strcpy(value, e); }
   else  {
      strcpy(value, ""); }
}

//-------------------------------------------------
//  Build a file list:
//
//  This routine is intended to be called from my Fortran code.
//  It is the equivalent of:
//     CALL SYSTEM('DIR/B '//TRIM(fspec)//' > '//TRIM(fname))
//  or something kinda like the linux
//     CALL SYSTEM('ls -al '//TRIM(fspec)//' > '//TRIM(fname))
//
//  But it implements that functionality without using the CALL SYSTEM,
//  which momentarily causes a popup cmd window (very very very ugly).
//
//  Note that it is Windows-specific. I will need to rewrite it
//  for use in linux.
//
//  For parameter passing, I am following the coding method found at:
//    https://gcc.gnu.org/onlinedocs/gfortran/Interoperable-Subroutines-and-Functions.html 
//
//
//  To call this from Fortran, use code like the following example:
//
//       INTEGER :: ModeC
//       CHARACTER(LEN=200) :: FSpecC, FNameC
//
//       use iso_c_binding, only: C_CHAR, C_NULL_CHAR
//       interface
//         subroutine makefilelist(mode, fspec, fname) bind(C)
//           use iso_c_binding
//           integer (kind=c_int)   :: mode
//           character(kind=c_char) :: fspec, fname
//         end subroutine makefilelist
//       end interface
//
//       Mode   = 1                                 ! 1=filename only; 2=name, size, modified date
//       FSpecC = TRIM('*.f90') // CHAR(0)          ! create null-terminated string
//       FNameC = TRIM('FileList.txt') // CHAR(0)   ! create null-terminated string
//       CALL makefilelist(Mode, FSpecC, FNameC)
//
//  When mode == 1, this routine produces a simple bare filename list,
//
//  When mode == 2, it produces a filename list, plus a second column with last modified date.
//                  The two entries are separated by a "|" character
//
//  When mode == 3, it produces a filename list with last modified date and adds the file size.
//                  The three entries are separated by a "|" character
//
//----------------------------------------------

#ifdef __windows__     /* Windows Version */
   void makefilelist(int *modeR, char *fspec, char *outname) 
   {
      WIN32_FIND_DATA fd;
      HANDLE hFind;
      int mode = *modeR;
      
      hFind = FindFirstFile(fspec, &fd); 
      if (hFind != INVALID_HANDLE_VALUE) 
      { 
         FILE *f = fopen(outname, "w");
         do {
            if (!(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
               if (mode == 1) {
                  fprintf (f, "%s\n", fd.cFileName);
               }   
               else if ((mode == 2) || (mode == 3)) {
                  FILETIME    ft;
                  SYSTEMTIME  stUTC, stLocal;
                  DWORD       nFileSizeHigh, nFileSizeLow;
                  
                  ft = fd.ftLastWriteTime;
                  FileTimeToSystemTime(&ft, &stUTC);
                  SystemTimeToTzSpecificLocalTime(NULL, &stUTC, &stLocal);
                  
                  //
                  //  Build a string showing the date and time.
                  //
                  LPTSTR  dtString;
                  DWORD   dwRet;
                  char  dtStr[25];
                  int i = sprintf(dtString, "%02d/%02d/%04d-%02d:%02d",
                                   stLocal.wMonth, stLocal.wDay, stLocal.wYear,
                                   stLocal.wHour, stLocal.wMinute);
                                   
                  //
                  //  Calculate the file size. Allow for very large file sizes.
                  //
                  
                  long fSize = (fd.nFileSizeHigh * (MAXDWORD+1)) + fd.nFileSizeLow;
                  
                  if (mode == 2) {
                     fprintf(f, "%s | %s\n", fd.cFileName, dtString);
                  } else {
                     fprintf(f, "%s | %s | %ld\n", fd.cFileName, dtString, fSize);
                  }
               }
            }
         } while (FindNextFile(hFind, &fd)); 
         FindClose(hFind); 
         fclose(f);
      } 
   }
   
#else     /* LINUX VERSION */
   
   //
   //  fnmatch returns: "Zero if string matches pattern, 
   //                    FNM_NOMATCH if there is no match or
   //                    another nonzero value if there is an error."
   //
   int myfilter(const struct dirent *entry)
   {
      int fmv = fnmatch(uglyGlobalStringForDList, entry->d_name, FNM_CASEFOLD);
      if (fmv == 0) return 1;        // file matches
      return 0;                      // no good
   }   
   
   //
   //
   void makefilelist(int *modeR, char *fspec, char *outname) 
   {
      struct dirent **namelist;
      int  p, n;
      char *path, *fname, *lastSlash;

      //
      //  allocate memory for path variable
      //      
      path = (char *) malloc(250);
      
      //
      //  separate path and filename
      //
      lastSlash = strrchr(fspec, '/');
      if (!lastSlash) {
         strcpy(path, "./");
         fname = fspec;
      }        
      else {  
         fname = lastSlash + 1;
         int sz = lastSlash - fspec + 1;  // including that last slash character
         strncpy(path, fspec, sz);  path[sz] = 0;
      }

      int i = strlen(fname);
      uglyGlobalStringForDList = (char *) malloc(i+1);
      strcpy(uglyGlobalStringForDList, fname);      // from fname to ugly
      FILE *f = fopen(outname, "w");

      n = scandir(path, &namelist, myfilter, alphasort);
      if (n < 0) {
         printf("Error running scandir()\n");
         free(namelist);
      }
      else {
         for (int i=0; i<n; i++) {
            fprintf(f, "%s\n", namelist[i]->d_name);
            free(namelist[i]);
         }
         free(namelist);
      }
      fclose(f);
      
      if (path) free(path);
      free(uglyGlobalStringForDList);
   }
#endif        /* the makefilelist routine */


//-------------------------------------------------
//  Get info about a particular file:
//  This can be especially useful to determine if <filename> is
//  a regular file or a directory.
//-------------------------------------------------
void file_info(const char* filename, int* mode, int* exist, int* time) 
{
  int k;
  struct stat buf;
  k = stat(filename,&buf);
  if (k != 0) {
    *mode  = 0;
    *exist = 0;
    *time  = 0;
  } else {
    *mode = buf.st_mode;
    if (*mode == 0) *exist = 0; else *exist = 1;
    *time = buf.st_mtime;
  }
}

//----------------------------------------
//  Test a "filename" to see if it is a directory
//----------------------------------------
void file_is_directory(char *filename, int *isdir) 
{
   int k;
   struct stat buf;
   *isdir=0;              // default response is FALSE
   k = stat(filename,&buf);
   if (k != 0) return;
   if (S_ISDIR(buf.st_mode)) *isdir=1;
}

//----------------------------------------
//  create a directory
//----------------------------------------
void create_directory(char *dirname)
{
   
#ifdef __windows__     /* Windows Version */
   CreateDirectory(dirname, NULL);
#else                  /* LINUX VERSION */
   struct stat st = {0};
   if (stat(dirname, &st) == -1) {
      mkdir(dirname, 0777);
   }
#endif
}


import sys, fileinput, tempfile, gzip, subprocess, ftplib
# from ftplib import FTP

#-----------------------------------------------------
def parseLineForID(line):
   usaf = ''
   wban = ''
   blankID = ''
   
   # verify line format is valid for a station (i.e. not header, etc)
   if len(line) < 48: return blankID
   if len(line[ 6: 7].strip()) != 0: return blankID 
   if len(line[12:13].strip()) != 0: return blankID
   if len(line[42:43].strip()) != 0: return blankID
   if len(line[45:46].strip()) != 0: return blankID
   
   usaf = line[0:6].strip()
   wban = line[7:12].strip()
   if len(usaf) < 6: return blankID
   if len(wban) < 5: return blankID
   
   retStr = usaf.strip() + '-' + wban.strip()
   return retStr

#-----------------------------------------------------
def procStation(stationID, yr1, yr2):
   print('Retrieve files for ' + stationID)
   lumpName = 'lumped_' + stationID.strip() + '.txt'
   goodYears = 0
   lumpF = open(lumpName, 'wb')
   for y in range (yr1, yr2+1):
      yrStr = str(y)
      server_gzName = yrStr.strip() + '/' + stationID.strip() + '-' + yrStr.strip() + '.gz'
      local_gzName = stationID.strip() + '.gz'
      ftp = ftplib.FTP('ftp.ncdc.noaa.gov', 'anonymous', 'tim.hunter@noaa.gov')
      ftp.cwd('/pub/data/noaa/')
      worked = False
      try:
         ftp.retrbinary('RETR %s' % server_gzName, open("temp.gz", "wb").write)
         worked = True
      except:
         worked = False
      ftp.quit()
      
      if worked:
         goodYears += 1
         with gzip.open("temp.gz", "rb") as z:
            file_lines = z.readlines()
            for line in file_lines:
               lumpF.write(line)
         del(local_gzName)
      
   lumpF.close()
   
   #
   #  Assuming we got some data via ftp, process that lumped file
   #  using the external program Process_ISH (written in Fortran, btw).
   #  p.wait is used to make sure the process finishes and all of
   #  the output is done before going to the next station.
   if (goodYears > 0): 
     p = subprocess.Popen(["Process_ISH", lumpName])
     p.wait()
   
   return
   
#-----------------------------------------------------
#-----------------------------------------------------
#  Main application
#
#  Command-line processing
#
numArgs = len(sys.argv)
if numArgs < 4:
   print('USAGE: runabunch filename year1 year2')
   print('   filename = name of a ISH history-format file')
   print('Each line of the input file will be parsed for station ID and then')
   print('the ncdc ftp site will be accessed to get all of files in the')
   print('specified year range for that station.')
   sys.exit()
   
   
filename = sys.argv[1]
year1 = int(sys.argv[2])
year2 = int(sys.argv[3])

ok = True
if year1 < 1899:  ok = False
if year2 > 2029:  ok = False
if year2 < year1: ok = False
if not ok:
   print('Invalid year(s) specified')
   sys.exit()

#
#  For each line in the history file, parse out the
#  station ID part and use that to process.
#
histFile = open(filename, "rU", 1)
i = 0
for line in histFile:
   i += 1
   stationID = parseLineForID(line)
   if len(stationID) >= 12:
      procStation(stationID, year1, year2)
      sys.stdout.flush()
   else:
      print('line ', i, ' was not a valid one')
      sys.stdout.flush()

histFile.close()   

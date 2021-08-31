import sys, os, subprocess
import gzip, ftplib, time
# import tempfile, fileinput

class ClassStnMeta:
    id          = ''
    name        = ''
    lat         = -9999.9
    long        = -9999.9    
    use_string  = '.......'       # tmin,tmax,prec,tavg,dewp,wind,cldc
    exclude_yrs = []

#-----------------------------------------------------
#  Parses a basic NCEI station history file that lists stations.
#  Returns a single ClassStnMeta object.
#  Since the NCEI file doesn't specify data type usage or excluded
#  years, we fill those in as use everything for LLTM.
#
def parseLineForMeta(line):
    sm = ClassStnMeta()
    try:
        sm.id   = line[0:6].strip() + '-' + line[7:12]
        sm.name = line[13:43].strip()
        sm.lat  = float(line[57:64].strip())
        sm.long = float(line[65:73].strip())
        sm.use_string = '...YYYY'
        sm.exclude_yrs = []
    except:
        sm.id   = ''
        sm.name = ''
        sm.lat  = -999.9
        sm.long = -999.9
        sm.use_string = '.......'
        sm.exclude_yrs = []
    return sm

#-----------------------------------------------------
def parse_exclusion_year_spec(ystr):
    try:
        s1 = ystr[:4]             # first 4 characters, which should be 1st year
        if len(ystr) > 4:
           ss = ystr.split('-')
           s2 = ss[1].strip()    # should be the end year
        else:
           s2 = s1
        sy = int(s1)
        ey = int(s2)
    except:
        sy = -1
        ey = -1
    return sy,ey

#-----------------------------------------------------
#  When using the NCEI-supplied station history file as the 
#  station list, we specify to use all data types.
def readListNCEI(filename):
    stnlist = []
    with open(filename) as listFile:
        i = 0
        for line in listFile:
           i += 1
           stnMeta = parseLineForMeta(line)
           if len(stnMeta.id) >= 11:
              stnlist.append(stnMeta)
              
    # for debug... make sure the master list was read ok
    with open('debug_ncei_list.txt', 'wt') as dbg:
        dbg.write('verification of ncei list read...\n')
        for suo in stnlist:
            dbg.write(suo.id.strip() + ',' + suo.use_string)
            for i in range(len(suo.exclude_yrs)):
                dbg.write(',' + str(suo.exclude_yrs[i]))
            dbg.write(',' + str(suo.lat) + ',' + str(suo.long))
            dbg.write(',' + suo.name)
            dbg.write('\n')
              
              
    return stnlist

#-----------------------------------------------------
#  When using the master list, usage of data types is specified
#  in the file.
def readMasterList(filename, dtype):
    stnlist = []

    #
    #  Convert dtype string into an index integer value.
    #
    if dtype=='tmin': dnum = 1
    if dtype=='tmax': dnum = 2
    if dtype=='prec': dnum = 3
    if dtype=='tavg': dnum = 4
    if dtype=='dewp': dnum = 5
    if dtype=='wind': dnum = 6
    if dtype=='cldc': dnum = 7
    if dtype=='any' : dnum = -1

    #
    #  Read the entire file into a list
    #
    with open(filename, "r", 1) as myfile:
        lines = myfile.readlines()
    
    #
    #  Find the line that marks the end of the header.
    #
    hdr_end = -1
    i = 0
    while (i < len(lines)):
        if lines[i][:10] == '----------':
            hdr_end = i
            i = len(lines)
        i = i + 1
    if hdr_end == -1:
        print('Attempt to read master list failed. Never found end of header.')
        return stnlist
        
    #
    #  Now process the lines after the header section
    #
    for line in lines[hdr_end+1:]:
        items = line.split(',')
        if len(items) >= 3:
            suo = ClassStnMeta()
            suo.id         = items[0].strip()
            suo.lat        = items[1].strip()
            suo.long       = items[2].strip()
            suo.use_string = items[3].strip()
            suo.name       = items[4].strip()
            suo.exclude_yrs = []
            if len(items) > 5:
                s = items[5].strip()
                t = s.split(';')
                for excl in t:
                    xs = excl.strip()
                    if len(xs) > 3:
                        sy,ey = parse_exclusion_year_spec(xs)
                        if sy == -1:
                           print('Unable to parse exclusion years for ' + suo.id)
                        else:
                           for y in range(sy,ey+1):
                               suo.exclude_yrs.append(y)
            if dnum > 0:
                ss = suo.use_string[dnum-1:dnum]      # get the single character
                if ss.lower() == 'y':
                    stnlist.append(suo)
            else:
                stnlist.append(suo)

    # for debug... make sure the master list was read ok
    with open('debug_masterlist.txt', 'wt') as dbg:
        dbg.write('verification of master list read...\n')
        for suo in stnlist:
            dbg.write(suo.id.strip() + ',' + suo.use_string)
            for i in range(len(suo.exclude_yrs)):
                dbg.write(',' + str(suo.exclude_yrs[i]))
            dbg.write('\n')
    
    return stnlist

#-----------------------------------------------------
def getListOfStations(filename, dtype):
    #
    #  Read the first 10 (or fewer) lines to use in auto-detecting the file format
    #    
    try:
        with open(filename, "r", 1) as myfile:
            line10 = myfile.readlines(10)
    except:
       print('Error opening the specified list file. [' + filename + ']')
       sys.exit()

    num_lines = min(len(line10), 10)

    #
    #  Does the file match the NCEI station list format?
    #
    formatNCEI = True
    for i in range(num_lines):
        line = line10[i]
        try:
            lat  = float(line[58:64].strip())
            long = float(line[66:73].strip())
            elev = float(line[75:81].strip())
        except:
            formatNCEI = False
    print('formatNCEI = ', formatNCEI)

    #
    #  If it's not in the default NCEI format, is it
    #  in our new master file format?
    #  I'm doing a real simple check here. If line 1 starts with 
    #  "Master List" I assume it's good. Case-insensitive, of course.
    #
    formatMaster = False
    if not formatNCEI:
        line = line10[0].lower()
        if line[:11] == 'master list': formatMaster = True

    #
    #  Read the list using the appropriate routine.
    #
    theList = None
    if formatNCEI:   theList = readListNCEI(filename)
    if formatMaster: theList = readMasterList(filename, 'any')
    return theList
         
#-----------------------------------------------------
#  Download each of the 1-year pieces from the server and stitch them together
#  into a single file.
#  I am attempting to cleanly handle errors that may occur during the download,
#  but there may very well be a better way to do this.
#
#  The sleep() command that I am using is intended to allow the ftp socket a
#  chance to kind of "get cleared up" in the case of a problem. I think that
#  most of the time when an error is encountered during the download of a file it
#  will be because the file doesn't exist. In theory, I don't think we really
#  need any pause for that. But perhaps other errors might benefit from a brief 
#  pause?  I really don't know.  Prior to putting in this error handling stuff, the
#  most common problem I saw was that we would get a timeout error on the socket,
#  and the entire process would crash. The cause of the timeout error is unclear
#  to me, though I suspect it may be an intermittent (temporary) loss of connection.
#  I'm hoping that this exception handling so will result in the downloads being
#  able to pick up and continue. We will see, I guess.
#
def procStation(stationID, yr1, yr2, listfile):
    print('Retrieve files for ' + stationID + '     ')
    lumpName = 'lumped_' + stationID.strip() + '.txt'
    goodYears = 0
    lumpF = open(lumpName, 'wb')
    ftp = ftplib.FTP('ftp.ncdc.noaa.gov', 'anonymous', 'tim.hunter@noaa.gov')
    ftp.cwd('/pub/data/noaa/')
    for y in range (yr1, yr2+1):
        yrStr = str(y)
        server_gzName = yrStr.strip() + '/' + stationID.strip() + '-' + yrStr.strip() + '.gz'
        local_gzName = stationID.strip() + '_' + yrStr.strip() + '.gz'
        sys.stdout.write('\r downloading ' + local_gzName + '       ')
        sys.stdout.flush()
        worked = False
        attempts = 0
        while (not worked) and (attempts < 3):
            try:
                ftp.retrbinary('RETR %s' % server_gzName, open(local_gzName, "wb").write)
                worked = True
            except:
                attempts += 1
                time.sleep(0.5)    # pause for 1/2 second before next attempt
        
        if worked:
            goodYears += 1
            with gzip.open(local_gzName, "rb") as z:
                file_lines = z.readlines()
                for line in file_lines:
                    lumpF.write(line)
        try:    
            os.remove(local_gzName)
        except:
            pass
    try:
        ftp.quit()
    except:
        pass
    sys.stdout.write('\r')
    sys.stdout.flush()
      
    lumpF.close()
   
    #
    #  Assuming we got some data via ftp, process that lumped file
    #  using the external program process_ish (written in Fortran, btw).
    #  p.wait is used to make sure the process finishes and all of
    #  the output is done before going to the next station.
    if (goodYears > 0): 
        p = subprocess.Popen(["./process_ish", listfile, "24", lumpName])
        p.wait()
   
    try:
        os.remove(lumpName)
    except:
        pass
   
    return
   
#-----------------------------------------------------
#-----------------------------------------------------
#  Main application
#
#  Command-line processing
#
numArgs = len(sys.argv)
if numArgs < 4:
   print('USAGE: python get_all_ish.py filename year1 year2')
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
#  Get the list of stations
#  Each entry is a ClassStnMeta object
#
stnlist = getListOfStations(filename, 'any')
print('number of stations in list = ', len(stnlist))

#
#  For each station in the list...
#
for stn in stnlist:
    stnID = stn.id
    procStation(stnID, year1, year2, filename)
    sys.stdout.flush()

print(' ')


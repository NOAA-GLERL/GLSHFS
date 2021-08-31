import os, sys, fileinput, tempfile, gzip, tarfile, datetime
import numpy as np
import ftplib
import csv, time
 
 
valid_data_sources = [' ', '0', '6', '7', 'B', 'C', 'K', 'U', 'X', 'Z', 'H', 'S']
valid_prc_sources  = [' ', '0', '6', '7', 'B', 'C', 'K',      'X', 'Z', 'H', 'S']

#
#  definition for an object that tells whether to use a particular
#  type of data from a station.
#
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
#  years, we fill those in as use the tmin, tmax and precip data.
#  GHCND does not contain the LLTM forcing data.
#
def parseLineForMeta(line):
    sm = ClassStnMeta()
    try:
        sm.id   = line[0:11].strip()
        sm.name = line[41:86].strip()
        sm.lat  = float(line[11:21].strip())
        sm.long = float(line[21:31].strip())
        sm.use_string = 'YYY....'
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
def parseDlyLine(line):
   id   = line[0:11].strip()
   type = line[17:21].strip()
   year = int(line[11:15].strip())
   mo   = int(line[15:17].strip())
   dlyVals = [-9.9e9 for j in range(31)]
   if type=='PRCP':
       valid_src = valid_prc_sources
   else:
       valid_src = valid_data_sources

   for i in range(31):
      try:
         j = 21 + i*8
         val = int(line[j:j+5].strip())
         mFlag = line[j+5:j+6]
         qFlag = line[j+6:j+7]
         dSrc  = line[j+7:j+8]      # data source
         if dSrc in valid_src:
             if qFlag == ' ':
                dlyVals[i] = val
             else:
                dlyVals[i] = -9.9e9
      except:
         dlyVals[i] = -9.9e9
   return id, type, year, mo, dlyVals


#-----------------------------------------------------
def isLeapYear(year):
   # Is it evenly divisible by 4? If not, not a leap year
   if year%4 != 0: return False
   
   # Is it a century year? If not, it's a leap year.
   if year%100 != 0: return True
   
   # So.. it IS a century year. Then to be a leap year 
   # it must be evenly divisible by 400 (1600, 2000, 2400)
   if year%400 == 0:
      return True
   else:
      return False

#-----------------------------------------------------
def daysInMonth(mo, yr):
   if mo == 1: return 31
   if mo == 2: 
      if isLeapYear(yr):
         return 29
      else:
         return 28
   if mo == 3: return 31
   if mo == 4: return 30
   if mo == 5: return 31
   if mo == 6: return 30
   if mo == 7: return 31
   if mo == 8: return 31
   if mo == 9: return 30
   if mo == 10: return 31
   if mo == 11: return 30
   if mo == 12: return 31

#-----------------------------------------------------
def get_jday(yr, mo, dy):
   d1 = datetime.date(yr, 1, 1)
   d2 = datetime.date(yr, mo, dy)
   delta = d2 - d1
   return (delta.days + 1)
   
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


#-----------------------------------------------------------------
#  stnid    = single station id;  e.g. 'USC00201234'
#  stations = list of ClassStnMeta objects
#
def get_station_index(stnid, stations):
    for snum in range(len(stations)):
        stn = stations[snum]
        if stn.id == stnid: 
            return snum
    return -1            # station not found

#-----------------------------------------------------------------
# Read a master list file and return a list of ClassStnMeta objects
#
def read_stnlist(filename):
    stnlist = []

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
    while (i < len(lines)) and hdr_end==-1:
        if lines[i][:10] == '----------':
            hdr_end = i
        i = i + 1
    if hdr_end == -1:
        print('Attempt to read master list failed. Never found end of header.')
        print('file = [' + filename.strip() + ']')
        return stnlist
        
    #
    #  Now process the lines after the header section
    #
    for line in lines[hdr_end+1:]:
        items = line.split(',')
        if len(items) >= 3:
            suo = ClassStnMeta()
            suo.id   = items[0].strip()
            suo.lat  = items[1].strip()
            suo.long = items[2].strip()
            suo.name = items[4].strip()
            suo.use_string = items[3].strip()
            suo.exclude_yrs = []
            if len(items) > 5:
                s = items[5].strip()
                if (len(s) > 0):
                    t = s.split(';')
                    for ys in t:
                        sy,ey = parse_exclusion_year_spec(ys)
                        if sy == -1:
                           print('Unable to parse exclusion years for ' + suo.id)
                        else:
                           for y in range(sy,ey+1):
                               suo.exclude_yrs.append(y)
            stnlist.append(suo)
    return stnlist

#-----------------------------------------------------
#  stn is a ClassStnMeta object
#
def procDlyToMetCsv(stn, year1, year2):
    stnID = stn.id
    print('processing ' + stnID)   
    dlyFileName = stnID.strip() + '.dly'
    try:
        dlyFile = open(dlyFileName, "r", 1)
    except:
        print('Error opening the specified dly file. ['+dlyFileName+']')
        return

    #
    #  Gotta know start/end years.
    #  I will create arrays for full years of all 31-day months.
    #  That leaves some unused slots, but makes indexing easier.
    #
    syear = 9999999
    eyear = -999999
    for line in dlyFile:
        try:
            year = int(line[11:15].strip())
            if year < syear: syear = year
            if year > eyear: eyear = year
        except:
            pass

    #
    #  Now adjust start/end years to only include the ones
    #  the user requested.
    #
    if year1 > syear: syear = year1
    if year2 < eyear: eyear = year2
    if syear > eyear: return


    #
    #  Create an array for the daily data
    #
    start_date = datetime.date(syear, 1, 1)
    end_date   = datetime.date(eyear,12,31)
    delta = end_date - start_date
    num_days = delta.days + 1
    all_data = np.full((3, num_days), -9.9e29)

    #
    #  Build a list of years that we WILL use
    #  Data in any of the excluded years will be ignored.
    #  We will also be ignoring any data before the user-requested 
    #  start and after the user-requested end.
    #
    use_yr = []
    for yr in range(syear, eyear+1):
        if yr not in stn.exclude_yrs:
            use_yr.append(yr)
   
    #
    dlyFile.seek(0)    # rewind file to beginning
    for line in dlyFile:
        id, type, year, mo, dlyVals = parseDlyLine(line)
        if year in use_yr:
            dm = daysInMonth(mo, year)
            for day in range(dm):                            # day 0..30
                this_date = datetime.date(year,mo,day+1)     # day 1..31
                delta = this_date - start_date
                dnum = delta.days
                if type=='TMIN': all_data[0,dnum] = dlyVals[day] / 10.0      # tenths of C -> deg C
                if type=='TMAX': all_data[1,dnum] = dlyVals[day] / 10.0      # tenths of C -> deg C
                if type=='PRCP': all_data[2,dnum] = dlyVals[day] / 10.0      # tenths of mm -> mm
    dlyFile.close()
    
    #
    #  If the stn object does not say to use the data type, set it
    #  back to missing data values.
    #
    if stn.use_string[0].lower() != 'y': 
        for i in range(num_days): all_data[0,i] = -9.9e9
    if stn.use_string[1].lower() != 'y': 
        for i in range(num_days): all_data[1,i] = -9.9e9
    if stn.use_string[2].lower() != 'y':
        for i in range(num_days): all_data[2,i] = -9.9e9

    writeMetCsv(stn, syear, eyear, all_data)
    os.remove(dlyFileName)


#-----------------------------------------------------
#  Write a met_*.csv file in the format required by the GLSHFS package.
#  Comma-separated entries.
#
#  stn      = ClassStnMeta object
#  syear    = start of data
#  eyear    = end of data
#  datavals = numpy array [3, numdays]
#              [0, :] = tmin data    (degrees C)
#              [1, :] = tmax data    (degrees C)
#              [2, :] = prec data    (mm)
#
def writeMetCsv(stn, syear, eyear, datavals):
    #
    #  find the date extents for VALID (not missing) data
    #
    sdate = datetime.date(2500, 1, 1)    # ridiculous future date
    edate = datetime.date(1500, 1, 1)    # ridiculous past date
   
    datastart = datetime.date(syear, 1, 1)    # start of data in the array
    dataend   = datetime.date(eyear,12,31)    # end of data in the array
    delta = dataend - datastart
    numdays = delta.days + 1

    for d in range(numdays):
        useit = False
        if datavals[0,d] > -998.0: useit = True
        if datavals[1,d] > -998.0: useit = True
        if datavals[2,d] > -998.0: useit = True
        if useit:
            this_date = datastart + datetime.timedelta(days=d)
            if this_date < sdate: sdate = this_date
            if this_date > edate: edate = this_date

    #
    #  If there is no valid data to be written, don't write a file.
    #
    if edate < sdate: return
    
    #
    #
    #
    stnID = stn.id
    outputFilename = 'met_' + stnID.strip() + '.csv'
    try:
       f = open(outputFilename, 'w')
    except:
       print('Error opening output file ' + outputFilename)

    f.write('Daily data for a single station.\n')
   
    s = 'StationID:  , ' + stnID.strip() + '\n'
    f.write(s)

    # Station names can have imbedded commas, so enclose it in double-quotes
    s = 'StationName:, "' + stn.name.strip() + '"\n'
    f.write(s)
   
    s = 'Lat&Long    , ' + str(stn.lat) + ', ' + str(stn.long) + '\n'
    f.write(s)

    s = 'Elevation:  , -9999.000\n'
    f.write(s)

    s = 'Starts(YMD):,' + sdate.strftime("%Y-%m-%d") + '\n'
    f.write(s)

    s = 'Ends(YMD):  ,' + edate.strftime("%Y-%m-%d") + '\n'
    f.write(s)
   
    s = 'Date  ,AirTempMin, AirTempMax, Precipitation, AirTemp, Dewpoint, WindSpeed, CloudCover\n'
    f.write(s)
   
    s = 'YYYY-MM-DD, Celsius, Celsius, Millimeter, Celsius, Celsius, MetersPerSecond, Percent\n'
    f.write(s)
   
    fstr = "{:4d}-{:02d}-{:02d},{:s},{:s},{:s},{:s},{:s},{:s},{:s}\n"
    numdays = (edate-sdate).days + 1
    skip = (sdate-datastart).days    # days at start of datavals with no data
    for dnum in range(numdays):
        this_date = sdate + datetime.timedelta(days=dnum)
        y = this_date.year
        m = this_date.month
        d = this_date.day
        
        n = skip + dnum                    # index of this day's data
        if datavals[0,n] < -999.0:         # tmin
            v1 = "NA"
        else:
            v1 = "{:.2f}".format(datavals[0,n])
            
        if datavals[1,n] < -999.0:         # tmax
            v2 = "NA"
        else:
            v2 = "{:.2f}".format(datavals[1,n])
            
        if datavals[2,n] < -999.0:         # prec
            v3 = "NA"
        else:
            v3 = "{:.2f}".format(datavals[2,n])
            
        v4 = "NA"
        v5 = "NA"
        v6 = "NA"
        v7 = "NA"
        
        s = fstr.format(y,m,d,v1,v2,v3,v4,v5,v6,v7)
        f.write(s)
    f.close()

#-----------------------------------------------------
#  stn is a ClassStnMeta object
#
def procStation(stn, year1, year2):
    stnID = stn.id.strip()
    dlyName = stnID + '.dly'    # e.g. USC00201234.dly
    if os.path.exists(dlyName):
        procDlyToMetCsv(stn, year1, year2)
   

#-----------------------------------------------------
#  stn is a ClassStnMeta object
#
def download_dly_files(stnlist):
    ftp = ftplib.FTP('ftp.ncdc.noaa.gov', 'anonymous', 'tim.hunter@noaa.gov')
    ftp.cwd('/pub/data/ghcn/daily/all/')
    sys.stdout.write(' downloading')
    
    #
    #  loop through all stations in the list
    #
    for stn in stnlist:
        stnID = stn.id.strip()
        dlyName = stnID + '.dly'    # e.g. USC00201234.dly
        sys.stdout.write('\r downloading ' + dlyName + '    ')
        sys.stdout.flush()
        done = False
        fails = 0
        while not done:
            try:
                ftp.retrbinary('RETR %s' % dlyName, open(dlyName, "wb").write)
                done = True
            except:
                fails += 1
                if fails == 5:
                    print('   unable to download file for ', stn.id)        
                    done = True
                    os.remove(dlyName)
    
    #
    #  Close the connection
    #  Sometimes this crashes, and I don't know why, but it is somewhat
    #  irrelevant. If the quit() fails, we can still proceed with the
    #  processing of the data. So just let that happen.
    #  
    try:
       ftp.quit()
    except:
       pass
    
   
#-----------------------------------------------------
#-----------------------------------------------------
#  Main application
#
#  Command-line processing
#
#  Main application
#

# get the starting timestamp for computing elapsed time
start_time = time.time()

#
#  Command-line processing
#
numArgs = len(sys.argv)
if numArgs < 4:
   print('USAGE: python process_ghcnd_nrt.py filename year1 year2')
   print('   filename = name of a master-list format file')
   print('   year1 = earliest year to retrieve/process')
   print('   year2 = latest year to retrieve/process')
   print('')
   print('The specified year(s) will be used to control the download of ')
   print('files from the NCEI ftp site. The GHCND data is stored there in')
   print('a "by_year" directory such that each yearly file contains ALL data')
   print('for a particular year.')
   print('The master list will then be used to control the processing of stations')
   print('from those downloaded files. The result will be a single met_*.csv file')
   print('for each station.')
   sys.exit()
   
listfile = sys.argv[1]
year1 = int(sys.argv[2])
year2 = int(sys.argv[3])

ok = True
if year1 < 1763:  ok = False
if year2 > 2029:  ok = False
if year2 < year1: ok = False
if not ok:
   print('Invalid year(s) specified')
   print('The GHCND site set has data from 1763-present')
   sys.exit()

#
#  Get the list of stations to process, including all of the metadata.
#  This is a list of ClassStnMeta objects.
#
print('reading the station list')
stations = read_stnlist(listfile)

#
#  download the files all at once. This process:
#     open connection
#     d/l all station files
#     close connection
#     process all stations
#  is better than:
#     open the connection
#     download one file
#     close connection
#     process one station
#     repeat as needed
#  Multiple connection open/close cycles causes us to get rejected.
#  Likely because the remote host thinks we might be a DOS attack.
#
print('Downloading the files from NCEI')
download_dly_files(stations)

#
#  For each station in the list...
#
num_stns = len(stations)
for stn in stations:
    procStation(stn, year1, year2)

#
#  Get the ending timestamp for computing elapsed time
#  Then compute and print that interval.
#
end_time = time.time()
hours, rem = divmod(end_time - start_time, 3600)
minutes, seconds = divmod(rem, 60)
s = "{:0>2}:{:0>2}:{:05.2f}".format(int(hours),int(minutes),seconds)
print('total elapsed time is (h:m:s)', s)




import os, sys, os.path
import csv, math
import numpy as np

from datetime import datetime, timedelta, date
from calendar import monthrange
from copy import copy, deepcopy

missing_data      = -9.9e29
missing_data_test = missing_data * 0.95

class ClassStnData:
    id          = ''
    name        = ''
    lat         = -9999.9
    long        = -9999.9    
    start_date  = datetime.strptime('2399-01-01', "%Y-%m-%d")
    end_date    = datetime.strptime('1799-01-01', "%Y-%m-%d")
    datavals    = np.full((1), missing_data)

class ClassStnMeta:
    id          = ''
    name        = ''
    lat         = -9999.9
    long        = -9999.9    
    use_string  = '.......'       # tmin,tmax,prec,tavg,dewp,wind,cldc

#-----------------------------------------------------------------
def print_usage():
    print('USAGE: glshfs_build_stnlist bsn')
    print('   bsn   = sup, mic, hur, etc')
    
#-----------------------------------------------------------------
#  Read the station data values from the specified file.
#  The file columns are assumed to be:
#      Date, station1, station2, station3, etc.
#
def read_stndata(filename):
    #
    #  I am choosing NOT to trust the header, so we will
    #  initialize with silly dates, then find file date extents
    #
    fsdate = datetime.strptime('2399-01-01', "%Y-%m-%d")
    fedate = datetime.strptime('1799-01-01', "%Y-%m-%d")
    with open(filename) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        line_count = 0
        for row in csv_reader:
            line_count += 1
            if line_count == 6:
                num_stations = len(row) - 1      # first column is label
            if line_count > 10:
                this_date = datetime.strptime(row[0], "%Y-%m-%d")
                if this_date < fsdate:
                    fsdate = this_date
                if this_date > fedate:
                    fedate = this_date

    #
    #  Set up the data objects
    #
    numdays = (fedate-fsdate).days + 1
    stndata = [ClassStnData() for i in range(num_stations)]
    for i in range(num_stations):
        stndata[i].start_date = fsdate
        stndata[i].end_date   = fedate
        stndata[i].datavals   = np.full((numdays), missing_data)

    #
    #  Now read the file again, this time storing the data values.
    #    
    with open(filename) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        line_count = 0
        for row in csv_reader:
            line_count += 1
            if line_count == 6:
                for i in range(num_stations):
                    col = i + 1
                    try:
                       stndata[i].id = row[col]
                    except:
                       stndata[i].id = 'xxx'
            if line_count == 7:
                for i in range(num_stations):
                    col = i + 1
                    try:
                       stndata[i].name = row[col]
                    except:
                       stndata[i].name = 'unknown'
            if line_count == 8:
                for i in range(num_stations):
                    col = i + 1
                    try:
                       stndata[i].lat = float(row[col])
                    except:
                       stndata[i].lat = -9999.9
            if line_count == 9:
                for i in range(num_stations):
                    col = i + 1
                    try:
                       stndata[i].long = float(row[col])
                    except:
                       stndata[i].long = -9999.9
            if line_count > 10:
                this_date = datetime.strptime(row[0], "%Y-%m-%d")
                d = (this_date - stndata[0].start_date).days
                for i in range(num_stations):
                    col = i + 1
                    try:
                       stndata[i].datavals[d] = float(row[col])
                    except:
                       stndata[i].datavals[d] = missing_data
    return stndata


#-----------------------------------------------------------------
min_years_data = 15
def build_default_meta(sdata_7types):
    stn_meta = []
    for dtype in range(7):
        stndata = sdata_7types[dtype]   # list of ClassStnData objects for this data type
        for i in range(len(stndata)):
            stn = stndata[i]          # ClassStnData object for this station
            id = stn.id.strip()
            valid_days = 0
            numdays = (stn.end_date - stn.start_date).days + 1
            for d in range(numdays):
                if stn.datavals[d] > missing_data_test: 
                    valid_days = valid_days + 1
            if valid_days > min_years_data * 365:
                found = False
                for m in range(len(stn_meta)):
                    if id.strip() == stn_meta[m].id.strip():
                        found = True
                        s1 = stn_meta[m].use_string
                        s2 = s1[:dtype] + 'Y' + s1[dtype+1:]
                        stn_meta[m].use_string = s2
                if not found:
                    sm = ClassStnMeta()
                    sm.id   = stn.id.strip()
                    sm.name = stn.name.strip()
                    sm.lat  = stn.lat
                    sm.long = stn.long
                    s1 = '.......'
                    s2 = s1[:dtype] + 'Y' + s1[dtype+1:]
                    sm.use_string = s2
                    stn_meta.append(sm)
    return stn_meta

#-----------------------------------------------------------------
def sort_meta(stn_meta_unsorted):
    #
    #  Build a temporary list that has ONLY the station IDs
    #
    id_list = []
    for stn in stn_meta_unsorted:
        id_list.append(stn.id)

    #
    #  Sort that list using python's built-in sort()
    #
    id_list.sort()
    
    #
    #  Now build a new ClassStnMeta object with the stations in sorted order
    #
    stn_meta_sorted = []
    for id in id_list:
        i = -1
        for j in range(len(stn_meta_unsorted)):
            if stn_meta_unsorted[j].id.strip() == id.strip():
                stn = deepcopy(stn_meta_unsorted[j])
                stn_meta_sorted.append(stn)
    
    return stn_meta_sorted

#-----------------------------------------------------------------
#  stn_meta is a list of ClassStnMeta objects
#
def write_station_list(stn_meta, bsn):
    filename = 'stnlist_' + bsn + '.txt'
    with open(filename, "wt") as f:
        f.write('Master List of stations from stndata files. This is a template for the\n')
        f.write('GHCND data extraction/processing program.\n\n')
        f.write('Column 2 specifies which data can be extracted for each station.\n')
        f.write('The interpretation of this string of characters is:\n')
        f.write('  position 1 = TMIN\n')
        f.write('  position 2 = TMAX\n')
        f.write('  position 3 = PREC\n')
        f.write('  position 4 = TAVG\n')
        f.write('  position 5 = DEWP\n')
        f.write('  position 6 = WIND\n')
        f.write('  position 7 = CLDC\n')
        f.write('e.g. We may want to use everything EXCEPT precipitation, in which case\n')
        f.write('the string will be YY.YYYY\n') 
        f.write('Y (upper or lower case) means yes (use it) while anything else means no.\n')
        f.write('A dot is used for no in hopes of making it easier to visualize at a glance.\n\n')
        f.write('The station name is also for user convenience and is not used.\n\n')
        f.write('The excluded years section for each station is usually blank, but in\n')
        f.write('some cases the station is one that we want to use except for a specific\n')
        f.write('period(s) during which it had obvious bad data.\n')
        f.write('If there are multiple periods they need to be separated by semicolons\n')
        f.write('Note that if an excluded period is specified it will apply to ALL data types.\n\n')
        f.write('station_id,   Latitude,  Longitude, UseData, station_name,             excluded_years\n')
        f.write('------------, ---------, ---------, -------, -------------------------,  \n')
        for i in range(len(stn_meta)):
            stn = stn_meta[i]          # ClassStnData object for this station
            sid   = stn.id.ljust(12, ' ')
            sname = stn.name[:25]
            slat  = "{:9.3f}".format(stn.lat)
            slong = "{:9.3f}".format(stn.long)
            suse  = stn.use_string
            s = sid + ', ' + slat + ', ' + slong + ', ' + suse + ', ' + sname)
            f.write(s + ', \n')

#=========================================================================================================
#=========================================================================================================
#=========================================================================================================
#=========================================================================================================
#
#  Command-line processing
#
numArgs = len(sys.argv)
if numArgs < 2:
    print_usage()
    sys.exit()
   
bsn  = sys.argv[1]

#
#  Get the station metadata from each of the stndata_*.csv files.
#
all_types_all_data = []
for dnum in range(7):
    if dnum==0: dtype = 'airtempmin'
    if dnum==1: dtype = 'airtempmax'
    if dnum==2: dtype = 'precipitation'
    if dnum==3: dtype = 'airtemp'
    if dnum==4: dtype = 'dewpoint'
    if dnum==5: dtype = 'windspeed'
    if dnum==6: dtype = 'cloudcover'

    stn_file = 'stndata_' + dtype + '_' + bsn + '.csv'
    print('Reading station data file: ' + stn_file)
    sdata = read_stndata(stn_file)
    num = len(sdata)
    print('num_stations=', num)
    all_types_all_data.append(sdata)

#
#  Build a list of ClassStnMeta objects that includes all of the
#  stations with *ENOUGH* valid data for at least one data type.
#  The use_string variable will be built to accurately reflect
#  which data types had enough.
#
stn_meta = build_default_meta(all_types_all_data)

sorted_meta = sort_meta(stn_meta)

#
#  Write the template "master list" for the GHCND data extraction.
#
write_station_list(sorted_meta, bsn)


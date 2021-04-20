import os, sys, os.path
import csv, math

from datetime import datetime, timedelta, date
from copy import copy, deepcopy

class ClassStnMeta:
    id          = ''
    name        = ''
    use_string  = '.......'       # tmin,tmax,prec,tavg,dewp,wind,cldc
    exclude_yrs = []

#-----------------------------------------------------------------
def print_usage():
    print('USAGE: python glshfs_merge_stnlists.py')

#-----------------------------------------------------
def parse_exclusion_year_spec(ystr):
    ys = ystr.strip()
    try:
        s1 = ys[:4]              # first 4 characters, which should be 1st year
        if len(ys) > 4:
           ss = ys.split('-')
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
def merge_exclusion_years(list1, list2):
    # create python sets from each list, then merge them
    set1 = set()
    set2 = set()
    if (len(list1) > 0): set1.update(list1)
    if (len(list2) > 0): set2.update(list2)
    mset = set1.union(set2)   # merged set (no duplicates)
    
    # convert merged set back to a list, and sort it
    mlist = []
    while len(mset) > 0:
        mlist.append(mset.pop())
    mlist.sort()
    
    return mlist

#-----------------------------------------------------------------
#  Turns a list of years into a shorter string.  e.g.:
#   [1950,1951,1952,1953]  becomes
#        "1950-1953"
# 
#   [1950,1951,1952,1953,1961,1962,1963,1964]  becomes
#        "1950-1953;1961-1964"
#
def shorthand_list_of_years(years):
    #
    #  special case of a single year
    #
    if len(years) == 1:
        outstring = str(years[0])
        return outstring
        
    #
    #  multiple years; including multiple sections
    #
    years.sort()         # just to be sure it's sorted
    outstring = ''
    y1 = years[0]        # start of segment
    y2 = y1              # end of segment
    for i in range(1, len(years)):
        if years[i] > (y2 + 1):
            sy = str(y1)
            ey = str(y2)
            outstring = outstring + sy + '-' + ey + ';'
            y1 = years[i]
            y2 = y1
        else:
            y2 = years[i]

    #  The final segment (which may also be the first)    
    sy = str(y1)
    ey = str(y2)
    outstring = outstring + sy + '-' + ey
    return outstring

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
            suo.name = items[2].strip()
            suo.use_string = items[1].strip()
            suo.exclude_yrs = []
            if len(items) > 3:
                s = items[3].strip()
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
    
#-----------------------------------------------------------------
#  stn is the ClassStnMeta object for a single station
#  If master_list already contains an object with that station id,
#  the info will be merged. Otherwise this new stn is appended.
#  Note that the only *merging* is of the exclusion years.
#  
def add_stn_to_master(master_list, stn):
    new_master = deepcopy(master_list)
    s_id = stn.id.strip()
    match = -1
    for i in range(len(new_master)):
        item = new_master[i]
        m_id = item.id.strip()
        if s_id == m_id:
            match = i

    if match >= 0:
        item = new_master[match]
        smo = ClassStnMeta()
        smo.id   = item.id
        smo.name = item.name
        smo.use_string = item.use_string
        smo.exclude_yrs = merge_exclusion_years(item.exclude_yrs, stn.exclude_yrs)
        new_master[match] = smo
    else:
        new_master.append(stn)
        
    return new_master

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
def write_debug_list(stnlist, fname):
    with open(fname, 'wt') as dbg:
        for suo in stnlist:
            dbg.write(suo.id.strip() + ',' + suo.use_string)
            for i in range(len(suo.exclude_yrs)):
                dbg.write(',' + str(suo.exclude_yrs[i]))
            dbg.write('\n')
    
#-----------------------------------------------------------------
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
        f.write('station_id,   UseData, station_name,              excluded_years\n')
        f.write('------------  -------, -------------------------, --------------\n')
        for i in range(len(stn_meta)):
            stn = stn_meta[i]          # ClassStnData object for this station
            f.write(stn.id.ljust(12, ' ') + ', ' + stn.use_string + ', ')
            f.write(stn.name.ljust(25) + ', ')
            if len(stn.exclude_yrs) > 0:
                exyrs = shorthand_list_of_years(stn.exclude_yrs)
                f.write(exyrs + '\n')
            else:
                f.write('\n')

#=========================================================================================================
#=========================================================================================================
#=========================================================================================================
#=========================================================================================================

#
#  Merge each individual stnlist_*.txt file with the master list
#
master_list = []
seq = 1
lakes = ['sup', 'mic', 'hur', 'geo', 'stc', 'eri', 'ont']
for lk in range(7):
    bsn = lakes[lk]
    fname = 'stnlist_' + bsn + '.txt'
    print('reading and adding stations from ' + fname)
    slist = read_stnlist(fname)
    for stn in slist:
        master_list = add_stn_to_master(master_list, stn)
    
#
#  Now that I have a big list of ALL stations, sort it.
#
sorted_master = sort_meta(master_list)

#
#  write it to a new file
#
print('writing the new stnlist_all.txt file')
write_station_list(sorted_master, 'all')


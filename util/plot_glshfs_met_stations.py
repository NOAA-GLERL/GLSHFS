
import os, sys, os.path
import csv, math
import numpy as np
from datetime import datetime, timedelta, date
from calendar import monthrange
from copy import copy, deepcopy

#import matplotlib
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt

missing_data      = -9.9e29
missing_data_test = missing_data * 0.95

class ClassDailyData:
    id          = ''
    name        = ''
    lat         = -9999.9
    long        = -9999.9    
    start_date  = datetime.strptime('2399-01-01', "%Y-%m-%d")
    end_date    = datetime.strptime('1799-01-01', "%Y-%m-%d")
    datavals    = np.full((1), missing_data)

class ClassMonData:
    id          = ''
    name        = ''
    lat         = -9999.9
    long        = -9999.9
    start_date  = datetime.strptime('2399-01-01', "%Y-%m-%d")
    end_date    = datetime.strptime('1799-01-01', "%Y-%m-%d")
    datavals    = np.zeros((1,12))     # year, month

class ClassWeightDetails:
    num_stns = 0
    num_days = 0
    stn_ids  = []
    weights  = np.full((1,1), -9.9)       # stn_num, daynum

class GlerlMap:
    #
    #  Defined values for each lake
    #
    lake_names   = ['sup', 'mic', 'hur', 'geo', 'stc', 'eri', 'ont']
    map_subs     = [ 22,  27,  16,  13,   7,  21,  15]
    map_widths   = [760, 472, 411, 552, 240, 584, 456]
    map_heights  = [516, 608, 436, 426, 184, 424, 392]
    map_xoffsets = [406.039150,  227.973530,  279.796362,  279.796362,
                     64.526407,  298.661220,  262.877366]
    map_yoffsets = [-5121.245120, -4586.637700, -4709.846822, -4859.846822,
                    -4648.658407, -4473.268070, -4632.988223]
    map_meridian = [-87.85, -86.87, -81.79, -81.79, -82.71, -81.74, -76.90]
    map_radius   = [6365.2, 6363.7, 6346.2, 6346.2, 6328.6, 6338.3, 6341.8]
    

    #-----------------------------------------
    def __init__(self):
        self.bsn          = 'xxx'
        self.width        = -1       # number of cells
        self.height       = -1       # number of cells
        self.cell_size    = -1       # meters, cells assumed to be square
        self.subbasins    = -1
        self.out_of_basin = -1       # Code for cells that are outside the basin
        self.x_offset     = -1
        self.y_offset     = -1
        self.ref_meridian = -1       # degrees EAST of Prime meridian (G.L. ~ -65 to -95)
        self.radius       = -1       # radius of the earth (km)
        self.map2d        = [0][0]   # [0:MapHeight-1][0:MapWidth-1]   i.e. [y][x] or [row][col]
        
    #-----------------------------------------
    def lake_number_from_name(self, bsn):
        s = bsn.lower()
        for i in range(7):
            if self.lake_names[i] == s:
                return i
        raise Exception('Invalid lake name in lake_number_from_name()')
    
    #-----------------------------------------
    def read_glerl_map_old_format(self, bsn):
        #
        #  set up the map parameters
        #
        try:
            lk = self.lake_number_from_name(bsn)
            self.bsn          = bsn
            self.width        = self.map_widths[lk]
            self.height       = self.map_heights[lk]
            self.cell_size    = 1000.0
            self.subbasins    = self.map_subs[lk]
            self.out_of_basin = self.subbasins + 2
            self.x_offset     = self.map_xoffsets[lk]
            self.y_offset     = self.map_yoffsets[lk]
            self.ref_meridian = self.map_meridian[lk]
            self.radius       = self.map_radius[lk]
            self.map2d        = [[-1 for i in range(self.width)] for j in range(self.height)]
        except:
            raise Exception('Unable to set up map')

        #
        #  Read the map cell codes.  The list is indexed [row][column] and the file is
        #  arranged in row-major order (south to north), with each row reading west to east.  
        #  So the first byte in the file is the SW corner of the rectangular map, with 
        #  index [0][0].  The next byte is the cell [0][1], which is one cell to the east. 
        #  Then cell [0][2], then [0][3], ...  until [0][mapwidth-1].  Then we have the
        #  second row - [1][0], [1][1], [1][2], [1][3], etc.
        # 
        try:
            fname = bsn + 'bytcd.map'
            f = open(fname, "rb")
            for r in range(self.height):
                row = f.read(self.width)
                for c in range(self.width):
                    self.map2d[r][c] = row[c]    # implicitly converts from binary char to int
                    
        except:
            print('r,height:', r, self.height)
            print('c, width:', c, self.width)
            raise Exception('Error reading map file')
        
        #
        #  The historical map file contains cells with a "bridge" code. These are water cells
        #  that were assigned this bridge code to connect them to the mainland subbasins.
        #  This scheme is irrelevant for our purposes here, so I just reassign them to 
        #  subbasin 0 (lake surface), which simplifies the subsequent processing code.
        #
        bridge = self.subbasins + 1
        for r in range(self.height):
            for c in range(self.width):
                if self.map2d[r][c] == bridge:
                    self.map2d[r][c] = 0

        #
        #  The historical map file uses subbasins+2 as the outside-of-basin code for cells 
        #  that are not part of the lake's drainage basin. I want to simplify that a bit,
        #  and just use -1 as the code, regardless of lake.
        #
        oob = self.subbasins + 2
        for r in range(self.height):
            for c in range(self.width):
                if self.map2d[r][c] == oob:
                    self.map2d[r][c] = -1

    #-----------------------------------------
    def latlong_to_xy(self, lat, long):
        ok = True
        if (lat  <  -90): ok = False
        if (lat  >   90): ok = False
        if (long < -180): ok = False
        if (long >  180): ok = False
        if (not ok):
            raise Exception('Invalid lat/long in latlong_to_xy()')
            
        #
        #  Get latitude in radians and longitude in radians from reference meridian.
        #
        lt = lat / 57.29578
        ln = (long - self.ref_meridian) / 57.29578

        tp = math.tan(lt)
        snpl = math.sin(lt) * ln
        x = self.radius * math.sin(snpl) / tp
        y = self.radius * (lt + (1. - math.cos(snpl)) / tp)
        x = x + self.x_offset
        y = y + self.y_offset

        return x,y    
    
    #-----------------------------------------   
    def xy_to_latlong(self, x, y):
        #
        #  Choose an arbitrary Lat/Long point as a first guess...
        #  (Gotta use Ann Arbor, of course.  ha-ha)
        #  Also set an initial delta in each direction (with repect to lat/long)
        # 
        lta =  42.25
        lna = -83.75
        delta_lat = 1.0
        delta_lon = 1.0

        #
        #  Compute the X,Y coordinates of our first guess
        #
        try:
            xa, ya = self.latlong_to_xy(lta, lna)
        except:
            raise Exception('Unable to compute initial x,y in xy_to_latlong()')
         
        #
        #  How far off target is that guess?
        #  Note that I intentionally skip the square root for a tiny bit 
        #  of computational efficiency. I simply set Tolerance to the
        #  square of the desired actual tolerance.
        #
        xdiffa = xa - x
        ydiffa = ya - y
        diff = xdiffa**2 + ydiffa**2
        tolerance = 1.0e-6
         
        #
        #  If we got real lucky and hit the mark, assign final value and return
        #
        if diff <= tolerance:
            return lta, lna
            
        #
        #  Iterate to try to find a solution.
        #  Basic idea is this...
        #    A is our starting (incorrect) guess. B is a trial guess.
        #    B will likely be wrong as well.
        #    Assume the difference from A to B in the X/Y values is proportional
        #       to the difference between them in Lat/Long.
        #
        max_iterations = 50
        iterations = 1
        while iterations <= max_iterations:
            
            #
            #  Set a trial point B and compute the resulting X,Y
            #
            ltb = lta + delta_lat
            lnb = lna + delta_lon
            xb,yb = self.latlong_to_xy(ltb, lnb)
            
            #
            #  Compute a scaled directional factor to apply to the delta.
            #  
            sx = (x-xa) / (xb-xa)
            sy = (y-ya) / (yb-ya)
            
            #
            #  Compute a revised delta
            #
            delta_lat = delta_lat * sy
            delta_lon = delta_lon * sx
            
            #
            #  Apply that revised delta to get a new point A.
            #
            lta = lta + delta_lat
            lna = lna + delta_lon
            xa,ya = self.latlong_to_xy(lta, lna)
            
            #
            #  Did we hit the mark with the new point A?
            #
            xdiffa = xa - x
            ydiffa = ya - y
            diff = xdiffa**2 + ydiffa**2
            if diff <= tolerance:
               return lta,lna

            #
            #  Getting here means we were still too far away, but presumably point
            #  A is a lot closer than our previous guess. And that means we 
            #  shouldn't need to move as far away for our next trial guess (B).
            #  So we want to reduce the delta values. How much??? That's a
            #  bit of a guess. Let's arbitrarily say 25% of the move we just made.
            #
            delta_lat = delta_lat * 0.25
            delta_lon = delta_lon * 0.25
            iterations = iterations + 1
            
        raise Exception('Unable to converge.')

    #------------------------------------------------------------------------------
    #  Compute the distance (in meters) between two cells (center of each cell)
    #------------------------------------------------------------------------------
    def distance_from_cell_index(self, x1,y1, x2,y2):
        #
        #  First, compute ccdist, which is the "cell count" distance.
        #  Then convert that to meters for the final result.
        #
        ccdist = (x2-x1)*(x2-x1)  +  (y2-y1)*(y2-y1)
        cdist = math.sqrt(ccdist)
        dist  = cdist * self.cell_size
        return dist

    #------------------------------------------------------------------------------
    #  Compute the distance (in meters) between two (Lat,Long) points
    #------------------------------------------------------------------------------
    def distance_from_lat_long(self, lat1, lon1, lat2, lon2):
        #
        #  Convert Lat,Long to X,Y. Then compute distance between them.
        #
        x1,y1 = self.latlong_to_xy(lat1, lon1)
        x2,y2 = self.latlong_to_xy(lat2, lon2)
        ccdist = (x2-x1)*(x2-x1)  +  (y2-y1)*(y2-y1)
        cdist = math.sqrt(ccdist)
        dist  = cdist * self.cell_size
        return dist

    #------------------------------------------------------------------------------
    #  Compute the distance to the nearest cell of the specified subbasin.
    #  If subbasin = 0..NumSubbasins, compute to nearest cell of that subbasin.
    #  If Subbasin = SubNum_Land,  then compute distance to the nearest LAND cell
    #  If Subbasin = SubNum_Basin, then compute distance to the nearest BASIN cell (lake or land)
    #------------------------------------------------------------------------------
    def distance_to_subbasin(self, lat, long, subbasin):
        #
        #  Compute the X,Y coordinates of the specified Lat,Long
        #  Then use the X,Y values to get the corresponding cell coordinates.
        #
        x,y = self.latlong_to_xy(lat, long)
      
        #
        #  Let's do a real quick check that might save a lot of extra processing...
        #  If the specified lat,long falls inside the subbasin of interest, then
        #  the distance is 0 and we don't need to do any more testing.
        #
        col = int(x)   # truncate 
        row = int(y)   # truncate 
        if (((col >= 0) and (col <= self.width-1)) and
            ((row >= 0) and (row <= self.height-1))):
            sub = self.map2d[row][col]              # subbasin code (INTEGER) for this cell
            if sub == subbasin:                     # simple case where a single subbasin was specified
                return 0.0                          
            if sub == subnum_overland:
                if ((sub >= 1) and (sub <= self.subbains)):
                    return 0.0
            if sub == subnum_overbasin:
                if ((sub >= 0) and (sub <= self.subbains)):
                    return 0.0
      
        #
        #  Initialize the minimum distance to a ridiculously large value
        #  This is actually the SQUARE of the distance, since we will always
        #  skip doing the square root part of the distance calculations.
        #  No point in doing unnecessary SQRT() calcs.
        #
        min_dist_sq = 9.9e29
      
        #
        #  Main loop. 
        #  Any time we find a distance of 0, we know we can call it quits. In order
        #  to take advantage of that, I will structure the loop to be a WHILE loop
        #  rather than just a simple double nest.  I am assuming that this will
        #  result in better efficiency by eliminating a bunch of distance calcs.
        #  The trade-off, of course, is the added IF tests in each loop step.
        # 
        #  Note that the test is for (min_dist_sq > 0.7). That is to deal with
        #  any rounding issues. 
        #
        done = False
        cx = 0.5        # column, or x   (center of cell 0,0)
        ry = 0.5        # row,    or y   (center of cell 0,0)
        while ((not done) and (min_dist_sq > 0.7)):
            col = int(cx)   # truncate 
            row = int(ry)   # truncate 
            sub = self.map2d[row][col]     # subbasin code for the cell of interest
         
            if (sub == subbasin):
                dist_sq = (cx-x)*(cx-x) + (ry-y)*(ry-y)
                min_dist_sq = min(min_dist_sq, dist_sq)
         
            if (subbasin == subnum_overland):
                if ((sub >= 1) and (sub <= self.subbasins)):
                    dist_sq = (cx-x)*(cx-x) + (ry-y)*(ry-y)
                    min_dist_sq = min(min_dist_sq, dist_sq)
             
            if (subbasin == subnum_overbasin):
                if ((sub >= 0) and (sub <= self.subbasins)):
                    dist_sq = (cx-x)*(cx-x) + (ry-y)*(ry-y)
                    min_dist_sq = min(min_dist_sq, dist_sq)
             
            cx = cx + 1 
            if (cx >= self.width): 
                cx = 0
                ry = ry + 1
                if (ry >= self.height):
                    done = True
      
        dist = math.sqrt(min_dist_sq)        # for the final value, do the SQRT()
        return dist

#---------------------------------------------------------------------
#  Build a list of visual vertices for plotting the subbasin outlines
#  gmap = a GlerlMap object
#  Returns lists of horizontal and vertical locations where little lines should
#  be drawn in order to create a visual subbasin map. These locations are in terms
#  of (X,Y) coordinates, not (lat,long).
#  These X,Y coordinates start at an origin (0,0) in the SW corner.
#
def get_subbasin_vertices_for_plot(gmap):
    rows = gmap.height
    cols = gmap.width
    
    #
    #  first draw all of the tiny little vertical lines where there is
    #  a subbasin boundary
    #
    list_vert_x = []
    list_vert_y = []
    for row in range(rows):
        for col in range(cols-1):
            sub1 = gmap.map2d[row][col]
            sub2 = gmap.map2d[row][col+1]
            if sub1 != sub2:
                list_vert_x.append(col+0.5)
                list_vert_y.append(row+0.5)

    list_horz_x = []
    list_horz_y = []
    for col in range(cols):
        for row in range(rows-1):
            sub1 = gmap.map2d[row][col]
            sub2 = gmap.map2d[row+1][col]
            if sub1 != sub2:
                list_horz_x.append(col+0.5)
                list_horz_y.append(row+0.5)

    return list_vert_x, list_vert_y, list_horz_x, list_horz_y


#-----------------------------------------------------------------
def print_usage():
    print('USAGE: plot_glshfs_met_stations bsn dtype')
    print('   bsn   = sup, mic, hur, etc')
    print('   dtype = airtempmin, airtempmax, precipitation, etc')
    print('   trailing-months = months to average (2,3,4,etc). default is 1.')
    print('When trailing-months is specified, the plotted values will be a rolling')
    print('average of the last N months. Useful for seeing trends in noisy data like temps')
    print('')
    print('stndata_[dtype]_[bsn].csv needs to be in the current directory')
    print('subdata_[bsn]00.csv needs to be in the current directory')
    print('')
    print('In the future you may want to compare station data to a particular')
    print('subbasin or aggregation. That will require some minor code revision.')    
    
#-----------------------------------------------------------------
#  Read the subbasin precipitation values from the specified file.
#  The file columns are assumed to be:
#      Date, TMin, Tmax, Precip, TAvg, Dewpt, Wind, Cloud.
#
def read_subdata(filename, dtype):
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
            if line_count > 9:
                this_date = datetime.strptime(row[0], "%Y-%m-%d")
                if this_date < fsdate:
                    fsdate = this_date
                if this_date > fedate:
                    fedate = this_date

    #
    #  Set up the data object
    #
    dlydat = ClassDailyData()
    dlydat.start_date = fsdate
    dlydat.end_date = fedate
    numdays = (fedate-fsdate).days + 1
    dlydat.datavals = np.full((numdays), missing_data)
 
    #
    #  Which column do we read?
    #
    if dtype == 'airtempmin':    datacol = 1
    if dtype == 'airtempmax':    datacol = 2
    if dtype == 'precipitation': datacol = 3
    if dtype == 'airtemp':       datacol = 4
    if dtype == 'dewpoint':      datacol = 5
    if dtype == 'windspeed':     datacol = 6
    if dtype == 'cloudcover':    datacol = 7
 
    #
    #  Now read the file again, this time storing the data values.
    #  For simplicity, I will assume that precip is in millimeters.
    #  If that is in doubt, you will need to read the column headers.
    #    
    with open(filename) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        line_count = 0
        for row in csv_reader:
            line_count += 1
            if line_count > 9:
                this_date = datetime.strptime(row[0], "%Y-%m-%d")
                d = (this_date - dlydat.start_date).days
                try:
                   dlydat.datavals[d] = float(row[datacol])
                except:
                   dlydat.datavals[d] = missing_data
    return dlydat
    

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
    stndata = [ClassDailyData() for i in range(num_stations)]
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
#  This routine is kind of messy/complicated.  We are given a list
#  of station IDs, and we need to build an entire timeseries of
#  thiessen weights for each station. 
#
#  The files that contain the weighting details are one per day, and contain 
#  a fair bit of info that we don't care about.  Additionally, the weights are for
#  a LOCATION, not a STATION.  There may be multiple stations at a single
#  location, and that will be reflected in the staion list for that location.
#  For purposes of what we need in this program, I will just use the weights
#  for that location and assign the same value to each station.
#
#  Also, keep in mind that a station in the stn_data_mon list passed to the 
#  routine may not show up in the detail file for a particular day.  The 
#  detail files list only those stations that report and have a weight for that day.
#
def read_details_dly(stn_data_mon, subbasin, dtype, sdate, edate):
    numdays = (edate-sdate).days + 1
    
    #
    #  Set up the details object
    #
    det_obj = ClassWeightDetails()
    det_obj.num_stns = len(stn_data_mon)
    det_obj.num_days = numdays
    for i in range(len(stn_data_mon)):
        det_obj.stn_ids.append(stn_data_mon[i].id)
    det_obj.weights = np.full((det_obj.num_stns, numdays), missing_data)
    
    #
    #  loop through all days    
    #
    for dnum in range(numdays):
        dt = sdate + timedelta(days=dnum)
        ds = dt.strftime("%Y-%m-%d")
        try:
            filename = 'met_details/details_' + dtype + '_' + ds + '.txt'
            print('reading ' + ds.strip() + '...', end='\r')

            #
            #  Set up some temporary lists that will store the data of interest from 
            #  this single file.
            #  Note that in the file these locations are numbered 1..n. 
            #  I will, however, adjust that indexing to 0..n-1 so that I can keep the
            #  list indexing more "python-like"
            #
            loc_list = []      # each entry will be a weight (float value)
            stn_list = []      # each entry will be [id, index], where index is 0..n-1

            #
            #  First read the section with station info (index, x,y,val,id).
            #  Save just the index and id. There may be multiple stations
            #  at a single location, so they will have the same index. For
            #  purposes of this program that is ok.
            #
            #  The ID list may have imbedded commas, so just treat it
            #  as a regular file, not a csv file.
            #
            with open(filename, 'r') as textfile:
                line = textfile.readline()
                while line[0:22] != " ------------ Location":
                    line = textfile.readline()
                line = textfile.readline()      # skip the column header line
                line = textfile.readline()      # presumably first line of data
                while line[27:28] == '[':
                    items = line.split(',')
                    ndx = int(items[0])      # number 1..n
                    loc_list.append(-9.9)    # weight is missing at this point

                    # the id list always starts at column 28, and is surrounded by []
                    j   = line.index(']')
                    s   = line[28:j]        # the list of ids, without the surrounding [] 
                    ids = s.split(',')
                    for i in range(len(ids)):
                        stn_list.append([ids[i],ndx-1])    # index now 0..n-1
                    line = textfile.readline()
                    
            #
            #  Now read the section with subbasin weights.
            #
            with open(filename) as textfile:
                line = textfile.readline()
                while line[0:30] != " ------------ subbasin weights":
                    line = textfile.readline()
                line = textfile.readline()         # skip the column header line
                line = textfile.readline()         # presumably first line of data
                items = line.split(',')
                try:
                    n = int(items[0].strip()) - 1   # index number 0..n-1
                except:
                    n = -9
                snum = 0                        # 0..n-1
                scol = subbasin + 2             # column of interest for requested subbasin
                while n == snum:
                    weight = float(items[scol].strip())
                    loc_list[n-1] = weight
                    line = textfile.readline()
                    items = line.split(',')
                    try:
                        n = int(items[0].strip()) - 1
                    except:
                        n = -9
                    snum = snum + 1
        #
        #  If there was a problem reading the file, set up dummy "blank" lists
        #  for adding to the big details object.
        #  This is most commonly encountered when the details file does not exist.
        #  For example, LLTM input variables in a big historical run don't start 
        #  until 1948, but the LBRM data typically starts in 1940 (as we are doing
        #  things in 2020, anyay).
        #
        except:
            stn_list = []
            loc_list = []

        #
        #  Now transfer the relevant info from this file to the weight details object
        #  
        sndx = -1
        for i in range(len(stn_list)):
            id  = stn_list[i][0]
            ndx = int(stn_list[i][1])          # index 0..n-1
            wgt = loc_list[ndx]
            j = 0
            while j < len(det_obj.stn_ids):
                if det_obj.stn_ids[j] == id:
                    if wgt > missing_data_test:
                        det_obj.weights[j,dnum] = wgt
                    else:
                        det_obj.weights[j,dnum] = missing_data
                    j = len(det_obj.stn_ids)
                j = j + 1
    print()
    return det_obj        


#-----------------------------------------------------------------
def compute_monthly(dly_data, dtype, trail_months):
    num_days = (dly_data.end_date - dly_data.start_date).days + 1
    y1 = dly_data.start_date.year
    m1 = dly_data.start_date.month
    y2 = dly_data.end_date.year
    m2 = dly_data.end_date.month
    num_years = y2 - y1 + 1

    mon_data = ClassMonData()
    mon_data.id         = dly_data.id
    mon_data.name       = dly_data.name
    mon_data.lat        = dly_data.lat
    mon_data.long       = dly_data.long
    mon_data.start_date = dly_data.start_date
    mon_data.end_date   = dly_data.end_date
    mon_data.datavals = np.full((num_years,12), missing_data)

    #
    #  accumulate values
    #
    tot = np.full((num_years,12), 0.0)
    cnt = np.full((num_years,12), 0)
    for d in range(num_days):
        if dly_data.datavals[d] > missing_data_test:
            dt = dly_data.start_date + timedelta(days=d)
            y = dt.year - mon_data.start_date.year
            m = dt.month - 1
            tot[y,m] = tot[y,m] + dly_data.datavals[d]
            cnt[y,m] = cnt[y,m] + 1

    #
    #  Is the monthly value a total or average?
    #  Almost always an average, but precip is a total.
    #
    sum = False
    if dtype=='precipitation': sum = True
            
    #
    #  Assign values for every month where the total number of reporting days
    #  is at least some minimum value, as set here.
    #
    min_days_per_month = 20
    for y in range(num_years):
        for m in range(12):
            if cnt[y,m] >= min_days_per_month:
                if sum:
                    mon_data.datavals[y,m] = tot[y,m]
                else:
                    mon_data.datavals[y,m] = tot[y,m] / cnt[y,m]

    #
    #  Now handle the trailing-month specification
    #
    if trail_months == 1:
        mdata = mon_data
    else:       
        mdata = ClassMonData()
        mdata.id   = mon_data.id
        mdata.name = mon_data.name
        mdata.lat  = mon_data.lat
        mdata.long = mon_data.long
        y = mon_data.start_date.year     # e.g. 1950
        m = mon_data.start_date.month    # 1..12
        m = m + trail_months - 1
        while m > 12:
            y = y + 1
            m = m - 12
        mdata.start_date = datetime(y, m, 1)
        mdata.end_date   = mon_data.end_date
        numy = mdata.end_date.year - mdata.start_date.year + 1

        mdata.datavals   = np.full((numy,12), missing_data)
        sym = mdata.start_date.year*100 + mdata.start_date.month    # e.g. 194807
        eym = mdata.end_date.year*100   + mdata.end_date.month
        yoffset = mdata.start_date.year - mon_data.start_date.year
        for y in range(numy):
            for m in range(12):
                ym = (mdata.start_date.year+y)*100 + (m+1)
                if ym>=sym and ym<=eym:
                    ttot = 0.0
                    tn   = 0
                    for i in range(trail_months):
                        ty = y
                        tm = m - i       # index 0..n
                        while tm < 0:
                            ty = ty - 1     # index 0..numy-1
                            tm = tm + 12    # index  0..11
                        dval = mon_data.datavals[y-yoffset,tm]
                        if dval > missing_data_test:
                            ttot = ttot + dval
                            tn   = tn + 1
                    if tn > 0:
                        if sum:
                            mdata.datavals[y,m] = ttot
                        else:
                            mdata.datavals[y,m] = ttot / tn
                
    return mdata


#-----------------------------------------------------------------
def plot_stations_to_pdf(subdata, stndata, wgtdata, bsn, dtype, trailm):
    per_page = 6

    #
    #  Read the digital subbasin map file (old GLERL format).
    #  This will be used to make a thumbnail location plot for each station.
    #
    gmap = GlerlMap()
    gmap.read_glerl_map_old_format(bsn)

    #
    #  Get vertice coordinates for drawing subbasin outlines on the thumbnails
    #
    map_vert_x, map_vert_y, map_horz_x, map_horz_y = get_subbasin_vertices_for_plot(gmap)

    #  When drawing the thumbnail, this is WAY too many vertices.
    #  So to make it more "readable" and reduce the file size, only use a 
    #  fraction of the vertices.
    #
    draw_vert_x = []
    draw_vert_y = []
    draw_horz_x = []
    draw_horz_y = []
    stepsize = 8

    j = len(map_vert_x) // stepsize
    for i in range(j):
        draw_vert_x.append(map_vert_x[i*stepsize])
        draw_vert_y.append(map_vert_y[i*stepsize])

    j = len(map_horz_x) // stepsize
    for i in range(j):
        draw_horz_x.append(map_horz_x[i*stepsize])
        draw_horz_y.append(map_horz_y[i*stepsize])

    #
    #  Build the figure title string
    #
    titlestr = 'Monthly ' + dtype + ' stations vs lakewide'
    if trailm > 1:
        titlestr = titlestr + ' (' + str(trailm) + '-month trailing '
        if dtype == 'precipitation':
            titlestr = titlestr + 'total)'
        else:
            titlestr = titlestr + 'avg)'
    
    #
    #  Set up the x-axis stuff. All datasets have the same
    #  date extents, so I can do this once and be done.
    #  And, all data sets start in January, so no need to offset anything.
    #    
    num_mons = subdata.datavals.shape[0] * 12
    xvals = np.zeros((num_mons))
    for i in range(num_mons):
        xvals[i] = i

    my_xtick_vals    = []
    my_xtick_strings = []
    y1 = subdata.start_date.year
    y2 = subdata.end_date.year
    if num_mons < 121:
        yearstep = 1
    elif num_mons < 241:
        yearstep = 2
    else:
        yearstep = 5
    for yr in range(y1, y2+1, yearstep):
        my_xtick_vals.append((yr-y1)*12)
        my_xtick_strings.append(str(yr))
    
    #    
    #  Set up the subdata plot stuff. This too is consistent
    #  across all plots
    #
    sub_min =  9.9e9
    sub_max = -9.9e9
    sub_yvals = np.full((num_mons), np.nan)
    y1 = subdata.start_date.year
    y2 = subdata.end_date.year
    i = 0
    for yr in range(y1,y2+1):
        y = yr - y1           # normalizing to zero-based index
        for m in range(12):
            if subdata.datavals[y,m] > missing_data_test:
                sub_yvals[i] = subdata.datavals[y,m]
                if sub_yvals[i] < sub_min: sub_min = sub_yvals[i]
                if sub_yvals[i] > sub_max: sub_max = sub_yvals[i]
            else:
                sub_yvals[i] = np.nan
            i = i + 1
    
    ymin = min(0.0, sub_min * 1.10)
    ymax = sub_max * 1.25
    
    if dtype=='precipitation': ymin = 0.0 - (ymax * 0.05)

    
    stn_num = 0
    done = False
    if trailm == 1:
        fname = 'glshfs_stns_vs_lake_' + dtype + '_' + bsn + '.pdf'
    else:
        fname = 'glshfs_stns_vs_lake_' + dtype + '_' + bsn + '_' + str(trailm) + '.pdf'
    with PdfPages(fname) as pdf:
        while not done:
            if stn_num+(per_page-1) < len(stndata):
                plots_this_page = per_page
            else:
                plots_this_page = len(stndata) - stn_num
            # fig, ax = plt.subplots(nrows=plots_this_page, ncols=1, sharex=True)
            fig = plt.figure()
            fig.suptitle(titlestr, fontsize=11)
            
            #
            #  Use add_gridspec to create a grid that is "per_page" rows and 2 columns.
            #  Then, each of the left columns of these grid will be further subdivided
            #  into 2 rows, where the top row is the data and the lower row is the weights plot.
            #  The thumbnail map will be in column 2 of the outer gridspec.
            #  
            #  Note that the main/outer grid is built using the per_page variable so that the
            #  plots are all the same size, even on the final page that may have fewer plots.
            #
            hr = [1] * per_page
            gs = fig.add_gridspec(per_page, 2, width_ratios=[8,1], height_ratios=hr, wspace=0.05)
            
            ax = [None for i in range(plots_this_page*3)]      # 3 plots per station
            for r in range(plots_this_page):
                gsl = gs[r*2].subgridspec(2,1, height_ratios=[5,2], hspace=0.0)     # inner grid for left side
                p1 = r*3 + 0                       # index of the left plot (data)
                p2 = r*3 + 1                       # index of the left plot (weights)
                p3 = r*3 + 2                       # index of the right plot (map)
                ax[p1] = fig.add_subplot(gsl[0,0])
                ax[p2] = fig.add_subplot(gsl[1,0])
                ax[p3] = fig.add_subplot(gs[r,1])
                
                #
                #  Station data values
                #
                stn = stndata[stn_num]          # ClassMonData object for this station
                stn_yvals = np.full((num_mons), np.nan)
                y1 = stn.start_date.year
                y2 = stn.end_date.year
                i = 0
                for yr in range(y1,y2+1):
                    y = yr - y1           # normalizing to zero-based index
                    for m in range(12):
                        if stn.datavals[y,m] > missing_data_test:
                            stn_yvals[i] = stn.datavals[y,m]
                        else:    
                            stn_yvals[i] = np.nan
                        i = i + 1
                ax[p1].set_ylim([ymin,ymax])
                ax[p1].set_xlim([0,len(xvals)])
                ax[p1].plot(xvals, sub_yvals, color='grey', linewidth=1, label='lakewide', zorder=0)
                ax[p1].plot(xvals, stn_yvals, color='blue', linewidth=1, label='station', zorder=1)
                idlabel = stn.id + ' (' + stn.name + ')'
                ax[p1].text(0.02, 0.7, idlabel, transform=ax[p1].transAxes, fontsize=6)
                ax[p1].tick_params(axis='y', labelsize=6)
                ax[p1].set_xticks([])
                ax[p1].set_xticklabels('')

                #
                #  Station weights
                #  If the weight is 0.0, set it to np.nan so that nothing plots.  This makes the
                #  resulting plot much easier to read.
                #                
                wgt = wgtdata[stn_num]          # ClassMonData object for this station
                wgt_yvals = np.full((num_mons), np.nan)
                y1 = stn.start_date.year
                y2 = stn.end_date.year
                i = 0
                wgts_good = False
                for yr in range(y1,y2+1):
                    y = yr - y1           # normalizing to zero-based index
                    for m in range(12):
                        if wgt.datavals[y,m] > 0.001:
                            wgt_yvals[i] = wgt.datavals[y,m]
                            wgts_good = True
                        else:    
                            wgt_yvals[i] = np.nan
                        i = i + 1
                ax[p2].set_xlim([0,len(xvals)])
                ax[p2].set_ylim([-0.1,0.5])
                ax[p2].set_xticks(my_xtick_vals)
                ax[p2].set_xticklabels('')
                ax[p2].set_yticklabels('')
                ax[p2].set_yticks([])
                ax[p2].axhline(y=0.0, xmin=0, xmax=len(xvals), color='gray', linewidth=0.3, zorder=0)
              #  ax[p2].axhline(y=0.5, xmin=0, xmax=len(xvals), color='gray', linewidth=1, zorder=0)
                ax[p2].plot(xvals, wgt_yvals, color='navy', linewidth=0.6, zorder=6)
                
                #
                #  Draw the thumbnail map with station located on it
                #
                ax[p3].set_ylim([-50,gmap.height+50])
                ax[p3].set_xlim([-50,gmap.width+50])
                ax[p3].set_xticks([], [])
                ax[p3].set_yticks([], [])
                ax[p3].scatter(draw_vert_x, draw_vert_y, 0.05, marker='.', color='gray')
                ax[p3].scatter(draw_horz_x, draw_horz_y, 0.05, marker='.', color='gray')
                sx, sy = gmap.latlong_to_xy(stn.lat, stn.long)
                ax[p3].scatter(sx,sy, 50.0, marker='.', color='blue')
                
                if stn_num == len(stndata)-1: done = True
                stn_num = stn_num + 1
            ax[(plots_this_page-1)*3+1].set_xticks(my_xtick_vals)
            ax[(plots_this_page-1)*3+1].set_xticklabels(my_xtick_strings, fontsize=6, rotation=45)
            
            plt.subplots_adjust(hspace=0.4)
            pdf.savefig()  # saves the current figure into a pdf page
            plt.close(fig)

#-----------------------------------------------------------------
def print_station_list(stn_data_mon, bsn, dtype):
    filename = 'stnlist_' + dtype + '_' + bsn + '.csv'
    with open(filename, "wt") as f:
        f.write('List of stations from stndata file. This can be a template for the\n')
        f.write('master list for the GHCND data extraction/processing program.\n')
        f.write('The excluded years section for each station is usually blank, but in\n')
        f.write('some cases the station is one that we want to use except for a specific\n')
        f.write('period(s) during which it had obvious bad data.\n')
        f.write('If there are more than one such period they need to be separated by semicolons\n')
        f.write('station_id,      station_name,        excluded_years')
        f.write('---------------  -------------------- ')
        for i in range(len(stndata)):
            stn = stndata[i]          # ClassMonData object for this station
            f.write(stn.id.ljust(15, ' ') + ', ' + stn.name[:20] + ',  \n')


#=========================================================================================================
#=========================================================================================================
#=========================================================================================================
#=========================================================================================================
#
#  Command-line processing
#
numArgs = len(sys.argv)
if numArgs < 3:
    print_usage()
    sys.exit()
   
bsn  = sys.argv[1]
dtype = sys.argv[2]
trail_months = 1
if len(sys.argv) > 3:
    try:
        trail_months = int(sys.argv[3])
    except:
        print_usage()
        sys.exit()


#
#  verify and normalize the dtype string
#
dnum = -1
s = dtype.lower()
if s == 'tmin':       dnum = 1
if s == 'airtempmin': dnum = 1

if s == 'tmax':       dnum = 2
if s == 'airtempmax': dnum = 2

if s == 'prc':            dnum = 3
if s == 'prec':           dnum = 3
if s == 'precip':         dnum = 3
if s == 'precipitation':  dnum = 3

if s == 'air':         dnum = 4
if s == 'tavg':        dnum = 4
if s == 'airt':        dnum = 4
if s == 'airtemp':     dnum = 4
if s == 'airtempavg':  dnum = 4

if s == 'dew':         dnum = 5
if s == 'dewp':        dnum = 5
if s == 'dewpt':       dnum = 5
if s == 'dewpoint':    dnum = 5

if s == 'wnd':        dnum = 6
if s == 'wind':       dnum = 6
if s == 'windspeed':  dnum = 6

if s == 'cld':         dnum = 7
if s == 'cloud':       dnum = 7
if s == 'cloudcover':  dnum = 7

if dnum==1: dtype = 'airtempmin'
if dnum==2: dtype = 'airtempmax'
if dnum==3: dtype = 'precipitation'
if dnum==4: dtype = 'airtemp'
if dnum==5: dtype = 'dewpoint'
if dnum==6: dtype = 'windspeed'
if dnum==7: dtype = 'cloudcover'

if dnum<0:
    print_usage()
    sys.exit()   

if dnum==1: dtype4 = 'tmin'
if dnum==2: dtype4 = 'tmax'
if dnum==3: dtype4 = 'prec'
if dnum==4: dtype4 = 'tavg'
if dnum==5: dtype4 = 'dewp'
if dnum==6: dtype4 = 'wind'
if dnum==7: dtype4 = 'cldc'

#
#  Get the subbasin 0 (lakewide) daily data
#
sub_file = 'subdata_' + bsn + '00.csv'
print('Reading subbasin file: ' + sub_file)
sub_data_dly = read_subdata(sub_file, dtype)

#
#  Get the individual station data
#
stn_file = 'stndata_' + dtype + '_' + bsn + '.csv'
print('Reading station data file: ' + stn_file)
stn_data_dly = read_stndata(stn_file)
num_stations = len(stn_data_dly)
print('num_stations=', num_stations)

#
#  Truncate end date to be exactly the end of a month.
#
sdate = stn_data_dly[0].start_date
edate = stn_data_dly[0].end_date
days_in_month = monthrange(edate.year, edate.month)[1]
if days_in_month != edate.day:
    yy = edate.year
    mm = edate.month - 1
    if mm == 0:
        yy = yy -1
        mm = 12
    dd = monthrange(yy,mm)[1]   #  number of days in the month
    edate = datetime(yy,mm,dd)

#
#  Determine number of days and months
#
num_days = (edate - sdate).days + 1
y1 = sdate.year
m1 = sdate.month
y2 = edate.year
m2 = edate.month

#
#  Compute monthly data for each station
#  All stations have the same period of record.
#  Only save the stations that actually have some data.
#
print('Computing monthly values for each station')
stn_data_mon = []
for i in range(num_stations):
    md = compute_monthly(stn_data_dly[i], dtype, trail_months)
    y1 = md.start_date.year
    y2 = md.end_date.year
    good = False
    for yr in range(y1, y2+1):
        y = yr - y1
        for m in range(12):
            if md.datavals[y,m] > missing_data_test: good = True
    if good:
        stn_data_mon.append(md)
            
#
#  Compute monthly data for subbasin.
#  First build a daily data set that is an exact match (date-wise)
#  to the station data.
#
print('Computing monthly values for the lake')
temp = ClassDailyData()
temp.start_date = stn_data_dly[0].start_date
temp.end_date   = edate                    # truncated end date
temp.datavals   = np.full((num_days), missing_data)

sub_offset = temp.start_date - sub_data_dly.start_date
for d in range(num_days):
    offset = sub_offset.days + d     # effective offset into the subbasin data
    if (offset >= 0):
        temp.datavals[d] = sub_data_dly.datavals[offset]
sub_data_mon = compute_monthly(temp, dtype, trail_months)

#
#  Get the daily station weights
#  This routine needs to receive the stn_data_mon object so that it 
#  can get the station ID's for matching up. 
#  We use stn_data_mon rather than stn_data_dly because it is the
#  trimmed-down set of stations (only those with data).
#
print('Reading met_details')
subbasin = 0
sdate = stn_data_dly[0].start_date
edate = stn_data_dly[0].end_date
details_dly = read_details_dly(stn_data_mon, subbasin, dtype, sdate, edate)

#
#  Compute monthly values for the weights
#
stn_wgts_mon = [None] * num_stations
temp = ClassDailyData()
temp.start_date = sdate
temp.end_date   = edate
num_days = (edate - sdate).days + 1
temp.datavals = np.full((num_days), 0.0)
num_stations = details_dly.weights.shape[0]
for i in range(num_stations):
    temp.id = details_dly.stn_ids[i]
    temp.datavals[:] = 0.0
    for d in range(num_days):
        temp.datavals[d] = details_dly.weights[i,d]
    stn_wgts_mon[i] = compute_monthly(temp, 'weights', trail_months)

#
#  for debug/verification.  print these weights
#
num_days = (stn_wgts_mon[0].end_date - stn_wgts_mon[0].start_date).days + 1
y1 = stn_wgts_mon[0].start_date.year
m1 = stn_wgts_mon[0].start_date.month
y2 = stn_wgts_mon[0].end_date.year
m2 = stn_wgts_mon[0].end_date.month
num_years = y2 - y1 + 1

with open('verify_weights.txt', 'w') as textfile:
    textfile.write('YYYY-MM,')
    for i in range(num_stations):
        stn = stn_wgts_mon[i]
        print(stn.id)
        textfile.write("{:>12s},".format(stn.id.strip()))
    textfile.write("\n")
    for y in range(num_years):
        for m in range(12):
            textfile.write("{:4d}-{:02d},".format(y1+y,m+1))
            for i in range(num_stations):
                w = stn_wgts_mon[i].datavals[y,m]
                if w < -1.0: w = 0.0
                textfile.write("{:12.5f},".format(w))
            textfile.write("\n")

#
#  Now plot the stuff
#
print('Building plots...')
plot_stations_to_pdf(sub_data_mon, stn_data_mon, stn_wgts_mon, bsn, dtype, trail_months)

#
#  Print a list of the stations that can later be used for building the
#  "master list" for the GHCND data extraction.
#
print_station_list(stn_data_mon, bsn, dtype4)


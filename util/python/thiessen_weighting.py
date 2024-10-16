import math
import datetime as datetime
import csv
from itertools import islice
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt




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

    #--------------------------------------------------------------------------
    def plot_map(self):
        xx = [x for x in range(self.width)]
        yy = [y for y in range(self.height)]
        pdata = [[0 for c in range(self.width)] for r in range(self.height)]
        for r in range(self.height):
            for c in range(self.width):
                pdata[r][c] = self.map2d[self.height-1-r][c]
        
        plt.matshow(pdata)
        plt.gray()
        plt.show()



#================================================================
#------------------------------------------
#  TStationsMeta contains the metadata for a set of stations.
#     id    is the station ID 
#     name  is the station name
#     lat   is the station latitude in degrees north of the equator
#     long  is the station longitude in degrees EAST of the Prime Meridian.
#             For the GL region, this will typically be about -95 to -75
#     x     is the column index (west to east) for the map raster
#     y     is the row index (south to north) for the map raster
#------------------------------------------
class TStationsMeta:
    def __init__(self):
        self.num_entries = 0
        self.entries = []     # each entry in this list will be a list [id, name, lat, long, x, y]
        
    def add_station(self, id, name, lat, long, x, y):
        new_entry = [id, name, lat, long, x, y]
        self.entries.append(new_entry)
        self.num_entries += 1
        
    
    def get_index(self, id):
        for i in range(num_entries):
            if self.entries[i][0] == id:
                return i
        return -1
        
    def get_station_info_by_index(self, i):
        info = self.entries[i]
        return info
        
    def get_station_info_from_id(self, id):
        i = self.get_index(id)
        if i >= 0:
            info = self.entries[i]
            return info
        else:
            return None
        
#------------------------------------------
#  TNetworkLocations is a list of locations for a single station network. 
#  Each location may contain one or more stations. There may be any number
#  of unique locations (1+) for computing a set of weights.
#
#  Each entry in the locations[] list will be a tuple containing:
#  [x, y, id1, id2, ..., idN]  where,
#     x is the column index (west to east) for the map raster
#     y is the row index (south to north) for the map raster
#     id entries are the station ID strings of all stations that map to that cell
#  Each entry in stndata[] will be a tuple containing the data values for each of
#     those stations.
#  Each entry in celldata[] will be the mean data value from all of the stations
#     in that location.
#  The 3 lists are matched up by index.
#
#  There is one entry in each list for each unique cell that contains a station.
#------------------------------------------
class TNetworkLocations:
    #-------------------------------------------------------        
    def __init__(self):
        self.num_locations = 0
        self.locations = []      # e.g. [[x,y,id,id],   [x,y,id], [x,y,id,id,id]]
        self.stndata   = []      # e.g. [[float,float], [float],  [float,float,float]]
        self.celldata  = []      # [float]    aggregated value for each location
        
    
    #-------------------------------------------------------        
    def get_index_xy(self, x, y):
        for i in range(self.num_locations):
            ix = self.locations[i][0]
            iy = self.locations[i][1]
            if (ix == x) and (iy == y):
                return i
        return -1
    
    #-------------------------------------------------------        
    def get_index_id(self, id):
        for i in range(self.num_locations):
            j = length(self.locations[i])    # how many entries in this tuple?
            for j in range(2, length(self.locations[i])):
                s = self.locations[i][j]
                if j == s:
                    return i
        return -1
    
    #-------------------------------------------------------        
    def get_sorted_index(self, x, y):
        tag = x*1000 + y
        si  = -1
        for i in range(self.num_locations):
            tx = self.locations[i][0]
            ty = self.locations[i][1]
            ttag = tx*1000 + ty
            #
            #  The given location already exists in the list.
            #  Return i as the index, flagged as EXISTING.
            #
            if ttag == tag:
                return [i, True]
                
            #  The given location is not in the list, but the entry at
            #  location i is PAST where the given location would go.
            #  So return i as the index, and flag it as NEW.
            if ttag < tag:
                return [i, False]

        #
        #  We never found a spot, so that means this new entry needs to go 
        #  at the very end. Return an appropriate index and flag it as NEW.
        #
        i = self.num_locations
        return[i, False]
    
    
    #-------------------------------------------------------        
    def add_station(self, x, y, id, dval):
        [i, existing] = self.get_sorted_index(x,y)
        
        if existing:           
            # location exists, so just add this station to that location
            self.locations[i].append(id)
            self.stndata[i].append(dval)
        else:
            # need to add a new location
            self.locations.insert(i, [x, y, id])
            self.stndata.insert(i, [dval])
            self.celldata.insert(i, [-9.9e9])
            self.num_locations += 1

    #-------------------------------------------------------        
    def compute_cell_values(self):
        for i in range(self.num_locations):
            tot = 0.0
            cnt = 0
            for j in range(len(self.stndata[i])):
                tot += self.stndata[i][j]
                cnt += 1
            self.celldata[i] = tot / cnt
        
    #-------------------------------------------------------        
    #  Is the "other" network the same as this one?
    def is_same(self, other):
        if not isinstance(other, TNetworkLocations):
            return False
        if self.num_locations != other.num_locations:
            return False
        for i in range(self.num_locations):
            x1 = self.locations[i][0]
            y1 = self.locations[i][1]
            x2 = other.locations[i][0]
            y2 = other.locations[i][1]
            if (x1 != x2) or (y1 != y2):
                return False
        return True

    #-------------------------------------------------------        
    def list_all(self):
        print('num_locations = ', self.num_locations)
        for i in range(self.num_locations):
            x  = self.locations[i][0]
            y  = self.locations[i][1]
            cv = self.celldata[i]
            s1 = '[' + str(x) + ',' + str(y) + ' (' + str(cv) + ')]'
            
            id = self.locations[i][2]
            sv = self.stndata[i][0]
            s2 = '[' + id + '(' + str(sv) + ')'
            for j in range(3, len(self.locations[i])):
                id = self.locations[i][j]
                sv = self.stndata[i][j-2]
                s2 = s2 + ',' + id + '(' + str(sv) + ')'
            s2 = s2 + ']'
            print(s1, ':', s2)
    
#------------------------------------------
#  TNetworkWeights has the information about a single set of weights.
#------------------------------------------
class TNetworkWeights:

    def __init__(self, network_locs, gmap):
        self.nlo           = network_locs      # a TNetworkLocations object
        self.map           = gmap              # a TGlerlMap object
        self.mapwidth      = gmap.width
        self.mapheight     = gmap.height
        self.num_locations = self.nlo.num_locations
        self.map_valid     = False
        
        #
        # Set up the grid with the index into the locations object. The integer is the 
        # index (in TNetworkLocations) of the nearest location for this cell.
        # The indexing order for this grid is loc_map[height][width]  (i.e. [row][col])
        #
        self.loc_map = [[-1 for c in range(self.mapwidth)] for r in range(self.mapheight)]

        #
        # Set up the weights grid based on the number of subbasins and number of locations.
        # The indexing order is [subbasin][location]
        #
        self.weights = [[0.0 for loc in range(self.num_locations)] for s in range(gmap.subbasins+1)]
    
    #------------------------------------------------------------------------
    def build_closest_map(self, basin_only=False):
        """  
        Builds a 2-d grid that matches the rectangular basin map. Each cell contains the
        index of the closest location. This effectively is the thiessen polygon map.
        
        If basin_only is set to True, then the index assignment will only be done for the
        cells that match a valid basin drainage cell, and the out-of-basin cells will remain
        set to -1.  Otherwise, the closest station index assignment will be done for every
        cell in the rectangular map, including cells outside the actual basin.
        """
        #
        #  Step through the entire map, cell-by-cell. 
        #  For each cell, determine which location is closest.
        #
        for r in range(self.mapheight):
            for c in range(self.mapwidth):
                assign_it = True
                if basin_only and (self.map.map2d[r][c] == -1):
                    assign_it = False
                if assign_it:
                    min_dist_sq = 9.9e29
                    closest = -1                 # index of the closest station to this cell
                    for i in range(self.num_locations):
                        x = self.nlo.locations[i][0]
                        y = self.nlo.locations[i][1]
                        dist_sq = (x-c)**2 + (y-r)**2       
                        if dist_sq < min_dist_sq:
                            closest = i
                            min_dist_sq = dist_sq
                    self.loc_map[r][c] = closest
        self.map_valid = True

    #--------------------------------------------------------------------------
    def compute_weights(self):
        self.build_closest_map(True)
        for s in range(self.map.subbasins+1):
            #
            #  First count the cells in this subbasin
            #
            tcount = 0   
            for r in range(self.mapheight):
                for c in range(self.mapwidth):
                    cellsub = self.map.map2d[r][c]
                    if cellsub == s:
                        tcount += 1

            #
            #  Next determine the count of how many cells in that subbasin are
            #  closest to each of the locations.
            #
            ccount = [0 for loc in range(self.num_locations)]    # count of cells for each location
            for loc in range(self.num_locations):
                for r in range(self.mapheight):
                    for c in range(self.mapwidth):
                        cellsub = self.map.map2d[r][c]
                        if cellsub == s:
                            closest = self.loc_map[r][c]    # index of location closest to this cell
                            if closest == loc:              
                                ccount[loc] += 1

            #
            #  Compute the fractional weight for each location
            #            
            for loc in range(self.num_locations):
                self.weights[s][loc] = float(ccount[loc]) / tcount

    #--------------------------------------------------------------------------
    def write_details(self, filename, celldata):
        """
        self.nlo           = network_locs      # a TNetworkLocations object
        self.map           = gmap              # a TGlerlMap object
        self.mapwidth      = gmap.width
        self.mapheight     = gmap.height
        self.num_locations = self.nlo.num_locations
        self.map_valid     = False
        
        #
        # Set up the grid with the index into the locations object. The integer is the 
        # index (in TNetworkLocations) of the nearest location for this cell.
        # The indexing order for this grid is loc_map[height][width]  (i.e. [row][col])
        #
        self.loc_map = [[-1 for c in range(self.mapwidth)] for r in range(self.mapheight)]

        #
        # Set up the weights grid based on the number of subbasins and number of locations.
        # The indexing order is [subbasin][location]
        #
        self.weights = [[0.0 for loc in range(self.num_locations)] for s in range(gmap.subbasins+1)]
        """
        f = open(filename, "wt")
        if not self.map_valid:
            f.write('This map is not valid\n')
            f.close()
            return
        
        f.write('map width=' + "{:3d}".format(self.mapwidth) + '\n')
        f.write('map height=' + "{:3d}".format(self.mapheight) + '\n')
        f.write('number of locations = ' + "{:d}".format(self.num_locations) + '\n')
        f.write('map of location indices:\n')
        for r in range(self.mapheight-1, -1, -1):
            for c in range(self.mapwidth):
                v = self.loc_map[r][c]
                if v == -1:
                    s = '   .'
                else:
                    s = "{:4d}".format(v)
                f.write(s)
            f.write('\n')

        f.write('---- subbasin weights ----------------------------\n')
        f.write('index     value    x    y')
        for s in range(self.map.subbasins+1):
            f.write('   sub' + "{:02d}".format(s))
        f.write('\n')
        for i in range(self.nlo.num_locations):
            x = self.nlo.locations[i][0]
            y = self.nlo.locations[i][1]
            val = self.nlo.celldata[i]
            f.write("{:5d},{:9.3f},{:4d},{:4d}".format(i,val,x,y))
            for s in range(self.map.subbasins+1):
                f.write("{:8.3f}".format(self.weights[s][i]))
            f.write('\n')
            
        f.write('---- location info --------------------------------\n')
        f.write('index     value    x    y    stations...\n')
        for i in range(self.nlo.num_locations):
            x = self.nlo.locations[i][0]
            y = self.nlo.locations[i][1]
            val = celldata[i]
            f.write("{:5d},{:9.3f},{:4d},{:4d}, [".format(i,val,x,y))
            num_stns = len(self.nlo.locations[i]) - 2
            f.write("{:12s}".format(self.nlo.locations[i][2]))
            for j in range(1, num_stns):
                f.write(", {:12s}".format(self.nlo.locations[i][j+2]))
            f.write(']\n')
        f.close()
        
    #--------------------------------------------------------------------------
    def plot_map(self):
        if not self.map_valid:
            raise Exception('Map has not yet been built, so unable to plot map.')
        
        xx = [x for x in range(self.mapwidth)]
        yy = [y for y in range(self.mapheight)]
        pdata = [[0 for c in range(self.mapwidth)] for r in range(self.mapheight)]
        for r in range(self.mapheight):
            for c in range(self.mapwidth):
                pdata[r][c] = self.loc_map[self.mapheight-1-r][c]
        
        plt.matshow(pdata)
        plt.show()




#------------------------------------------
#  Read the station data and return all of the relevant info in 
#  appropriate objects
#------------------------------------------
def read_station_data(filename, gmap=None):

    #
    #  First read just the header info, using python's standard csv reader.
    #  Column 1 is a row label that we really don't need. The rest of the
    #  columns are the relevant info.
    #  This will give us the list of column names which we will then use when
    #  we read with pandas.
    #
    with open(filename, 'r') as f:
        reader = csv.reader(f)
        [next(reader, None) for item in range(5)]      # skip 5 lines
        stn_ids   = next(reader, None)
        stn_names = next(reader, None)
        stn_lats  = next(reader, None)
        stn_longs = next(reader, None)

    #
    #  Build the metadata structure, a list of all the station metadata.
    #  Column 1 from the file is ignored, of course.
    #
    try:
        all_stations = TStationsMeta()
        for i in range(1, len(stn_ids)):
            id   = stn_ids[i]
            name = stn_names[i]
            lat  = float(stn_lats[i])
            long = float(stn_longs[i])
            x,y = gmap.latlong_to_xy(lat, long)
            all_stations.add_station(id, name, lat, long, int(x), int(y))
    except:
        raise Exception('Error when assigning station metadata')
        
    #        
    #  Prepare the list of column names that will be used for the pandas data frame.
    #  We will use the station IDs, changing column 1 from the row label into the
    #  string "Date" to reflect what will be in that first column.  It's the date
    #  in YYYY-MM-DD format.
    #  Then read the data section with the pandas csv reader.
    #
    colhdr = stn_ids[:]     # make a true copy of the list by using the slice operator
    colhdr[0]  = 'Date'
    df = pd.read_csv(filename, skiprows=10, header=None, names=colhdr, sep=',')

    return df, all_stations


#------------------------------------------
#  Build the network location set from the station data set.
#  This will only include the stations that report on the specified date.
#   stations  = TStationsMeta object with all of the location info
#   dataframe = pandas dataframe with all of the meteorology data (for entire period of record)
#   target_date = datetime object for the desired day
#------------------------------------------
def build_network(stations, dataframe, target_date):
    #
    #  Extract just the line of interest that matches the specified date
    #
    found = False
    i = 0
    while i < len(dataframe.index):
        ds = dataframe.loc[i, 'Date']            # date string
        dt = datetime.datetime.strptime(ds, "%Y-%m-%d").date()
        if dt == target_date:
            Found = True
            sdata = dataframe.iloc[[i]] 
        i += 1
    if not Found:
        raise Exception('Unable to find requested date in the station data.')

    network = TNetworkLocations()
    for i in range(sdata.size-1):
        try:
            dval = float(sdata.iat[0,i+1])
            stn = stations.get_station_info_by_index(i)
            id = stn[0]
            x  = stn[4]
            y  = stn[5]
            network.add_station(x, y, id, dval)
        except:
            continue
    network.compute_cell_values()
    
    return network


#================================================================
def compute_thsn_weights(network, gmap):

    weights = TNetworkWeights(network, gmap)
    weights.compute_weights()
#    weights.plot_map()
    return weights


#================================================================



gmap = GlerlMap()
gmap.read_glerl_map_old_format('sup')
# gmap.plot_map()

#df, allstations = read_station_data('testdata_precip_sup.csv', gmap)
df, allstations = read_station_data('stndata_precipitation_sup.csv', gmap)

f = open('subbasin_output.csv', "wt")
f.write('subbasin weighted values\n')
f.write('YYYY-MM-DD')
for sub in range(gmap.subbasins+1):
    f.write(",{:8d}".format(sub))
f.write('\n')

previous_network = None
previous_weights = None
new_wgts = 0
prv_wgts = 0
numdays = len(df.index)
for i in range(numdays):
    ds = df.loc[i, 'Date']                                  # date string from i-th row
    dt = datetime.datetime.strptime(ds, "%Y-%m-%d").date()
    f.write(dt.strftime("%Y-%m-%d"))
    print('Processing ', dt.strftime("%Y-%m-%d"))
    
    ntwrk = build_network(allstations, df, dt)
    if ntwrk.is_same(previous_network):
        print('using previous weights')
        prv_wgts += 1
        wgt = previous_weights
    else:
        print('computing new weights')
        new_wgts += 1
        wgt = compute_thsn_weights(ntwrk, gmap)

    fname = 'weights_' + ds + '.txt'
    wgt.write_details(fname, ntwrk.celldata)

    for sub in range(gmap.subbasins+1):
        tot = 0.0
        for loc in range(wgt.num_locations):
            dval = ntwrk.celldata[loc]
            wt   = wgt.weights[sub][loc]
            tot += dval * wt
        f.write(",{:8.3f}".format(tot))
    f.write('\n')
   
    previous_network = ntwrk
    previous_weights = wgt
   
f.close()
print('Used previous weights ', prv_wgts, ' times')
print('Computed new weights  ', new_wgts, ' times')



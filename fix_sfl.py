DELIM = '\t'

CRUISE = 'CRUISE'                          # str
FILE = 'FILE'                              # str
date = 'DATE'                              # str
FILE_DURATION = 'FILE DURATION'            # float
LAT = 'LAT'                                # str because it needs the '+'/'-'? Format: Decimal Degrees (DDD)
LON = 'LON'                                # str because it needs the '+'/'-'? Format: Decimal Degrees (DDD)
CONDUCTIVITY = 'CONDUCTIVITY'              # float
SALINITY = 'SALINITY'                      # float
OCEAN_TEMP = 'OCEAN TEMP'                  # float
PAR = 'PAR'                                # float
BULK_RED  = 'BULK RED'                     # float
STREAM_PRESSURE  = 'STREAM PRESSURE'       # float
EVENT_RATE  = 'EVENT RATE'                 # int

FLOATS = [FILE_DURATION, SALINITY, OCEAN_TEMP, BULK_RED, STREAM_PRESSURE, CONDUCTIVITY]
INTS = [EVENT_RATE]
STRS = [FILE, LAT, LON, PAR]

DB_COLUMNS = ['CRUISE', 'FILE', 'DATE', 'FILE_DURATION', 'LAT', 'LON',
              'CONDUCTIVITY', 'SALINITY', 'OCEAN_TEMP', 'PAR', 'BULK_RED',
              'STREAM_PRESSURE', 'EVENT_RATE']

# default for july 2014 cruise, change later
cruise_id = 'july2014'


# takes data and header as lists, dbpath as a string
def fix_and_insert_sfl(data, header, dbpath, cruise=cruise_id):
    if len(data) != len(header):
        raise IndexError("Different number of items in data and header: h - " + str(len(header)) + ", d - " + str(len(data)))

    dbcolumn_to_fixed_data = {}
    
    for d, h in zip(data, header):
        h = h.upper()
        if h in FLOATS:
            h = h.strip().replace(' ', '_')
            try:
                dbcolumn_to_fixed_data[h] = float(d)
            except:
                dbcolumn_to_fixed_data[h] = None
        elif h in INTS:
            h = h.strip().replace(' ', '_')
            try:
                dbcolumn_to_fixed_data[h] = int(d)
            except:
                dbcolumn_to_fixed_data[h] = None
        elif h in STRS:
            h = h.strip().replace(' ', '_')
            if h == LAT or h == LON:
                # do this at some point
                # dbcolumn_to_fixed_data[h] = fix_lat_or_lon(d)
                # delete this later
                try:
                  dbcolumn_to_fixed_data[h] = d
                except:
                  dbcolumn_to_fixed_data[h] = None
            else:
                try:
                    dbcolumn_to_fixed_data[h] = d
                except:
                    dbcolumn_to_fixed_data[h] = None
        # else, do nothing
        
    dbcolumn_to_fixed_data[CRUISE] = cruise
            
    # any fields that weren't passed in should be None
    for c in DB_COLUMNS:
        if not c in dbcolumn_to_fixed_data:
            dbcolumn_to_fixed_data[c] = None

    # TODO: insert dbcolumn_to_fixed_data to db. 
    return dbcolumn_to_fixed_data

def fix_lat_or_lon(l):
    return None

if __name__ == "__main__":
    sfl_header = 'SALINITY\tBLAHHH\tOCEAN TEMP'
    sfl_sample_line = '10.2\tNOOOOO\tnot a float'
    dbpath = ''
    print
    print fix_and_insert_sfl(sfl_sample_line.split('\t'), sfl_header.split('\t'), dbpath)

    print 
    print 

    real_sfl_header = 'FILE\tFILE DURATION\tLAT\tLON\tCONDUCTIVITY\tSALINITY\tOCEAN TEMP\tBULK RED\tSTREAM PRESSURE\tFILTER PRESSURE\tMACHINE TEMP\tXaccel\tYaccel\tZaccel\tMILLISECOND TIMER\tLASER POWER\tEVENT RATE\tFLOW METER\tposition\tNULL\tNULL\tNULL'
    real_sfl_line = '2014-05-15T17-07-08+0000\t180.141\t18.7178\t3.53\t-829.34\t2.95\t0.0026\t0.0052\t0.0029\t146767125\t0.0\t0.00\tnometer\tC\t\t\t\t\t\t\t\t' 
    dbpath = ''

    print fix_and_insert_sfl(real_sfl_line.split('\t'), real_sfl_header.split('\t'), dbpath)
    print 

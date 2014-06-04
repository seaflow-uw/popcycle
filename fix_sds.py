DELIM = '\t'

FILE = 'FILE'                              # str
computerUTC = 'computerUTC'                # int (or long?)
DMY = 'DMY'                                # str  DD-MM-YYYY
HMS = 'HMS'                                # str  HH:MM:SS
LAT = 'LAT'                                # str because it needs the '+'/'-'? Format: Decimal Degrees (DDD)
LON = 'LON'                                # str because it needs the '+'/'-'? Format: Decimal Degrees (DDD)
CONDUCTIVITY = 'CONDUCTIVITY'              # ?  -- str for now
SALINITY = 'SALINITY'                      # float
OCEAN_TEMP = 'OCEAN TEMP'                  # float
BULK_RED  = 'BULK RED'                     # float
STREAM_PRESSURE  = 'STEAM PRESSURE'        # float
FILTER_PRESSURE  = 'FILTER PRESSURE'       # float
MACHINE_TEMP  = 'MACHINE TEMP'             # float
Xaccel  = 'Xaccel'                         # float
Yaccel  = 'Yaccel'                         # float
Zaccel  = 'Zaccel'                         # float
MILLISECOND_TIMER  = 'MILLISECOND TIMER'   # int (or long?)
LASER_POWER  = 'LASER POWER'               # float
EVENT_RATE  = 'EVENT RATE'                 # int
FLOW_METER  = 'FLOW METER'                 # float
POSITION  = 'POSITION'                     # ? -- str for now 
CHL  = 'CHL'                               # float
TRANS  = 'TRANS'                           # float
PAR = 'PAR'                                # ? -- str for now

FLOATS = [SALINITY, OCEAN_TEMP, BULK_RED, STREAM_PRESSURE, FILTER_PRESSURE, MACHINE_TEMP, 
          Xaccel, Yaccel, Zaccel, LASER_POWER, FLOW_METER, CHL, TRANS]
INTS = [computerUTC, MILLISECOND_TIMER]
STRS = [FILE, DMY, HMS, POSITION, PAR]

COLUMNS = [FILE, computerUTC, DMY, HMS, LAT, LON, CONDUCTIVITY, SALINITY, OCEAN_TEMP, 
           BULK_RED, STEAM_PRESSURE, FILTER_PRESSURE, MACHINE_TEMP, Xaccel, Yaccel, Zaccel, MILLISECOND_TIMER, 
           LASER_POWER, EVENT_RATE, POSITION, CHL, TRANS, PAR]

def set_columns(header_line):
    COLUMNS = header_line.strip().split(DELIM)

# assumes that items in line are in the column same order as header
def insert_sds_line_to_db(line, db, header_line=None):
    if header_line:
        set_columns(header_line)

    items = line.split(DELIM) # might want a check that len(items) = len(columns)
    
    items_fixed = items + [None]*len(COLUMNS) # last len(COLUMNS) items will represent bad data
    
    for i, item in enumerate(items):
        if COLUMNS[i] in FLOATS:
            try: 
                items[i] = float(item)
            except:
                items[len(COLUMNS)+i-1] = item
                items[i] = None
        elif COLUMNS[i] in INTS:
            try:
                items[i] = int(item)
            except:
                items[len(COLUMNS)+i-1] = item
                items[i] = None
        else: # should be in STRS
            if COLUMNS[i] == 'LAT' or COLUMNS[i] == 'LON':
                # question: do they need to be fixed together or is independently ok?
                # can we assume one is always after the other in the order?
                items[len(COLUMNS)+i-1] = item
                items[i] = fix_lat_or_lon(item)
            # else, for now, assume everything else is already a string and in an ok format
            # may want to separately check format of DMY/HMS, but not yet

    # TODO: insert items_fixed into db

def fix_lat_or_lon(l):
    # try to fix -- put in Decimal Degrees (DDD) Format
    
    # check if WICOR format:
    # can't find info on converting this -- help! 
    
    # check if NMEA (GGA) format:
    # example: 
    # 4807.038,N  --> Latitude 48 deg 07.038' N --> 
    # 01131.000,E -->  Longitude 11 deg 31.000' E --> 


    # if possible, return new lat/lon
    # if not possible, return None
    return None

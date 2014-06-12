import sqlite3
import os
import glob
import re
import sys

DELIM = '\t'

CRUISE = 'CRUISE'                          # str
FILE = 'FILE'                              # str
DATE = 'DATE'                              # str
FILE_DURATION = 'FILE DURATION'            # float
LAT = 'LAT'                                # float --> Format: Decimal Degrees (DDD)
LON = 'LON'                                # float --> Format: Decimal Degrees (DDD)
CONDUCTIVITY = 'CONDUCTIVITY'              # float
SALINITY = 'SALINITY'                      # float
OCEAN_TEMP = 'OCEAN TEMP'                  # float
PAR = 'PAR'                                # float
BULK_RED  = 'BULK RED'                     # float
STREAM_PRESSURE  = 'STREAM PRESSURE'       # float
FLOW_RATE = 'FLOW RATE'                    # float
EVENT_RATE  = 'EVENT RATE'                 # int

FLOATS = [FILE_DURATION, SALINITY, OCEAN_TEMP, BULK_RED, STREAM_PRESSURE, CONDUCTIVITY, PAR, LAT, LON]
INTS = [EVENT_RATE]
STRS = [FILE, DATE]

DB_COLUMNS = ['CRUISE', 'FILE', 'DATE', 'FILE_DURATION', 'LAT', 'LON',
              'CONDUCTIVITY', 'SALINITY', 'OCEAN_TEMP', 'PAR', 'BULK_RED',
              'STREAM_PRESSURE', 'FLOW_RATE','EVENT_RATE']

# default for july 2014 cruise, change later
cruise_id = 'july2014'


# takes data and header as lists, dbpath as a string
def fix_and_insert_sfl(data, header, dbpath, cruise=cruise_id):
    #if len(data) != len(header):
    #    raise IndexError("Different number of items in data and header: h - " + str(len(header)) + ", d - " + str(len(data)))

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
            try:
                dbcolumn_to_fixed_data[h] = d
            except:
                dbcolumn_to_fixed_data[h] = None
        # else, do nothing

    # add cruise and data 
    dbcolumn_to_fixed_data[CRUISE] = cruise
    try :
      iso_split = dbcolumn_to_fixed_data[FILE].split('T')
      iso_split[1] = iso_split[1].replace('-', ':')
      #iso_split[1] = iso_split[1][:-2] + ':' + iso_split[1][-2:]
      dbcolumn_to_fixed_data[DATE] = 'T'.join(iso_split)
    except:
      dbcolumn_to_fixed_data[DATE] = None
            
    # any fields that weren't passed in should be None
    for c in DB_COLUMNS:
        if not c in dbcolumn_to_fixed_data:
            dbcolumn_to_fixed_data[c] = None

    # populate list
    db_tuple = []
    for c in DB_COLUMNS :
      db_tuple.append(dbcolumn_to_fixed_data[c])

    # insert into sqlite
    conn = sqlite3.connect(dbpath)
    c = conn.cursor()
    c.execute("insert into sfl values (?,?,?,?,?,?,?,?,?,?,?,?,?)", tuple(db_tuple))
    conn.commit()
    conn.close()

    #return db_tuple

def insert_file_bulk(sfl_file) :
    lines = open(sfl_file).readlines()
    header = lines[0].split('\t')
    dbpath = os.path.expanduser('~/popcycle/sqlite/popcycle.db')
    for line in lines[1:] :
        data = line.split('\t')
        conn = sqlite3.connect(dbpath)
        c = conn.cursor()
        c.execute("delete from sfl where file == '" + data[0] + "'")
        conn.commit()
        conn.close()

        fix_and_insert_sfl(data, header, dbpath)

def insert_last_entry() :
    dbpath = os.path.expanduser('~/popcycle/sqlite/popcycle.db')
    evt_path = os.path.expanduser('~/SeaFlow/datafiles/evt/')
    latest_day = sorted([ name for name in os.listdir(evt_path) if os.path.isdir(os.path.join(evt_path, name)) ])[-1]
    sfl_file = glob.glob(os.path.join(evt_path,latest_day) + '/*.sfl')[0]
    lines = open(sfl_file).readlines()

    fix_and_insert_sfl(lines[-1].split('\t'), lines[0].split('\t'), dbpath)

def insert_from_command_line() :
    dbpath = os.path.expanduser('~/popcycle/sqlite/popcycle.db')
    lines = []
    for line in sys.stdin :
        print line
        lines.append(line)

    fix_and_insert_sfl(lines[-1].split('\t'), lines[0].split('\t'), dbpath)

if __name__ == "__main__":
    insert_last_entry()

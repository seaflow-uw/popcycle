import sqlite3
import os
import glob
import re
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

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

# Default file system locations
DEFAULT_EVT_DIR = '~/SeaFlow/datafiles/evt/'
DEFAULT_DB = '~/popcycle/sqlite/popcycle.db'

# Default for july 2014 cruise
DEFAULT_CRUISE_ID = 'july2014'


# takes data and header as lists, dbpath as a string
def fix_and_insert_sfl(data, header, dbpath = DEFAULT_DB, cruise = DEFAULT_CRUISE_ID):
    #if len(data) != len(header):
    #    raise IndexError("Different number of items in data and header: h - " + str(len(header)) + ", d - " + str(len(data)))

    dbcolumn_to_fixed_data = {}
    
    for d, h in zip(data, header):
        h = h.upper()
        h = h.strip('\n')
        d = d.strip('\n')
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
    if re.match('\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}\+0000', dbcolumn_to_fixed_data[FILE]):
        # New style EVT file names, e.g. 2014-05-15T17-07-08+0000.
        # Convert to a ISO 8601 date string
        try :
          iso_split = dbcolumn_to_fixed_data[FILE].split('T')
          iso_split[1] = iso_split[1].replace('-', ':')
          #iso_split[1] = iso_split[1][:-2] + ':' + iso_split[1][-2:]
          dbcolumn_to_fixed_data[DATE] = 'T'.join(iso_split)
        except:
          dbcolumn_to_fixed_data[DATE] = None

    # try to add flow rate
    try :
      stream_pressure = dbcolumn_to_fixed_data['STREAM_PRESSURE']
      ratio_evt_stream = 0.14756
      flow_rate = 1000 * (-9*10**-5 * stream_pressure**4 + 0.0066 * stream_pressure**3 - 0.173 * stream_pressure**2 + 2.5013 * stream_pressure + 2.1059) * ratio_evt_stream
      dbcolumn_to_fixed_data['FLOW_RATE'] = flow_rate
    except Exception as e:
      dbcolumn_to_fixed_data['FLOW_RATE'] = None
            
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
    c.execute("insert into sfl values (?,?,?,?,?,?,?,?,?,?,?,?,?,?)", tuple(db_tuple))
    conn.commit()
    conn.close()

    #return db_tuple

def insert_file_bulk(sfl_file, db = DEFAULT_DB, cruise = DEFAULT_CRUISE_ID) :
    lines = open(sfl_file).readlines()
    header = lines[0].split('\t')
    dbpath = os.path.expanduser(db)
    for line in lines[1:] :
        data = line.split('\t')
        conn = sqlite3.connect(dbpath)
        c = conn.cursor()
        c.execute("delete from sfl where file == '" + data[0] + "'")
        conn.commit()
        conn.close()

        fix_and_insert_sfl(data, header, dbpath, cruise)

def insert_last_entry(db = DEFAULT_DB, evt_path = DEFAULT_EVT_DIR, cruise = DEFAULT_CRUISE_ID) :
    dbpath = os.path.expanduser(db)
    evt_path = os.path.expanduser(evt_path)
    latest_day = sorted([ name for name in os.listdir(evt_path) if os.path.isdir(os.path.join(evt_path, name)) ])[-1]
    sfl_file = glob.glob(os.path.join(evt_path,latest_day) + '/*.sfl')[0]
    lines = open(sfl_file).readlines()

    fix_and_insert_sfl(lines[-1].split('\t'), lines[0].split('\t'), dbpath, cruise)

def insert_last_file(db = DEFAULT_DB, evt_path = DEFAULT_EVT_DIR, cruise = DEFAULT_CRUISE_ID) :
    dbpath = os.path.expanduser(db)
    evt_path = os.path.expanduser(evt_path)
    latest_day = sorted([ name for name in os.listdir(evt_path) if os.path.isdir(os.path.join(evt_path, name)) ])[-1]
    sfl_file = glob.glob(os.path.join(evt_path,latest_day) + '/*.sfl')[0]
    insert_file_bulk(sfl_file, dbpath, cruise)

def insert_from_command_line(db = DEFAULT_DB, cruise = DEFAULT_CRUISE_ID) :
    dbpath = os.path.expanduser(db)
    header = None
    for line in sys.stdin :
        if not header:
            header = line.split('\t')
        else:
            fields = line.split('\t')
            fix_and_insert_sfl(line.split('\t'), header, dbpath, cruise)

if __name__ == "__main__":
    parser = ArgumentParser(
        description='Insert SFL file data into a popcycle sqlite3 database',
        prog='fix_sfl.py',
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-d', '--db',
        help='sqlite3 database file',
        default=DEFAULT_DB)
    parser.add_argument(
        '-c', '--cruise',
        help='cruise name',
        default=DEFAULT_CRUISE_ID)
    parser.add_argument(
        '-e', '--evt_dir',
        help='EVT data directory',
        default=DEFAULT_EVT_DIR)
    parser.add_argument(
        '-s', '--sfl',
        help='''SFL file.  If not provided, the SFL file for the latest day in
                EVT_DIR will be used.  Use - to read from STDIN.''')

    args = parser.parse_args()

    if args.sfl == '-':
        # From STDIN
        insert_from_command_line(args.db, args.cruise)
    elif not args.sfl:
        # SFL file for last day
        insert_last_file(args.db, args.evt_dir, args.cruise)
    else:
        # User specific SFL file
        insert_file_bulk(args.sfl, args.db, args.cruise)

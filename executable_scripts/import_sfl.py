#!/usr/bin/env python
"""
Import SFl file into sqlite3 database.  Assumes FLOW RATE is a column in input
SFL file.
"""

import sqlite3
import os
import glob
import re
import sys
from argparse import ArgumentParser

DELIM = '\t'

CRUISE = 'CRUISE'                          # str
FILE = 'FILE'                              # str
DATE = 'DATE'                              # str
FILE_DURATION = 'FILE DURATION'            # float
LAT = 'LAT'                                # string --> Format: Decimal Degrees (DDD) or GGA
LON = 'LON'                                # string --> Format: Decimal Degrees (DDD) or GGA
CONDUCTIVITY = 'CONDUCTIVITY'              # float
SALINITY = 'SALINITY'                      # float
OCEAN_TEMP = 'OCEAN TEMP'                  # float
PAR = 'PAR'                                # float
BULK_RED  = 'BULK RED'                     # float
STREAM_PRESSURE  = 'STREAM PRESSURE'       # float
FLOW_RATE = 'FLOW RATE'                    # float
EVENT_RATE  = 'EVENT RATE'                 # float

FLOATS = [FILE_DURATION, SALINITY, OCEAN_TEMP, BULK_RED, STREAM_PRESSURE, FLOW_RATE, CONDUCTIVITY, PAR, EVENT_RATE]
STRS = [FILE, DATE, LAT, LON]

DB_COLUMNS = ['CRUISE', 'FILE', 'DATE', 'FILE_DURATION', 'LAT', 'LON',
              'CONDUCTIVITY', 'SALINITY', 'OCEAN_TEMP', 'PAR', 'BULK_RED',
              'STREAM_PRESSURE', 'FLOW_RATE','EVENT_RATE']


# takes data and header as lists, dbpath as a string
def fix_and_insert_sfl(data, header, dbpath, cruise):
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
            except ValueError:
                dbcolumn_to_fixed_data[h] = None
        elif h in STRS:
            h = h.strip().replace(' ', '_')
            dbcolumn_to_fixed_data[h] = d
        # else, do nothing

    # add cruise and data
    dbcolumn_to_fixed_data[CRUISE] = cruise
    if re.match(r'\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}\+00-?00', dbcolumn_to_fixed_data[FILE]):
        # New style EVT file names, e.g. 2014-05-15T17-07-08+0000 or 2014-05-15T17-07-08+00-00
        # Convert to a ISO 8601 date string
        iso_split = dbcolumn_to_fixed_data[FILE].split('T')
        iso_split[1] = iso_split[1].replace('-', ':')
        dbcolumn_to_fixed_data[DATE] = 'T'.join(iso_split)

    # Make sure DATE field is formatted consistently
    # Add ":" in time zone offset if not present for consistency in DB.  All DATE fields in DB should
    # have time zone format 00:00, not 0000
    if (dbcolumn_to_fixed_data[DATE][-3] != ":"):
        dbcolumn_to_fixed_data[DATE] = dbcolumn_to_fixed_data[DATE][:-2] + ":" + dbcolumn_to_fixed_data[DATE][-2:]

    # any fields that weren't passed in should be
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

def insert_files_bulk(sfl_files, db, cruise):
    for sfl_file in sfl_files:
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

def insert_last_entry(db, evt_path, cruise):
    dbpath = os.path.expanduser(db)
    evt_path = os.path.expanduser(evt_path)
    latest_day = sorted([ name for name in os.listdir(evt_path) if os.path.isdir(os.path.join(evt_path, name)) ])[-1]
    sfl_file = glob.glob(os.path.join(evt_path,latest_day) + '/*.sfl')[0]
    lines = open(sfl_file).readlines()

    fix_and_insert_sfl(lines[-1].split('\t'), lines[0].split('\t'), dbpath, cruise)

def insert_last_file(db, evt_path, cruise):
    dbpath = os.path.expanduser(db)
    evt_path = os.path.expanduser(evt_path)
    latest_day = sorted([ name for name in os.listdir(evt_path) if os.path.isdir(os.path.join(evt_path, name)) ])[-1]
    sfl_file = glob.glob(os.path.join(evt_path,latest_day) + '/*.sfl')[0]
    insert_files_bulk([sfl_file], dbpath, cruise)

def insert_all_files(db, evt_path, cruise):
    dbpath = os.path.expanduser(db)
    evt_path = os.path.expanduser(evt_path)
    insert_files_bulk(find_sfl_files(evt_path), dbpath, cruise)

def insert_from_command_line(db, cruise):
    dbpath = os.path.expanduser(db)
    header = None
    for line in sys.stdin :
        if not header:
            header = line.split('\t')
        else:
            fix_and_insert_sfl(line.split('\t'), header, dbpath, cruise)

def find_sfl_files(evt_path):
    evt_path = os.path.expanduser(evt_path)
    sfl_paths = []
    for dirpath, dirnames, filenames in os.walk(evt_path):
        for f in filenames:
            if f.endswith(".sfl"):
                sfl_paths.append(os.path.join(dirpath, f))
    return sfl_paths

if __name__ == "__main__":
    parser = ArgumentParser(
        description='Insert SFL file data into a popcycle sqlite3 database',
        prog='import_sfl.py')
    parser.add_argument(
        '-d', '--db',
        required=True,
        help='sqlite3 database file, e.g. ~/popcycle/sqlite/popcycle.db')
    parser.add_argument(
        '-c', '--cruise',
        required=True,
        help='cruise name, e.g. CMOP_3')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        '-e', '--evt_dir',
        help='EVT data directory if specific SFL file not provided, e.g ~/SeaFlow/datafiles/evt/')
    group.add_argument(
        '-s', '--sfl',
        help='''SFL file.  If not provided, all SFL files in EVT_DIR will be
             imported.''')

    args = parser.parse_args()

    if not args.sfl:
        # Try to insert all SFl files in EVT dir
        insert_all_files(args.db, args.evt_dir, args.cruise)
    else:
        # User specified SFL file
        insert_files_bulk([args.sfl], args.db, args.cruise)

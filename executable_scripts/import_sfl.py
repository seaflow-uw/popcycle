#!/usr/bin/env python
"""
Import SFl file into sqlite3 database.  Assumes FLOW RATE is a column in input
SFL file.
"""

from argparse import ArgumentParser
import datetime
import dateutil.parser
import glob
import os
import re
import sqlite3
import sys


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


def fix_one_sfl_line(data, header, cruise):
    """Convert one line of SFL file into tuple ready for db insert"""
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

    # add cruise, date, and add julian day if missing
    dbcolumn_to_fixed_data[CRUISE] = cruise
    if "DATE" in dbcolumn_to_fixed_data:
        # Input is an SFL converted from SDS which has a DATE column
        pass
    else:
        # Input is new style SFL where date is parsed from file
        dbcolumn_to_fixed_data[DATE] = date_from_file_name(dbcolumn_to_fixed_data[FILE])

    if len(dbcolumn_to_fixed_data[FILE].split("/")) == 1:
        dbcolumn_to_fixed_data[FILE] = "/".join([
            julian_from_file_name(dbcolumn_to_fixed_data[FILE]),
            dbcolumn_to_fixed_data[FILE]
        ])


    # any fields that weren't passed in should be
    for c in DB_COLUMNS:
        if not c in dbcolumn_to_fixed_data:
            dbcolumn_to_fixed_data[c] = None

    # populate list
    db_tuple = []
    for c in DB_COLUMNS :
        db_tuple.append(dbcolumn_to_fixed_data[c])

    return tuple(db_tuple)

def insert_files_bulk(sfl_files, db, cruise):
    for sfl_file in sfl_files:
        lines = open(sfl_file).readlines()
        header = lines[0].split('\t')
        to_insert = []
        for line in lines[1:]:
            data = line.split('\t')
            to_insert.append(fix_one_sfl_line(data, header, cruise))
        insert_tuples(to_insert, db)

def insert_tuples(to_insert, db):
    dbpath = os.path.expanduser(db)
    conn = sqlite3.connect(dbpath)
    c = conn.cursor()

    cruise_idx = DB_COLUMNS.index("CRUISE")
    file_idx = DB_COLUMNS.index("FILE")
    to_delete = [(x[cruise_idx], x[file_idx]) for x in to_insert]

    c.executemany("DELETE FROM sfl WHERE cruise == ? AND file == ?", to_delete)
    conn.commit()

    c.executemany("INSERT INTO sfl VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)", to_insert)
    conn.commit()

    conn.close()

def insert_all_files(db, evt_path, cruise):
    dbpath = os.path.expanduser(db)
    evt_path = os.path.expanduser(evt_path)
    if not os.path.isdir(evt_path):
        raise ValueError("%s is not directory or does not exist" % evt_path)
    insert_files_bulk(find_sfl_files(evt_path), dbpath, cruise)

def find_sfl_files(evt_path):
    evt_path = os.path.expanduser(evt_path)
    sfl_paths = []
    for dirpath, dirnames, filenames in os.walk(evt_path):
        for f in filenames:
            if f.endswith(".sfl"):
                sfl_paths.append(os.path.join(dirpath, f))
    return sfl_paths

def date_from_file_name(file_name):
    date = None
    match = re.match(r'(\d{4}-\d{2}-\d{2})T(\d{2}-\d{2}-\d{2})([+-]\d{2}-?\d{2})', file_name)
    if match:
        # New style EVT file names, e.g.
        # - 2014-05-15T17-07-08+0000
        # - 2014-05-15T17-07-08+00-00
        # - 2014-05-15T17-07-08-0700
        # - 2014-05-15T17-07-08-07-00

        # Convert to a ISO 8601 date string
        datestamp, timestamp, tz = match.groups()
        if len(tz) == 5:
            # If the timezone string (e.g. +0000) does not have a
            # "-" in the middle, add ":" in its place
            tz = tz[:3] + ":" + tz[3:]
        else:
            # Convert middle "-" to ":"
            tz = tz[:3] + ":" + tz[4:]
        # SeaFlow EVT file names have "-"s instead of ":"s due to filesystem
        # naming rules. Fix things up to make valid time strings here
        timestamp = timestamp.replace('-', ':')
        # Put it all back together
        date = datestamp + 'T' + timestamp + tz

    if date is None:
        raise ValueError("Could not parse file name %s\n" % file_name)

    return date

def julian_from_file_name(file_name):
    """Converts a dated EVT file name to a Seaflow day of year folder name.

    "2014-07-04T00-00-02+00-00" or "2014-07-04T00-00-02+0000" would return
    "2014_185".

    Args:
        evt_filename: EVT filename, may include path information
    """
    iso8601 = date_from_file_name(os.path.basename(file_name))
    dt = dateutil.parser.parse(iso8601)
    dt_jan1 = datetime.date(dt.year, 1, 1)
    day = dt.toordinal() - dt_jan1.toordinal() + 1
    return "%i_%i" % (dt.year, day)


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

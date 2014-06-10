#!/usr/bin/env python

from collections import defaultdict
import os
import sys
import string
import glob
import re

def get_last_sfl() :
    evt_path = os.path.expanduser('~/SeaFlow/datafiles/evt/')
    latest_day = sorted([ name for name in os.listdir(evt_path) if os.path.isdir(os.path.join(evt_path, name)) ])[-1]
    return glob.glob(os.path.join(evt_path,latest_day) + '/*.sfl')[0]

lines = open(get_last_sfl()).readlines()
sys.stdout = open(get_last_sfl(), 'w')

for line in lines:
    # Remove any trailing newline characters
    while len(line) > 0 and (line[-1] == '\r' or line[-1] == '\n'):
        line = line[:-1]
    # Split the line into fields based on tabs
    line = line.split('\t')

    # First line of the file, just print it
    if line[0] == 'FILE':
        sys.stdout.write("%s" % '\t'.join(line))
    # Any line that starts with 'sds', terminate the previous line and print it
    # starts with 'sds' because all filenames start with 'sds' and are in the first column
    elif re.search('[0-9]{4}-[0-9]{2}-[0-9]{2}T', line[0]) is not None:
        sys.stdout.write("%s%s" % (os.linesep, '\t'.join(line)))
    # Any other line, just print it
    else:
        sys.stdout.write("%s" % '\t'.join(line))

# Terminate the lsat line
sys.stdout.write(os.linesep)

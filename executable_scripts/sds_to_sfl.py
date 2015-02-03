#!/usr/bin/env python
"""
Convert old Seaflow SDS file format to SFL, with STREAM PRESSURE converted
to FLOW RATE with user supplied ratio.
"""

import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

# key = instrument serial number
# value = ratio_evt_stream for that instrument
SERIALS = {
    "740": 0.1738,
    "751": 0.2769,
    "989": 0.1800
}

parser = ArgumentParser(
    description="""Convert old Seaflow SDS file format to SFL, with STREAM
                PRESSURE converted to FLOW RATE with user supplied ratio.""",
    prog='sds_to_sfl.py',
    formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument(
    '--sds',
    required=True,
    help='Input SDS file')
parser.add_argument(
    '--sfl',
    required=True,
    help='Output SFL file.')
parser.add_argument(
    '--serial',
    required=True,
    help='Seaflow instrument serial number')

args = parser.parse_args()
if not (args.sds and args.sfl and args.serial):
    print("A required argument is missing\n")
    parser.print_help()

try:
    ratio_evt_stream = SERIALS[args.serial]
except KeyError as e:
    sys.stderr.write("Instrument serial number %s not recognized\n" % args.serial)
    sys.exit(1)

sds_file = args.sds
f = open(sds_file)
f2 = open(args.sfl, 'w')

header = f.readline()
elements = header.split('\t')
elements[-2] = 'FILE'           # replace 'day' column with 'FILE'
elements[-1] = 'FILE DURATION'  # replace 'file' column
elements[-3] = 'DATE'           # replace 'time' column

# Older SDS files had periods between words of a single header column
# e.g. STREAM.PRESSURE
# To make these column headers compatible with new SFL file processing
# these periods should be converted to a single space
elements = [x.replace('.', ' ') for x in elements]

# Find STREAM PRESSURE column index
stream_index = None
for i, name in enumerate(elements):
    if name == 'STREAM PRESSURE':
        stream_index = i
if stream_index is None:
    sys.stderr.write('STREAM PRESSURE not found in SDS file\n')
    sys.exit(1)

elements.append('FLOW RATE')

f2.write('\t'.join(elements))
f2.write('\n')

files_seen = set()
for line in f :
    elements = line.split('\t')
    # Build 'FILE' value from 'file' and 'day'
    elements[-2] = elements[-2] + '/' + elements[-1].strip() + '.evt'
    # FILE DURATION values fixed at 180 seconds
    elements[-1] = '180'
    # Construct a ISO 8601 date string from 'time' field of SDS file and
    # replace 'time' field with this string
    elements[-3] = elements[-3].replace(' ', 'T') + '+0000'
    # Create a new FLOW RATE element from STREAM PRESSURE and ratio_evt_stream
    try:
        stream_pressure = float(elements[stream_index])
        flow_rate = str(1000 * (-9*10**-5 * stream_pressure**4 + 0.0066 * stream_pressure**3 - 0.173 * stream_pressure**2 + 2.5013 * stream_pressure + 2.1059) * ratio_evt_stream)
    except ValueError as e:
        flow_rate = 'NA'

    if not (elements[-2] in files_seen):
        files_seen.add(elements[-2])
        elements.append(flow_rate)
        f2.write('\t'.join(elements))
        f2.write('\n')

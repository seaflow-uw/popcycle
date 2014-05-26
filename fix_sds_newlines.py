#!/usr/bin/env python

from collections import defaultdict
import os
import sys
import string

DEBUG = False
if DEBUG:
    counts = defaultdict(int)

for line in sys.stdin:
    # Remove any trailing newline characters
    while len(line) > 0 and (line[-1] == '\r' or line[-1] == '\n'):
        line = line[:-1]
    # Split the line into fields based on tabs
    line = line.split('\t')

    # debugging: keep track of how many of each line length we saw
    if DEBUG:
        counts[len(line)] += 1

    # First line of the file, just print it
    if line[0] == 'FILE':
        sys.stdout.write("%s" % '\t'.join(line))
    # Any line that starts with 'sds', terminate the previous line and print it
    # starts with 'sds' because all filenames start with 'sds' and are in the first column
    elif line[0].startswith('sds'):
        sys.stdout.write("%s%s" % (os.linesep, '\t'.join(line)))
    # Any other line, just print it
    else:
        sys.stdout.write("%s" % '\t'.join(line))

# Terminate the lsat line
sys.stdout.write(os.linesep)

# For debugging, print the line length counts to stderr
if DEBUG:
    print >> sys.stderr, counts

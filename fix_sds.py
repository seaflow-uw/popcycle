from collections import defaultdict
import os
import sys
import string

## questions: 
## 1. if one row is correct, can we assume all rows are correct? 
## 2. should the final sds have a certain set of columnns (left blank if no data in original)? or just the columns from the original?
## 3. do we want to do one line at a time as they come in or all at once at the end? If one line at a time, what will the deal with the newlines be?
## 4. should i calculate date based on utc? 
## 5. are column names always the same and/or in the same order? 

# copied from Dan's earlier code
# not completely adapted/fixed
def fix_file_newlines(bad_sds, good_output):
    orig_file = open(bad_sds)
    out_file = open(good_output, 'w')

    DEBUG = False
    if DEBUG:
        counts = defaultdict(int)

    for line in orig_file:
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
            out_file.write("%s" % '\t'.join(line))
        # Any line that starts with 'sds', terminate the previous line and print it
        # starts with 'sds' because all filenames start with 'sds' and are in the first column
        elif line[0].startswith('sds'):
            out_file.write("%s%s" % (os.linesep, '\t'.join(line)))
        # Any other line, just print it
        else:
            out_file.write("%s" % '\t'.join(line))

        # Terminate the lsat line
        out_file.write(os.linesep)

        # For debugging, print the line length counts to stderr
        if DEBUG:
            print >> sys.stderr, counts

def fix_sds_file(bad_sds, good_output):
    bad_sds_file = open(bad_sds)
    header = bad_sds_file.readline()
    columns = header.split('\t')

    
    good_output_file = open(good_output, "w")
    good_output_file.write(header + '\n')

    # iterate through the remaining lines
    for line in bad_sds_file:
        good_output_file.write(fix_sds_line(line, columns) + '\n')

def fix_sds_line(sds_line, columns):
    line_contents = sds_line.split('\t')
    if len(line_contents) != len(columns):
        print "there's a problem."
    for i, item in enumerate(line_contents):
        column = columns[i]
        fix_item(item, column)
    return sds_line

def fix_item(item, column):
    print column, item
    if column == "LAT":
        return fix_lat(item)
    ## etc. --> figure out what the column is and fix the data as expected
    return item

def fix_lat(bad_lat):
    # actually return good_latlong
    return bad_lat

if __name__ == '__main__' : 
    bad_sds_name = 'SDS/Bad_sds_2013_241.txt'
    good_output_name = 'better_sds.txt'
    fix_sds_file(bad_sds_name, good_output_name)

    

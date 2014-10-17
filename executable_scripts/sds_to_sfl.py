import sys
sds_file = sys.argv[1]
f = open(sds_file)
f2 = open(sys.argv[2], 'w')

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

  if not (elements[-2] in files_seen):
    files_seen.add(elements[-2])
    f2.write('\t'.join(elements))
    f2.write('\n')

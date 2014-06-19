import sys
sds_file = sys.argv[1]
f = open(sds_file)
f2 = open(sys.argv[2], 'w')

header = f.readline()
elements = header.split('\t')
elements[-2] = 'FILE'
f2.write('\t'.join(elements[:-1]))
f2.write('\n')

for line in f :
  elements = line.split('\t')
  elements[-2] = elements[-2] + '/' + elements[-1].strip() + '.evt'
  f2.write('\t'.join(elements[:-1]))
  f2.write('\n')

# TODO : 
# 1) REPLACE "." by " " in column names of the SDS.TAB
# 2) ADD Column FILE DURATION and filled it with value 180
# 3) REMOVE duplicate filename, if present.

import rpy2
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

flowPhyto = importr('flowPhyto') 
readSeaflow = robjects.r['readSeaflow']
writeSeaflow = robjects.r['writeSeaflow']

#read evt, filter, output to opp
def filter_evt(evt_filepath, opp_filepath):
	## if we want different types of filter, we should specify this 
	## the same way we specify the type of classify
	evt = readSeaflow(evt_filepath)
	opp = flowPhyto.filter(evt)
	writeSeaflow(file_path=opp_filepath, df=opp)

#read opp, classify method, output vct
def classify_flowphyto(opp_filepath, vct_filepath, classify_method='flowPhyto'): 
	opp = readSeaflow(opp_filepath)
	## untested - probably needs to be called differently, with different args, etc.
	## will also need to be switched to call the method specified above
	vct = flowPhyto.classify(args='whatever') 

	## i.e., 
	## if classify_method == 'flowPhyto':
	## 	classify_flowPhyto(##same args)


def classify_flowPhyto(blah='blah'):
	pass	

def classify_gmm(blah='blah'):
	pass

def classify_flowMeans(blah='blah'):
	pass

def classify_dumb(blah='blah'):
	## can write a quick function here that classifies randomly or all the same
	## just to check that things are working 
	pass

def test_function(evt_file, opp_output_file) :
 	flowPhyto = importr('flowPhyto')
 	readSeaflow = robjects.r['readSeaflow']
 	writeSeaflow = robjects.r['writeSeaflow']

  	evt = readSeaflow(evt_file)
	opp = flowPhyto.filter(evt)
  	writeSeaflow(file_path=opp_output_file, df=opp)

if __name__ == '__main__' :
  	evt_file = '37.evt'
  	output_file = 'crazy_new_opp.opp'
  	test_function(evt_file, output_file)

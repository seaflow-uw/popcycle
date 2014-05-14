import numpy as np
import rpy2
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
import rpy2.robjects.numpy2ri

# for GMM
import sklearn as sk
import sklearn.mixture as mix

rpy2.robjects.numpy2ri.activate()


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
def classify(opp_filepath, vct_filepath, classify_method='flowPhyto', args = {}): 
  opp = readSeaflow(opp_filepath)
  ## untested - probably needs to be called differently, with different args, etc.
  ## will also need to be switched to call the method specified above
  #vct = flowPhyto.classify(args='whatever') 

  ## i.e., 
  if classify_method == 'flowPhyto':
    vct = classify_flowPhyto(opp, args) #some other args?
    np.savetxt(vct_filepath, vct, fmt="%s")

  if classify_method == 'gmm' :
    vct = classify_gmm(opp, args)
    np.savetxt(vct_filepath, vct)

def classify_flowPhyto(opp, args={}):
  new_opp = flowPhyto.classify(opp)
  vct = np.array(new_opp.rx('pop'))
  vct.shape = (vct.shape[1])
  return vct

def classify_gmm(opp, args = {}):
  k = args['k']
  model = mix.GMM(k, 'full', n_init=1, params = 'wmc', init_params='wc')
  vars = [4,5,7,8]
  df = np.array(opp).T
  model.fit(df[:,vars])
  vct = model.predict(df[:,vars])
  return vct

def classify_flowMeans(opp, args = {}):
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
  evt_file = '/home/hyrkas/popcycle_repo/popcycle/data/37.evt'
  opp_file = '/home/hyrkas/popcycle_repo/popcycle/data/crazy_new_opp.opp'
  filter_evt(evt_file, opp_file)
  vct_file1 = '/home/hyrkas/popcycle_repo/popcycle/data/37.evt.opp.vct.flowPhyto'
  vct_file2 = '/home/hyrkas/popcycle_repo/popcycle/data/37.evt.opp.vct.gmm'
  classify(opp_file, vct_file1)
  args = {}
  args['k'] = 7
  classify(opp_file, vct_file2, 'gmm', args)

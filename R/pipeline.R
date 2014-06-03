filter_evt <- function(evt, filter_func, ...) {
  opp <- filter_func(evt, ...)
  
  # SANITY CHECKS
  # need same columns for opp
  if (!all(names(evt) == names(opp))) {
    stop('Filtering function produced OPP with different columns')
  }
  
  # filtered all particles out?
  if (dim(opp)[1] < 1) {
    stop('Filtering dropped all particles.')
  }

  return (opp)
}

classify_opp <- function(opp, classify_func, ...) {
  vct <- classify_func(opp, ...)
  
  # SANITY CHECKS
  # dropped particles
  if (!(dim(opp)[1] == length(vct))) {
    stop('Filtering function returned incorrect number of labels.')
  }
  
  # in case classify_func didn't return text
  vct <- as.character(vct)
  
  return (vct)
}

# TODO(hyrkas): add summarization wrapper
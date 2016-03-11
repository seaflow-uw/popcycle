
classify.opp <- function(opp, classify.func, ...) {
  vct <- classify.func(opp, ...)

  # SANITY CHECKS
  # dropped particles
  if (!(dim(opp)[1] == length(vct))) {
    stop('Filtering function returned incorrect number of labels.')
  }

  # in case classify.func didn't return text
  vct <- as.character(vct)

  return (vct)
}


set.gating.params <- function(opp, popname, para.x, para.y, poly.log=NULL) {
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)

  par(mfrow=c(1,1))
  plot.gating.cytogram(opp, poly.log, para.x, para.y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para.x, para.y)

  poly.l <- list(poly)
  names(poly.l) <- popname

  if (is.null(poly.log)) {
    # Start a new gating entry
    poly.log <- poly.l
  } else {
    # if gate parameters for the same population already exist, overwrite,
    # otherwise append gate parameters for new population
    poly.log[popname] <- poly.l
  }
  return(poly.log)
}


ManualGating <- function(opp, poly.log){
  opp$pop <- "unknown"

  if (length(poly.log) == 0) {
    stop("No gate parameters found!")
   }

	for (i in 1:length(poly.log)) {
		pop <- names(poly.log[i]) # name of the population
    # print(paste('Gating',pop))
		poly <- poly.log[i][[1]] # Get parameters of the gate for this population
		para <- colnames(poly)
		df <- subset(opp, pop=="unknown")[,para]

		colnames(poly) <- colnames(df) <- c("x","y") # to stop stupid Warnings from inout()
		vct <- subset(df, inout(df,poly=poly, bound=TRUE, quiet=TRUE)) # subset particles based on Gate
		opp[row.names(vct),"pop"] <- pop
	}

	return(opp$pop)
}


run.gating <- function(db, cruise.name, opp.dir, opp.list, vct.dir,
                       gating.id=NULL) {
  if (is.null(gating.id)) {
    gating.params <- get.gating.params.latest(db)
  } else {
    gating.params <- get.gating.params.by.id(db, gating.id)
  }

  if (length(gating.params$poly.log) == 0) {
    stop('No gate paramters yet; no gating.')
  }

  i <- 0
  for (opp.file in opp.list) {
    message(round(100*i/length(opp.list)), "% completed \r", appendLF=FALSE)

    tryCatch({
      #print(paste('Loading', opp.file))
      opp <- get.opp.by.file(opp.dir, opp.file, ,
                             channel=c("fsc_small", "fsc_perp", "pe", "chl_small"))
      #print(paste('Classifying', opp.file))
      vct <- classify.opp(opp, ManualGating, gating.params$poly.log)

      opp$pop <- factor(vct)
      # delete old vct entries if they exist so we keep cruise/file/particle distinct
      delete.vct.stats.by.file(db, opp.file)
      delete.vct.by.file(vct.dir, opp.file)
      # store vct
      #print('Uploading labels to the database')
      save.vct.stats(db, opp, cruise.name, opp.file, 'Manual Gating',
                     gating.params$row$id)
      save.vct.file(vct, vct.dir, opp.file)

      #print("Calculating cytometric diversity")
      df <- opp[!(opp$pop == 'beads'),]
      indices <- cytodiv(df, para=c("fsc_small","chl_small","pe"), Ncat=16)
      delete.cytdiv.by.file(db, opp.file)
      save.cytdiv(db, indices, cruise.name, opp.file)
    }, error = function(e) {
      print(paste("Encountered error with file", opp.file))
      print(e)
    })

    i <-  i + 1
    flush.console()
  }
}

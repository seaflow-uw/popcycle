setGateParams <- function(opp, popname, dim.x, dim.y, override=TRUE){

  require(splancs)
  
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  
  
  popname <- as.character(popname)
  dim.x <- as.character(dim.x)
  dim.y <- as.character(dim.y)
  
  
  para <- c(dim.x, dim.y)
  
  par(mfrow=c(1,1), pty="s", cex=1)
  plot(opp[,para], pch=16, cex=0.4, col = densCols(log10(opp[,para]), colramp = cols), main=paste("Set Gate for:",popname), log='xy') #plot 2D cytogram
  poly <- getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- para
  
  write.csv(poly, paste(gating.param.location, "/",popname,".csv",sep=""), quote=FALSE, row.names=FALSE)
  
  
  #time <- format(Sys.time(),format="%FT%H-%M-%S+0000", tz="GMT")
  # TODO(hyrkas): add archive_folder for Params in Globals
  #write.csv(poly, paste(gating.param.location_archived, "/",popname,".",time,".csv",sep=""), quote=FALSE, row.names=FALSE)
  
  return(poly)

}







gating <- function(opp,gate_path=gating.param.location){

  opp$pop <- "unknown"
  
  list.params <- list.files(path=gate_path, pattern= ".csv", full.names=TRUE)

	for(p in list.params){

		pop <- basename(sub(".csv","",p)) # name of the population
		poly <- read.csv(p) # Get parameters of the gate for this population
		para <- colnames(poly)
		df <- subset(opp, pop=="unknown")[,para]

		colnames(poly) <- colnames(df) <- c("x","y") # to stop stupid Warnings from inout()
		vct <- subset(df, inout(df,poly=poly, bound=TRUE, quiet=TRUE)) # subset particles based on Gate
		opp[row.names(vct),"pop"] <- pop
		
	}

	return(opp$pop)

}







### testing
#opp.path <- system.file("extdata","seaflow_cruise","2011_001", "2.evt.opp", package="flowPhyto")
#opp <- readSeaflow(opp.path, transform=T)

#pop1 <- setGateParams(opp, popname="beads",dim.x="chl_small", dim.y="pe")
#pop2 <- setGateParams(opp, popname="synecho",dim.x="fsc_small", dim.y="chl_small")

#vct <- gating(opp)










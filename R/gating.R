## merge vct to opp if vct already exist 
setGateParams <- function(opp, popname, para.x, para.y, override=TRUE){

  require(splancs)
  
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
    
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)
  
  par(mfrow=c(1,1))
  plot.vct.cytogram(opp, para.x, para.y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para.x, para.y)
  
# if gating parameters already exist for hte population, remove them
    list.params <- list.files(gating.param.location, pattern= ".csv", full.names=TRUE)
	id <- grep(popname, list.params)
	if(length(id) == 1) system(paste("rm", list.params[id]))

# Save gating
  time <- format(Sys.time(),format="%FT%H-%M-%S+0000", tz="GMT")
  write.csv(poly, paste0(gating.param.location, time, "_",popname,".csv"), quote=FALSE, row.names=FALSE)
  write.csv(poly, paste0(gating.param.location_archived, time, "_",popname,".csv"), quote=FALSE, row.names=FALSE)
  



  return(poly)

}







gating <- function(opp,gating.param.location){

  opp$pop <- "unknown"
  
  list.params <- list.files(gating.param.location, pattern= ".csv", full.names=TRUE)

	for(p in list.params){

		pop <- sub(".csv","",strsplit(basename(p),"_")[2]) # name of the population
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










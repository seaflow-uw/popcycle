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

  poly.l <- list(poly)
  names(poly.l) <- popname

# Save gating
  time <- format(Sys.time(),format="%FT%H-%M-%S+0000", tz="GMT")
  write.csv(poly, paste0(log.gate.location, "/",time, "_", popname, ".csv"), quote=FALSE, row.names=FALSE)
  write.csv(poly, paste0(param.gate.location, "/",popname, ".csv"), quote=FALSE, row.names=FALSE)

# load the list of gate parameters
  params <- try(load(paste0(param.gate.location,"/params.RData")),silent=T)
# if list already exists, modify the list, if not, save the list
  if(class(params)==("try-error")){
     poly.log <- poly.l
     save(poly.log, file=paste0(param.gate.location,"/params.RData"))
   }else{
    id <- which(names(poly.log) == popname)
          if(length(id)>0){ # if gate paramters for the same population already exist, overwrite. 
          poly.log[[id]] <- poly
        }else{poly.log <- c(poly.log, poly.l)} # otherwise, add the new gate parameters to the end of the array.
   save(poly.log, file=paste0(param.gate.location,"/params.RData"))
  }

  return(poly)

}


Gating <- function(opp,gate_path=param.gate.location){

  opp$pop <- "unknown"
  
  params <- try(load(paste0(param.gate.location,"/params.RData")),silent=T)
  if(class(params)==("try-error")){
    print("No gate parameters found!")
    stop
   }

	for(i in 1:length(poly.log)){
		pop <- names(poly.log[i]) # name of the population
    print(paste('Gating',pop))
		poly <- poly.log[i][[1]] # Get parameters of the gate for this population
		para <- colnames(poly)
		df <- subset(opp, pop=="unknown")[,para]

		colnames(poly) <- colnames(df) <- c("x","y") # to stop stupid Warnings from inout()
		vct <- subset(df, inout(df,poly=poly, bound=TRUE, quiet=TRUE)) # subset particles based on Gate
		opp[row.names(vct),"pop"] <- pop
		
	}

	return(opp$pop)

}


resetGateParams <- function(){

  system(paste0("rm ", param.gate.location,"/*.csv"))
  system(paste0("rm ", param.gate.location,"/*.RData"))

}

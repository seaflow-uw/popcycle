filter.notch <- function(evt, width, notch) {

  # TODO(hyrkas): if data is not log transformed, leave it be?
  if(any(max(evt[,-c(1,2)]) > 10^3.5)){
    stop(paste("ERROR, data are not LOG-transform"))
  }
  
  ##### FILTRING OPP #####
  detected <- subset(evt, D1 > 1 & D2 > 1) # filtering particles not detected by D1 or D2
  unsaturated <- subset(detected, D1 < max(evt[,"D1"]) & D2 < max(evt[,"D1"])) # filtering particles with saturated signals on D1 or D2
  unsaturated[,-c(1,2)] <- (log10(unsaturated[,-c(1,2)])/3.5)*2^16 ## linearize the LOG transformed data
  
  origin.unsaturated <- subset(unsaturated, D1 > 10000 & D2 > 10000) # Exclude potenital electrical noise from calculation.	
  origin <- median(origin.unsaturated$D2) - median(origin.unsaturated$D1) 	# Difference of sensitivity between D2 and D1.	 
  # Correction for the difference of sensitivity between D1 and D2
  if(origin > 0)  unsaturated$D2 <- 	unsaturated$D2 - origin
  if(origin < 0)  unsaturated$D1 <- 	unsaturated$D1 + origin	
  
  aligned <- subset(unsaturated, D2 > (D1 - width * 10^4) & D2 < (D1 + width*10^4)) # filtering aligned particles (D1 = D2)
  focused <- subset(aligned, D2/fsc_small > (D1/fsc_small - notch) & D2/fsc_small < (D1/fsc_small + notch))# filtering focused particles (D1/fsc_small = D2/fsc_small)
  opp <- subset(focused, D1/fsc_small < notch | D2/fsc_small < notch) # filtering focused particles (D/fsc_small < notch)
  opp[,-c(1,2)] <-  10^((opp[,-c(1,2)]/2^16)*3.5)
  
  return(opp)
}


best.filter.notch <- function(evt, notch=seq(0.4, 1.4, by=0.1),width =1, do.plot=TRUE){

  DF <- NULL
  
  for(n in notch){
    opp <- filter.notch(evt, notch=n, width=width)
    fsc.max <- round(max(opp[,"fsc_small"]))
    para <- data.frame(cbind(notch=n, fsc.max))
    DF <- rbind(DF, para)
    }

  best.notch.id <- min(which(DF$fsc.max == max(DF$fsc.max)))
  best.notch <- DF$notch[best.notch.id]

  if(do.plot){
  par(mfrow=c(2,1),cex=1)
  par(pty='m')
  plot(DF[,c('notch', 'fsc.max')], main=paste("Best notch=",best.notch)); points(best.notch, DF$fsc.max[best.notch.id], col=2, pch=16)
  opp <- filter.notch(evt, notch=best.notch, width=width)
  plot.cytogram(opp,"fsc_small","chl_small", vct=FALSE); mtext(paste("OPP with notch=",best.notch),3,line=1)
  }

return(best.notch)

}

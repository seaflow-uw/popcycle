# set width and notch, log old parameters if they exist
setFilterParams <- function(width, notch) {
  params <- data.frame(width = width, notch = notch)
  
  #log
  time <- format(Sys.time(),format="%FT%H:%M:%S+00:00", tz="GMT")
  log.file <- paste(log.location, 'filter.csv', sep='/')
  
  if (file.exists(log.file)) {  
    write.table(cbind(time, width, notch), log.file, 
                row.names = F, col.names = F, append = T)  
  } else {
    write.table(data.frame(time=time, width=width, notch=notch), log.file,
                row.names = F, col.names = T, append = T)
  }
  
  #write params
  write.table(params, file = filter.param.location, sep = ",", row.names=F)
}

filter.notch <- function(evt, width, notch) {

  notch <- as.numeric(notch)
  width <- 1 + as.numeric(width)
   
   if(any(max(evt[,-c(1,2)]) > 10^3.5)){
    stop(paste("ERROR, data are not LOG-transform"))
   }
  
  m.D1 <- max(evt[,"D1"])
  m.D2 <- max(evt[,"D2"])
  origin <- median(evt[which(evt$D1 < m.D1 & evt$D1 < m.D2),"D1"]/evt[which(evt$D1 < m.D1 & evt$D1 < m.D2),"D2"])   # Difference of sensitivity between D2 and D1.   

 
  #####################
  ### FILTERING OPP ###
  #####################
  # filtering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  if(origin > 1) aligned <- subset(evt, D2 * origin < (D1 * width) & D2 * origin > (D1 / width))
  if(origin < 1) aligned <- subset(evt, D2 < ((D1/origin) * width) & D2 > ((D1/origin)/ width))
    
  # filtering focused particles (D/fsc_small < notch)
  opp <- subset(aligned, D1/fsc_small < notch | D2/fsc_small < notch) 
     
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
  plot.cytogram(opp,"fsc_small","chl_small"); mtext(paste("OPP with notch=",best.notch),3,line=1)
  }

return(best.notch)

}

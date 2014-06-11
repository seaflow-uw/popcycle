# set width and notch, log old parameters if they exist
setFilterParams <- function(width, notch) {
  params <- data.frame(width = width, notch = notch)
  
  #log
  time <- format(Sys.time(),format="%FT%H:%M:%S+00:00", tz="GMT")
  log.file <- paste(log.filter.location, 'filter.csv', sep='/')
  
  if (file.exists(log.file)) {  
    write.table(data.frame(time=time, widht=width, notch=notch), log.file, 
                row.names = F, col.names = F, append = T, quote = F, sep=',')  
  } else {
    write.table(data.frame(time=time, width=width, notch=notch), log.file,
                row.names = F, col.names = T, quote=F, sep=',')
  }
  
  #write params
  write.table(params, file = paste(param.filter.location, 'filter.csv', sep='/'), sep = ",",
              quote=F, row.names=F)
}

filter.notch <- function(evt, width, notch) {

  notch <- as.numeric(notch)
  width <- 1 + as.numeric(width)
   
   if(any(max(evt[,-c(1,2)]) > 10^3.5)){
    stop(paste("ERROR, data are not LOG-transform"))
   }
  

  evt <- subset(evt, D1 > 1 & D2 > 1 & fsc_small > 1) # filtering particles not detected by D1, D2 not fsc_small
    m.D1 <- max(evt[,"D1"])
    m.D2 <- max(evt[,"D2"])
  evt <- subset(evt, D1 < m.D1 & D2 < m.D2) # filtering particles with saturated signals on D1 or D2

  origin <- median(evt[,"D1"]/evt[,"D2"])   # Difference of sensitivity between D2 and D1.   

 
  #####################
  ### FILTERING OPP ###
  #####################
  # filtering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  if(origin > 1) aligned <- subset(evt, D2 * origin < (D1 * width) & D2 * origin > (D1 / width))
  if(origin < 1) aligned <- subset(evt, D2 < ((D1/origin) * width) & D2 > ((D1/origin)/ width))
    
  # filtering focused particles (D/fsc_small < notch)
  if(origin > 1) opp <- subset(aligned, D1/fsc_small < notch | D2*origin/fsc_small < notch) 
  if(origin < 1) opp <- subset(aligned, (D1/origin)/fsc_small < notch | D2/fsc_small < notch) 
      
  return(opp)
}


best.filter.notch <- function(evt, notch=seq(0.1, 1.4, by=0.1),width=0.5, do.plot=TRUE){

  DF <- NULL
  
  for(n in notch){
    print(paste("filtering notch=",n))
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
  plot(DF[,c('notch', 'fsc.max')],ylim=c(1,10^3.5), main=paste("Best notch=",best.notch)); points(best.notch, DF$fsc.max[best.notch.id], col=2, pch=16)
  opp <- filter.notch(evt, notch=best.notch, width=width)
  plot.cytogram(opp,"fsc_small","chl_small"); mtext(paste("OPP with notch=",best.notch),3,line=1)
  }

return(best.notch)

}

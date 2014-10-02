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
  width <- as.numeric(width)
  slope <- 1
  
  # linearize the LOG transformed data 
  t <- FALSE
    id <- which(colnames(evt) == "pulse_width" | colnames(evt) == "time" | colnames(evt) =="pop")
    if(!any(max(evt[,-c(id)]) > 10^3.5)){
      evt[,-c(id)] <- (log10(evt[,-c(id)])/3.5)*2^16  
      t <- TRUE
         }

  # filtering particles not detected by D1 or D2 or fsc_small
  evt <- subset(evt, D1 > 1 & D2 > 1 & fsc_small > 1) 

  # filtering particles saturating D1 or D2
  max <- max(evt[,c("D1","D2")])
  evt <- subset(evt, D1 < max & D2 < max) 
  
  # Correction for the difference of sensitivity between D1 and D2
  origin <- median(evt[evt$D2>5000,"D2"])-median(evt[evt$D1>5000,"D1"])
  
  # filtering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt, D2 - origin < D1*slope + width * 10^4 & D1 < D2*slope - origin + width * 10^4) # filtering aligned particles (D1 = D2)

  # filtering focused particles (D/fsc_small < notch)
  if(origin >= 0) opp <- subset(aligned, D1/fsc_small < notch & (D2-origin)/fsc_small < notch) 
  if(origin < 0) opp <- subset(aligned, (D1+origin)/fsc_small < notch & D2/fsc_small < notch) 


  if(t){
  opp[,-c(id)] <- 10^((opp[,-c(id)]/2^16)*3.5)
    }

return(opp)

}


best.filter.notch <- function(evt, notch=seq(0.5, 1.5, by=0.1),width=0.1, do.plot=TRUE){

  DF <- NULL
  for(n in notch){
    print(paste("filtering notch=",n))
    opp <- filter.notch(evt, notch=n, width=width)
    fsc.max <- round(max(opp[,"fsc_small"]))
    id <- length(which(opp[,"fsc_small"] >= max(opp[,"fsc_small"])))
    para <- data.frame(cbind(notch=n, fsc.max, id))
    DF <- rbind(DF, para)
    }

  best.notch.id <- min(which(DF$fsc.max == max(DF$fsc.max) & DF$id == max(DF$id)))
  best.notch <- DF$notch[best.notch.id]

 if(do.plot){
  def.par <- par(no.readonly = TRUE) # save default, for resetting...

  par(mfrow=c(2,1),oma=c(2,2,2,4), cex=1)
  par(pty='m')
  plot(DF[,c('notch', 'fsc.max')], main=paste("Best notch=",best.notch))
  abline(v=best.notch, col=3, lwd=3)
  par(new=TRUE)
  plot(DF[,'notch'], DF[,'id'], pch=3, xaxt='n',yaxt='n',xlab=NA,ylab=NA, col=2, ylim=c(0, max(DF[,'id'])))
  axis(4)
  mtext("count", 4, line=3)
  legend('topleft',legend=c('max(fsc_small)','count'), pch=c(1,3), col=c(1,2), bty='n')
  opp <- filter.notch(evt, notch=best.notch, width=width)
  plot.cytogram(opp,"fsc_small","chl_small"); mtext(paste("OPP with notch=",best.notch),3,line=1)
  par(def.par)    
  }

return(best.notch)

}

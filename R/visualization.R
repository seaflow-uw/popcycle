plot.cytogram <- function(opp, para.x = 'fsc_small', para.y = 'chl_small'){		

	cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
	par(pty='s')
	plot(opp[,c(para.x, para.y)], pch=16, cex=0.6, col = densCols(log10(opp[,c(para.x, para.y)]), colramp = cols), log='xy', xlim=c(1,10^3.5), ylim=c(1,10^3.5)) #plot 2D cytogram
	
}

## OPP merged with VCT
plot.vct.cytogram <- function(opp,para.x = 'fsc_small', para.y = 'chl_small'){		
		
		if(!is.null(opp$pop)){
			par(pty='s')
      ## TODO[francois] Order OPP by frequency (most abundant pop plotted first, least abundant pop plotted last)
      plot(opp[,c(para.x, para.y)], pch=16, cex=0.6, col = as.numeric(as.factor(opp$pop)), log='xy',xlim=c(1,10^3.5), ylim=c(1,10^3.5)) #plot 2D cytogram
			legend('topleft',legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16,pt.cex=0.6,bty='n')
		}else{
			print("No Gating parameters yet!")
			plot.cytogram(opp, para.x, para.y)
			mtext(paste("No Gating parameters yet!"),3,line=-1,font=2)
		}
}



plot.vct.cytogram.by.file <- function(file_name, para.x = 'fsc_small', para.y = 'chl_small'){
  
  vct <- get_vct_by_file(file_name)
  opp <- get_opp_by_file(file_name)
  opp$pop <- vct
  plot.vct.cytogram(opp, para.x = para.x, para.y = para.y)

}




plot.filter.cytogram <- function(evt, width=1, notch=1){


  notch <- as.numeric(notch)
  width <- 1 + as.numeric(width)
   
   if(any(max(evt[,-c(1,2)]) > 10^3.5)){
    stop(paste("ERROR, data are not LOG-transform"))
   }
  
  evt <- subset(evt, D1 > 1 & D2 > 1) # filtering particles not detected by D1 or D2
    m.D1 <- max(evt[,"D1"])
    m.D2 <- max(evt[,"D2"])
  evt <- subset(evt, D1 < m.D1 & D2 < m.D2) # filtering particles with saturated signals on D1 or D2

 
  #####################
  ### FILTERING OPP ###
  #####################
    # filtering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
    if(origin > 1) aligned <- subset(evt, D2 * origin < (D1 * width) & D2 * origin > (D1 / width))
  	if(origin < 1) aligned <- subset(evt, D2 < ((D1/origin) * width) & D2 > ((D1/origin)/ width))
  	
  	# filtering focused particles (D/fsc_small < notch)
  	if(origin > 1) opp <- subset(aligned, D1/fsc_small < notch | D2*origin/fsc_small < notch) 
  	if(origin < 1) opp <- subset(aligned, (D1/origin)/fsc_small < notch | D2/fsc_small < notch) 
    

  ################
  ### PLOTTING ###
  ################
	 cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
	percent.opp <- round(100*nrow(opp)/nrow(evt),2)
	
	if(nrow(evt) > 1000){display <- 1000}else{display <- nrow(evt)}

  par(mfrow=c(2,2),pty='s')                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
  plot.cytogram(evt[1:display,], "D1", "D2")
  	mtext("Alignment", side=3, line=1, font=2)
  	lines(c(1,m.D1),c(1,m.D1)*width/origin, lwd=2, col=2)
  	lines(c(1,m.D1),c(1,m.D1)/(width*origin), lwd=2, col=2)
  	mtext(paste("D1/D2 =", round(origin,2)),outer=T,side=3, line=-1.5,font=2)
  	mtext(paste("Width=", width-1),outer=T,side=3, line=-3,font=2)
  	mtext(paste("Notch=", notch),outer=T,side=3, line=-4,font=2)
  	mtext(paste("OPP =", percent.opp,"% EVT"), outer=T,side=1, line=-1.5,font=2,col=2)

  	aligned <- subset(aligned[1:display,], D1/fsc_small<2 & D2/fsc_small<2)
      if(origin > 1){ 
      	aligned$D1.fsc_small <- aligned$D1/aligned$fsc_small
      	aligned$D2.fsc_small <- origin*aligned$D2/aligned$fsc_small
      }
      if(origin < 1){
      	aligned$D1.fsc_small <- (aligned$D1/origin)/aligned$fsc_small
      	aligned$D2.fsc_small <- aligned$D2/aligned$fsc_small
      }
  plot(aligned[,c("D1.fsc_small", "D2.fsc_small")], pch=16, cex=0.6, col = densCols(aligned[,c("D1.fsc_small", "D2.fsc_small")], colramp = cols), xlim=c(0,2), ylim=c(0,2)) 
       mtext("Focus", side=3, line=1, font=2)
       abline(v=notch, h=notch, col=2,lwd=2)
  	 # abline(b=1, a=notch, col='red', lwd=2)
  	 # abline(b=1, a=-notch, col='red', lwd=2)
  	
  plot.cytogram(opp, "fsc_small", "pe")
     	mtext("OPP", side=3, line=1, font=2)
  plot.cytogram(opp, "fsc_small","chl_small")
     	mtext("OPP", side=3, line=1, font=2)
 
}


plot.map <- function(stat,popname,param,...){
  ## plot cell abundances of a population on a map
  require(maps, quietly=T)
  require(mapdata, quietly=T)
  require(plotrix, quietly=T)

  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))

  map.type <- 'worldHires'
  
    xlim <- range(stat$lon, na.rm=T)        
    ylim <- range(stat$lat, na.rm=T)   
    
    if(xlim[1] < 0 & xlim[2] > 0){
        neg.lon <- subset(stat, lon < 0)
      stat[row.names(neg.lon), "long"] <- neg.lon$lon + 360
      xlim <- c(min(stat$lon, na.rm=TRUE), max(stat$lon, na.rm=TRUE))
      stat <- stat[-which(is.na(stat$lon)),]
      stat$lon[stat$lon < 0] <- stat$lon[stat$lon < 0] + 360
      map.type <- 'world2Hires'
        }
  
  # plot the cruise track as gray line back-ground
  pop <- subset(stat, pop == popname)
  plot(pop$lon, pop$lat, xlim=xlim, ylim=ylim, asp=1, main=paste(popname),
            xlab="Longitude (deg W)",ylab="Latitude (deg N)",type='l',lwd=3,col='lightgrey',...)
  try(maps::map(map.type, fill=F, col='black',add=TRUE))
  points(pop$lon, pop$lat, pch=16, asp=1, col=cols(100)[cut(pop[,param],100)],...)

    ylim <- par('usr')[c(3,4)]
    xlim <- par('usr')[c(1,2)]



    color.legend(xlim[2], ylim[1], xlim[2] + 0.02*diff(xlim), ylim[2], 
      legend=pretty(pop[,param]), rect.col=cols(100), gradient='y',align='rb',...)
  mtext(paste(param), side=4, line=3)  
  

}

plot.time <- function(stat, popname,param, ...){

stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')
pop <- subset(stat, pop == popname)
plot(pop$time, pop[,param], xlab="time", ylab=paste(param),main=paste(popname),...)

}
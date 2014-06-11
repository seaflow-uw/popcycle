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
      plot(opp[,c(para.x, para.y)], pch=16, cex=0.6, col = as.numeric(as.factor(opp$pop)), log='xy') #plot 2D cytogram
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


plot.map <- function(lat, long, track=NULL, margin=2, col='red', legend=NULL, pch=20, cex=1.5, lwd=1, lty=2, xlim=NULL, ylim=NULL, xlab="Longitude (deg W)",ylab="Latitude (deg N)",zlab=NA, ...){
  ## plot longitude and latitude on a map
  require(maps, quietly=T)
  require(mapdata, quietly=T)
  require(plotrix, quietly=T)
  
  map.type <- 'worldHires'
  
    if(is.null(track) | class(track)=='try-error')
  track <- data.frame(lat=lat, long=long)
    
    if(is.null(xlim))
          xlim <- c(min(track$long, na.rm=TRUE)-margin, max(track$long, na.rm=TRUE)+margin)         
    if(is.null(ylim))
          ylim <- c(min(track$lat , na.rm=TRUE)-margin, max(track$lat , na.rm=TRUE)+margin)
    
    if(xlim[1] < 0 & xlim[2] > 0){
      neg.long <- subset(track, long < 0)
      track[row.names(neg.long), "long"] <- neg.long$long + 360
      xlim <- c(min(track$long, na.rm=TRUE)-margin, max(track$long, na.rm=TRUE)+margin)
      long <- na.exclude(long); lat <- na.exclude(lat)
      long[long < 0] <- long[long < 0] + 360
      map.type <- 'world2Hires'
        }
  
  # plot the cruise track as gray line back-ground
  plot(track$long, track$lat, xlim=xlim, ylim=ylim, lwd=lwd,lty=lty,
       xlab=xlab,ylab=ylab,
       pch=20, col='gray', type='o', asp=1, ...)
  try(maps::map(map.type, fill=FALSE, col='black',add=TRUE))
  points(long, lat, xlim=xlim, ylim=ylim, pch=pch, cex=cex, col=col)
  
  if(!is.null(legend)){
    ylim <- par('usr')[c(3,4)]
    xlim <- par('usr')[c(1,2)]

    color.legend(xlim[2], ylim[1], xlim[2] + diff(xlim)/40, ylim[2], 
      legend=legend, rect.col=.rainbow.cols(100), gradient='y',align='rb',cex=cex,...)
  mtext(zlab, side=4, line=3,cex=cex)  

  }
  if(length(long) == 1)
    mtext(paste("Long/Lat: ",round(mean(long),3),'/', round(max(lat),3)),col='red', line=-2, )
}

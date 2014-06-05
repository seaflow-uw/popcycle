plot.cytogram <- function(opp, para.x, para.y){		
	
	cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
	par(pty='s')
	plot(opp[,c(para.x, para.y)], pch=16, cex=0.4, col = densCols(log10(opp[,c(para.x, para.y)]), colramp = cols), log='xy') #plot 2D cytogram
	
}

## OPP merged with VCT
plot.vct.cytogram <- function(opp,para.x, para.y){		
		
	par(pty='s')
		
		if(!is.null(opp$pop)){
		plot(opp[,c(para.x, para.y)], pch=16, cex=0.4, col = as.numeric(as.factor(opp$pop)), log='xy') #plot 2D cytogram
		legend('topleft',legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16,pt.cex=0.4,bty='n')
		}else{
			print("No Gating parameters found !")
			plot(opp[,c(para.x, para.y)], pch=16, cex=0.4, col = densCols(log10(opp[,c(para.x, para.y)]), colramp = cols), log='xy')
			mtext(paste("No Gating parameters found !"),3,line=1,font=2)
		}	  

	}
}


plot.filter.cytogram <- function(evt, width=0.5, notch=1){
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))


  notch <- as.numeric(notch)
  width<- as.numeric(width)
   
   if(any(max(evt[,-c(1,2)]) > 10^3.5)){
    stop(paste("ERROR, data are not LOG-transform"))
   }
   
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

  ########################
  
	percent.opp <- round(100*nrow(opp)/nrow(evt),1)
	
	par(mfrow=c(2,2),pty='s')                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	
	events. <- evt[1:round(nrow(opp)),]
    
    plot.cytogram(events., "D1", "D2",vct=FALSE)
	 mtext("Filtering Aligned Particles", side=3, line=1, font=2)
	 mtext(paste("only", percent.opp,"% display"), side=3, cex=0.7,col='red')
	 abline(v=max(evt[,"D1"]), h=max(evt[,"D2"]), col='red',lwd=2)
	 abline(v=1, h=1, col='red',lwd=2)
		 par(new=T)
	 plot(1,1, xlim=c(0,2^16),ylim=c(0,2^16), bty='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA, pch=NA)
	 abline(b=1, a=origin + width*10^4, col='red',lwd=2)
	 abline(b=1, a=origin -width*10^4, col='red',lwd=2)

	mtext(paste("Sensitivity Difference D2 =", round(origin,0)),outer=T,side=3, line=-1.5,font=2)
	mtext(paste("Width=", width),outer=T,side=3, line=-3,font=2)
	mtext(paste("Notch=", notch),outer=T,side=3, line=-4,font=2)

	aligned. <- subset(aligned, aligned$D1/aligned$fsc_small<2 & aligned$D2/aligned$fsc_small<2)[1:round(nrow(opp)),]
    aligned.$para1 <- aligned.$D1/aligned.$fsc_small
    aligned.$para2 <- aligned.$D2/aligned.$fsc_small
    plot.cytogram(aligned., 'para1',  'para2')
     mtext("Filtering Focused Particles", side=3, line=1, font=2)
	 mtext(paste("only", percent.opp,"% display"), side=3, cex=0.7,col='red')
     abline(v=notch, h=notch, col='red',lwd=2)
	 abline(b=1, a=notch, col='red', lwd=2)
	 abline(b=1, a=-notch, col='red', lwd=2)
		
   plot.cytogram(opp, "fsc_small", "pe")
   mtext("Optimally Positioned Particles", side=3, line=1, font=2)
 	plot.cytogram(opp, "fsc_small","chl_small")
   mtext("Optimally Positioned Particles", side=3, line=1, font=2)
 
  }










# testing
evt.path <- system.file("extdata","seaflow_cruise","2011_001", "3.evt", package="flowPhyto")
evt <- readSeaflow(opp.path, transform=T)

plot.filter.cytogram(evt, width=0.5, notch=1)

notch <- find.filter.notch(evt,notch=seq(0.4, 1.4, by=0.1),width =1, do.plot=TRUE)

#opp.path <- system.file("extdata","seaflow_cruise","2011_001", "2.evt.opp", package="flowPhyto")
#opp <- readSeaflow(opp.path, transform=T)
#plot.cytogram(opp, "fsc_small", "chl_small",classification=TRUE)

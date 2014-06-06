plot.cytogram <- function(opp, para.x, para.y){		

	cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
	par(pty='s')
	plot(opp[,c(para.x, para.y)], pch=16, cex=0.6, col = densCols(log10(opp[,c(para.x, para.y)]), colramp = cols), log='xy', xlim=c(1,10^3.5), ylim=c(1,10^3.5)) #plot 2D cytogram
	
}

## OPP merged with VCT
plot.vct.cytogram <- function(opp,para.x, para.y){		
		
		if(!is.null(opp$pop)){
			par(pty='s')
			plot(opp[,c(para.x, para.y)], pch=16, cex=0.6, col = as.numeric(as.factor(opp$pop)), log='xy') #plot 2D cytogram
			legend('topleft',legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16,pt.cex=0.6,bty='n')
		}else{
			print("No Gating parameters yet!")
			plot.cytogram(opp, para.x, para.y)
			mtext(paste("No Gating parameters yet!"),3,line=-1,font=2)
		}
}


plot.filter.cytogram <- function(evt, width=1, notch=1){


  notch <- as.numeric(notch)
  width <- 1 + as.numeric(width)
   
   if(any(max(evt[,-c(1,2)]) > 10^3.5)){
    stop(paste("ERROR, data are not LOG-transform"))
   }
  
  m.D1 <- max(evt[,"D1"])
  m.D2 <- max(evt[,"D2"])
  origin <- median(evt[which(evt$D1 < m.D1 & evt$D1 < m.D2),"D1"]/evt[which(evt$D1 < m.D1 & evt$D1 < m.D2),"D2"]) 	# Difference of sensitivity between D2 and D1.	 

 
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
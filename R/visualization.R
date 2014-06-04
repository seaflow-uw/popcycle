plotCytogram <- function(opp, para.x, para.y, vct = TRUE){		
	

	breaks <- 10^((0:24)*3.5/24)	# log - spaced grid
	cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))

	hist.x <- hist(opp[,para.x], breaks=breaks, plot=FALSE)
	hist.y <- hist(opp[,para.y], breaks=breaks, plot=FALSE)

	
	def.par <- par(no.readonly = TRUE) # save default, for resetting...
	nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1,3,1,3), c(1,3,1,3,1,3), TRUE)

	par(mar=c(6,6,1,1),pty='s')
	if(vct==FALSE) plot(opp[,c(para.x, para.y)], pch=16, cex=0.4, col = densCols(log10(opp[,c(para.x, para.y)]), colramp = cols), log='xy') #plot 2D cytogram
	
	if(vct==TRUE){
		plot(opp[,c(para.x, para.y)], pch=16, cex=0.4, col = as.numeric(as.factor(opp$pop)), log='xy') #plot 2D cytogram
		legend('topleft',legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16,pt.cex=0.4,bty='n')
	}
	par(mar=c(0,6,1,1), pty='m')
	barplot(hist.x$counts, axes=FALSE, space=0, col=NA)
	par(mar=c(6,0,1,1),pty='m')
	barplot(hist.y$counts, axes=FALSE, space=0, horiz=TRUE, col=NA)

   
par(def.par)   	  

}

# testing
#opp.path <- system.file("extdata","seaflow_cruise","2011_001", "2.evt.opp", package="flowPhyto")
#opp <- readSeaflow(opp.path, transform=T)

#plotCytogram(opp, "fsc_small", "chl_small",classification=TRUE)

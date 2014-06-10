###############
### Plot_RT ###
###############

# load latest file
last.file <- get_latest_file()
opp <- get_opp_by_file(last.file)
vct <- get_opp_by_file(vct.file)
opp$pop <- vct


######################
### PLOT CYTOGRAMS ###
######################
png("cytogram.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(2,1))
plot.vct.cytogram(opp, "fsc_small","chl_small")
plot.vct.cytogram(opp, "fsc_small","pe")
dev.off()


########################
### PLOT PHYTO  CONC ###
########################
lim.t <- c(min(stat$time,na.rm=T), max(stat$time,na.rm=T))
xlim <- c(min(stat$long,na.rm=T)-margin, max(stat$long,na.rm=T)+margin)
ylim <- c(min(stat$lat,na.rm=T)-margin, max(stat$lat,na.rm=T)+margin)


png("cell_conc_time.png",width=15, height=9, unit='in', res=300)

par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(2,2,2,1), oma=c(1,1,3,0))
n <- 1
for(i in phyto){ 
	p <- subset(stat, pop == i)
		plot(p$time, p$conc, xlim=lim.t,xlab=NA,ylab=NA, main=paste(i), pch=21, bg=n)
		if(nrow(p) < 10) mtext(paste("only ", nrow(p), "data points"), side=1, line=-4)
		#if(n == 1) mtext(substitute(paste("Cell density (10"^{6}," cells L"^{-1},")")), side=3, line=2.5, cex=cex*1.5, font=2)
		if(n == 1) mtext("Cell density (10^6 cells per L)", outer=T, side=3, line=1, cex=cex*1.5, font=2)
		n <- n+1
}

dev.off()


png("cell_conc_map.png",width=15, height=9, unit='in', res=300)

par(mfrow=c(ceiling(length(phyto)/3),3), cex=cex, mar=c(2,2,2,3), oma=c(1,1,3,0))
n <- 1
for(i in phyto){ 
	p <- subset(stat, pop == i)
		plotStatMap(stat, pop=i, xlim=xlim, ylim=ylim, z.param='conc', main=paste(pop.def[i,"title"]), margin=margin, ma=1,lty=3, lwd=3,cex=cex, zlab=NA, ylab=NA, xlab=NA)
		points(stat[nrow(stat),"long"], stat[nrow(stat),"lat"], pch=3, lwd=3, cex=1.5)
		if(nrow(p) < 10) mtext(paste("only ", nrow(p), "data points"), side=1, line=-4)
		if(n == 1) mtext("Cell density (10^6 cells per L)", outer=T, side=3, line=1, cex=cex*1.5, font=2)
		n <- n+1
}

dev.off()

######################
### PLOT PHYTO FSC ###
######################
lim.t <- c(min(stat$time,na.rm=T), max(stat$time,na.rm=T))
pop <- pop.def$abrev


png("light_scatter.png",width=15, height=9, unit='in', res=300)

par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(2,2,2,1), oma=c(1,1,3,0))
n <- 1
for(i in phyto){ 
	p <- subset(stat, pop == i)
		plot(p$time, p$fsc_small_med/1000, xlim=lim.t,xlab=NA,ylab=NA, main=paste(pop.def[i,"title"]), pch=21, bg=pop.def[pop.def$abrev == i,"color"])
		if(nrow(p) < 10) mtext(paste("only ", nrow(p), "data points"), side=1, line=-4)
		if(n == 1) mtext("Cell size (light scatter)", outer=T, side=3, line=1, cex=cex*1.5, font=2)
		n <- 2
}

dev.off()
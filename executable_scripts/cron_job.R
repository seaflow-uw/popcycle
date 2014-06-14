library(popcycle)


############################
### ANALYZE LAST FILE(s) ###
############################
evaluate.last.evt()



######################
### PLOT CYTOGRAMS ###
######################
cex=1.4

last.file <- get.latest.file()
opp <- get.opp.by.file(last.file)
vct <- get.vct.by.file(last.file)
opp$pop <- vct

png("~/cytogram.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(1,2),cex=cex)
plot.vct.cytogram(opp, "fsc_small","chl_small")
plot.vct.cytogram(opp, "fsc_small","pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()


##################
### PLOT STATS ###
##################
stat <- get.stat.table()  
phyto <- unique(stat$pop)


png("cell_conc_map.png",width=15, height=9, unit='in', res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(2,2,2,3), oma=c(1,1,3,0))
for(i in phyto)	try(plot.map (stat, popname=i, param='abundance'))
dev.off()


png("cell_conc_time.png",width=15, height=9, unit='in', res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(2,2,2,3), oma=c(1,1,3,0))
for(i in phyto)	try(plot.time(stat, popname=i, param='abundance'))
dev.off()


png("~/light_scatter.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(2,2,2,3), oma=c(1,1,3,0))
for(i in phyto)	try(plot.time(stat, popname=i, param='fsc_small'))
dev.off()


png("~/chl_small.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(2,2,2,3), oma=c(1,1,3,0))
for(i in phyto)	try(plot.time(stat, popname=i, param='chl_small'))
dev.off()

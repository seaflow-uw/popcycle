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

print("creating cytogram.png")
png("~/cytogram.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(1,2),cex=cex)
plot.cytogram(opp, "fsc_small","chl_small")
plot.cytogram(opp, "fsc_small","pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()

print("creating vct.cytogram.png")
png("~/vct.cytogram.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(1,2),cex=cex)
plot.vct.cytogram(opp, "fsc_small","chl_small")
plot.vct.cytogram(opp, "fsc_small","pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()


##############
### CytDiv ###
##############
cytdiv <- get.cytdiv.table()

print("creating cytometric diversity.png")
png("~/div_indices.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(2,2),cex=cex)
plot.cytdiv.map(cytdiv, index="N0")
plot.cytdiv.map(cytdiv, index="H")
plot.cytdiv.time(cytdiv, index="N0")
plot.cytdiv.time(cytdiv, index="H")
dev.off()


##################
### PLOT STATS ###
##################
stat <- get.stat.table()  
phyto <- unique(stat$pop)

print("creating cell_conc_map.png")
png("cell_conc_map.png",width=15, height=9, unit='in', res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,2))
for(i in phyto)	try(plot.map (stat, popname=i, param='abundance'))
dev.off()

print("creating cell_conc_time.png")
png("cell_conc_time.png",width=15, height=9, unit='in', res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,1))
for(i in phyto)	try(plot.time(stat, popname=i, param='abundance'))
dev.off()

print("creating light_scatter.png")
png("~/light_scatter.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,1))
for(i in phyto) try(plot.time(stat, popname=i, param='fsc_small'))
dev.off()

print("creating chl_small.png")
png("~/chl_small.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,1))
for(i in phyto)	try(plot.time(stat, popname=i, param='chl_small'))
dev.off()

print("DONE")
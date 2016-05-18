library(popcycle)

set.cruise.id("realtime")


############################
### ANALYZE LAST FILE(s) ###
############################
opp.list <- get.opp.files()
evt.list <- get.evt.list()
id <- match(opp.list, unlist(lapply(evt.list, clean.file.name)))

if(length(id) > 0){
    for(evt.file in evt.list[-id]) evaluate.evt(evt.file)
}else{
	for(evt.file in evt.list) evaluate.evt(evt.file)
}





######################
### PLOT CYTOGRAMS ###
######################
cex=1.4

last.file <- get.latest.evt()
opp <- get.opp.by.file(last.file)
vct <- get.vct.by.file(last.file)
opp$pop <- vct

print("creating vct.cytogram.png")
png("~/vct.cytogram.png",width=15,height=9,unit='in',res=100)
par(mfrow=c(1,2),cex=cex)
plot.vct.cytogram(opp, "fsc_small","chl_small")
plot.vct.cytogram(opp, "fsc_small","pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()

print("creating gate.cytogram.png")
png("~/gate.cytogram.png",width=15,height=9,unit='in',res=100)
par(mfrow=c(1,2),cex=cex)
plot.gate.cytogram(opp, "fsc_small","chl_small")
plot.gate.cytogram(opp, "fsc_small","pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()


##################
### PLOT STATS ###
##################
stat <- get.stat.table()

print("saving stat.csv")
write.csv(stat, "~/stat.csv", row.names=FALSE, quote=FALSE)

phyto <- unique(stat$pop)

print("creating cell_conc_map.png")
png("~/cell_conc_map.png",width=15, height=9, unit='in', res=100)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,2))
for(i in phyto)	try(plot.map (stat, popname=i, param='abundance'))
dev.off()

print("creating cell_conc_time.png")
png("~/cell_conc_time.png",width=15, height=9, unit='in', res=100)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,1))
for(i in phyto)	try(plot.time(stat, popname=i, param='abundance'))
dev.off()

print("creating light_scatter.png")
png("~/light_scatter.png",width=15,height=9,unit='in',res=100)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,1))
for(i in phyto) try(plot.time(stat, popname=i, param='fsc_small'))
dev.off()

print("creating chl_small.png")
png("~/chl_small.png",width=15,height=9,unit='in',res=100)
par(mfrow=c(ceiling(length(phyto)/2),2), cex=cex, mar=c(4,4,3,4), oma=c(1,1,1,1))
for(i in phyto)	try(plot.time(stat, popname=i, param='chl_small'))
dev.off()



# ###############
# ### TS PLOT ###
# ###############

sfl <- get.sfl.table()
print("saving sfl.csv")
write.csv(sfl, "~/sfl.csv", row.names=FALSE, quote=FALSE)

print("creating TS_plot.png")
png("~/TS_plot.png",width=15, height=9, unit='in', res=100)
par(mfrow=c(1,1),cex=cex)
plot.TS(sfl)
dev.off()


print("DONE")

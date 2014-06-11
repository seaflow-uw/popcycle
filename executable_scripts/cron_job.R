library(popcycle)


############################
### ANALYZE LAST FILE(s) ###
############################
evaluate.last.evt()



########################
### load latest file ###
########################

last.file <- get.latest.file()
opp <- get.opp.by.file(last.file)
vct <- get.vct.by.file(last.file)
opp$pop <- vct



######################
### PLOT CYTOGRAMS ###
######################
png("~/cytogram.png",width=15,height=9,unit='in',res=300)
par(mfrow=c(1,2))
plot.vct.cytogram(opp, "fsc_small","chl_small")
plot.vct.cytogram(opp, "fsc_small","pe")
mtext(paste(last.file), side=3, outer=T)
dev.off()


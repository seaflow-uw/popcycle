library(popcycle)


############################
### ANALYZE LAST FILE(s) ###
############################
evaluate_last_evt()



########################
### load latest file ###
########################

last.file <- get_latest_file()
opp <- get_opp_by_file(last.file)
vct <- get_vct_by_file(last.file)
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


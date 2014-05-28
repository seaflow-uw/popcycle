library(flowCore)
library(splancs) 
library(flowPhyto)
cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))

opp.path <- system.file("extdata","seaflow_cruise","2011_001", "2.evt.opp", package="flowPhyto")
opp <- readSeaflow(opp.path, transform=F)
opp$pop <- 0



##############
### GATING ###
##############
par(mfrow=c(1,1), pty="s")

### BEADS

para <- c("chl_small","pe")

x <- subset(opp, pop==0)
plot(x[,para], pch=16, cex=0.4, col = densCols(x[,para], colramp = cols), main="Gating BEADS")

print("Gating Beads")
poly.beads <- getpoly(quiet=TRUE); colnames(poly.beads) <- para
beads <- subset(x,inout(x[,para],poly=poly.beads, bound=TRUE, quiet=TRUE))


### SYN
para <- c("fsc_small","pe")
plot(x[,para], pch=16, cex=0.4, col = densCols(x[,para], colramp = cols), main="Gating Synecho")
points(beads[,para],col=2)

print("Gating Synecho")
poly.syn <- getpoly(quiet=TRUE); colnames(poly.syn) <- para
syn <- subset(x,inout(x[,para],poly=poly.syn, bound=TRUE, quiet=TRUE))


##################
### ASSIGNMENT ###
##################

opp[row.names(beads),'pop'] <- "beads"
opp[row.names(syn),'pop'] <- "syn"

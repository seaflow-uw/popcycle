#' Plot EVT or OPP cytogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for densCols()
#' @return None
#' @export
plot.cytogram <- function(evtopp, para.x = 'fsc_small', para.y = 'chl_small',...) {
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))

  par(pty='s')
  id <- which(colnames(evtopp) == "fsc_small" | colnames(evtopp) == "chl_small" | colnames(evtopp) =="pe" | colnames(evtopp) =="fsc_perp")
  if(max(evtopp[,c(id)]) > 10^3.5) plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = densCols(evtopp[,c(para.x, para.y)], colramp = cols), xlim=c(0,2^16), ylim=c(0,2^16), ...)
  else plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = densCols(log10(evtopp[,c(para.x, para.y)]), colramp = cols), log='xy',xlim=c(1,10^3.5), ylim=c(1,10^3.5), ...)
}

#' Plot cytogram for one EVT file.
#'
#' @param evt.dir EVT file directory.
#' @param file.name File name with julian day directory.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for densCols()
#' @return None
#' @export
plot.evt.cytogram.by.file <- function(evt.dir, file.name, para.x = 'fsc_small', para.y = 'chl_small',...){
  evt <- readSeaflow(file.path(evt.dir, file.name))
  plot.cytogram(evt, para.x = para.x, para.y = para.y,...)
}

#' Plot cytogram for one OPP file.
#'
#' @param opp.dir OPP file directory.
#' @param file.name File name with julian day directory.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for densCols()
#' @return None
#' @export
plot.opp.cytogram.by.file <- function(opp.dir, file.name, para.x = 'fsc_small', para.y = 'chl_small',...){
  if (endswith(file.name, ".gz")) {
    # Remove .gz
    file.name <- substr(file.name, 1, nchar(file.name) - nchar(".gz"))
  }
  if (! endswith(file.name, ".opp")) {
    file.name <- paste0(file.name, ".opp")
  }
  opp <- readSeaflow(file.path(opp.dir, file.name))
  plot.cytogram(opp, para.x = para.x, para.y = para.y,...)
}

#' Plot cytogram with particles colored by population.
#'
#' @param opp OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for plot()
#' @return None
#' @export
plot.vct.cytogram <- function(opp, para.x = 'fsc_small', para.y = 'chl_small',...){
	if (!is.null(opp$pop)) {
		par(pty='s')
    ## TODO[francois] Order OPP by frequency (most abundant pop plotted first, least abundant pop plotted last)
    id <- which(colnames(opp) == "fsc_small" | colnames(opp) == "chl_small" | colnames(opp) =="pe" | colnames(opp) =="fsc_perp")
    if(max(opp[,c(id)]) > 10^3.5) plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = as.numeric(as.factor(opp$pop)), xlim=c(0,2^16), ylim=c(0,2^16),...)
    else plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = as.numeric(as.factor(opp$pop), log='xy', xlim=c(1,10^3.5), ylim=c(1,10^3.5),...)
       legend('topleft', legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16, pt.cex=0.6, bty='n')
    } else {
		print("No Gating parameters yet")
		plot.cytogram(opp, para.x, para.y)
		mtext(paste("No Gating parameters yet!"), 3, line=-1, font=2)
	}
}


# #' Plot cytogram with particles colored by population.
# #'
# #' @param opp OPP data frame.
# #' @param para.x Channel to use as x axis.
# #' @param para.y Channel to use as y axis.
# #' @param ... Additional parameters for plot()
# #' @return None
# #' @export
# plot.vct.cytogram.deluxe <- function(opp, para.x = 'fsc_small', para.y = 'chl_small',...){
#
#   breaks <- 50
#   def.par <- par(no.readonly = TRUE) # save default, for resetting...
#   nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1,3,1,3), c(1,3,1,3,1,3), TRUE)
#   if (!is.null(opp$pop)) {
# 		par(pty='m')
#     id <- which(colnames(opp) == "fsc_small" | colnames(opp) == "chl_small" | colnames(opp) =="pe" | colnames(opp) =="fsc_perp")
#     if(max(opp[,c(id)]) > 10^3.5){
#       par(mar=c(6,6,1,1))
#       plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = as.numeric(as.factor(opp$pop)), xlim=c(0,2^16), ylim=c(0,2^16))
#       legend('topleft', legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16, pt.cex=0.6, bty='n')
#       par(mar=c(0,6,1,1))
#       barplot(hist(opp[,para.x], plot=FALSE,breaks=seq(0,2^16, by=2^16/breaks))$counts, axes=FALSE,axisnames=FALSE, space=0, col=NA)
#         for(p in unique(opp$pop)) barplot(hist(opp[which(opp$pop == p),para.x], breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)$counts, add=T, axes=FALSE,axisnames=FALSE, space=0, col=as.numeric(as.factor(opp[which(opp$pop == p),"pop"])))
#       par(mar=c(6,0,1,1))
#       barplot(hist(opp[,para.y], breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)$counts, axes=FALSE,axisnames=FALSE, space=0, col=NA, horiz=T)
#         for(p in unique(opp$pop)) barplot(hist(opp[which(opp$pop == p),para.y], breaks=seq(0,2^16, by=2^16/50), plot=FALSE)$counts, add=T, axes=FALSE,axisnames=FALSE, space=0, col=as.numeric(as.factor(opp[which(opp$pop == p),"pop"])), horiz=T)
#
#       } else{
#       par(mar=c(6,6,1,1))
#       plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = as.numeric(as.factor(opp$pop)), log='xy', xlim=c(1,10^3.5), ylim=c(1,10^3.5))
#       legend('topleft', legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16, pt.cex=0.6, bty='n')
#       par(mar=c(0,6,1,1))
#       barplot(hist(log10(opp[,para.x]), plot=FALSE, breaks=log10(seq(0,10^3.5, by=10^3.5/breaks)))$counts, axes=FALSE,axisnames=FALSE, space=0, col=NA)
#         for(p in unique(opp$pop)) barplot(hist(log10(opp[which(opp$pop == p),para.x]), plot=FALSE,breaks=log10(seq(0,10^3.5, by=10^3.5/breaks)))$counts, add=T, axes=FALSE,axisnames=FALSE, space=0, col=as.numeric(as.factor(opp[which(opp$pop == p),"pop"])))
#       par(mar=c(6,0,1,1))
#       barplot(hist(log10(opp[,para.y]), plot=FALSE,breaks=log10(seq(0,10^3.5, by=10^3.5/breaks)))$counts, axes=FALSE,axisnames=FALSE, space=0, col=NA, horiz=T)
#         for(p in unique(opp$pop)) barplot(hist(log10(opp[which(opp$pop == p),para.y]), plot=FALSE,breaks=log10(seq(0,10^3.5, by=10^3.5/breaks)))$counts, add=T, axes=FALSE,axisnames=FALSE, space=0, col=as.numeric(as.factor(opp[which(opp$pop == p),"pop"])), horiz=T)
#
#     } else {
# 		print("No Gating parameters yet")
# 		plot.cytogram(opp, para.x, para.y)
# 		mtext(paste("No Gating parameters yet!"), 3, line=-1, font=2)
# 	}
#   par(def.par)
# }
#
# hist(opp[,para.x], plot=FALSE, breaks=seq(0,10^3.5, by=10^3.5/breaks))

#' Plot cytogram with particles colored by population for one file.
#'
#' @param opp.dir OPP file directory.
#' @param vct.dir VCT file directory.
#' @param file.name File name with julian day directory.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for densCols()
#' @return None
#' @export
plot.vct.cytogram.by.file <- function(opp.dir, vct.dir, file.name, para.x = 'fsc_small', para.y = 'chl_small',...){
  opp <- get.opp.by.file(opp.dir, file.name, vct.dir=vct.dir)
  plot.vct.cytogram(opp, para.x = para.x, para.y = para.y,...)
}

#' Plot cytogram with gates.
#'
#' @param opp OPP data frame.
#' @param poly.log Gating polygon(s) to draw (optional)
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @return None
#' @export
plot.gating.cytogram <- function(opp, poly.log=NULL, para.x = 'fsc_small', para.y = 'chl_small') {
	plot.cytogram(opp, para.x, para.y)
	if (!is.null(poly.log)) {
		for (i in 1:length(poly.log)) {
      method <- poly.log[[i]]$method
      if (method != "manual") {
        next
      }

	    pop <- names(poly.log[i]) # name of the population
	    poly <- poly.log[[i]]$poly # Get parameters of the gate for this population
	    para <- colnames(poly)
	    if (para[1]==para.x & para[2]==para.y) {
	      polygon(poly, lwd=3,border=i, col=NA)
	      text(mean(poly[,1]), mean(poly[,2]),labels=pop, col=i, font=2)
	    }
		}
	}
}

#' Plot cytogram with gates for one file.
#'
#' @param opp.dir OPP file directory.
#' @param file.name File name with julian day directory.
#' @param poly.log Gating polygon(s) to draw (optional).
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @return None
#' @export
plot.gating.cytogram.by.file <- function(opp.dir, file.name, poly.log=NULL, para.x = 'fsc_small', para.y = 'chl_small') {
  opp <- get.opp.by.file(opp.dir, file.name)
  plot.gating.cytogram(opp, poly.log, para.x = para.x, para.y = para.y)
}


#' Plot cell abundances of a population on a map.
#'
#' @return None
#' @export
plot.map <- function(stat,popname,param,...){
  require(maps, quietly=T)
  require(mapdata, quietly=T)
  require(plotrix, quietly=T)

  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))

  map.type <- 'worldHires'

    xlim <- range(stat$lon, na.rm=T)
    ylim <- range(stat$lat, na.rm=T)

    if(xlim[1] < 0 & xlim[2] > 0){
        neg.lon <- subset(stat, lon < 0)
      stat[row.names(neg.lon), "long"] <- neg.lon$lon + 360
      xlim <- c(min(stat$lon, na.rm=TRUE), max(stat$lon, na.rm=TRUE))
      stat <- stat[-which(is.na(stat$lon)),]
      stat$lon[stat$lon < 0] <- stat$lon[stat$lon < 0] + 360
      map.type <- 'world2Hires'
        }

  # plot the cruise track as gray line back-ground
  pop <- subset(stat, pop == popname)
  plot(pop$lon, pop$lat, xlim=xlim, ylim=ylim, asp=1, main=paste(popname),
            xlab=expression(paste("Longitude (",degree,"W)")),ylab=expression(paste("Latitude (",degree,"N)")),type='l',lwd=3,col='lightgrey',...)
  try(maps::map(map.type, fill=F, col='black',add=TRUE))
  points(pop$lon, pop$lat, pch=16, asp=1, col=cols(100)[cut(pop[,param],100)],...)

    ylim <- par('usr')[c(3,4)]
    xlim <- par('usr')[c(1,2)]

    color.legend(xlim[2], ylim[1], xlim[2] + 0.02*diff(xlim), ylim[2],
      legend=pretty(pop[,param]), rect.col=cols(100), gradient='y',align='rb',...)
  mtext(paste(param), side=4, line=2,...)


}

#' plot.time
#'
#' @return None
#' @export
plot.time <- function(stat, popname,param, ...){

  stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')
  pop <- subset(stat, pop == popname)
  plot(pop$time, pop[,param], xlab="Time", ylab=paste(param),main=paste(popname),...)

}
#' Temperature salinity plot.
#'
#' @return None
#' @export
plot.TS <- function(sfl,...){

  require(plotrix, quietly=T)

  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  sfl$date <- as.POSIXct(sfl$date,format="%FT%T",tz='GMT')

  par(pty='s')
  plot(sfl$ocean_tmp, sfl$salinity, col=cols(50)[cut(sfl$date,50)],pch=16,xlab=expression(paste("Temp (",degree,"C)")), ylab="Salinity (psu)",...)
  ylim <- par('usr')[c(3,4)]
  xlim <- par('usr')[c(1,2)]
  color.legend(xlim[2], ylim[1], xlim[2] + 0.02*diff(xlim), ylim[2],
    legend=c("start","end"), rect.col=cols(50), gradient='y',align='rb',...)
  mtext("Time", side=4, line=2,...)
}

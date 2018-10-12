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
      else plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = as.numeric(as.factor(opp$pop)), log='xy', xlim=c(1,10^3.5), ylim=c(1,10^3.5),...)
    legend('topleft', legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16, pt.cex=0.6, bty='n')
    } else {
		print("No Gating parameters yet")
		plot.cytogram(opp, para.x, para.y)
		mtext(paste("No Gating parameters yet!"), 3, line=-1, font=2)
	}
}


#' Plot cytogram with gates.
#'
#' @param opp OPP data frame.
#' @param poly.log Gating polygon(s) to draw (optional)
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @return None
#' @export
plot.gating.cytogram <- function(opp, gating.log=NULL, para.x = 'fsc_small', para.y = 'chl_small') {
	plot.cytogram(opp, para.x, para.y)
	if (!is.null(gating.log)) {
		for (i in 1:length(gating.log)) {
      method <- gating.log[[i]]$method
      if (method != "manual") {
        next
      }

	    pop <- names(gating.log[i]) # name of the population
	    poly <- gating.log[[i]]$poly # Get parameters of the gate for this population
	    para <- colnames(poly)
	    if (para[1]==para.x & para[2]==para.y) {
	      polygon(poly, lwd=3,border=i, col=NA)
	      text(mean(poly[,1]), mean(poly[,2]),labels=pop, col=i, font=2)
	    }
		}
	}
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
  try(maps::map(map.type, fill=T, col='darkgrey',add=TRUE))
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
  require(plotrix, quietly=T)

  stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')
  pop <- subset(stat, pop == popname)
  if(any(colnames(stat)== paste0(param,".sd"))) plotCI(pop$time, pop[,param], uiw=pop[,paste0(param,".sd")], sfrac=0,xlab="Time", ylab=paste(param),main=paste(popname), scol='grey',...)
  else plot(pop$time, pop[,param], xlab="Time", ylab=paste(param),main=paste(popname),...)


}

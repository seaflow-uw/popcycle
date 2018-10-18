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
#' @param min.count Minimum number of particles for drawing contour lines.
#' @param ... Additional parameters for plot()
#' @return None
#' @export
plot.vct.cytogram <- function(opp, para.x = 'fsc_small', para.y = 'chl_small', min.count=300, ...){
  require(scales, quietly=T)
  require(flowViz, quietly=T)

  if (!is.null(opp$pop)) {
    popu <- c('unknown','picoeuk','croco','prochloro','synecho','beads')
    color <- c('grey','seagreen3','darkorchid','skyblue3','orange','red3')
    for(i in 1:length(popu)) opp[which(opp$pop==popu[i]),'col'] <- color[i]

		par(pty='s')
        d <- log(mean(par("pin")))
        plot(opp[,c(para.x, para.y)], cex=d, pch=16, col = alpha(opp$col,0.4), log='xy', xlim=c(1,10^3.5), ylim=c(1,10^3.5),...)
          par(new=T)
          plot(1,1,pch=NA, xaxt='n', yaxt='n', xlim=log10(c(1,10^3.5)), ylim=log10(c(1,10^3.5)), xlab=NA, ylab=NA, bty='n')


        for(i in c('prochloro','synecho')){
          df <- subset(opp, pop==i)
            if(nrow(df) < min.count) next
          c <- rgb2hsv(col2rgb(df$col))
          cols <- rainbow(10,s=c[2], v=c[3],start=c[1], alpha=0.1)
          df.f <- flowFrame(as.matrix(log10(df[,c(para.x, para.y)])))
          contour(df.f,fill=alpha('white',0.5), col='grey', grid.size=c(30,30),  add=T)
          contour(df.f,fill=cols, col=cols, grid.size=c(30,30), add=T)
        }

    legend('topleft', legend=(unique(opp$pop)), col=unique(opp$col), pch=16, pt.cex=0.6, bty='n')
    } else {
		print("No Gating parameters yet")
		plot.cytogram(opp, para.x, para.y, ...)
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

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
    else plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = as.numeric(as.factor(opp$pop)), log='xy',xlim=c(1,10^3.5), ylim=c(1,10^3.5),...)
		legend('topleft',legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16,pt.cex=0.6,bty='n')
	} else {
		print("No Gating parameters yet")
		plot.cytogram(opp, para.x, para.y)
		mtext(paste("No Gating parameters yet!"),3,line=-1,font=2)
	}
}

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
	    pop <- names(poly.log[i]) # name of the population
	    poly <- poly.log[i][[1]] # Get parameters of the gate for this population
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

#' Plot helpful cytograms for exploring filtering parameters.
#'
#' @param evt EVT data frame.
#' @param origin,width,notch1,notch2,offset Filtering parameters. origin,
#'   notch1, and notch2 will be calculated if NA.
#' @return None
#' @export
plot.filter.cytogram <- function(evt, origin=NA, width=0.5, notch1=NA, notch2=NA, offset=0) {
  origin <- as.numeric(origin)
  width <- as.numeric(width)

  notch1 <- as.numeric(notch1)
  notch2 <- as.numeric(notch2)
  offset <- as.numeric(offset)

  # linearize the LOG transformed data
  id <- which(colnames(evt) == "fsc_small" | colnames(evt) == "chl_small" | colnames(evt) =="pe" | colnames(evt) =="fsc_perp" | colnames(evt) =="D1" | colnames(evt) =="D2")
  if (!any(max(evt[,c(id)]) > 10^3.5)) {
    evt[,c(id)] <- (log10(evt[,c(id)])/3.5)*2^16
  }

  # Correction for the difference of sensitivity between D1 and D2
  if (is.na(origin)) origin <- median(evt$D2-evt$D1)

 # Filtering particles detected by fsc_small
  evt. <- subset(evt, fsc_small > 0)

  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt., D2 < (D1+origin) + width * 10^4 & (D1+origin) < D2 + width * 10^4)

 # finding the notch
  if (is.na(notch1)) {
    d.min1 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D1"])
    fsc.max1 <- max(aligned[which(aligned$D1 == d.min1),"fsc_small"])
    notch1 <- fsc.max1 / (d.min1+ 10000)
  }

  if (is.na(notch2)) {
    d.min2 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D2"])
    fsc.max2 <- max(aligned[which(aligned$D2 == d.min2),"fsc_small"])
    notch2 <- fsc.max2 / (d.min2 + 10000)
  }

  # Filtering focused particles (fsc_small > D + notch)
  opp <- subset(aligned, fsc_small > D1*notch1 - offset*10^4 & fsc_small > D2*notch2 - offset*10^4)

  ################
  ### PLOTTING ###
  ################
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  percent.opp <- round(100*nrow(opp)/nrow(evt),2)

  origin1 <- origin + width*10^4
  origin2 <- origin - width*10^4

  if(nrow(evt.) > 10000)  evt. <- evt.[round(seq(1,nrow(evt.), length.out=10000)),]
  if(nrow(aligned) > 10000)  aligned <- aligned[round(seq(1,nrow(aligned), length.out=10000)),]

  def.par <- par(no.readonly = TRUE) # save default, for resetting...

  par(mfrow=c(2,3),pty='s')

  plot.cytogram(evt., "D1", "D2")
  mtext("Alignment", side=3, line=4, font=2, col=2)
  abline(b=1, a=origin1, col='red',lwd=2)
  abline(b=1, a=origin2, col='red',lwd=2)
  mtext(paste("D2 - D1=", round(origin,2)),side=3, line=2,font=2)
  mtext(paste("Width=", width),side=3, line=1,font=2)

  plot.cytogram(aligned, "fsc_small", "D1")
  mtext("Focus", side=3, line=4, font=2,col=2)
  mtext(paste("Notch 1=", round(notch1, 2)),side=3, line=2,font=2)
  mtext(paste("Offset=", offset),side=3, line=1,font=2)
  abline(b=1/notch1, a=offset*10^4, col=2,lwd=2)

  plot.cytogram(aligned, "fsc_small", "D2")
  mtext("Focus", side=3, line=4, font=2,col=2)
  mtext(paste("Notch 2=", round(notch2, 2)),side=3, line=2,font=2)
  mtext(paste("Offset=", offset),side=3, line=1,font=2)
  abline(b=1/notch2, a=offset*10^4, col=2,lwd=2)

  plot.cytogram(opp, "fsc_small", "pe")
  mtext("OPP", side=3, line=1, font=2)
  plot.cytogram(opp, "fsc_small","chl_small")
  mtext("OPP", side=3, line=1, font=2)
  mtext(paste("OPP =", percent.opp,"% EVT"), outer=T,side=1, line=-1.5,font=2,col=2)
  plot.cytogram(opp, "chl_small","pe")
  mtext("OPP", side=3, line=1, font=2)

  par(def.par)
}

#' Plot helpful cytograms for exploring filtering parameters for one file.
#'
#' @param evt.dir EVT file directory.
#' @param file.name File name with julian day directory.
#' @param origin,width,notch1,notch2,offset Filtering parameters. origin,
#'   notch1, and notch2 will be calculated if NA.
#' @return None
#' @export
plot.filter.cytogram.by.file <- function(evt.dir, file.name, origin=NA, width=0.5, notch1=NA, notch2=NA, offset=0) {
  file.name <- clean.file.path(file.name)
  evt <- readSeaflow(file.path(evt.dir, file.name))
  plot.filter.cytogram(evt, origin=origin, notch1=notch1, notch2=notch2,
		                   width=width, offset=offset)
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

#' plot.cytdiv.map
#'
#' @return None
#' @export
plot.cytdiv.map <- function(cytdiv,index,...){
  require(maps, quietly=T)
  require(mapdata, quietly=T)
  require(plotrix, quietly=T)

 cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))

  map.type <- 'worldHires'

    xlim <- range(cytdiv$lon, na.rm=T)
    ylim <- range(cytdiv$lat, na.rm=T)

    if(xlim[1] < 0 & xlim[2] > 0){
        neg.lon <- subset(cytdiv, lon < 0)
      cytdiv[row.names(neg.lon), "long"] <- neg.lon$lon + 360
      xlim <- c(min(cytdiv$lon, na.rm=TRUE), max(cytdiv$lon, na.rm=TRUE))
      cytdiv <- cytdiv[-which(is.na(cytdiv$lon)),]
      cytdiv$lon[cytdiv$lon < 0] <- cytdiv$lon[cytdiv$lon < 0] + 360
      map.type <- 'world2Hires'
        }

  plot(cytdiv$lon, cytdiv$lat, xlim=xlim, ylim=ylim, asp=1,
            xlab=expression(paste("Longitude (",degree,"W)")),ylab=expression(paste("Latitude (",degree,"N)")),type='l',lwd=3,col='lightgrey',...)
  try(maps::map(map.type, fill=F, col='black',add=TRUE))
  points(cytdiv$lon, cytdiv$lat, pch=16, asp=1, col=cols(100)[cut(cytdiv[,index],100)],...)

    ylim <- par('usr')[c(3,4)]
    xlim <- par('usr')[c(1,2)]

    color.legend(xlim[2], ylim[1], xlim[2] + 0.02*diff(xlim), ylim[2],
      legend=pretty(cytdiv[,index]), rect.col=cols(100), gradient='y',align='rb',...)
  mtext(paste(index), side=4, line=2,...)

}

#' plot.cytdiv.time
#'
#' @return None
#' @export
plot.cytdiv.time <- function(cytdiv,index, ...){

  cytdiv$time <- as.POSIXct(cytdiv$time,format="%FT%T",tz='GMT')
  plot(cytdiv$time, cytdiv[,index], xlab="Time", ylab=paste(index),...)

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

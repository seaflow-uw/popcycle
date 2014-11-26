plot.cytogram <- function(opp, para.x = 'fsc_small', para.y = 'chl_small',...){		

	cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
	
  par(pty='s')
  id <- which(colnames(opp) == "pulse_width" | colnames(opp) == "time" | colnames(opp) =="pop")
  if(max(opp[,-c(id)]) > 10^3.5) plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = densCols(opp[,c(para.x, para.y)], colramp = cols), xlim=c(0,2^16), ylim=c(0,2^16), ...)
  else plot(opp[,c(para.x, para.y)], pch=16, cex=0.3, col = densCols(log10(opp[,c(para.x, para.y)]), colramp = cols), log='xy',xlim=c(1,10^3.5), ylim=c(1,10^3.5), ...) 

}


plot.cytogram.by.file <- function(file.name, para.x = 'fsc_small', para.y = 'chl_small',...){
  
  opp <- get.opp.by.file(file.name)
  plot.cytogram(opp, para.x = para.x, para.y = para.y,...)

}


## OPP merged with VCT
plot.vct.cytogram <- function(opp,para.x = 'fsc_small', para.y = 'chl_small',...){		
		
		if(!is.null(opp$pop)){
			par(pty='s')
      ## TODO[francois] Order OPP by frequency (most abundant pop plotted first, least abundant pop plotted last)
      id <- which(colnames(opp) == "pulse_width" | colnames(opp) == "time" | colnames(opp) =="pop")
      if(max(opp[,-c(id)]) > 10^3.5) plot(opp[,c(para.x, para.y)], pch=16, cex=0.6, col = as.numeric(as.factor(opp$pop)), xlim=c(0,2^16), ylim=c(0,2^16),...)
      else plot(opp[,c(para.x, para.y)], pch=16, cex=0.6, col = as.numeric(as.factor(opp$pop)), log='xy',xlim=c(1,10^3.5), ylim=c(1,10^3.5),...)
			legend('topleft',legend=(unique(opp$pop)), col=unique(as.numeric(as.factor(opp$pop))), pch=16,pt.cex=0.6,bty='n')
		}else{
			print("No Gating parameters yet!")
			plot.cytogram(opp, para.x, para.y)
			mtext(paste("No Gating parameters yet!"),3,line=-1,font=2)
		}
}


plot.vct.cytogram.by.file <- function(file.name, para.x = 'fsc_small', para.y = 'chl_small',...){
  
  vct <- get.vct.by.file(file.name)
  opp <- get.opp.by.file(file.name)
  opp$pop <- vct
  plot.vct.cytogram(opp, para.x = para.x, para.y = para.y,...)

}


plot.gate.cytogram <- function(opp,para.x = 'fsc_small', para.y = 'chl_small'){

  plot.cytogram(opp, para.x, para.y)

     params <- list.files(param.gate.location,"params.RData")
      if(length(params)==0){
        print("No gate parameters found!")
        stop
       }else{load(paste0(param.gate.location,"/params.RData"))}

            for(i in 1:length(poly.log)){
                pop <- names(poly.log[i]) # name of the population
                poly <- poly.log[i][[1]] # Get parameters of the gate for this population
                para <- colnames(poly)
                if(para[1]==para.x & para[2]==para.y){
                  polygon(poly, lwd=2,border=i, col=NA)
                  text(mean(poly[,1]), mean(poly[,2]),labels=pop, col=i, font=2)
                  }
          }
}


plot.gate.cytogram.by.file <- function(file.name, para.x = 'fsc_small', para.y = 'chl_small'){

  opp <- get.opp.by.file(file.name)

  plot.gate.cytogram(opp,para.x = para.x, para.y = para.y)

}




plot.filter.cytogram <- function(evt, width=0.2, notch=1){


  notch <- as.numeric(notch)
  width <- as.numeric(width)
  slope <- 1

  # linearize the LOG transformed data 
  t <- FALSE
    id <- which(colnames(evt) == "pulse_width" | colnames(evt) == "time" | colnames(evt) =="pop")
    if(!any(max(evt[,-c(id)]) > 10^3.5)){
      evt[,-c(id)] <- (log10(evt[,-c(id)])/3.5)*2^16  
      t <- TRUE
         }

 # Filtering particles detected by fsc_small 
    evt. <- subset(evt, fsc_small > 1)

 # Filtering particles detected by D1 and D2 
    evt. <- subset(evt., D1 > 1 & D2 > 1)

  # Fltering particles not saturating D1 and D2 (both)
  D1D2.max <- max(evt[,c("D1","D2")])
  evt. <- subset(evt., D1 < D1D2.max & D2 < D1D2.max)
  
  # Correction for the difference of sensitivity between D1 and D2
      #remove potential electrical noise from calculation
  evt.origin  <- subset(evt., D2 > 5000 | D1 > 5000)
  origin <- median(evt.origin$D2)-median(evt.origin$D1)
      if(origin > 0)  evt.$D1 <-  evt.$D1 + origin
      if(origin < 0)  evt.$D2 <-   evt.$D2 - origin 
 
  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt., D2 < D1*slope + width * 10^4 & D1 < D2*slope + width * 10^4)

  # Filtering focused particles (D/fsc_small < notch)
 opp <- subset(aligned, D1/fsc_small < notch & D2/fsc_small < notch)    

  ################
  ### PLOTTING ###
  ################
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  percent.opp <- round(100*nrow(opp)/nrow(evt),2)
  
  origin1 <- origin + width*10^4
  origin2 <- origin - width*10^4
 
  if(nrow(evt) > 10000)  evt <- evt[round(seq(1,nrow(evt), length.out=10000)),]

  def.par <- par(no.readonly = TRUE) # save default, for resetting...

  par(mfrow=c(2,2),pty='s')                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
  plot.cytogram(evt, "D1", "D2")
    mtext("Alignment", side=3, line=1, font=2)
   # TODO[FRANCOIS] ADD LINE FOR CASE WHEN DATA UNTRANSFORM...
   abline(b=slope, a=origin1, col='red',lwd=2)
   abline(b=1/slope, a=origin2, col='red',lwd=2)
  mtext(paste("D2 - D1=", round(origin,2)),outer=T,side=3, line=-1.5,font=2)
    mtext(paste("Width=", width),outer=T,side=3, line=-3,font=2)
    mtext(paste("Notch=", notch),outer=T,side=3, line=-4,font=2)
    mtext(paste("OPP =", percent.opp,"% EVT"), outer=T,side=1, line=-1.5,font=2,col=2)

        aligned$D1.fsc_small <- aligned$D1/aligned$fsc_small
        aligned$D2.fsc_small <- aligned$D2/aligned$fsc_small
        aligned <- subset(aligned, D1.fsc_small<2 & D2.fsc_small<2)
        if(nrow(aligned) > 10000)  aligned <- aligned[round(seq(1,nrow(aligned), length.out=10000)),]
     
  plot(aligned[,c("D1.fsc_small", "D2.fsc_small")], pch=16, cex=0.3, col = densCols(aligned[,c("D1.fsc_small", "D2.fsc_small")], colramp = cols), xlim=c(0,2), ylim=c(0,2)) 
       mtext("Focus", side=3, line=1, font=2)
       abline(v=notch, h=notch, col=2,lwd=2)
     # abline(b=1, a=notch, col='red', lwd=2)
     # abline(b=1, a=-notch, col='red', lwd=2)
    
  plot.cytogram(opp, "fsc_small", "pe")
      mtext("OPP", side=3, line=1, font=2)
  plot.cytogram(opp, "fsc_small","chl_small")
      mtext("OPP", side=3, line=1, font=2)
 
  par(def.par)      

}



plot.filter.cytogram.by.file <- function(evt.location,file.name,width=0.2,notch=1, ...){

    evt.list <- get.evt.list(evt.location)
    id <- which(file.name == evt.list)
    evt.file <- evt.list[id]
    evt <- readSeaflow(evt.file)

  plot.filter.cytogram(evt, notch=notch, width=width)

}


plot.map <- function(stat,popname,param,...){
  ## plot cell abundances of a population on a map
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

plot.time <- function(stat, popname,param, ...){

  stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')
  pop <- subset(stat, pop == popname)
  plot(pop$time, pop[,param], xlab="Time", ylab=paste(param),main=paste(popname),...)

}


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


plot.cytdiv.time <- function(cytdiv,index, ...){

  cytdiv$time <- as.POSIXct(cytdiv$time,format="%FT%T",tz='GMT')
  plot(cytdiv$time, cytdiv[,index], xlab="Time", ylab=paste(index),...)

}

plot.TS <- function(sfl,...){
 
 require(plotrix, quietly=T)

  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  sfl$date <- as.POSIXct(sfl$date,format="%FT%T",tz='GMT')

plot(sfl$ocean_tmp, sfl$salinity, col=cols(100)[cut(sfl$date,100)],pch=16,xlab=expression(paste("Temp (",degree,"C)")), ylab="Salinity (psu)",...)
    ylim <- par('usr')[c(3,4)]
    xlim <- par('usr')[c(1,2)]
   color.legend(xlim[2], ylim[1], xlim[2] + 0.02*diff(xlim), ylim[2], 
      legend=c("start","end"), rect.col=cols(100), gradient='y',align='rb',...)
mtext("Time", side=4, line=2,...)  

  
}

#' Plot EVT or OPP cytogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for densCols()
#' @return None
#' @usage plot.cytogram(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...)
plot.cyt <- function(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...) {
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))

  par(pty='s')
  id <- which(colnames(evtopp) == "fsc_small" | colnames(evtopp) == "chl_small" | colnames(evtopp) =="pe" | colnames(evtopp) =="fsc_perp")
  if(max(evtopp[,c(id)]) > 10^3.5) plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = densCols(evtopp[,c(para.x, para.y)], colramp = cols), xlim=c(0,2^16), ylim=c(0,2^16), ...)
  else plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = densCols(log10(evtopp[,c(para.x, para.y)]), colramp = cols), log='xy',xlim=c(1,10^3.5), ylim=c(1,10^3.5), ...)
}


#' Plot EVT or OPP cytogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param transform Log transformation for both x- and y-axis'
#' @return None
#' @usage plot.cytogram(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...)
#' @export plot.cytogram
plot.cytogram <- function(evtopp, para.x = 'fsc_small', para.y = 'chl_small', transform=T) {
  require(viridis, quietly=T)

  cols <- colorRampPalette(viridis(256))

      x <- evtopp[,para.x]
      y <- evtopp[,para.y]

      if(any(names(evtopp) == 'file')){
        file <- evtopp[,'file']
        }else file <- NA

      if(transform){ df <- data.frame(x, y, d = densCols(log10(x), log10(y), colramp = cols), file)
      }else{ df <- data.frame(x, y, d = densCols(x, y, colramp = cols), file=file)}

      p <- ggplot(df) +
          geom_point(aes(x, y, col = d), size = 0.1) +
          scale_color_identity() +
          theme_bw()+
          coord_fixed() +
          labs(x=para.x, y=para.y)

      if(transform) p <- p + scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10')

      if(any(names(evtopp) == 'file'))   p <- p +  facet_wrap( ~ file)

  print(p)

}




#' Plot cytogram with particles colored by population.
#'
#' @param opp OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param transform Log transformation for both x- and y-axis'
#' @return None
#' @usage plot.vct.cytogram(opp, para.x = 'fsc_small', para.y = 'chl_small')
#' @export plot.vct.cytogram
plot.vct.cytogram <- function(opp, para.x = 'fsc_small', para.y = 'chl_small', transform=T) {

    group.colors <- c(unknown='grey', beads='red3', prochloro='skyblue3',synecho="orange",picoeuk="seagreen3", croco='darkorchid')

    x <- opp[,para.x]
    y <- opp[,para.y]

    if(any(names(opp) == 'pop')){ population <- opp[,'pop']
    }else{ print("no gating parameters yet")
            plot.cytogram(opp, para.x, para.y, transform=transform)
          }

      if(any(names(opp) == 'file')){
        file <- opp[,'file']
      }else file <- NA


    df <- data.frame(x, y, population, file)
    p <- ggplot(df) +
          geom_point(aes(x=x, y=y, colour=population), size=0.1, alpha=0.25) +
          theme_bw() +
          coord_fixed() +
          stat_density2d(aes(x, y, group=population), bins=5, geom = 'polygon', show.legend=F,fill='white') +
          stat_density2d(aes(x, y, group=population, fill = factor(population), alpha=log10(stat(level))), bins=5, geom = 'polygon', show.legend=F) +
          scale_fill_manual(values=group.colors) +
          scale_color_manual(values=group.colors) +
          labs(x=para.x, y=para.y) +
          guides(colour = guide_legend(override.aes = list(size=2, alpha=0.5)))

      if(transform) p <- p + scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10')

      if(any(names(opp) == 'file'))   p <- p +  facet_wrap( ~ file)


    print(p)

}


#' Plot cell abundances of a population on a map.
#'
#' @param stat Stat table from get.stat.table function
#' @param param Parameter to display
#' @param transform Log transformation of the parameter'
#' @return None
#' @usage plot.map(stat, param, ...)
#' @export plot.map
plot.map <- function(stat, param, transform=FALSE, ...){
  require(mapproj, quietly=T)
  require(viridis, quietly=T)
  cols <- colorRampPalette(viridis(256))

  df <- data.frame(x=stat$lon, y=stat$lat, z=stat[,param], pop=stat$pop)

  p <- ggplot(df) + geom_point(aes(x, y, color=z), size=1, alpha=0.5,show.legend=T) +
    borders("world", colour = "gray85", fill = "gray80") +
    labs(x="Longitude", y= "Latitude") +
    xlim(range(stat$lon, na.rm=T)) +
    ylim(range(stat$lat, na.rm=T)) +
    facet_wrap(. ~ pop) +
    theme_bw()

    if(transform) p <- p + scale_color_gradientn(colours=cols(100), trans='log10', name=paste(param))
    if(!transform) p <- p + scale_color_gradientn(colours=cols(100), name=paste(param))


    print(p)

}

#' plot.time
#'
#' @param stat Stat table from get.stat.table function
#' @param param Parameter to display
#' @param transform Log transformation of the parameter'
#' @return None
#' @usage plot.time(stat, popname,param, ...)
#' @export plot.time
plot.time <- function(stat, param, transform=FALSE, ...){

  group.colors <- c(unknown='grey', beads='red3', prochloro='skyblue3',synecho="orange",picoeuk="seagreen3", croco='darkorchid')
  df <- data.frame(time=as.POSIXct(stat$time,format="%FT%T",tz='GMT'), y=stat[,param], population = stat$pop)

  p <- ggplot(df) + geom_point(aes(time, y, color=population), size=1, alpha=0.25) +
  theme_bw() +
  labs(y=paste(param)) +
  scale_color_manual(values=group.colors) +
  facet_wrap( ~ population) +
  guides(colour = guide_legend(override.aes = list(size=2, alpha=0.5)))

  if(transform) p <- p + scale_y_continuous(trans='log10')

  print(p)

}



plot.histogram <- function()

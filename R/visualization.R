#' Plot EVT or OPP cytogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for densCols()
#' @return None
#' @usage plot.cytogram(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...)
plot.cyt <- function(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...) {

  cols <- colorRampPalette(viridis(256))

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
#' @param bins Number of bin (hex) to display.
#' @param transform Log transformation for both x- and y-axis'
#' @return None
#' @usage plot.cytogram(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...)
#' @export plot.cytogram
plot.cytogram <- function(evtopp, para.x = 'fsc_small', para.y = 'chl_small', bins=100, transform=T) {
      x <- evtopp[,para.x]
      y <- evtopp[,para.y]

  p <- evtopp %>%
          ggplot() +
          stat_bin_2d(aes(x, y), bins=bins, color=NA) +
          scale_fill_viridis() +
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

    if(!any(names(opp) == 'pop')) opp[,'pop'] <- 'unknown'

    p <- opp %>%
          ggplot(aes(x, y)) +
          stat_bin_2d(aes(fill = population, colour = NA, alpha=..count..,),bins=100, show.legend=F) +
          theme_bw() +
          coord_fixed() +
          stat_density2d(aes(x, y, color = factor(population), fill=NA), bins=5, geom = 'polygon', show.legend=F) +
          scale_fill_manual(values=group.colors) +
          scale_alpha_continuous(range=c(0.3,1)) +
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
plot.map <- function(stat, param, transform=FALSE){
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
#' @usage plot.time(stat, popname,param)
#' @export plot.time
plot.time <- function(stat, param, transform=FALSE){

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


#' Plot histogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param transform Log transformation of the parameter'
#' @param position Position adjustment, either as a string ("stack" or "identity"), or the result of a call to a position adjustment function.
#' @return None
#' @usage plot.histogram(opp, para.x='fsc_small', transform=T)
#' @export plot.histogram

plot.histogram <- function(evtopp, para.x = "fsc_small", transform=T, position='identity'){

group.colors <- c(unknown='grey', beads='red3', prochloro='skyblue3',synecho="orange",picoeuk="seagreen3", croco='darkorchid')

  if(!any(names(evtopp) == 'pop')) evtopp[,'pop'] <- 'unknown'

    x <- evtopp[,para.x]
    y <- evtopp[,para.y]

    p <- evtopp %>%
        ggplot() +
          geom_histogram(aes(x, fill=pop),binwidth=0.02, alpha=0.5, color=NA, position=position) +
          theme_bw() +
          scale_fill_manual(values=group.colors) +
          guides(fill=guide_legend(title="population")) +
          labs(x=para.x)

    if(transform) p <- p + scale_x_continuous(trans='log10')

    if(any(names(opp) == 'file')) p <- p + facet_wrap(~ file, scale='free_y')

    print(p)

  }

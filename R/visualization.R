#' Plot EVT or OPP cytogram with only builtin R graphics.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for densCols()
#' @return None
#' @usage plot.cyt(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...)
plot.cyt <- function(evtopp, para.x = 'fsc_small', para.y = 'chl_small', ...) {

  par(pty='s')
  id <- which(colnames(evtopp) == 'fsc_small' | colnames(evtopp) == 'chl_small' | colnames(evtopp) =='pe' | colnames(evtopp) =='fsc_perp')
  if(max(evtopp[,c(id)]) > 10^3.5) plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = grDevices::densCols(evtopp[,c(para.x, para.y)], colramp = viridis::viridis), xlim=c(0,2^16), ylim=c(0,2^16), ...)
  else plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = grDevices::densCols(log10(evtopp[,c(para.x, para.y)]), colramp = viridis::viridis), log='xy',xlim=c(1,10^3.5), ylim=c(1,10^3.5), ...)
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

  if(!any(names(evtopp) == 'file')) evtopp[,'file'] <- NA

  p <- evtopp %>%
          ggplot() +
          stat_bin_2d(aes_string(para.x, para.y), bins=bins, color=NA) +
          viridis::scale_fill_viridis() +
          theme_bw()+
          coord_fixed() +
          facet_wrap( ~ file)

      if(transform) p <- p + scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10')

  return(p)

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


    group.colors <- c(unknown='grey', beads='red3', prochloro=viridis::viridis(4)[1],synecho=viridis::viridis(4)[2],picoeuk=viridis::viridis(4)[3], croco=viridis::viridis(4)[4])

    if(!any(names(opp) == 'pop')) opp[,'pop'] <- 'unknown'
    if(!any(names(opp) == 'file')) opp[,'file'] <- ''

    p <- opp %>%
          ggplot() +
          stat_bin_2d(aes_string(para.x, para.y, fill = 'pop', alpha=quote(..count..)), colour = NA, bins=100, show.legend=T) +
          theme_bw() +
          coord_fixed() +
          stat_density_2d(aes_string(para.x, para.y, color = 'pop'), bins=5, show.legend=F) +
          scale_fill_manual(values=group.colors) +
          scale_alpha_continuous(range=c(0.3,1)) +
          scale_color_manual(values=group.colors) +
          guides(color='none', alpha='none', fill= guide_legend(override.aes = list(size=2, alpha=0.5),title='population')) +
          facet_wrap(~ file)

      if(transform) p <- p + scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10')

      return(p)

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

  cols <- viridis::viridis(256)

  p <- stat %>%
      ggplot() + geom_point(aes_string('lon', 'lat', color=param), size=1, alpha=0.5,show.legend=T) +
      borders('world', colour = 'gray85', fill = 'gray80') +
      labs(x='Longitude', y= 'Latitude') +
      xlim(range(stat[,'lon'], na.rm=T)) +
      ylim(range(stat[,'lat'], na.rm=T)) +
      facet_wrap(~ pop) +
      theme_bw()

  if(transform) p <- p + scale_color_gradientn(colours=cols(100), trans='log10', name=paste(param))
  if(!transform) p <- p + scale_color_gradientn(colours=cols(100), name=paste(param))


  return(p)

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

  group.colors <- c(unknown='grey', beads='red3', prochloro=viridis::viridis(4)[1],synecho=viridis::viridis(4)[2],picoeuk=viridis::viridis(4)[3], croco=viridis::viridis(4)[4])

  stat <- stat[,c('time',param,'quantile','pop')]
  stat$time <- as.POSIXct(stat$time,format='%FT%T',tz='GMT')
  stat2 <- spread(data=stat, key=quantile, value=param)
  names(stat2) <- c('time', 'pop','upr','mid','lwr')

  p <- stat2 %>%
      ggplot() + geom_linerange(aes(x=time,ymin=lwr, ymax=upr), color='lightgrey') +
      geom_point(aes(x=time,y=mid, fill=pop), pch=21, size=3, alpha=0.25, show.legend=F) +
      theme_bw() +
      labs(y=param) +
      scale_fill_manual(values=group.colors) +
      facet_grid( pop ~ ., scale='free_y')

  if(transform) p <- p + scale_y_continuous(trans='log10')

  return(p)

}


#' Plot histogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param transform Log transformation of the parameter'
#' @param position Position adjustment, either as a string ('stack' or 'identity'), or the result of a call to a position adjustment function.
#' @param free Should the y-scale be free (TRUE) or fixed (FIXED)
#' @return None
#' @usage plot.histogram(opp, para.x='fsc_small', transform=T)
#' @export plot.histogram

plot.histogram <- function(evtopp, para.x = 'fsc_small', transform=T, position='identity', free=T){

  group.colors <- c(unknown='grey', beads='red3', prochloro=viridis::viridis(4)[1],synecho=viridis::viridis(4)[2],picoeuk=viridis::viridis(4)[3], croco=viridis::viridis(4)[4])

  if(!any(names(evtopp) == 'pop')) evtopp[,'pop'] <- 'unknown'
  if(!any(names(evtopp) == 'pop')) evtopp[,'pop'] <- 'unknown'
  if(!any(names(evtopp) == 'file')) evtopp[,'file'] <- NA

    p <- evtopp %>%
        ggplot() + geom_histogram(aes_string(para.x, fill='pop'),binwidth=0.02, alpha=0.5, color=NA, position=position) +
        theme_bw() +
        scale_fill_manual(values=group.colors) +
        guides(fill=guide_legend(title='population'))

        if(free){p <- p + facet_wrap(~ file, scale="free_y")
        }else{ p <- p + facet_wrap(~ file)}

        if(transform) p <- p + scale_x_continuous(trans='log10')

        return(p)

  }

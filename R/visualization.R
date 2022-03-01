#' Plot EVT or OPP cytogram with only builtin R graphics.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param ... Additional parameters for plot()
#' @return None
#' @usage plot_cyt(evtopp, para.x = "fsc_small", para.y = "chl_small")
#' @export plot_cyt

plot_cyt <- function(evtopp, para.x = "fsc_small", para.y = "chl_small", ...) {

  par(pty="s")
  if(max(evtopp[,c(para.x, para.y)]) > 10^3.5) plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = grDevices::densCols(evtopp[,c(para.x, para.y)], colramp = viridis::viridis), xlim=c(0,2^16), ylim=c(0,2^16), ...)
  else plot(evtopp[,c(para.x, para.y)], pch=16, cex=0.3, col = grDevices::densCols(log10(evtopp[,c(para.x, para.y)]), colramp = viridis::viridis), log="xy",xlim=c(1,10^3.5), ylim=c(1,10^3.5), ...)
}



#' Plot cytograms for exploring filtering parameters.
#'
#' @param evt EVT data frame.
#' @param filter_params Filtering parameters in a one row data frame or named
#'   list. Columns should include width, notch.small.D1, notch.small.D2,
#'   notch.large.D1, notch.large.D2, offset.small.D1, offset.small.D2,
#'   offset.large.D1, offset.large.D2.
#' @return None
#' @usage plot_filter_cytogram(evt, filter_params)
#' @export plot_filter_cytogram

plot_filter_cytogram <- function(evt, filter_params) {

  fp <- subset(filter_params, quantile == 50)


  # linearize the LOG transformed data
  columns <- unlist(lapply(evt, is.numeric)) 
  if (!any(max(evt[, columns]) > 10^3.5)) {
    evt <- untransformData(evt)
  }

  # Filtering noise
  evt. <- evt[evt$fsc_small > 1 | evt$D1 > 1 | evt$D2 > 1, ]

  # Fltering aligned particles (D1 = D2)
  aligned <- subset(evt., D2 < D1 + fp$width & D1 < D2 + fp$width)

  # Filtering focused particles (fsc_small > D * notch)
  opp <- filter_evt(evt, filter_params)

  ################
  ### PLOTTING ###
  ################
  percent.opp <- round(100*nrow(opp)/nrow(evt.),2)
  percent.noise <- round(100-100*nrow(evt.)/nrow(evt),0)

  if(nrow(evt.) > 10000)  evt. <- evt.[round(seq(1,nrow(evt.), length.out=10000)),]
  if(nrow(aligned) > 10000)  aligned <- aligned[round(seq(1,nrow(aligned), length.out=10000)),]

  def.par <- par(no.readonly = TRUE) # save default, for resetting.

  par(mfrow=c(2,3),pty="s")

  plot_cyt(evt., "D1", "D2")
  mtext("Alignment", side=3, line=3, font=2, col=2)
  abline(b=1, a=fp$width, col="red",lwd=2)
  abline(b=1, a=-fp$width, col="red",lwd=2)
  mtext(paste0("Noise = ", percent.noise, "%" ), side=3, line=1,font=2)

  plot_cyt(aligned, "fsc_small", "D1")
  mtext("Focus", side=3, line=3, font=2,col=2)
  abline(b=fp$notch.small.D1, a=fp$offset.small.D1,col=2)
  abline(b=fp$notch.large.D1, a=fp$offset.large.D1,col=3)
  points(fp$beads.fsc.small,fp$beads.D1, cex=2, pch=16)

  plot_cyt(aligned, "fsc_small", "D2")
  mtext("Focus", side=3, line=3, font=2,col=2)
  abline(b=fp$notch.small.D2, a=fp$offset.small.D2,col=2)
  abline(b=fp$notch.large.D2, a=fp$offset.large.D2,col=3)
  points(fp$beads.fsc.small,fp$beads.D2, cex=2, pch=16)

  plot_cyt(opp, "fsc_small", "pe"); abline(v=fp$beads.fsc.small, lty=2, col="grey")
  mtext("OPP", side=3, line=1, font=2, col=2)
  plot_cyt(opp, "fsc_small","chl_small"); abline(v=fp$beads.fsc.small, lty=2, col="grey")
  mtext("OPP", side=3, line=1, font=2, col=2)
  plot_cyt(opp, "chl_small","pe")
  mtext("OPP", side=3, line=1, font=2, col=2)
  mtext(paste("OPP =", percent.opp,"% EVT"), outer=T,side=1, line=-2,font=2,col=1)

  par(def.par)
}


#' Plot EVT or OPP cytogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param bins Number of bin (hex) to display.
#' @param transform Log transformation for both x- and y-axis"
#' @param xlim limits for x-axis.
#' @param ylim limits for y-axis.
#' @return None
#' @usage plot_cytogram(evtopp, para.x = "fsc_small", para.y = "chl_small")
#' @export plot_cytogram
plot_cytogram <- function(evtopp, para.x = "fsc_small", para.y = "chl_small", bins=100, transform=T, xlim=NULL, ylim=NULL) {

  if (!any(names(evtopp) == "file")) {
    # Try to use file_id, otherwise set to ""
    if (any(names(evtopp) == "file_id")) {
      evtopp[, "file"] <- evtopp[, "file_id"]
    } else {
      evtopp[, "file"] <- ""
    }
  }

  p <- evtopp %>%
        ggplot2::ggplot() +
        ggplot2::stat_bin_2d(ggplot2::aes_string(para.x, para.y), bins=bins, color=NA) +
        viridis::scale_fill_viridis() +
        ggplot2::theme_bw()+
        ggplot2::facet_wrap( ~ file)

      if(transform){ p <- p + ggplot2::scale_y_continuous(trans="log10", limits=xlim) + ggplot2::scale_x_continuous(trans="log10", limits=ylim)
      }else{ p <- p + ggplot2::scale_y_continuous(limits=xlim) + ggplot2::scale_x_continuous(limits=ylim)}

  return(p)

}




#' Plot cytogram with particles colored by population.
#'
#' @param opp OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param transform Log transformation for both x- and y-axis"
#' @param xlim limits for x-axis.
#' @param ylim limits for y-axis.
#' @return None
#' @usage plot_vct_cytogram(opp, para.x = "fsc_small", para.y = "chl_small")
#' @export plot_vct_cytogram
plot_vct_cytogram <- function(opp, para.x = "fsc_small", para.y = "chl_small", transform=T, xlim=NULL, ylim=NULL) {

  group.colors <- c(unknown="grey", beads="red3", prochloro=viridis::viridis(4)[1],synecho=viridis::viridis(4)[2],picoeuk=viridis::viridis(4)[3], croco=viridis::viridis(4)[4])

  if(!any(names(opp) == "pop")) opp[,"pop"] <- "unknown"
  if (!any(names(opp) == "file")) {
    # Try to use file_id, otherwise set to ""
    if (any(names(opp) == "file_id")) {
      opp[, "file"] <- opp[, "file_id"]
    } else {
      opp[, "file"] <- ""
    }
  }
  opp$pop <- factor(opp$pop, levels = names(group.colors))

  p <- opp %>%
        ggplot2::ggplot() +
        ggplot2::stat_bin_2d(ggplot2::aes_string(para.x, para.y, fill = "pop", alpha=quote(..count..)), colour = NA, bins=100, show.legend=T) +
        ggplot2::theme_bw() +
        #ggplot2::stat_density_2d(ggplot2::aes_string(para.x, para.y, color = "pop"), bins=5, show.legend=F) +
        ggplot2::scale_fill_manual(values=group.colors) +
        ggplot2::scale_alpha_continuous(range=c(0.3,1)) +
        ggplot2::scale_color_manual(values=group.colors) +
        ggplot2::guides(color="none", alpha="none", fill= ggplot2::guide_legend(override.aes = list(size=2, alpha=0.5),title="population")) +
        ggplot2::facet_wrap(~ file)

        if(transform){ p <- p + ggplot2::scale_y_continuous(trans="log10", limits=xlim) + ggplot2::scale_x_continuous(trans="log10", limits=ylim)
        }else{ p <- p + ggplot2::scale_y_continuous(limits=xlim) + ggplot2::scale_x_continuous(limits=ylim)}

 return(p)

}


#' Plot population distribution on a map.
#'
#' @param stat Stat table from get_stat_table function
#' @param param Parameter to display
#' @param transform Log transformation of the parameter"
#' @return None
#' @usage plot_map(stat, param)
#' @export plot_map
plot_map <- function(stat, param, transform=FALSE){

  p <- stat %>%
        ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes_string("lon", "lat", color=param), size=1, alpha=0.5,show.legend=T) +
        ggplot2::borders("world", colour = "black", fill = "gray80") +
        ggplot2::labs(x="Longitude", y= "Latitude") +
        ggplot2::coord_fixed(ratio = 1, xlim = range(stat[,"lon"], na.rm=T), ylim = range(stat[,"lat"], na.rm=T)) +
        ggplot2::facet_wrap(~ pop) +
        ggplot2::theme_bw()

  if(transform) p <- p + ggplot2::scale_color_gradientn(colours=viridis::viridis(100), trans="log10", name=paste(param))
  if(!transform) p <- p + ggplot2::scale_color_gradientn(colours=viridis::viridis(100), name=paste(param))


  return(p)

}


#' Plot cruise track on a map.
#'
#' @param stat a dataframe that contains columns "lat" and "lon"
#' @param param Parameter to display
#' @return None
#' @usage plot_cruisetrack(stat, param)
#' @export plot_cruisetrack
plot_cruisetrack <- function(stat, param){

  geo <- list(
    showland = TRUE,
    showlakes = TRUE,
    showcountries = TRUE,
    showocean = TRUE,
    countrywidth = 0.5,
    landcolor = plotly::toRGB("grey90"),
    lakecolor = plotly::toRGB("white"),
    oceancolor = plotly::toRGB("white"),
    projection = list(
      type = "orthographic",
      rotation = list(
        lon = -100,
        lat = 40,
        roll = 0
      )
    ),
    lonaxis = list(
      showgrid = TRUE,
      gridcolor = plotly::toRGB("gray40"),
      gridwidth = 0.5
    ),
    lataxis = list(
      showgrid = TRUE,
      gridcolor = plotly::toRGB("gray40"),
      gridwidth = 0.5
    )
  )

  p <- plotly::plot_geo(stat, lat = ~lat, lon = ~lon, color = stat[,param], colors = viridis::viridis_pal(option = "D")(100), alpha=0.5) %>%
        plotly::colorbar(title = paste(param)) %>%
        plotly::layout(showlegend=T, geo = geo)

  return(p)
}




#' plot population dynamics over time
#'
#' @param stat Stat table from get_stat_table function
#' @param param Parameter to display
#' @param transform Log transformation of the parameter"
#' @return None
#' @usage plot_time(stat, popname,param)
#' @export plot_time
plot_time <- function(stat, param, transform=FALSE){

  group.colors <- c(unknown="grey", beads="red3", prochloro=viridis::viridis(4)[1],synecho=viridis::viridis(4)[2],picoeuk=viridis::viridis(4)[3], croco=viridis::viridis(4)[4])

  stat <- stat[,c("time","par",param,"quantile","pop")]
  stat$time <- as.POSIXct(stat$time,format="%FT%T",tz="GMT")
  stat2 <- tidyr::spread(data=stat, key=quantile, value=param)
  names(stat2) <- c("time", "par", "pop", "upr", "mid", "lwr")
  stat2$pop <- factor(stat2$pop, levels = names(group.colors))

  p <- stat2 %>%
        ggplot2::ggplot() +
        #ggplot2::geom_vline(xintercept=as.POSIXct(ifelse(stat2$par != min(stat2$par,na.rm=T), stat2$time, 0),origin="1970-01-01",tz="GMT"), col="lightgrey",alpha=0.01, lwd=2) +  # only works for curated PAR
        ggplot2::geom_linerange(ggplot2::aes(x=time,ymin=lwr, ymax=upr), color="grey") +
        ggplot2::geom_point(ggplot2::aes(x=time,y=mid, fill=pop), pch=21, size=3, alpha=0.25, show.legend=F) +
        ggplot2::theme_bw() +
        ggplot2::labs(y=param) +
        ggplot2::scale_fill_manual(values=group.colors) +
        ggplot2::facet_grid( pop ~ ., scale="free_y")

  if(transform) p <- p + ggplot2::scale_y_continuous(trans="log10")

  return(p)

}


#' Plot histogram.
#'
#' @param evtopp EVT or OPP data frame.
#' @param para.x Channel to use as x axis.
#' @param binwidth The width of the bins. Can be specified as a numeric value, or a function that calculates width from x. T
#' The default is to use bins bins that cover the range of the data. You should always override this value, exploring multiple widths to find the best to illustrate the stories in your data.
#' @param transform Log transformation of the parameter"
#' @param position Position adjustment, either as a string ("stack" or "identity"), or the result of a call to a position adjustment function.
#' @param xlim limits for x-axis.
#' @return None
#' @usage plot_histogram(opp, para.x="fsc_small", transform=T)
#' @export plot_histogram

plot_histogram <- function(evtopp, para.x = "fsc_small", binwidth=0.02, transform=TRUE, position="identity", xlim=NULL){

  group.colors <- c(unknown="grey", beads="red3", prochloro=viridis::viridis(4)[1],synecho=viridis::viridis(4)[2],picoeuk=viridis::viridis(4)[3], croco=viridis::viridis(4)[4])

  if(!any(names(evtopp) == "pop")) evtopp[,"pop"] <- "unknown"
  if (!any(names(evtopp) == "file")) {
    # Try to use file_id, otherwise set to NA
    if (any(names(evtopp) == "file_id")) {
      evtopp[, "file"] <- evtopp[, "file_id"]
    } else {
      evtopp[, "file"] <- NA
    }
  }
  evtopp$pop <- factor(evtopp$pop, levels = names(group.colors))

  p <- evtopp %>%
      ggplot2::ggplot() + ggplot2::geom_histogram(ggplot2::aes_string(para.x, fill="pop"),binwidth=binwidth, alpha=0.5, color=NA, position=position) +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_manual(values=group.colors) +
      ggplot2::guides(fill=ggplot2::guide_legend(title="population"))

  if(transform) p <- p + ggplot2::scale_x_continuous(trans="log10", limit= xlim)
  if(!transform) p <- p + ggplot2::scale_x_continuous(limit= xlim)

  return(p)

}


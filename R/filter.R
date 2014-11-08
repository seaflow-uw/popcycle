# set width and notch, log old parameters if they exist
setFilterParams <- function(width, notch) {
  params <- data.frame(width = width, notch = notch)
  
  #log
  time <- format(Sys.time(),format="%FT%H:%M:%S+00:00", tz="GMT")
  log.file <- paste(log.filter.location, 'filter.csv', sep='/')
  
  if (file.exists(log.file)) {  
    write.table(data.frame(time=time, width=width, notch=notch), log.file, 
                row.names = F, col.names = F, append = T, quote = F, sep=',')  
  } else {
    write.table(data.frame(time=time, width=width, notch=notch), log.file,
                row.names = F, col.names = T, quote=F, sep=',')
  }
  
  #write params
  write.table(params, file = paste(param.filter.location, 'filter.csv', sep='/'), sep = ",",
              quote=F, row.names=F)
}

filter.notch <- function(evt, width, notch) {

 
  notch <- as.numeric(notch)
  width <- as.numeric(width)
  slope <- 1

  # Check for empty evt data frame.  If empty return empty opp data frame.
  if (nrow(evt) == 0) {
    return(data.frame(c()))
  }
  
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
    evt.origin  <- subset(evt., D2 > 5000 | D1 > 5000)
    origin <- median(evt.origin$D2)-median(evt.origin$D1)
      if(origin > 0)  evt.$D1 <-  evt.$D1 + origin
      if(origin < 0)  evt.$D2 <-   evt.$D2 - origin 
 
  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt., D2 < D1*slope + width * 10^4 & D1 < D2*slope + width * 10^4)

  # Filtering focused particles (D/fsc_small < notch)
 opp <- subset(aligned, D1/fsc_small < notch & D2/fsc_small < notch)
    
 # Back to original D1 and D2 values
    if(origin > 0)  opp$D1 <-  opp$D1 - origin
    if(origin < 0)  opp$D2 <-   opp$D2 + origin 
 
  if(t & nrow(opp) > 0){
    opp[,-c(id)] <- 10^((opp[,-c(id)]/2^16)*3.5)
  }

  return(opp)
}

best.filter.notch <- function(evt, notch=seq(0.5, 1.5, by=0.1),width=0.1, do.plot=TRUE){

  DF <- NULL
  for(n in notch){
    print(paste("filtering notch=",n))
    opp <- filter.notch(evt, notch=n, width=width)
    if (nrow(opp) == 0) {
      fsc.max = 0
    } else {
      fsc.max <- max(opp[,"fsc_small"])
    }
    id <- length(which(opp[,"fsc_small"] >= fsc.max))
    para <- data.frame(cbind(notch=n, fsc.max=round(fsc.max), id, original=nrow(evt), passed=nrow(opp)))
    DF <- rbind(DF, para)
  }
  print(DF)

  # subset DF to only rows with globally maximum fsc.max value
  max.row.indexes <- which(DF$fsc.max == max(DF$fsc.max))
  DF.max <- DF[max.row.indexes, ]

  # get row index for smallest notch which has the most "saturated" fsc_small
  # measurements, where saturated means equal to globally maximum fsc_small
  best.notch.id <- min(which(DF.max$id == max(DF.max$id)))
  best.notch <- DF.max$notch[best.notch.id]

  if(do.plot){
    def.par <- par(no.readonly = TRUE) # save default, for resetting...

    par(mfrow=c(2,1),oma=c(2,2,2,4), cex=1)
    par(pty='m')
    plot(DF[,c('notch', 'fsc.max')], main=paste("Best notch=",best.notch))
    abline(v=best.notch, col=3, lwd=3)
    par(new=TRUE)
    plot(DF[,'notch'], DF[,'id'], pch=3, xaxt='n',yaxt='n',xlab=NA,ylab=NA, col=2, ylim=c(0, max(DF[,'id'])))
    axis(4)
    mtext("count", 4, line=3)
    legend('topleft',legend=c('max(fsc_small)','count'), pch=c(1,3), col=c(1,2), bty='n')
    opp <- filter.notch(evt, notch=best.notch, width=width)
    plot.cytogram(opp,"fsc_small","chl_small"); mtext(paste("OPP with notch=",best.notch),3,line=1)
    par(def.par)    
  }

  return(best.notch)
}

# Filter a list of EVT files in parallel and upload OPP data to multiple sqlite
# dbs, then merge data into main db.
#
# Args:
#   evt.list = list of EVT file paths, e.g. get.evt.list(evt.location)
#   notch = notch size for filtering
#   width = width size for filtering
#   cruise = cruise name [cruise.id]
#   db = sqlite3 db path [db.name]
#   evt.loc = location of evt files listed in evt.list [evt.location]
#   cores = number of cpu cores to use when filtering EVT files.  If > 1, the
#           R package snow will be used to process EVT files in parallel.
#           If == 1, this function uses the current process to filter. 
#           Must be > 0. [1]
#
# Returns:
#   Return list of EVT files which produced no OPP data.
filter.evt.files.parallel <- function(evt.list, notch, width, cruise=cruise.id,
                                      db=db.name, evt.loc=evt.location,
                                      cores=1) {
  if (cores == 1) {
    # Just iterate over files and filter one by one
    filter.evt.files.serial(evt.list, notch, width, cruise=cruise, db=db,
                            evt.loc=evt.loc, check=TRUE)
  } else {
    # Snow parallel filtering to use multiple cores

    # Cleanup any lingering database parts
    reset.db(parts.only=T)

    # Prepare data lists and databases for child workers
    buckets <- make.buckets(evt.list, cores, db=db)
    dbs <- sapply(buckets, function(x) { x[["db"]] })
    sapply(dbs, make.sqlite.db)

    # Create snow cluster
    cl <- makeCluster(cores, type="SOCK")
    parallel.func <- function(b, notch, width, cruise, evt.loc) {
      filter.evt.files.serial(b[["files"]], notch, width, cruise=cruise, 
                              db=b[["db"]], evt.loc=evt.loc, check=FALSE)
    }

    # Run filtering in parallel
    clusterApply(cl, buckets, parallel.func, notch, width, cruise, evt.loc)
    stopCluster(cl)

    # Merge databases
    merge.dbs(dbs)

    # Return list of EVT files which produced no OPP data
    return(get.empty.evt.files(evt.list))
  }
}

# Filter a list of EVT files and upload OPP data to sqlite db.
# 
# Args:
#   evt.list = list of EVT file paths, e.g. get.evt.list(evt.location)
#   notch = notch size for filtering
#   width = width size for filtering
#   cruise = cruise name [cruise.id]
#   db = sqlite3 db path [db.name]
#   evt.loc = location of evt files listed in evt.list [evt.location]
#   check = if TRUE return a list of evt files that produced no OPP data,
#           else return NULL
#
# Returns:
#   If check is TRUE return list of EVT files which produced no OPP data.
filter.evt.files.serial <- function(evt.list, notch, width, cruise=cruise.id,
                                    db=db.name, evt.loc=evt.location,
                                    check=TRUE) {
  for (evt.file in evt.list) {
    # Read EVT file
    evt <- tryCatch({
      readSeaflow(paste(evt.loc, evt.file, sep='/'))
    }, warnings = function(war) {
      print(war)
    }, error = function(err) {
      # Return empty data frame on error
      return(data.frame(c()))
      print(err)
    })

    # Filter EVT to OPP
    opp <- tryCatch({
      filter.evt(evt, filter.notch, notch=notch, width=width)
    }, warnings = function(war) {
      print(war)
    }, error = function(err) {
      # Return empty data frame on error
      return(data.frame(c()))
      print(err)
    })

    # Upload OPP data
    .delete.opp.by.file(evt.file)
    if (nrow(opp) > 0) {
      upload.opp(opp.to.db.opp(opp, cruise, evt.file), db=db)
    }

    # Upload OPP/EVT particle count ratio
    .delete.opp.evt.ratio.by.file(evt.file)
    if (nrow(evt) > 0) {
      opp.evt.ratio <- nrow(opp) / nrow(evt)
      upload.opp.evt.ratio(opp.evt.ratio, cruise.id, evt.file, db=db)
    }
  }

  # Return list of EVT files which produced no OPP data
  if (check) {
    return(get.empty.evt.files(evt.list))
  }
}

# Create lists where each element contains information necessary to filter one
# part of EVT files.  Each element is a named list with a "db" item containing
# the path to a sqlite3 database file for that part, and a "files" item listing
# EVT files for that part.
#
# Args:
#   evt.list = list of EVT file paths, e.g. return value of get.evt.list(evt.location)
#   ways = number of ways to split the list of EVT files.  Must be > 0.
#   db = sqlite3 db path [db.name]
#
# Returns:
#   List of dbs and files for each part of a parallel EVT filtering analysis
make.buckets <- function(evt.list, ways, db=.db.name) {
  file.lists <- split.list(evt.list, ways)
  buckets <- list()
  i <- 1
  for (sublist in file.lists) {
    buckets[[i]] <- list("db"=paste(db, i, sep=""), "files"=sublist)
    i <- i + 1
  }
  return(buckets)
}

# Split a list into N sublists as evenly as possible
#
# Args:
#   some.list = list of things to split up
#   ways = number of ways to split some.list.  If ways > length(some.list) then
#          ways = length(some.list)
#
# Returns:
#   List of lists representing some.list evenly split N ways
split.list <- function(some.list, ways) {
  ways <- min(ways, length(some.list))
  bucket.size <- length(some.list) %/% ways
  bucket.rem <- length(some.list) %% ways
  buckets <- list()
  i <- 1
  bucket.i <- 1
  while (bucket.i < ways) {
    if (bucket.i <= bucket.rem) {
      buckets[[bucket.i]] <- some.list[i:(i+bucket.size)]
      i <- i + bucket.size + 1
    } else {
      buckets[[bucket.i]] <- some.list[i:(i+bucket.size-1)]
      i <- i + bucket.size
    }
    bucket.i <- bucket.i + 1
  }
  buckets[[bucket.i]] <- some.list[i:length(some.list)]
  return(buckets)
}


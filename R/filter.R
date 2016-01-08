filter.evt <- function(evt, filter.func, ...) {
  opp <- filter.func(evt, ...)
  
  # SANITY CHECKS
  # need same columns for opp
  if (!all(names(evt) == names(opp))) {
    stop('Filtering function produced OPP with different columns')
  }
  
  # filtered all particles out?
  if (dim(opp)[1] < 1) {
    stop('Filtering dropped all particles.')
  }

  return (opp)
}

# set width and notch, log old parameters if they exist
setFilterParams <- function(origin=NA, width=0.5, notch=c(NA, NA), offset=0) {
   #log
  if(length(notch) == !2) {
    stop('Notch should contains 2 values; filtering parameters not saved.')
  }

  time <- format(Sys.time(),format="%FT%H:%M:%S+00:00", tz="GMT")
   params <- data.frame(time=time, origin=origin, width = width, notch1 = notch[1], notch2=notch[2], offset=offset)
  
  log.file <- paste(log.filter.location, 'filter.csv', sep='/')
  
  if (file.exists(log.file)) {  
    write.table(params, log.file, row.names = F, col.names = F, append = T, quote = F, sep=',')  
  } else {
    write.table(params, log.file, row.names = F, col.names = T, quote=F, sep=',')
  }
  
  #write params
  write.table(params, file = paste(param.filter.location, 'filter.csv', sep='/'), sep = ",",
              quote=F, row.names=F)
}

filter.notch <- function(evt, origin=NA, width=0.5, notch=c(NA, NA), offset=0) {

  origin <- as.numeric(origin)
  width <- as.numeric(width)
  notch1 <- as.numeric(notch[1])
  notch2 <- as.numeric(notch[2])
  offset <- as.numeric(offset)

  # Check for empty evt data frame.  If empty return empty opp data frame.
  if (nrow(evt) == 0) {
    return(data.frame(c()))
  }
  
  # linearize the LOG transformed data 
  lin <- FALSE
  id <- which(colnames(evt) == "pulse_width" | colnames(evt) == "time" | colnames(evt) =="pop")
  if(!any(max(evt[,-c(id)]) > 10^3.5)){
    evt <- .untransformData(evt)
    lin <- TRUE
  }

# Correction for the difference of sensitivity between D1 and D2
    if(is.na(origin))  origin <- median(evt$D2-evt$D1)
  

  # Filtering particles detected by fsc_small 
    evt. <- subset(evt, fsc_small > 1)

  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
    aligned <- subset(evt., D2 < (D1+origin) + width * 10^4 & (D1+origin) < D2 + width * 10^4)

 # finding the notch
    if(is.na(notch1)){
      d.min1 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D1"]) 
      fsc.max1 <- max(aligned[which(aligned$D1 == d.min1),"fsc_small"]) 
      notch1 <- fsc.max1 / (d.min1+ 10000)
        }

    if(is.na(notch2)){
      d.min2 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D2"]) 
      fsc.max2 <- max(aligned[which(aligned$D2 == d.min2),"fsc_small"]) 
      notch2 <- fsc.max2 / (d.min2 + 10000)
        }

    # Filtering focused particles (fsc_small > D + notch) 
    opp <- subset(aligned, fsc_small > D1*notch1 - offset*10^4 & fsc_small > D2*notch2 - offset*10^4)
  
 # # Back to original D1 and D2 values
 #    if(origin > 0)  opp$D1 <-  opp$D1 - origin
 #    if(origin < 0)  opp$D2 <-   opp$D2 + origin 
 
  if(lin & nrow(opp) > 0){
    opp <- .transformData(opp)
  }

  return(opp)
}

# find.filter.notch <- function(evt, notch=seq(-0.5, 0.5, by=0.1),width=0.1, do.plot=TRUE){

#   DF <- NULL
#   for(n in notch){
#     print(paste("filtering notch=",n))
#     opp <- filter.notch(evt, notch=n, width=width)
#     if (nrow(opp) == 0) {
#       fsc.max = 0
#     } else {
#       fsc.min<- median(opp[,"fsc_small"])
#     }
#     id <- length(which(opp[,"fsc_small"] >= fsc.min))
#     para <- data.frame(cbind(notch=n, fsc.min=round(fsc.min), id, original=nrow(evt), passed=nrow(opp)))
#     DF <- rbind(DF, para)
#   }
#   print(DF)

#   # subset DF to only rows with globally maximum fsc.max value
#   max.row.indexes <- which(DF$fsc.min == max(DF$fsc.min))
#   DF.max <- DF[max.row.indexes, ]

#   # get row index for smallest notch which has the most "saturated" fsc_small
#   # measurements, where saturated means equal to globally maximum fsc_small
#   best.notch.id <- min(which(DF.max$id == max(DF.max$id)))
#   best.notch <- DF.max$notch[best.notch.id]

#   if(do.plot){
#     def.par <- par(no.readonly = TRUE) # save default, for resetting...

#     par(mfrow=c(2,1),oma=c(2,2,2,4), cex=1)
#     par(pty='m')
#     plot(DF[,c('notch', 'fsc.max')], main=paste("Best notch=",best.notch))
#     abline(v=best.notch, col=3, lwd=3)
#     par(new=TRUE)
#     plot(DF[,'notch'], DF[,'id'], pch=3, xaxt='n',yaxt='n',xlab=NA,ylab=NA, col=2, ylim=c(0, max(DF[,'id'])))
#     axis(4)
#     mtext("count", 4, line=3)
#     legend('bottomright',legend=c('max(fsc_small)','count'), pch=c(1,3), col=c(1,2), bty='n')
#     opp <- filter.notch(evt, notch=best.notch, width=width)
#     plot.cytogram(opp,"fsc_small","chl_small"); mtext(paste("OPP with notch=",best.notch),3,line=1)
#     par(def.par)    
#   }

#   return(best.notch)
# }

# Filter a list of EVT files  and upload OPP data to multiple sqlite
# dbs, then merge data into main db.
#
# Args:
#   evt.list: list of EVT file paths, e.g. get.evt.list(evt.location)
#   cruise: cruise name [cruise.id]
#   db: sqlite3 db path [db.name]
#   evt.loc: location of evt files listed in evt.list [evt.location]
#   param.loc: location of paramter files [param.filter.location]
#   cores: number of cpu cores to use when filtering EVT files. If > 1, the
#     R package snow will be used to process EVT files in parallel.
#     If == 1, this function uses the current process to filter. 
#     Must be > 0. [1]
#
# Returns:
#   Return list of EVT files which produced no OPP data.
filter.evt.files <- function(evt.list, cruise=cruise.id, db=db.name,
                             evt.loc=evt.location, param.loc=param.filter.location,
                             cores=1) {
  if (cores == 1) {
    # Just iterate over files and filter one by one
    return(.filter.evt.files.serial(evt.list, cruise=cruise, db=db, evt.loc=evt.loc,
                                    param.loc=param.loc, check=TRUE))
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
    parallel.func <- function(b, cruise, evt.loc, param.loc) {
      .filter.evt.files.serial(b[["files"]], cruise=cruise, db=b[["db"]],
                               evt.loc=evt.loc, param.loc=param.loc, check=FALSE)
    }

    # Run filtering in parallel
    # It's important to pass in evt.loc as an argument here because child
    # processes created by SNOW don't have access to these variables. i.e.
    # the function called here should be reentrant, except for reads from
    # the filesystem. Likewise, param.loc must be provided because
    # param.filter.location is not available to child processes.
    clusterApply(cl, buckets, parallel.func, cruise, evt.loc, param.loc)
    stopCluster(cl)

    # Merge databases
    merge.dbs(dbs)

    # Return list of EVT files which produced no OPP data
    return(get.empty.evt.files(evt.list))
  }
}


# Filter a list of EVT files in serial on one coreand upload OPP data to sqlite
# db.
# 
# Args:
#   evt.list: list of EVT file paths, e.g. get.evt.list(evt.location)
#   cruise: cruise name [cruise.id]
#   db: sqlite3 db path [db.name]
#   evt.loc: location of evt files listed in evt.list [evt.location]
#   check: if TRUE return a list of evt files that produced no OPP data,
#     else return NULL
#
# Returns:
#   If check is TRUE return list of EVT files which produced no OPP data.
.filter.evt.files.serial <- function(evt.list, cruise=cruise.id, db=db.name,
                                     evt.loc=evt.location,
                                     param.loc=param.filter.location,
                                     check=TRUE) {
  # Get notch and width to use from params file
  # Return empty data frame on warning or error
  params <- tryCatch({
    read.csv(paste(param.loc, 'filter.csv', sep='/'))
  }, warnings = function(err) {
    return(data.frame())
  }, error = function(err) {
    return(data.frame())
  })

    if (is.null(params$origin)) {
    stop('Origin not defined; skipping filtering.')
  }
    if (is.null(params$width)) {
    stop('Width not defined; skipping filtering.')
  }
  if (is.null(params$notch1)) {
    stop('Notch1 not defined; skipping filtering.')
  }
    if (is.null(params$notch2)) {
    stop('Notch2 not defined; skipping filtering.')
  }
 if (is.null(params$offset)) {
    stop('Offset not defined; skipping filtering.')
  }

  i <- 0
  for (evt.file in evt.list) {
    message(round(100*i/length(evt.list)), "% completed \r", appendLF=FALSE)

    evt.file.clean <- clean.file.name(evt.file)

    # Read EVT file
    # Return empty data frame on warning or error
    evt <- tryCatch({
      readSeaflow(evt.file, path=evt.loc)
    }, warnings = function(err) {
      return(data.frame())
    }, error = function(err) {
      return(data.frame())
    })

    # Filter EVT to OPP
    # Return empty data frame on warning or error
    opp <- tryCatch({
      filter.evt(evt, filter.notch, origin=params$origin, width=params$width,notch=c(params$notch1,params$notch2), offset=params$offset)
    }, warnings = function(err) {
      return(data.frame())
    }, error = function(err) {
      return(data.frame())
    })

    # Upload OPP data
    .delete.opp.by.file(evt.file.clean, db=db)
    if (nrow(opp) > 0) {
      upload.opp(opp.to.db.opp(opp, cruise, evt.file.clean), db=db)
    }

    # Upload OPP/EVT particle count ratio
    .delete.opp.evt.ratio.by.file(evt.file.clean, db=db)
    if (nrow(evt) > 0) {
      opp.evt.ratio <- nrow(opp) / nrow(evt)
      upload.opp.evt.ratio(opp.evt.ratio, cruise, evt.file.clean, db=db)
    }

    # Upload unknown VCT classifications
    .delete.vct.by.file(evt.file.clean, db=db)
    if (nrow(opp) > 0) {
      vct <- rep("unknown", nrow(opp))
      upload.vct(vct.to.db.vct(vct, cruise, evt.file.clean, 'None'), db)
    }

    i <-  i + 1
    flush.console()
  }

  # Return list of EVT files which produced no OPP data
  if (check) {
    return(get.empty.evt.files(evt.list))
  }
}

# Create lists where each element contains information necessary to filter one
# part of EVT files. Each element is a named list with a "db" item containing
# the path to a sqlite3 database file for that part, and a "files" item listing
# EVT files for that part.
#
# Args:
#   evt.list: list of EVT file paths, e.g. return value of get.evt.list(evt.location)
#   ways: number of ways to split the list of EVT files. Must be > 0.
#   db: sqlite3 db path [db.name]
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
#   some.list: list of things to split up
#   ways: number of ways to split some.list. If ways > length(some.list) then
#     ways = length(some.list)
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

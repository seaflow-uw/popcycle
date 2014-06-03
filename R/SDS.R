.tab2df <- function(x,...){

  is.tabrix <- class(x)[1] %in% c('table','matrix')
  row.nms <- rownames(x)
  
  if(NCOL(x)>1){
    clm.nms <- colnames(x)
    if(is.tabrix)
      x <- lapply(clm.nms, function(i) x[,i]) 
  }else{                                      
    if(is.tabrix){ # single column but matrix output
      clm.nms <- row.nms  # assume we want a 2 clmn dataframe
      row.nms <- NULL
    }else{   # vectors
      clm.nms <- NULL
      if(!is.null(names(x))) #named vector
        row.nms <- names(x) 
      else    # unnamed vector
        row.nms <- 1:length(x)
    }
  }
  
  if(!is.null(clm.nms)){
    x <- as.list(x)
    names(x) <- clm.nms
  }
  df <- data.frame(x,...)
  rownames(df) <- row.nms
  return(df)
  
}

.nv <- function(x, name){

  if(class(x)=='data.frame'){
    v <- x[,name[1]]
    if(length(name)==2)
      names(v) <- x[,name[2]]
    else
      names(v) <- rownames(x)
  }else{
    v <- as.vector(x)
    names(v) <- name
  }
  v
}
   
.getSDS <- function(file.path){
  ## grab one line of the SDS file based on the particular event file you are processing
  
  file.number <- getFileNumber(file.path) 
  
  dir.path <- dirname(file.path)
  sds.file <- list.files(dir.path, pattern='.txt')
  path.vect <- strsplit(dir.path ,'/')[[1]]
  dir.name <- path.vect[length(path.vect)]
  sds.name <- paste('sds_', dir.name,sep='')

  sds <- try(read.delim(paste(dir.path,"/",sds.file,sep="")))
  if(class(sds) != 'try-error'){
    line <- sds[sds$FILE == paste(sds.name, '_', file.number, sep=''),]
    ndf <- nrow(line)
    if(ndf < 1){
    	warning("file number found no matching sds records, returning null") 
    	return(NULL)
    }
    if(ndf > 1){
    	warning("file number returned more than one sds record, using the last one")
    	line <- line[ndf,]
    }
    return(line)
  }else{
    warning(paste("couldn't find sds file:", sds.file))
    return (NULL)
  }

}

.SDScompUTC2POSIX <- function(tm){

  l <- sapply(tm,nchar)
  p <- 13 - l
  prefix <- sapply(p, function(x) substr( '200', 1, x))
  tm <- paste(prefix,tm, sep='')
  
  timestamp <- as.POSIXct(tm, format='%Y%j%H%M%S', tz='UTC')
  return(timestamp)
}




.SDSll2LL <- function(lat, long){
  ## convert the lat and long entries in the sds file to an R friendly vector of decimal values
  ## example: lat <- '36 32.764 N'; long <- '003 52.237 W'
  
  lat.long <- sapply(c(lat,long), function(x) sub(' ','', x))
  lat.long <- sapply(strsplit(lat.long,' '), paste, collapse=',')
  return (.SDSpos2LL(paste(c('dummy',lat.long),collapse=',')))
 
}


.dms2decideg <- function(posv=.nv(c('3647.001','12154.04'),c('lat','long')), dirv=.nv(c('N','W'),c('ns','ew'))){

  ## find the decimal place -2 seperator & then split the string into degrees and seconds
  posv.div <- sapply(posv, function(x) gregexpr('\\.', x)[[1]]-2)
  posv.deg <- sapply(names(posv) , function(x) substr(posv[x],1,posv.div[x]-1))
  posv.min <- sapply(names(posv) , function(x) substr(posv[x],posv.div[x],nchar(posv[x])))
  ## add the minutes to the degrees
  posv.out <- as.numeric(posv.deg) + as.numeric(posv.min)/60
  
  ## convert West and South into negative numbers
  posv.neg <- ( dirv == 'S' | dirv == 'W')
  ## apply this directionality 
  posv.out <- c(1, -1)[posv.neg+1] * posv.out

  return(posv.out)
}

.SDSpos2LL <- function(position){
  ## convert the lat/long position as printed in the SDS position field into R ready lat/long
  ## example : position <- "230530.000,4727.3916,N,12224.5073,W"
  ## THIS CODE WILL BE DEPRICATED AT SOME POINT AS WE STANDARDIZE AROUND THE SHIP DATA
  
  posv.list <- strsplit(as.character(position), ',')
  posv <- posv.list[[1]]
  	
 if(length(posv) == 5){
      
  	        names(posv) <- c('dummy','lat','ns','long','ew')
      
  	        posv.out <- .dms2decideg(posv=posv[c('lat','long')], dirv=posv[c('ns','ew')])
      
  	        return(posv.out)
    
  	        }
    
  	  if(length(posv) == 4){
  	  
    	names(posv) <- c('lat','ns','long','ew')
  	        posv.out <- .dms2decideg(posv=posv[c('lat','long')], dirv=posv[c('ns','ew')])
     
  	        return(posv.out)
    
  	        }	
    if(length(posv) > 5 | length(posv) < 4){
      warning(paste("position", position, "doesn't have the expected number elements. \n Exiting .SDSpos2LL and returning NA."))
      return(c(NA,NA))
    }
}
 
.getSDSlatlon <- function(sds.line){
  if(is.null(sds.line)){
    return(NULL)
  }else{
    if(is.na(sds.line$LAT) | is.na(sds.line$LON)){
      lat.lon <- .SDSpos2LL(as.character(sds.line$position)) # this method is going away (just for original Thompson data)
    }else{
      lat <- sds.line$LAT
      lon <- sds.line$LON
      if(class(lat)== 'numeric')
        lat.lon <- as.numeric(c(lat,lon)) ## newer Thompson
      else
        lat.lon <- .SDSll2LL(as.character(lat), as.character(lon)) ##  Tara
    }
    return(lat.lon)
  }
}


.makeSDSlatlonDF <- function(sds){

  lat.lon <- t(sapply(1:nrow(sds), function(x) .getSDSlatlon(sds[x,])))
  colnames(lat.lon)<- c('lat','long')
  lat.lon.df <- .tab2df(lat.lon)
  lat.lon.df <- subset(lat.lon.df , !apply(lat.lon.df,1,function(x) any(is.na(x))) )
  return(lat.lon.df)
}

combineSdsFiles <- function(cruise.dir='.'){
  SDSs <- NULL
  for(this.path in getCruiseFiles(cruise.dir, prefix='sds_', ext='\\.txt')){
      sds.tab <- read.delim(this.path, as.is=TRUE)
      SDSs <- rbind.data.frame(SDSs, sds.tab)
  }
  return(SDSs)
}

.cleanSDS <- function(sds){

  lat.lons <- t(unlist(sapply(1:nrow(sds), function(x)  .getSDSlatlon(sds[x,]))))
  sds$LAT <- round(lat.lons[,1],6)
  sds$LON <- round(lat.lons[,2],6)
  ## do some field conversions
  	if(length(which(colnames(sds) =="computerUTC")) >0){
  		sds$time <- .SDScompUTC2POSIX(sds$computerUTC)
  				}
  sds$time[is.na(sds$time)] <- as.POSIXct(paste(sds$DMY, sds$HMS),format='%m/%d/%Y %H:%M:%S', tz='UTC')[is.na(sds$time)]
  	if(length(which(colnames(sds) =="FILE")) >0){
  		sds$day <- sapply(as.character(sds$FILE), .getYearDay)
  		sds$file <- sapply(as.character(sds$FILE), getFileNumber)				}
  sds$computerUTC <- sds$FILE <- NULL
  sds$SALINITY[sds$SALINITY==''] <- NA
  sds$OCEAN.TEMP[sds$OCEAN.TEMP==''] <- NA
  sds$FLOW.METER[sds$FLOW.METER == 'nometer'] <- -1
  # convert Inf values to -1 ...  probably need more of these
  sds$EVENT.RATE[is.infinite(sds$EVENT.RATE)] <- -1

  sds <- subset(sds, !is.na(time))
  return(sds)
}


.cleanbineSdsFiles <- function(cruise.dir='.'){
  ## useful for merging (and cleaning) multiple sds files within a directory 
  # find . -regex ".*sds_2010_2[5-6][0-9][^a-z]txt" -exec rm {} \;
  files <- getCruiseFiles(cruise.dir, prefix='sds_', ext='\\.txt')
  sapply(files[grep('[0-9]\\.txt',files)], function(f) file.rename(f, sub('.txt','a.txt',f)))
  files <- getCruiseFiles(cruise.dir, prefix='sds_', ext='\\.txt')

  header.list <- sapply(files, function(s) system(paste('head -n 1', s), intern=T))
  if(all(header.list!= header.list[1]))
     stop("headers don't match")
  else
     headers <- strsplit(header.list[1],'\t')[[1]]
  headers[length(headers)] <- sub('\r','',headers[length(headers)])

  readdelim <- function(pp){
    tmp <- read.delim(as.character(pp), stringsAsFactors=F)
    ##tmp <- subset(tmp, !is.na(FILE) & computerUTC  != 'E')
    ##tpp <- sapply(tmp[,c('LAT','LON')], as.character)
    ##mtp <- t(sapply(1:nrow(tpp),function(x) .dms2decideg(.nv(tpp[x,] ,c('lat','long')))))
    ##tmp$LAT <- mtp[,1]
    ##tmp$LON <- mtp[,2]
    tmp

  }  
  file.groups <- by(files, sapply(files, .getYearDay), function(p) do.call(rbind.data.frame, lapply(p, function(pp) readdelim(pp))))

  dummy <- lapply(names(file.groups), function(n) write.delim(df=file.groups[[n]], file=paste(cruise.dir,'/',n,'/sds_',n,'.txt', sep='')))


  
}

joinSDS <- function(x, opp.paths){

  if(is.null(names(opp.paths)))
    names(opp.paths) <- .nv(opp.paths, unique(x$file))
  sds.line <- .getSDS(opp.paths[as.character(x$file[1])])  #depends on .getNextFiles naming its output vector
  if(is.null(sds.line)){  # null SDS lines due to the ship's data stream going off line
    lat.lon <- c(NA,NA) 
    utc.time <- as.POSIXct(NA)
  }else{
    lat.lon <- .getSDSlatlon(sds.line)
    utc.time <- .SDScompUTC2POSIX(sds.line$computerUTC)
  }
  if(is.numeric(sds.line$STREAM.PRESSURE)){  # null Stream pressure on old runs
    psi <- sds.line$STREAM.PRESSURE
    flow.rate <- .streampressure2flowrate(psi)*1000 # ul/min
  }else{
    psi <- flow.rate <- NA
  }
  if(is.null(sds.line$CHL)){
  	 if(is.numeric(sds.line$NULL.)){
  	 	fluorescence <- sds.line$NULL.
  	 	}else{
  	 		fluorescence <- NA
  	 		print("No fluorescence data found")}
   }else{
   	fluorescence <- sds.line$CHL
   	}
  
  data.frame(x,
                    lat       = rep(lat.lon[1], nrow(x)),
                    long      = rep(lat.lon[2], nrow(x)),
                    time.UTC  = rep(utc.time, nrow(x)),
                    flow.rate = rep(flow.rate, nrow(x)),
                    bulk.red = rep(sds.line$BULK.RED,  nrow(x)),
                    salinity = rep(sds.line$SALINITY,  nrow(x)),
                    temperature = rep(sds.line$OCEAN.TEMP,  nrow(x)),
                    event.rate = rep(sds.line$EVENT.RATE,  nrow(x)),
		    		fluorescence = rep(fluorescence,  nrow(x))  
                    )
}



.interpolateSDSLatLon <- function(path){

  sds <- read.delim(path)
  sds$LAT[sds$LAT==-999999.0] <- NA
  sds$LON[sds$LON==-999999.0] <- NA
  
  require(zoo, quietly=T)
  sds$LAT <- na.approx(sds$LAT)
  sds$LON <- na.approx(sds$LON)
  
  write.delim(sds, path)
  
}

## returns flow rate of volume analyzed by SeaFlow (in  ml per min)
.streampressure2flowrate <- function(x, ratio.evt.stream = 0.14756){
    # ratio.evt.stream is the ratio of the stream volume to detectable volume by the instrument (calculated as the ratio of a known concentration of beads to concentration of EVT from SeaFlow)
    # Conversion from PSI to flow rate (ml/min)
  return( (-9*10^-5 * x^4 + 0.0066 * x^3 - 0.173 * x^2 + 2.5013 * x + 2.1059) * ratio.evt.stream) #The following conversion equation is based on an (excel) 4th order polynomial fit to instrument calibration data

}

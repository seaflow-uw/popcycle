readSeaflow <- function(file.path, column.names = EVT.HEADER, column.size = 2, count.only=FALSE, transform=TRUE, add.yearday.file=FALSE){ 

  if(!(substr(file.path, nchar(file.path)-2, nchar(file.path)) %in% c('opp','evt')))
    warning("attempting to read a seaflow file that doesn't have an evt or opp extension")
  # reads a binary seaflow event file into memory as a dataframe
  if(!file.exists(file.path)){
    
    stop(paste("The file doesn't exist;", file.path))
    
  }else{
    
    ## initialize dimentional parameters
    n.bytes.header <- 4
    n.bytes.EOL <- 4
    n.columns <- length(column.names)
    n.extra.columns <- n.bytes.EOL / column.size  # number of 16 bit integers (2:10&0) in the EOL character
    n.int.columns <- n.columns + n.extra.columns
    n.bytes.file <- file.info(file.path)$size
    n.rows <- ((n.bytes.file - n.bytes.header) / column.size) / n.int.columns  
    n.events <- n.int.columns * n.rows

    ## open binary file for reading 
    con <- file(description = file.path, open="rb") 
    header <- readBin(con, integer(), n = 1, size = n.bytes.header, endian = "little")
    header.EOL <- readBin(con, integer(), n = 1, size = n.bytes.EOL, endian = "little")

    ## check number of events
    if(n.rows != header)
      stop(paste("ERROR: the predicted number of rows (",n.rows,") doesn't equal the header specified number of events (", header,")",sep=''))

    if(count.only){
     return(header) #return just the event count in the header
    }else{
      
      ## read the actual events
      integer.vector <- readBin(con, integer(), n = n.events, size = column.size, signed = FALSE, endian = "little")    
      ## reformat the vector into a matrix -> dataframe
      integer.matrix <- matrix(integer.vector[1:n.events], nrow = n.rows, ncol = n.int.columns, byrow=TRUE)
      integer.dataframe <- data.frame(integer.matrix[,1:n.columns])
      ## name the columns
      names(integer.dataframe) <- c(column.names)
      close(con) 
      
      ## Transform data to LOG scale
      
      if(transform) integer.dataframe[,EVT.HEADER[-c(1,2)]] <- 10^((integer.dataframe[,EVT.HEADER[-c(1,2)]]/2^16)*3.5)  
           
           
           
      if(add.yearday.file){
              integer.dataframe$file  <- getFileNumber(file.path)
              integer.dataframe$year_day <- .getYearDay(file.path)
      }
      
	
      return (integer.dataframe)
    }
  }
}


writeSeaflow <- function(file.path, df, column.names = EVT.HEADER, linearize=TRUE){
  if(!all(EVT.HEADER %in% names(df)))
    warning("attempting to read a seaflow file that doesn't have an evt or opp extension")

  # writes a binary seaflow event file from a dataframe in memory

  ## NEED TO ADD CHECK and REORDERING OF COLUMN NAMES
  n.bytes.header <- 4
  column.size <- 2
  EOL.double <- 10

	## UNTRANSFORM LOG-SCALED DATA (BACK TO ORIGINAL DATA)
	if(linearize) df[,EVT.HEADER[-c(1,2)]] <- (log10(df[,EVT.HEADER[-c(1,2)]])/3.5)*2^16

  ## open connection ##
  con <- file(description = file.path, open="wb")
  ## write newline ##
  writeBin(as.integer(c(nrow(df),EOL.double)), con, size = n.bytes.header, endian = "little")

  ## write out end of line character
  #writeBin(10, con, size = 4, endian = "little") #done 2 lines above?

  ## construct a vector of integers from the dataframe with the EOL integers at the end of each line
  out.vect <- as.integer(unlist(t(cbind(df, EOL.double, 0))))
  
  out.vect <- out.vect[1:(length(out.vect)-2)] # hack to remove the last two \r\n characters (see below)

  ## write it out
  writeBin(out.vect, con, size = column.size)
  close(con)
}



write.delim <- function(df, file, quote=FALSE, row.names=FALSE, sep='\t', ...){
	write.table(df, file,  quote=quote, row.names=row.names, sep=sep, ...)
}


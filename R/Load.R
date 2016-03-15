transformData <- function(integer.dataframe){
  id <- which(colnames(integer.dataframe) == "pulse_width" | colnames(integer.dataframe) == "time" | colnames(integer.dataframe) == "pop")
  if (length(id)) {
    integer.dataframe[,-c(id)] <- 10^((integer.dataframe[,-c(id)]/2^16)*3.5)
  } else {
    # This probably looks strange and you might wonder why we don't just assign
    # to integer.dataframe rather than integer.dataframe[, ].
    # Exponentiating a data frame in R returns a matrix, not a data frame, so
    # to keep integer.dataframe as a data frame we assign back into the original
    # data frame. Otherwise we would rebind integer.dataframe as a new matrix.
    integer.dataframe[, ] <- 10^((integer.dataframe/2^16)*3.5)
    #integer.dataframe <- data.frame(10^((integer.dataframe/2^16)*3.5))
  }
  return(integer.dataframe)
}

untransformData <- function(float.dataframe){
  id <- which(colnames(float.dataframe) == "pulse_width" | colnames(float.dataframe) == "time" | colnames(float.dataframe) =="pop")
  if (length(id)) {
    float.dataframe[,-c(id)] <-(log10(float.dataframe[,-c(id)])/3.5)*2^16
  } else {
    float.dataframe[, ] <-(log10(float.dataframe)/3.5)*2^16
  }
  return(float.dataframe)
}

readSeaflow <- function(path, column.names=EVT.HEADER,
                        count.only=FALSE, transform=TRUE, channel=NULL){
  # reads a binary seaflow event file into memory as a dataframe
  if (!file.exists(path)) {
    path <- paste0(path, ".gz")
    if (!file.exists(path)) {
      stop(paste("The file doesn't exist;", path))
    }
  }
  ## initialize dimensional parameters
  n.bytes.header <- 4
  n.bytes.padding <- 4
  column.size <- 2
  n.data.columns <- length(column.names)
  n.extra.columns <- 2  # 2 uint16 (10 and 0) at start of each row
  n.total.columns <- n.data.columns + n.extra.columns

  ## open binary file for reading
  if (file_ext(path) == "gz") {
    con <- gzfile(description=path, open="rb")
  } else {
    con <- file(description=path, open="rb")
  }
  header <- readBin(con, "integer", n = 1, size = n.bytes.header, endian = "little")
  # Check for empty file.  If empty return an empty data frame
  if (length(header) == 0) {
    warning(sprintf("File %s has size zero.", path))
    close(con)
    return(data.frame())
  }

  if (count.only) {
   return(header) #return just the event count in the header
  } else {
    ## read the actual events
    n.events <- header * n.total.columns
    expected.bytes <- n.events * column.size
    integer.vector <- readBin(con, "integer", n = n.events, size = column.size, signed = FALSE, endian = "little")
    received.bytes <- length(integer.vector) * column.size
    if (received.bytes != expected.bytes) {
      warning(sprintf("File %s has incorrect data size. Expected %i bytes, saw %i bytes",
                      path, expected.bytes, received.bytes))
      close(con)
      return(data.frame())
    }
    ## reformat the vector into a matrix -> dataframe
    integer.matrix <- matrix(integer.vector, nrow = header, ncol = n.total.columns, byrow=TRUE)
    # Convert to data frame dropping first two padding columns
    integer.dataframe <- data.frame(integer.matrix[,(n.extra.columns+1):n.total.columns])
    ## name the columns
    names(integer.dataframe) <- c(column.names)
    close(con)

    if (nrow(integer.dataframe) != header) {
      msg <- paste("In file ", path, " the declared number of events ", header,
           " doesn't equal the actual number of events", sep="")
      warning(msg)
      return(data.frame())
    }

    if (! is.null(channel)) {
      integer.dataframe <- integer.dataframe[, channel, drop=FALSE]
    }

    ## Transform data to LOG scale
    if(transform) integer.dataframe <- transformData(integer.dataframe)

    return (integer.dataframe)
  }
}

writeSeaflow <- function(df, path, column.names = EVT.HEADER, linearize=TRUE){
  # writes a binary seaflow event file from a dataframe in memory

  ## NEED TO ADD CHECK and REORDERING OF COLUMN NAMES
  n.bytes.header <- 4
  column.size <- 2
  EOL.double <- 10

	## UNTRANSFORM LOG-SCALED DATA (BACK TO ORIGINAL DATA)
  if(linearize)  df <- untransformData(df)

  ## open connection ##
  if (substr(path, nchar(path) - 2, nchar(path)) == ".gz") {
    con <- gzfile(description = path, open="wb")
  } else {
    con <- file(description = path, open="wb")
  }
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

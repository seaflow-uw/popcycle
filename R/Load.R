#' Exponentiate SeaFlow log data
#'
#' SeaFlow data is stored as 3.5 decades of log data on a linear 16-bit scale.
#' This function will exponentiate this data to a linear scale between 1 and
#' 10^3.5.
#'
#' @param df Data frame returned by readSeaflow
#' @return Exponentiated version of integer.dataframe
#' @examples
#' \dontrun{
#' evt <- transformData(evt)
#' }
#' @export
transformData <- function(df) {
  id <- which(colnames(df) == "pulse_width" | colnames(df) == "time" | colnames(df) == "pop")
  if (length(id)) {
    df[,-c(id)] <- 10^((df[,-c(id)]/2^16)*3.5)
  } else {
    # This probably looks strange and you might wonder why we don't just assign
    # to df rather thandf[, ].
    # Exponentiating a data frame in R returns a matrix, not a data frame, so
    # to keep df as a data frame we assign back into the original
    # data frame. Otherwise we would rebind df as a new matrix instead of data
    # frame.
    df[, ] <- 10^((df/2^16)*3.5)
  }
  return(df)
}

#' Log SeaFlow linear data
#'
#' Performs the reverse operation of transformData, converting exponentiated
#' data back to its original form of log data on a 2^16 linear scale.
#'
#' @param df Data frame returned by readSeaflow
#' @return Exponentiated version of df
#' \dontrun{
#' evt <- untransformData(evt)
#' }
#' @export
untransformData <- function(df) {
  id <- which(colnames(df) == "pulse_width" | colnames(df) == "time" | colnames(df) =="pop")
  if (length(id)) {
    df[,-c(id)] <-(log10(df[,-c(id)])/3.5)*2^16
  } else {
    df[, ] <-(log10(df)/3.5)*2^16
  }
  return(df)
}

#' Read an EVT or OPP binary file.
#'
#' Read a SeaFlow LabView binary particle data file. This file may be gzipped.
#'
#' @param path Path to binary file. If this file does not exist the same
#'   file ending with ".gz" will also be checked.
#' @param count.only Only return the count of particles from the file header.
#' @param transform Convert log data to linear.
#' @param channel Only return data for channels listed here. Can be a single
#'   channel name or a vector of channel names.
#' @return Data frame of particle data or number of particles.
#' @examples
#' \dontrun{
#' evt <- readSeaflow("foo/2014_213/2014-07-04T00-00-02+00-00.gz", channel=c("fsc_small", "pe"))
#' }
#' @export
readSeaflow <- function(path, count.only=FALSE, transform=TRUE, channel=NULL) {
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
  n.data.columns <- length(EVT.HEADER)
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
    names(integer.dataframe) <- c(EVT.HEADER)
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

#' Write an EVT or OPP binary file.
#'
#' Write a SeaFlow LabView binary particle data file.
#'
#' @param df EVT or OPP data frame.
#' @param path Path to binary file. If this file ends with ".gz" the output
#'   will be gzipped.
#' @param untransform Convert linear data to log.
#' @return None
#' @examples
#' \dontrun{
#' writeSeaflow(opp, "foo/2014_213/2014-07-04T00-00-02+00-00.gz")
#' }
#' @export
writeSeaflow <- function(df, path, untransform=TRUE) {
  ## NEED TO ADD CHECK and REORDERING OF COLUMN NAMES
  n.bytes.header <- 4
  column.size <- 2
  EOL.double <- 10

	## UNTRANSFORM BACK TO ORIGINAL DATA
  if(untransform)  df <- untransformData(df)

  ## open connection ##
  if (endswith(path, ".gz")) {
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

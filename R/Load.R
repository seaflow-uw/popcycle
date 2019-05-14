#' Exponentiate SeaFlow log data
#'
#' SeaFlow data is stored as 3.5 decades of log data on a linear 16-bit scale.
#' This function will exponentiate this data to a linear scale between 1 and
#' 10^3.5.
#'
#' @param df Data frame returned by readSeaflow
#' @param columns If not NULL, only these columns will be modified
#' @return Exponentiated version of integer.dataframe
#' @examples
#' \dontrun{
#' evt <- transformData(evt)
#' }
#' @export
transformData <- function(df, columns=NULL) {
  if (nrow(df) == 0) {
    return(df)
  }
  if (! is.null(columns)) {
    df[, columns] <- 10^((df[, columns]/2^16)*3.5)
  } else {
    id <- which(colnames(df) == "pulse_width" | colnames(df) == "time" | colnames(df) == "pop")
    if (length(id)) {
      df[,-c(id)] <- 10^((df[,-c(id)]/2^16)*3.5)
    } else {
      df[, ] <- 10^((df[, ]/2^16)*3.5)
    }
  }
  return(df)
}

#' Log SeaFlow linear data
#'
#' Performs the reverse operation of transformData, converting exponentiated
#' data back to its original form of log data on a 2^16 linear scale.
#'
#' @param df Data frame returned by readSeaflow
#' @param columns If not NULL, only these columns will be modified
#' @return Exponentiated version of df
#' \dontrun{
#' evt <- untransformData(evt)
#' }
#' @export
untransformData <- function(df, columns=NULL) {
  if (nrow(df) == 0) {
    return(df)
  }
  if (! is.null(columns)) {
    df[, columns] <-(log10(df[, columns])/3.5)*2^16
  } else {
    id <- which(colnames(df) == "pulse_width" | colnames(df) == "time" | colnames(df) =="pop")
    if (length(id)) {
      df[,-c(id)] <-(log10(df[,-c(id)])/3.5)*2^16
    } else {
      df[, ] <-(log10(df)/3.5)*2^16
    }
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
readSeaflow <- function(path, count.only=FALSE, transform=TRUE, channel=NULL, columns=NULL) {
  # reads a binary seaflow event file into memory as a dataframe
  if (!file.exists(path)) {
    path <- paste0(path, ".gz")
    if (!file.exists(path)) {
      stop(paste("The file doesn't exist;", path))
    }
  }
  if (is.null(columns)) {
    columns <- EVT.HEADER
  }

  ## initialize dimensional parameters
  n.bytes.header <- 4
  n.bytes.padding <- 4
  column.size <- 2
  n.data.columns <- length(columns)
  n.extra.columns <- 2  # 2 uint16 (10 and 0) at start of each row
  n.total.columns <- n.data.columns + n.extra.columns

  ## open binary file for reading
  if (tools::file_ext(path) == "gz") {
    con <- gzfile(description=path, open="rb")
  } else {
    con <- file(description=path, open="rb")
  }
  header <- readBin(con, "integer", n = 1, size = n.bytes.header, endian = "little")
  # Check for empty file.  If empty return an empty data frame
  if (length(header) == 0) {
    warning(sprintf("File %s has size zero or truncated header.", path))
    close(con)
    return(data.frame())
  }
  if (header == 0) {
    warning(sprintf("File %s has no particle data.", path))
    close(con)
    return(data.frame())
  }

  if (count.only) {
   return(header) #return just the event count in the header
  } else {
    # read the actual events
    n.events <- header * n.total.columns
    expected.bytes <- n.events * column.size
    integer.vector <- readBin(con, "integer", n = n.events, size = column.size, signed = FALSE, endian = "little")
    received.bytes <- length(integer.vector) * column.size

    # Now read any data left. There shouldn't be any.
    while (TRUE) {
      extra.bytes <- readBin(con, "integer", n = 8192, size = 1, signed = FALSE, endian = "little")
      received.bytes = received.bytes + length(extra.bytes)
      if (length(extra.bytes) == 0) {
        break
      }
    }

    if (received.bytes != expected.bytes) {
      warning(sprintf("File %s has incorrect data size. Expected %i bytes, saw %i bytes",
                      path, expected.bytes, received.bytes))
      close(con)
      return(data.frame())
    }
    # reformat the vector into a matrix -> dataframe
    integer.matrix <- matrix(integer.vector, nrow = header, ncol = n.total.columns, byrow=TRUE)
    # Convert to data frame dropping first two padding columns
    # Drop=FALSE is necessary here when subsetting the matrix. Otherwise in the
    # case of a one-row matrix the subsetted matrix will have the unnecessary
    # dimension "dropped" and you'll get an integer vector back instead of a
    # matrix, which creates a completely differently shaped data.frame.
    integer.dataframe <- data.frame(integer.matrix[,(n.extra.columns+1):n.total.columns, drop=F])
    ## name the columns
    names(integer.dataframe) <- columns
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
    if(transform) integer.dataframe <- transformData(integer.dataframe, columns=EVT.HEADER)

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
  if (untransform)  df <- untransformData(df, columns=EVT.HEADER)

  ## open connection ##
  if (endswith(path, ".gz")) {
    con <- gzfile(description = path, open="wb")
  } else {
    con <- file(description = path, open="wb")
  }
  ## write row count ##
  writeBin(as.integer(nrow(df)), con, size = n.bytes.header, endian = "little")

  # construct a vector of integers from the dataframe with the EOL integers at
  # the start of each line
  out.vect <- as.integer(unlist(t(cbind(EOL.double, 0, df))))

  ## write it out
  writeBin(out.vect, con, size = column.size)
  close(con)
}

#' Concatenate EVT files
#'
#' @param evt.list List of EVT files.
#' @param evt.dir Path to EVT files.
#' @param n Number of rows to return.
#' @param min.fsc, min.pe, min.chl Minimum value for fsc_small, pe and chl_small respectively
#' @return A dataframe with n rows.
#' @export
concatenate.evt <- function(evt.list, evt.dir, n=100000, min.fsc=0, min.pe=0, min.chl=0, transform=TRUE,...){
  n <- as.numeric(n)
  DF <- NULL
  i <- 0
  for (file in evt.list){
        message(round(100*i/length(evt.list)), "% completed \r", appendLF=FALSE)

        tryCatch({
          df <- get.evt.by.file(evt.dir, file, transform=transform)
          df <- subset(df, fsc_small > min.fsc & pe > min.pe & chl_small > min.chl)
          df <- df[round(seq(1,nrow(df), length.out=round(n/length(evt.list)))),]

            if(any(is.na(df))) next
            DF <- rbind(DF, df)
            }, error = function(e) {
              cat(paste0("Error with file ", file, ": ", e))
          })

          i <- i + 1
          flush.console()
          }

      return(DF)
}

#' Concatenate OPP files
#'
#' @param opp.list List of OPP files.
#' @param opp.dir Path to OPP files.
#' @param n Number of rows to return.
#' @param min.fsc, min.pe, min.chl Minimum value for fsc_small, pe and chl_small respectively
#' @return A dataframe with n rows.
#' @export
concatenate.opp <- function(opp.list, opp.dir, n=100000, min.fsc=0, min.pe=0, min.chl=0, transform=TRUE,...){
  n <- as.numeric(n)
  DF <- NULL
  i <- 0
  for (file in opp.list){
        message(round(100*i/length(opp.list)), "% completed \r", appendLF=FALSE)

        tryCatch({
          df <- get.opp.by.file(opp.dir, file, transform=transform)
          df <- subset(df, fsc_small > min.fsc & pe > min.pe & chl_small > min.chl)
          df <- df[round(seq(1,nrow(df), length.out=round(n/length(opp.list)))),]

            if(any(is.na(df))) next
            DF <- rbind(DF, df)
            }, error = function(e) {
              cat(paste0("Error with file ", file, ": ", e))
          })

          i <- i + 1
          flush.console()
          }

      return(DF)
}

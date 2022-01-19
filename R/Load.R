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
  if (is.null(columns)) {
    columns <- (names(df) %in% c("D1", "D2", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small", "chl_big"))
  }
  df[, columns] <- 10^((df[, columns]/2^16)*3.5)
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
  if (is.null(columns)) {
    columns <- (names(df) %in% c("D1", "D2", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small", "chl_big"))
  }
  df[, columns] <-(log10(df[, columns])/3.5)*2^16
  return(df)
}

#' Check if an EVT data frame has been transformed.
#'
#' @param df EVT data frame
#' @return Boolean indicating if df is transformed
is_transformed <- function(df) {
  cols <- colnames(df)[colnames(df) %in% c(CHANNELS, CHANNELS2)]
  return(!any(max(df[, cols]) > 10^3.5))
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

  ## initialize dimensional parameters
  n.rowcnt.bytes <- 4
  n.colcnt.bytes <- 4
  n.colcnt.columns <- 2  # column count 32-bit int in terms of 2 byte columns
  column.size <- 2  # column size in bytes
  
  ## open binary file for reading
  if (tools::file_ext(path) == "gz") {
    con <- gzfile(description=path, open="rb")
  } else {
    con <- file(description=path, open="rb")
  }
  num1 <- readBin(con, "integer", n = 1, size = n.rowcnt.bytes, endian = "little")
  num2 <- readBin(con, "integer", n = 1, size = n.colcnt.bytes, endian = "little")
  if (length(num1) == 0 || length(num2) == 0) {
    warning(sprintf("File %s has a truncated header section.", path))
    close(con)
    return(data.frame())
  }
  if (num2 == length(EVT.HEADER)) {
    # v1 EVT
    # Seek back to before we read the column count 32-bit int
    seek(con, where=n.rowcnt.bytes)
    if (is.null(columns)) {
      columns <- EVT.HEADER
    }
    columns_per_row <- n.colcnt.columns + length(columns)
    rowcnt <- num1
    version <- "v1"
  } else if (num1 == length(EVT.HEADER2) * column.size) {
    # v2 EVT
    # No need to seek back, there's only one column count 32-bit int in the file
    if (is.null(columns)) {
      columns <- EVT.HEADER2
    }
    columns_per_row <- length(columns)
    rowcnt <- num2
    version <- "v2"
  } else {
    warning(sprintf("File %s has a invalid header section.", path))
    close(con)
    return(data.frame())
  }

  # Check for empty file.  If empty return an empty data frame
  if (length(rowcnt) == 0 || rowcnt == 0) {
    warning(sprintf("File %s has no particle data.", path))
    close(con)
    return(data.frame())
  }
  
  if (count.only) {
    return(rowcnt) #return just the event count in the header
  }

  # Read the data
  expected.bytes <- rowcnt * columns_per_row * column.size
  integer.vector <- readBin(con, "integer", n = rowcnt * columns_per_row, size = column.size, signed = FALSE, endian = "little")
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
  integer.matrix <- matrix(integer.vector, nrow = rowcnt, ncol = columns_per_row, byrow=TRUE)
  # Convert to data frame
  if (version == "v1") {
    # v1 EVT, drop first two padding columns.
    # Drop=FALSE is necessary here when subsetting the matrix. Otherwise in the
    # case of a one-row matrix the subsetted matrix will have the unnecessary
    # dimension "dropped" and you'll get an integer vector back instead of a
    # matrix, which creates a completely differently shaped data.frame.
    integer.dataframe <- data.frame(integer.matrix[, (n.colcnt.columns+1):columns_per_row, drop=F])
  } else {
    # v2 EVT, no extra columns to drop
    integer.dataframe <- data.frame(integer.matrix)
  }
  ## name the columns
  names(integer.dataframe) <- columns
  close(con)

  if (nrow(integer.dataframe) != rowcnt) {
    msg <- paste("In file", path, "the declared number of events", rowcnt,
                 "does not equal the actual number of events")
    warning(msg)
    return(data.frame())
  }

  if (! is.null(channel)) {
    integer.dataframe <- integer.dataframe[, channel, drop=FALSE]
  }

  ## Transform data to LOG scale
  if(transform) integer.dataframe <- transformData(integer.dataframe, columns=columns)

  return (integer.dataframe)
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
  if (endsWith(path, ".gz")) {
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
          df <- get_evt_by_file(evt.dir, file, transform=transform)
          df <- subset(df, fsc_small > min.fsc & pe > min.pe & chl_small > min.chl)
          df <- df[round(seq(1,nrow(df), length.out=round(n/length(evt.list)))),]

            if(any(is.na(df))) next
            DF <- rbind(DF, df)
            }, error = function(e) {
              cat(paste("Error with file", file, ":", e))
          })

          i <- i + 1
          flush.console()
          }

      return(DF)
}

#' Concatenate OPP files
#'
#' @param db SQLite3 database file path.
#' @param opp.list List of OPP files.
#' @param opp.dir Path to OPP files.
#' @param n Number of rows to return.
#' @param min.fsc, min.pe, min.chl Minimum value for fsc_small, pe and chl_small respectively
#' @return A dataframe with n rows.
#' @export
concatenate.opp <- function(db, opp.list, opp.dir, n=100000, min.fsc=0, min.pe=0, min.chl=0, ...){
  n <- as.numeric(n)
  DF <- NULL
  i <- 0
  for (file in opp.list){
    message(round(100*i/length(opp.list)), "% completed \r", appendLF=FALSE)

    tryCatch({
      df <- get_opp_by_file(db, opp.dir, file)
      df <- subset(df, fsc_small > min.fsc & pe > min.pe & chl_small > min.chl)
      df <- df[round(seq(1,nrow(df), length.out=round(n/length(opp.list)))),]
      if (any(is.na(df))) {
        next
      }
      DF <- dplyr::bind_rows(DF, df)
    }, error = function(e) {
      cat(paste("Error with file", file, ":", e))
    })

    i <- i + 1
    flush.console()
  }
  # Remove unused levels in factors
  DF <- dplyr::mutate_if(DF, is.factor, forcats::fct_drop)

  return(DF)
}

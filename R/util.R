#' Get start indexes for run length encoding results.
#'
#' @param rle_result Result from rle()
#' @return Integer vector
#' @examples
#' \dontrun{
#' starts <- rle_starts(rle(c(1,1,1,2,2,3,3)))
#' }
rle_starts <- function(rle_result) {
  i <- 1
  loc <- 1
  starts <- c()
  for (i in 1:length(rle_result$lengths)) {
    starts[i] <- loc
    loc <- loc + rle_result$lengths[i]
    i <- i + 1
  }
  return(starts)
}

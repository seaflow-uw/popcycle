make_boundary_df <- function() {
  return(tibble::tibble(
    date = c(rep("date1", 10), rep("date2", 10)),
    a = c(
      1, 3, 2, 3, 2, 2, 4, 2, 4, 1,
      1, 2, 2, 4, 3, 3, 3, 1, 3, 2
    ),
    b = c(
      20, 20, 10, 40, 30, 20, 40, 20, 30, 10,
      40, 20, 20, 30, 40, 20, 20, 10, 30, 20
    ),
    c = c(seq(1, 10), seq(1, 10))
  ))
}

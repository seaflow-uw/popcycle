
make_small_vct <- function() {
  df <- tibble::tibble(
    date = c(rep("date1", 3), rep("date2", 3)),
    D1 = seq(6),
    D2 = seq(6) + 1,
    fsc_small = seq(6) * 2,
    q2.5 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    q50 = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
    diam_lwr_q2.5 = seq(6),
    diam_mid_q2.5 = seq(6) + 1,
    diam_upr_q2.5 = seq(6) + 2,
    diam_lwr_q50 = seq(6) + 3,
    diam_mid_q50 = seq(6) + 4,
    diam_upr_q50 = seq(6) + 5,
    Qc_lwr_q2.5 = seq(6) + 6,
    Qc_mid_q2.5 = seq(6) + 7,
    Qc_upr_q2.5 = seq(6) + 8,
    Qc_lwr_q50 = seq(6) + 9,
    Qc_mid_q50 = seq(6) + 10,
    Qc_upr_q50 = seq(6) + 11,
    pop_q2.5 = c("a", "a", "b", "b", "c", "c"),
    pop_q50 = c("a", "a", "b", "b", "b", "c"),
  )
  # Get _q2.5 column names, set !q2.5 values to NA
  q2.5_cols <- names(df)[endsWith(names(df), "_q2.5")]
  df[!df$q2.5, q2.5_cols] <- NA
  # Get _q50 column names, set !q50 values to NA
  q50_cols <- names(df)[endsWith(names(df), "_q50")]
  df[!df$q50, q50_cols] <- NA

  return(df)
}

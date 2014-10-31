library(testthat)
results <- test_dir("tests/testthat")
if (any(results$failed > 0)) {
    status <- 1
} else {
    status <- 0
}
quit(save="no", status=status)

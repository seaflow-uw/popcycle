library(testthat)
results <- test_dir("tests/testthat")
quit(save="no", status=any(results$failed > 0))

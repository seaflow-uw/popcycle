library(testthat)
results <- test_dir("tests/testthat", reporter="stop")
print(results)

This directory contains code and data for testing popcycle.

* The test framework used is the R package **testthat**.
* Each test is responsible for its own setup and teardown process.
* Each test should take place using a temporary directory as project directory
rather than working with popcycle default project directory locations in
order to insulate user data from test results.
* To make it easier to update tests after code changes, calculated results
are often printed in the test.  e.g. "opp1.count = 154".
* Tests can be run from the root of the popcycle repository with

```
Rscript tests/testthat.R
```

* `tests/testthat.R` will exit with non-zero status if any test fails or
experiences an error.
* The current working directory for each test is the directory in which the test file resides, not the directory from which the test script is called.  For example, consider tests in `tests/testthat/test_evaluate_last_file.R`.  Let's say our current working directory in a terminal is the root of the repository working copy.  We would run tests with `Rscript tests/testthat.R`.  Each test in `tests/testthat/test_evaluate_last_file.R` runs with a working directory of `tests/testthat/`, not the root of the repository working copy.

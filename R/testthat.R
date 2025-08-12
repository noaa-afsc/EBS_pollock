# tests/testthat.R
# Run all tests with: source("tests/testthat.R")
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Please install the 'testthat' package.")
}
testthat::test_dir("tests/testthat", reporter = "summary")

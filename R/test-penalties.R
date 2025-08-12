# tests/testthat/test-penalties.R
# Unit tests for first-difference penalties (time and age)

testthat::test_that("fd_penalty_time matches explicit loop and uses zero anchor (#test-fd-penalty-time)", {
  T <- 6; A <- 4
  set.seed(1); L <- matrix(rnorm(T*A, sd=0.2), T, A)
  w <- c(1, 0.5, 2, 1)

  fd_loop <- function(L, w){
    acc <- 0
    for (a in seq_len(ncol(L))) {
      acc <- acc + w[a]*(L[1,a]-0)^2
      for (y in 2:nrow(L)) acc <- acc + w[a]*(L[y,a]-L[y-1,a])^2
    }
    acc
  }

  fd_penalty_time <- function(log_sel, w = 1) {
    stopifnot(is.matrix(log_sel))
    T <- nrow(log_sel); A <- ncol(log_sel)
    if (length(w) == 1) w <- rep(w, A)
    stopifnot(length(w) == A)
    diffs <- rbind(log_sel[1, , drop = FALSE] - 0, apply(log_sel, 2, diff))
    sum(colSums((sqrt(w) * diffs)^2))
  }

  testthat::expect_equal(fd_penalty_time(L, w), fd_loop(L, w), tolerance = 1e-12)
})

testthat::test_that("fd_penalty_age matches construction and zero at a_min (#test-fd-penalty-age)", {
  T <- 5; A <- 6; a_min <- 1
  set.seed(2); L <- matrix(rnorm(T*A, sd=0.15), T, A)
  w <- rep(1, T)

  fd_penalty_age <- function(log_sel, w = 1, a_min = 1) {
    stopifnot(is.matrix(log_sel))
    T <- nrow(log_sel); A <- ncol(log_sel)
    stopifnot(a_min >= 1, a_min <= A)
    if (length(w) == 1) w <- rep(w, T)
    stopifnot(length(w) == T)
    diffs <- matrix(0, nrow = T, ncol = A)
    diffs[, a_min] <- log_sel[, a_min] - 0
    if (a_min < A) {
      for (a in (a_min+1):A) {
        diffs[, a] <- log_sel[, a] - log_sel[, a-1]
      }
    }
    sum(rowSums((sqrt(w) * diffs)^2))
  }

  # Construct expected penalty directly
  expected <- 0
  for (y in 1:T) {
    expected <- expected + w[y]*(L[y,a_min]-0)^2
    for (a in (a_min+1):A) expected <- expected + w[y]*(L[y,a]-L[y,a-1])^2
  }

  testthat::expect_equal(fd_penalty_age(L, w, a_min), expected, tolerance = 1e-12)
})

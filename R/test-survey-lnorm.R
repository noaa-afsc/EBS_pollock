# tests/testthat/test-survey-lnorm.R
# Checks for lognormal survey likelihood scaling and constant term toggle

testthat::test_that("lognormal NLL increases when sigma decreases for fixed residuals (#test-survey-lognorm)", {
  set.seed(3)
  obs <- abs(rnorm(100, mean = 100, sd = 10))
  exp <- obs * exp(rnorm(100, sd = 0.05))  # small multiplicative noise

  lognorm_nll <- function(obs, exp, sigma_log, include_const = TRUE) {
    r <- log(pmax(obs, .Machine$double.eps)) - log(pmax(exp, .Machine$double.eps))
    n <- length(r)
    nll <- sum(r^2) / (2 * sigma_log^2)
    if (include_const) nll <- nll + n*log(sigma_log) + n*0.5*log(2*pi)
    nll
  }

  nll_big  <- lognorm_nll(obs, exp, sigma_log = 0.2, include_const = TRUE)
  nll_small<- lognorm_nll(obs, exp, sigma_log = 0.1, include_const = TRUE)

  testthat::expect_true(nll_small > nll_big)  # tighter sigma -> higher NLL
})

testthat::test_that("including constants only differs by additive term", {
  set.seed(4)
  obs <- abs(rnorm(50, mean = 100, sd = 15))
  exp <- obs * exp(rnorm(50, sd = 0.1))

  lognorm_nll <- function(obs, exp, sigma_log, include_const = TRUE) {
    r <- log(pmax(obs, .Machine$double.eps)) - log(pmax(exp, .Machine$double.eps))
    n <- length(r)
    nll <- sum(r^2) / (2 * sigma_log^2)
    if (include_const) nll <- nll + n*log(sigma_log) + n*0.5*log(2*pi)
    nll
  }

  nll0 <- lognorm_nll(obs, exp, sigma_log = 0.15, include_const = FALSE)
  nll1 <- lognorm_nll(obs, exp, sigma_log = 0.15, include_const = TRUE)

  # Difference should equal additive constant
  r <- log(pmax(obs, .Machine$double.eps)) - log(pmax(exp, .Machine$double.eps))
  n <- length(r)
  const <- n*log(0.15) + n*0.5*log(2*pi)
  testthat::expect_equal(nll1 - nll0, const, tolerance = 1e-12)
})

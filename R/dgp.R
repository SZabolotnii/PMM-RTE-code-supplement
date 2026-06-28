# =============================================================================
# R/dgp.R — design matrix, error families, beta patterns, scenario grid, catalog
# Every error family is centred to mean 0 and scaled to unit variance, then x sigma,
# so g2/g3 depend only on the family shape (not on sigma).
# TN generator rtn() ported from PMM3_vs_MLE-TN/R/01_tn_distribution.R.
# =============================================================================

# Correlated Gaussian predictors: x_j = rho*z + sqrt(1-rho^2)*w_j (common factor z)
generate_x <- function(n, p, rho) {
  common <- rnorm(n)
  idio   <- matrix(rnorm(n * p), nrow = n, ncol = p)
  x <- rho * common + sqrt(1 - rho^2) * idio
  colnames(x) <- paste0("x", seq_len(p))
  x
}

# Two-piece Normal generator (Salinas et al. 2023, fold-and-sign); symmetric here
rtn <- function(n, xi = 0, eta = 1, lambda = 1) {
  stopifnot(n > 0, eta > 0, lambda >= 0)
  U <- rnorm(n, mean = lambda, sd = 1)
  S <- sample(c(-1L, 1L), n, replace = TRUE)
  xi + eta * (S * abs(U))
}

# Zero-mean, unit-variance error families (then scaled by sigma)
generate_error <- function(n, family, sigma = 1) {
  e <- switch(family,
    normal      = rnorm(n),
    gamma4      = (rgamma(n, shape = 4, scale = 1) - 4) / 2,                # g3=1,   g4=1.5
    gamma2      = (rgamma(n, shape = 2, scale = 1) - 2) / sqrt(2),          # g3=1.41,g4=3
    exponential = rexp(n) - 1,                                             # g3=2,   g4=6
    lognormal   = { s <- 0.5; (rlnorm(n, 0, s) - exp(s^2 / 2)) /
                      sqrt((exp(s^2) - 1) * exp(s^2)) },                    # heavy right tail
    uniform     = runif(n, -sqrt(3), sqrt(3)),                            # g4=-1.2 platykurtic
    tn1.0       = rtn(n, 0, 1, 1.0) / sqrt(1 + 1.0^2),                     # symmetric platykurtic
    tn1.5       = rtn(n, 0, 1, 1.5) / sqrt(1 + 1.5^2),
    tn2.0       = rtn(n, 0, 1, 2.0) / sqrt(1 + 2.0^2),
    triangular  = (runif(n) + runif(n) - 1) * sqrt(6),                    # g4=-0.6
    logistic    = rlogis(n, 0, sqrt(3) / pi),                             # g4=+1.2 leptokurtic
    student10   = rt(n, df = 10) / sqrt(10 / 8),                          # g4=+1   (misspec test)
    stop("Unknown error family: ", family, call. = FALSE))
  sigma * e
}

# True slope patterns (normalized to unit L2 norm)
make_beta <- function(p, pattern = "equal_positive") {
  b <- switch(pattern,
    equal_positive = rep(1, p),
    sparse         = c(rep(1, ceiling(p / 2)), rep(0, p - ceiling(p / 2))),
    alternating    = rep(c(1, -1), length.out = p),
    stop("Unknown beta pattern: ", pattern, call. = FALSE))
  b / sqrt(sum(b^2))
}

# Scenario grid for a phase (screening|final) and block (asym|sym) ------------
make_scenario_grid <- function(phase, block) {
  sc   <- CFG$scenarios[[phase]]
  errs <- CFG$errors[[block]][[phase]]
  if (phase == "screening") {
    np   <- do.call(rbind, lapply(sc$np_pairs, function(z) data.frame(n = z[1], p = z[2])))
    base <- expand.grid(np_i = seq_len(nrow(np)), rho = sc$rho, error = errs,
                        stringsAsFactors = FALSE)
    grid <- data.frame(n = np$n[base$np_i], p = np$p[base$np_i],
                       rho = base$rho, error = base$error, stringsAsFactors = FALSE)
  } else {
    n_vec <- sc$n
    # Compact-PMM3: the sym FINAL block runs a reduced n-grid (see config.R).
    if (block == "sym" && phase == "final" && !is.null(CFG$scenarios$final_sym_n))
      n_vec <- CFG$scenarios$final_sym_n
    grid <- expand.grid(n = n_vec, p = sc$p, rho = sc$rho, error = errs,
                        stringsAsFactors = FALSE)
  }
  grid$scenario_id <- sprintf("n%d_p%d_rho%.2f_%s", grid$n, grid$p, grid$rho, grid$error)
  grid$sid_index   <- seq_len(nrow(grid))
  grid[order(grid$scenario_id), , drop = FALSE]
}

# Nominal cumulant/g-factor catalog per family (large-sample, for Table 2 + verify)
dgp_catalog <- function(families = unique(unlist(CFG$errors, use.names = FALSE)),
                        n_big = 2e6, seed = CFG$seed_base) {
  set.seed(seed)
  rows <- lapply(families, function(fam) {
    e  <- generate_error(n_big, fam, sigma = 1)
    m  <- EstemPMM::compute_moments(e)            # m2,m3,m4,c3,c4,g  (g = g2)
    m3 <- EstemPMM::compute_moments_pmm3(e)       # gamma4,gamma6,g3,...
    data.frame(family = fam,
               gamma3 = m$c3, gamma4 = m$c4, gamma6 = m3$gamma6,
               g2 = m$g, g3 = m3$g3,
               var_reduction_pmm2 = 1 - m$g, var_reduction_pmm3 = 1 - m3$g3,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

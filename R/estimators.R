# =============================================================================
# R/estimators.R — base estimators, the RTE operator, eigen-admissibility, dispatch
# Wraps EstemPMM (NEVER reimplements PMM/PMM3 solvers).
# mle_tn() is ported verbatim from PMM3_vs_MLE-TN/R/02_mle_tn.R (Salinas et al. 2023).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.xnames <- function(p) paste0("x", seq_len(p))

make_design <- function(x) {
  x <- as.matrix(x)
  if (is.null(colnames(x))) colnames(x) <- .xnames(ncol(x))
  d <- cbind("(Intercept)" = 1, x)
  d
}

safe_solve <- function(a, b) {
  tryCatch(solve(a, b), error = function(e)
    tryCatch(qr.solve(a, b), error = function(e2) NULL))
}

.as_df <- function(x, y) {
  x <- as.matrix(x); colnames(x) <- .xnames(ncol(x))
  data.frame(y = y, x, check.names = FALSE)
}

# Align an estimator's named coef to c("(Intercept)", x1..xp)
.align_coef <- function(b, p) {
  out <- rep(NA_real_, p + 1L)
  names(out) <- c("(Intercept)", .xnames(p))
  nm <- names(b)
  nm[nm %in% c("(Intercept)", "X.Intercept.", "Intercept")] <- "(Intercept)"
  names(b) <- nm
  common <- intersect(names(out), names(b))
  out[common] <- b[common]
  as.numeric(out)
}

# --- base estimators (coef vector, intercept first) -------------------------
fit_ols_coef <- function(x, y) {
  design <- make_design(x)
  b <- safe_solve(crossprod(design), crossprod(design, y))
  if (is.null(b)) return(rep(NA_real_, ncol(design)))
  as.numeric(b)
}

fit_pmm2_coef <- function(x, y) {
  p <- ncol(as.matrix(x)); df <- .as_df(x, y)
  fit <- tryCatch(EstemPMM::lm_pmm2(y ~ ., data = df), error = function(e) NULL)
  if (is.null(fit)) return(rep(NA_real_, p + 1L))
  .align_coef(fit@coefficients, p)
}

# PMM3: wraps EstemPMM::lm_pmm3 (ships its own NR solver). OLS fallback on
# non-convergence (SPEC Risk 4); convergence flag attached for logging.
fit_pmm3_coef <- function(x, y, adaptive = FALSE) {
  p <- ncol(as.matrix(x)); df <- .as_df(x, y)
  fit <- tryCatch(suppressWarnings(EstemPMM::lm_pmm3(y ~ ., data = df, adaptive = adaptive)),
                  error = function(e) NULL)
  conv <- !is.null(fit) && isTRUE(tryCatch(as.logical(fit@convergence),
                                           error = function(e) FALSE))
  cf <- if (is.null(fit)) rep(NA_real_, p + 1L) else .align_coef(fit@coefficients, p)
  if (anyNA(cf)) { cf <- fit_ols_coef(x, y); conv <- FALSE }
  attr(cf, "pmm3_converged") <- conv
  cf
}

fit_mle_tn_coef <- function(x, y) {
  p <- ncol(as.matrix(x)); df <- .as_df(x, y)
  fit <- tryCatch(mle_tn(y ~ ., data = df), error = function(e) NULL)
  if (is.null(fit)) return(rep(NA_real_, p + 1L))
  .align_coef(fit$coefficients, p)
}

fit_base <- function(method, x, y) {
  switch(method,
    OLS    = fit_ols_coef(x, y),
    PMM2   = fit_pmm2_coef(x, y),
    PMM3   = fit_pmm3_coef(x, y),
    MLE_TN = fit_mle_tn_coef(x, y),
    stop("Unknown base method: ", method, call. = FALSE))
}

# --- RTE operator L_RTE(k,g;X) (the only bespoke math) ----------------------
# Canonical: L = (X'X + D)^-1 (X'X + gD) (X'X + kD)^-1 (X'X), D = diag(0,1,..,1).
rte_operator <- function(x, k, g) {
  design <- make_design(x); xtx <- crossprod(design); p <- ncol(design)
  d <- diag(c(0, rep(1, p - 1L)), p)
  left  <- safe_solve(xtx + d, xtx + g * d); if (is.null(left))  return(NULL)
  right <- safe_solve(xtx + k * d, xtx);     if (is.null(right)) return(NULL)
  left %*% right
}

rte_transform <- function(x, beta_base, k, g) {
  L <- rte_operator(x, k, g)
  if (is.null(L)) return(rep(NA_real_, length(beta_base)))
  as.numeric(L %*% beta_base)
}

# Prediction from an intercept-first coef vector
predict_coef <- function(x, coef) as.numeric(make_design(x) %*% coef)

# g(k) coupling (defect-1 ablation): collapses the free 2-D grid to a 1-D path
g_of_k <- function(k, fun = "liu", pars = NULL) {
  switch(fun,
    liu    = k,
    linear = pars[1] + pars[2] * k,
    stop("Unknown g(k) coupling: ", fun, call. = FALSE))
}

# --- eigen shrinkage + admissibility (DEFECT-1 root-cause fix) --------------
# s_j(k,g) = lambda_j (lambda_j + g) / ((lambda_j + 1)(lambda_j + k)), lambda_j of
# the centred slope Gram. At g < -lambda_j a direction gets negative shrinkage /
# sign-flip -> the boundary artefact. Drop any (k,g) with s_j out of [0,1].
eigen_shrinkage <- function(x, k, g) {
  Xs <- scale(as.matrix(x), center = TRUE, scale = FALSE)
  lam <- eigen(crossprod(Xs), symmetric = TRUE, only.values = TRUE)$values
  lam <- pmax(lam, 0)
  list(lambda = lam, s = lam * (lam + g) / ((lam + 1) * (lam + k)))
}

is_admissible <- function(x, k, g, tol = CFG$shrink_tol) {
  s <- eigen_shrinkage(x, k, g)$s
  all(is.finite(s)) && all(s >= -tol) && all(s <= 1 + tol)
}

# --- dispatch (wrap EstemPMM::pmm_dispatch + SPEC §5.1 sample-size gate) -----
dispatch_base <- function(residuals, n, cfg = CFG$dispatch) {
  d <- tryCatch(EstemPMM::pmm_dispatch(residuals,
        symmetry_threshold = cfg$symmetry_threshold,
        kurtosis_threshold = cfg$kurtosis_threshold,
        g2_threshold = cfg$g2_threshold, verbose = FALSE),
        error = function(e) NULL)
  base   <- if (is.null(d)) "OLS" else d$method
  status <- "publication"
  if (n < cfg$n_min) { base <- "OLS"; status <- "illustrative" }   # §5.1: small-n gate
  list(base = base, status = status, dispatch = d)
}

# =============================================================================
# Ported verbatim from PMM3_vs_MLE-TN/R/02_mle_tn.R (MLE-TN competitor, Block C).
# Salinas et al. (2023), Mathematics 11(5), 1271, formula (9). Not a PMM solver,
# so porting does not violate the "never reimplement PMM solvers" rule.
# =============================================================================
.ll_tn <- function(par, y, X, negative = TRUE) {
  p    <- ncol(X)
  beta <- par[seq_len(p)]
  eta  <- exp(par[p + 1])
  lam  <- par[p + 2]
  if (eta <= 0 || lam < 0) return(if (negative) Inf else -Inf)
  z  <- (y - as.vector(X %*% beta)) / eta
  n  <- length(y)
  lz <- lam * z
  log_cosh_sum <- sum(abs(lz) + log1p(exp(-2 * abs(lz))) - log(2))
  ll <- -n * log(2 * pi) / 2 - n * log(eta) -
        n * lam^2 / 2 - sum(z^2) / 2 + log_cosh_sum
  if (negative) -ll else ll
}

mle_tn <- function(formula, data = NULL, lambda_init = 1.0, maxit = 500) {
  cl <- match.call()
  mf <- model.frame(formula, data = data)
  y  <- model.response(mf)
  X  <- model.matrix(attr(mf, "terms"), data = mf)
  n  <- length(y); p <- ncol(X)
  ols  <- lm.fit(X, y)
  par0 <- c(coef(ols), log(sd(ols$residuals)), lambda_init)
  opt <- optim(par = par0, fn = .ll_tn, y = y, X = X, negative = TRUE,
               method = "L-BFGS-B",
               lower = c(rep(-Inf, p), -6, 0.001),
               upper = c(rep( Inf, p),  6, 15.0),
               control = list(maxit = maxit, factr = 1e7), hessian = TRUE)
  se <- tryCatch(sqrt(pmax(diag(solve(opt$hessian)), 0)),
                 error = function(e) rep(NA_real_, length(par0)))
  beta_hat   <- setNames(opt$par[seq_len(p)], colnames(X))
  fitted_val <- as.vector(X %*% beta_hat)
  structure(list(
    call = cl, terms = attr(mf, "terms"), coefficients = beta_hat,
    eta = unname(exp(opt$par[p + 1])), lambda = unname(opt$par[p + 2]),
    loglik = -opt$value, se_beta = se[seq_len(p)],
    converged = (opt$convergence == 0),
    aic = 2 * (p + 2) + 2 * opt$value, bic = log(n) * (p + 2) + 2 * opt$value,
    n_params = p + 2, n = n, fitted = fitted_val, residuals = y - fitted_val
  ), class = "mle_tn_fit")
}
coef.mle_tn_fit    <- function(object, ...) object$coefficients
predict.mle_tn_fit <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted)
  X_new <- model.matrix(delete.response(object$terms), newdata)
  as.vector(X_new %*% object$coefficients)
}

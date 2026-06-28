# =============================================================================
# R/metrics.R — beta-space + prediction metrics, paired (CRN) aggregation,
# covariance-transfer (Proposition 2) ratio, bootstrap CIs.
# =============================================================================

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)
slope_norm <- function(coef)
  if (length(coef) <= 1 || anyNA(coef)) NA_real_ else sqrt(sum(coef[-1]^2))

# Per-estimate beta-MSE (slopes only); coef is intercept-first, beta_true is slopes
beta_mse <- function(coef, beta_true) {
  if (anyNA(coef)) return(NA_real_)
  mean((coef[-1] - beta_true)^2)
}

# List of intercept-first coef vectors -> reps x p slopes matrix
slopes_matrix <- function(coef_list) {
  do.call(rbind, lapply(coef_list, function(co)
    if (anyNA(co)) rep(NA_real_, length(co) - 1L) else co[-1]))
}

trace_var_slope <- function(slopes) {
  slopes <- slopes[stats::complete.cases(slopes), , drop = FALSE]
  if (nrow(slopes) < 2) return(NA_real_)
  sum(apply(slopes, 2, stats::var))
}

bias2_variance_split <- function(slopes, beta_true) {
  slopes <- slopes[stats::complete.cases(slopes), , drop = FALSE]
  if (nrow(slopes) < 2) return(list(bias2 = NA_real_, variance = NA_real_, emse = NA_real_))
  mean_b   <- colMeans(slopes)
  bias2    <- sum((mean_b - beta_true)^2)
  variance <- sum(apply(slopes, 2, stats::var))
  list(bias2 = bias2, variance = variance, emse = bias2 + variance)
}

# Sign-flip rate vs a reference sign vector (synthetic: sign(beta_true))
sign_flip_rate <- function(slopes, ref_sign) {
  slopes <- slopes[stats::complete.cases(slopes), , drop = FALSE]
  if (!nrow(slopes)) return(NA_real_)
  mean(sweep(sign(slopes), 2, ref_sign, FUN = function(a, b) a != b))
}

# Mean Kendall tau of |slope| ranking vs a reference (synthetic: |beta_true|)
rank_stability_kendall <- function(slopes, ref_abs) {
  slopes <- slopes[stats::complete.cases(slopes), , drop = FALSE]
  if (nrow(slopes) < 2 || length(ref_abs) < 2 || stats::sd(ref_abs) == 0)
    return(NA_real_)   # constant reference (e.g. equal_positive beta) has no ranking
  taus <- apply(abs(slopes), 1, function(r)
    suppressWarnings(stats::cor(r, ref_abs, method = "kendall")))
  mean(taus, na.rm = TRUE)
}

# Proposition 2: trace(empCov(beta_PMM_RTE)) / trace(empCov(beta_RTE)) at fixed (k,g)
covariance_transfer_ratio <- function(slopes_pmm_rte, slopes_rte) {
  a <- trace_var_slope(slopes_pmm_rte); b <- trace_var_slope(slopes_rte)
  if (!is.finite(a) || !is.finite(b) || b <= 0) return(NA_real_)
  a / b
}

# Paired (CRN) comparison: per-rep ratio A/B with bootstrap CI + Wilcoxon sign test
paired_ratio <- function(metric_A, metric_B, B = 2000, seed = NULL) {
  ok <- is.finite(metric_A) & is.finite(metric_B) & metric_B > 0
  r  <- metric_A[ok] / metric_B[ok]
  if (length(r) < 2)
    return(list(median = NA_real_, lo = NA_real_, hi = NA_real_,
                p_wilcox = NA_real_, n = length(r)))
  if (!is.null(seed)) set.seed(seed)
  boots <- replicate(B, median(sample(r, length(r), replace = TRUE)))
  pw <- tryCatch(suppressWarnings(stats::wilcox.test(metric_A[ok] - metric_B[ok])$p.value),
                 error = function(e) NA_real_)
  list(median = stats::median(r),
       lo = unname(stats::quantile(boots, 0.025)),
       hi = unname(stats::quantile(boots, 0.975)),
       p_wilcox = pw, n = length(r))
}

# Residual-cumulant diagnostics (robust, bootstrapped) — reused for real-data
residual_g_factors <- function(residuals) {
  m  <- EstemPMM::compute_moments(residuals)
  m3 <- EstemPMM::compute_moments_pmm3(residuals)
  list(gamma3 = m$c3, gamma4 = m$c4, gamma6 = m3$gamma6, g2 = m$g, g3 = m3$g3)
}

# Percentile bootstrap CIs for slope coefficients of any base (reuse boot_base_coefs)
boot_ci_slopes <- function(base_method, x, y, B = CFG$ci$B, level = 0.95, seed = NULL) {
  Bc <- boot_base_coefs(base_method, x, y, B, seed)
  if (is.null(Bc) || nrow(Bc) < 10) return(NULL)
  a <- (1 - level) / 2
  slopes <- Bc[, -1, drop = FALSE]
  data.frame(term  = paste0("x", seq_len(ncol(slopes))),
             lo    = apply(slopes, 2, stats::quantile, a),
             hi    = apply(slopes, 2, stats::quantile, 1 - a),
             width = apply(slopes, 2, function(z) diff(stats::quantile(z, c(a, 1 - a)))),
             stringsAsFactors = FALSE)
}

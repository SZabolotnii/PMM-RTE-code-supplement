# =============================================================================
# R/tuning.R — admissible grid (DEFECT-1 fix), CV / bootstrap-stability / oracle
# Replaces the slope-norm proxy with the SPEC §6.3 constrained criterion:
#   minimize trace Var_boot(beta_slope)  s.t.  val_RMSE <= best_RMSE*(1+delta)
# Efficiency: B base refits ONCE, reused across all (k,g) via the linear L_RTE.
# =============================================================================

na_tune <- function() list(k = NA_real_, g = NA_real_, score = NA_real_,
                           slope_norm = NA_real_, tr_var = NA_real_,
                           boundary_hit = NA, n_admissible = 0L)

# Parse base / transform / tuning from a method name
method_spec <- function(m) {
  base <- if (grepl("^PMM2", m)) "PMM2"
          else if (grepl("^PMM3", m)) "PMM3"
          else if (grepl("^MLE_TN", m)) "MLE_TN" else "OLS"
  transform <- if (grepl("Ridge", m)) "ridge" else if (grepl("RTE", m)) "rte" else "none"
  tuning <- if (grepl("Oracle", m)) "oracle"
            else if (grepl("Stable", m)) "stable"
            else if (grepl("CV", m)) "cv" else "none"
  list(method = m, base = base, transform = transform, tuning = tuning)
}

# Admissible (k,g) candidates: drop any with an out-of-[0,1] eigen-shrinkage s_j.
# lambda_j is computed ONCE per design (it does not depend on (k,g)); only s_j does.
admissible_grid <- function(x, k_grid, g_spec) {
  Xs  <- scale(as.matrix(x), center = TRUE, scale = FALSE)
  lam <- pmax(eigen(crossprod(Xs), symmetric = TRUE, only.values = TRUE)$values, 0)
  if (g_spec$type == "coupled") {
    cand <- data.frame(k = k_grid,
                       g = vapply(k_grid, function(k) g_of_k(k, g_spec$fun), numeric(1)))
    g_free <- TRUE
  } else {
    cand <- expand.grid(k = k_grid, g = g_spec$grid)
    g_free <- length(g_spec$grid) > 1L
  }
  tol <- CFG$shrink_tol
  keep <- vapply(seq_len(nrow(cand)), function(i) {
    s <- lam * (lam + cand$g[i]) / ((lam + 1) * (lam + cand$k[i]))
    all(is.finite(s)) && all(s >= -tol) && all(s <= 1 + tol)
  }, logical(1))
  cand <- cand[keep, , drop = FALSE]
  attr(cand, "g_extremes") <- if (g_free) range(cand$g) else c(NA_real_, NA_real_)
  attr(cand, "g_free") <- g_free
  cand
}

candidates_for <- function(x, transform, g_spec) {
  gs <- if (transform == "ridge") list(type = "free", grid = CFG$ridge_g) else g_spec
  admissible_grid(x, CFG$k_grid, gs)
}

.boundary_hit <- function(g_sel, cand) {
  ext <- attr(cand, "g_extremes")
  if (!isTRUE(attr(cand, "g_free")) || anyNA(ext)) return(NA)
  isTRUE(g_sel <= ext[1] + 1e-12 || g_sel >= ext[2] - 1e-12)
}

# Evaluate validation RMSE + slope norm for each candidate (base fit ONCE)
cv_candidates <- function(x_tr, y_tr, x_val, y_val, base_method, cand) {
  base_coef <- fit_base(base_method, x_tr, y_tr)
  if (anyNA(base_coef) || !nrow(cand)) return(NULL)
  out <- cand; out$rmse <- NA_real_; out$slope_norm <- NA_real_
  for (i in seq_len(nrow(cand))) {
    coef <- rte_transform(x_tr, base_coef, cand$k[i], cand$g[i])
    if (anyNA(coef)) next
    out$rmse[i] <- rmse(y_val, predict_coef(x_val, coef))
    out$slope_norm[i] <- slope_norm(coef)
  }
  out[is.finite(out$rmse), , drop = FALSE]
}

tune_cv <- function(x_tr, y_tr, x_val, y_val, base_method, cand) {
  cv <- cv_candidates(x_tr, y_tr, x_val, y_val, base_method, cand)
  if (is.null(cv) || !nrow(cv)) return(na_tune())
  sel <- cv[which.min(cv$rmse), , drop = FALSE]
  list(k = sel$k, g = sel$g, score = sel$rmse, slope_norm = sel$slope_norm,
       tr_var = NA_real_, boundary_hit = .boundary_hit(sel$g, cand),
       n_admissible = nrow(cand))
}

# Residual bootstrap of base coefficients (B refits, reused across all (k,g))
boot_base_coefs <- function(base_method, x, y, B, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  beta0 <- fit_base(base_method, x, y)
  if (anyNA(beta0)) return(NULL)
  fitted <- predict_coef(x, beta0)
  resid  <- y - fitted; resid <- resid - mean(resid)
  n <- length(y); out <- matrix(NA_real_, B, length(beta0))
  for (b in seq_len(B)) {
    yb <- fitted + sample(resid, n, replace = TRUE)
    bb <- fit_base(base_method, x, yb)
    if (!anyNA(bb)) out[b, ] <- bb
  }
  out[stats::complete.cases(out), , drop = FALSE]
}

# SPEC §6.3 constrained bootstrap-stability tuning
tune_stable_boot <- function(x_tr, y_tr, x_val, y_val, base_method, cand, B, delta,
                             boot_coefs = NULL) {
  cv <- cv_candidates(x_tr, y_tr, x_val, y_val, base_method, cand)
  if (is.null(cv) || !nrow(cv)) return(na_tune())
  best <- min(cv$rmse, na.rm = TRUE)
  S <- cv[cv$rmse <= best * (1 + delta), , drop = FALSE]
  Bc <- if (is.null(boot_coefs)) boot_base_coefs(base_method, x_tr, y_tr, B) else boot_coefs
  if (is.null(Bc) || !nrow(Bc)) return(na_tune())
  S$tr_var <- NA_real_
  for (i in seq_len(nrow(S))) {
    L <- rte_operator(x_tr, S$k[i], S$g[i]); if (is.null(L)) next
    Tb <- Bc %*% t(L)                                   # B x (p+1) transformed coefs
    S$tr_var[i] <- sum(apply(Tb[, -1, drop = FALSE], 2, stats::var))
  }
  S <- S[is.finite(S$tr_var), , drop = FALSE]
  if (!nrow(S)) return(na_tune())
  sel <- S[which.min(S$tr_var), , drop = FALSE]
  list(k = sel$k, g = sel$g, score = sel$rmse, slope_norm = sel$slope_norm,
       tr_var = sel$tr_var, boundary_hit = .boundary_hit(sel$g, cand),
       n_admissible = nrow(cand))
}

# Oracle: tune directly by beta-MSE vs known beta_true (synthetic only; G7-tagged)
tune_oracle <- function(x_tr, y_tr, beta_true, base_method, cand) {
  base_coef <- fit_base(base_method, x_tr, y_tr)
  if (anyNA(base_coef) || !nrow(cand)) return(na_tune())
  best_mse <- Inf; sel <- NULL; sn <- NA_real_
  for (i in seq_len(nrow(cand))) {
    coef <- rte_transform(x_tr, base_coef, cand$k[i], cand$g[i])
    if (anyNA(coef)) next
    mse <- mean((coef[-1] - beta_true)^2)
    if (is.finite(mse) && mse < best_mse) { best_mse <- mse; sel <- cand[i, , drop = FALSE]; sn <- slope_norm(coef) }
  }
  if (is.null(sel)) return(na_tune())
  list(k = sel$k, g = sel$g, score = best_mse, slope_norm = sn, tr_var = NA_real_,
       boundary_hit = .boundary_hit(sel$g, cand), n_admissible = nrow(cand))
}

# Dispatch tuning for one method spec
tune_method <- function(spec, x_tr, y_tr, x_val, y_val, g_spec, B, delta,
                        beta_true = NULL, boot_coefs = NULL) {
  if (spec$transform == "none" || spec$tuning == "none") return(na_tune())
  cand <- candidates_for(x_tr, spec$transform, g_spec)
  if (!nrow(cand)) return(na_tune())
  switch(spec$tuning,
    cv     = tune_cv(x_tr, y_tr, x_val, y_val, spec$base, cand),
    stable = tune_stable_boot(x_tr, y_tr, x_val, y_val, spec$base, cand, B, delta, boot_coefs),
    oracle = tune_oracle(x_tr, y_tr, beta_true, spec$base, cand),
    na_tune())
}

# Final fit on the full training sample with the tuned (k,g)
fit_final <- function(spec, x_tr, y_tr, tune = NULL) {
  if (spec$transform == "none") return(fit_base(spec$base, x_tr, y_tr))
  base_coef <- fit_base(spec$base, x_tr, y_tr)
  if (anyNA(base_coef) || is.null(tune) || is.na(tune$k))
    return(rep(NA_real_, ncol(as.matrix(x_tr)) + 1L))
  g_use <- if (spec$transform == "ridge") CFG$ridge_g else tune$g
  rte_transform(x_tr, base_coef, tune$k, g_use)
}

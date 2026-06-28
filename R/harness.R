# =============================================================================
# R/harness.R — per-scenario Monte Carlo driver (shared by scripts 01 & 02).
# CRN: every rep seeds set.seed(scenario_seed(sid, rep)) so all methods see the
# same data within a rep -> paired comparisons. Parallel-safe (seeds are internal).
# =============================================================================

# Aggregate one scenario's per-rep results into a per-(scenario,method) summary
summarize_scenario <- function(scenario, methods, results, slopes, bmse, beta_true) {
  ref_sign <- sign(beta_true); ref_abs <- abs(beta_true)
  have_rtecv <- "RTE_CV" %in% methods
  out <- lapply(methods, function(m) {
    sl <- slopes[[m]]
    bv <- bias2_variance_split(sl, beta_true)
    pr_ols <- paired_ratio(bmse[[m]], bmse[["OLS"]], B = 1000)
    pr_rte <- if (have_rtecv) paired_ratio(bmse[[m]], bmse[["RTE_CV"]], B = 1000)
              else list(median = NA_real_, lo = NA_real_, hi = NA_real_, p_wilcox = NA_real_)
    rr <- results[results$method == m, , drop = FALSE]
    data.frame(
      scenario_id = scenario$scenario_id, n = scenario$n, p = scenario$p,
      rho = scenario$rho, error = scenario$error, method = m,
      median_beta_mse = stats::median(bmse[[m]], na.rm = TRUE),
      trace_var_slope = trace_var_slope(sl), bias2 = bv$bias2,
      variance = bv$variance, emse = bv$emse,
      sign_flip_rate = sign_flip_rate(sl, ref_sign),
      rank_kendall = rank_stability_kendall(sl, ref_abs),
      median_signal_rmse = stats::median(rr$signal_rmse, na.rm = TRUE),
      median_test_rmse = stats::median(rr$test_rmse, na.rm = TRUE),
      ratio_ols_med = pr_ols$median, ratio_ols_lo = pr_ols$lo,
      ratio_ols_hi = pr_ols$hi, ratio_ols_p = pr_ols$p_wilcox,
      ratio_rtecv_med = pr_rte$median, ratio_rtecv_lo = pr_rte$lo,
      ratio_rtecv_hi = pr_rte$hi, ratio_rtecv_p = pr_rte$p_wilcox,
      boundary_hit_frac = mean(rr$boundary_hit, na.rm = TRUE),
      median_k = stats::median(rr$selected_k, na.rm = TRUE),
      median_g = stats::median(rr$selected_g, na.rm = TRUE),
      ok_rate = mean(rr$ok), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

run_mc_scenario <- function(scenario, methods, ps, g_spec, beta_pattern) {
  n <- scenario$n; p <- scenario$p; rho <- scenario$rho; err <- scenario$error
  beta_true <- make_beta(p, beta_pattern)
  specs <- lapply(methods, method_spec); names(specs) <- methods
  bases_needed <- unique(vapply(specs, function(s) s$base, character(1)))
  reps <- ps$reps; test_n <- ps$test_n; B <- ps$B_boot

  rows <- vector("list", reps * length(methods)); ri <- 0L
  slopes <- setNames(lapply(methods, function(m) matrix(NA_real_, reps, p)), methods)
  bmse   <- setNames(lapply(methods, function(m) rep(NA_real_, reps)), methods)
  pmm3_conv <- rep(NA, reps)

  for (r in seq_len(reps)) {
    set.seed(scenario_seed(scenario$sid_index, r))                       # CRN
    X  <- generate_x(n, p, rho); sig  <- as.numeric(X  %*% beta_true)
    y  <- sig  + generate_error(n, err)
    Xt <- generate_x(test_n, p, rho); sigt <- as.numeric(Xt %*% beta_true)
    yt <- sigt + generate_error(test_n, err)
    ni <- floor(CFG$inner_frac * n); idx <- sample.int(n, ni)
    xin <- X[idx, , drop = FALSE]; yin <- y[idx]
    xva <- X[-idx, , drop = FALSE]; yva <- y[-idx]
    bc <- setNames(lapply(bases_needed,
            function(b) boot_base_coefs(b, xin, yin, B)), bases_needed)

    for (m in methods) {
      s    <- specs[[m]]
      tune <- tune_method(s, xin, yin, xva, yva, g_spec, B, CFG$delta_main,
                          beta_true = beta_true, boot_coefs = bc[[s$base]])
      co   <- fit_final(s, X, y, tune)
      pred <- if (anyNA(co)) rep(NA_real_, test_n) else predict_coef(Xt, co)
      ri <- ri + 1L
      rows[[ri]] <- data.frame(
        scenario_id = scenario$scenario_id, n = n, p = p, rho = rho, error = err,
        rep = r, method = m, beta_mse = beta_mse(co, beta_true),
        signal_rmse = rmse(sigt, pred), test_rmse = rmse(yt, pred),
        selected_k = tune$k, selected_g = tune$g, tr_var = tune$tr_var,
        boundary_hit = tune$boundary_hit, n_admissible = tune$n_admissible,
        ok = !anyNA(co), stringsAsFactors = FALSE)
      if (!anyNA(co)) { slopes[[m]][r, ] <- co[-1]; bmse[[m]][r] <- beta_mse(co, beta_true) }
      if (m == "PMM3") pmm3_conv[r] <- isTRUE(attr(co, "pmm3_converged"))
    }
  }
  results <- do.call(rbind, rows[seq_len(ri)])
  summary <- summarize_scenario(scenario, methods, results, slopes, bmse, beta_true)
  summary$pmm3_conv_rate <- if ("PMM3" %in% methods) mean(pmm3_conv, na.rm = TRUE) else NA_real_
  list(results = results, summary = summary)
}

# Main MC over a block's scenario grid (asym|sym) for the active PHASE/G_MODE
run_mc_block <- function(block, methods, parallel = NULL) {
  ps <- phase_settings(); g_spec <- g_mode_spec()
  grid <- make_scenario_grid(ps$phase, block)
  if (is.null(parallel)) parallel <- identical(Sys.getenv("PARALLEL"), "1")
  runner <- function(i) {
    sc <- grid[i, ]
    message(sprintf("  [%s|%s|g=%s] %s (reps=%d, test_n=%d, B=%d)",
                    block, ps$phase, g_spec$name, sc$scenario_id, ps$reps, ps$test_n, ps$B_boot))
    run_mc_scenario(sc, methods, ps, g_spec, CFG$beta_pattern_main)
  }
  res <- if (parallel) {
    parallel::mclapply(seq_len(nrow(grid)), runner,
                       mc.cores = max(1L, parallel::detectCores() - 2L))
  } else lapply(seq_len(nrow(grid)), runner)
  list(results = do.call(rbind, lapply(res, `[[`, "results")),
       summary = do.call(rbind, lapply(res, `[[`, "summary")),
       grid = grid)
}

# Dedicated Proposition-2 covariance-transfer evidence on BENIGN (asymptotic)
# scenarios at a fixed admissible (k,g) — where PMM2 reaches its g_S efficiency.
run_covtransfer <- function(reps = 300L, order = c("pmm2", "pmm3")) {
  order <- match.arg(order); ck <- CFG$covtransfer_kg
  benign <- if (order == "pmm2")
    data.frame(n = c(300L, 500L, 200L, 500L), p = c(4L, 4L, 8L, 4L),
               rho = c(0.8, 0.8, 0.9, 0.8),
               error = c("exponential", "gamma2", "gamma2", "exponential"),
               stringsAsFactors = FALSE)
  else
    data.frame(n = c(300L, 500L, 300L), p = c(4L, 4L, 8L),
               rho = c(0.8, 0.8, 0.9),
               error = c("uniform", "tn1.5", "uniform"), stringsAsFactors = FALSE)
  benign$scenario_id <- sprintf("ct_%s_n%d_p%d_rho%.2f_%s", order,
                                benign$n, benign$p, benign$rho, benign$error)
  benign$sid_index <- 900L + seq_len(nrow(benign))
  base_fit <- if (order == "pmm2") fit_pmm2_coef else fit_pmm3_coef
  rows <- lapply(seq_len(nrow(benign)), function(i) {
    sc <- benign[i, ]; bt <- make_beta(sc$p, "equal_positive")
    sr <- matrix(NA_real_, reps, sc$p); sp <- matrix(NA_real_, reps, sc$p)
    so <- matrix(NA_real_, reps, sc$p); spbase <- matrix(NA_real_, reps, sc$p)
    gS <- numeric(reps)
    for (r in seq_len(reps)) {
      set.seed(scenario_seed(sc$sid_index, r))
      X <- generate_x(sc$n, sc$p, sc$rho); y <- as.numeric(X %*% bt) + generate_error(sc$n, sc$error)
      bo <- fit_ols_coef(X, y); bp <- base_fit(X, y)
      so[r, ] <- bo[-1]; spbase[r, ] <- bp[-1]
      sr[r, ] <- rte_transform(X, bo, ck$k, ck$g)[-1]
      sp[r, ] <- rte_transform(X, bp, ck$k, ck$g)[-1]
      res0 <- y - predict_coef(X, bo)
      gS[r] <- if (order == "pmm2") EstemPMM::compute_moments(res0)$g
               else EstemPMM::compute_moments_pmm3(res0)$g3
    }
    data.frame(scenario_id = sc$scenario_id, order = order, n = sc$n, p = sc$p,
               rho = sc$rho, error = sc$error, k_fixed = ck$k, g_fixed = ck$g,
               raw_cov_ratio = trace_var_slope(spbase) / trace_var_slope(so),
               transferred_cov_ratio = covariance_transfer_ratio(sp, sr),
               gS_emp = mean(gS, na.rm = TRUE), reps = reps, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

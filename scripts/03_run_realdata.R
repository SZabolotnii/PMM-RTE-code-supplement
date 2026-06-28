#!/usr/bin/env Rscript
# 03_run_realdata.R — SPEC Block E (real-data coefficient-stability, supporting
# evidence) + Block D (Portland/Hald illustrative). Reads data/raw CSVs.
# Env: REALDATA_REPS (default 120), G_MODE (default deployable).
# Emits (results/): realdata_results.csv, realdata_coefficients.csv,
#   realdata_summary.csv, realdata_ratio_summary.csv, portland_diag.csv,
#   realdata_report.md.

suppressPackageStartupMessages(library(EstemPMM))
source("R/config.R"); invisible(load_project(verbose = FALSE)); ensure_dirs()

reps      <- as.integer(Sys.getenv("REALDATA_REPS", "120"))
B_boot    <- as.integer(Sys.getenv("REALDATA_B", "80"))
seed      <- CFG$seed_base
g_spec    <- g_mode_spec()
set.seed(seed)
DATA_RAW  <- CFG$paths$data_raw

load_local <- function(f) {
  path <- file.path(DATA_RAW, f)
  if (!file.exists(path)) stop("Missing dataset: ", path, call. = FALSE)
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

candidate_specs <- function() list(
  list(id = "college_expend", label = "ISLR2::College / Expend",
       data = load_local("ISLR2_College.csv"),
       formula = Expend ~ Apps + Accept + Enroll + Top10perc + Top25perc +
         F.Undergrad + P.Undergrad + Outstate + Room.Board + Books +
         Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Grad.Rate),
  list(id = "remifentanil_conc", label = "nlme::Remifentanil / conc",
       data = load_local("nlme_Remifentanil.csv"),
       formula = conc ~ Time + Rate + Amt + Age + Wt + Ht + LBM + BSA),
  list(id = "instinnovation_value", label = "sandwich::InstInnovation / value",
       data = load_local("sandwich_InstInnovation.csv"),
       formula = value ~ cites + patents + precites + randd + sales + employment +
         capital + competition + competition4 + acompetition + tobinq +
         institutions + dedicated + transient + quasiindexed))

make_model_frame <- function(spec, scale_name) {
  mf <- model.frame(spec$formula, data = spec$data, na.action = na.omit)
  yname <- names(mf)[1]; yraw <- mf[[yname]]
  if (scale_name == "log1p") {
    if (any(yraw <= -1, na.rm = TRUE)) return(NULL)
    mf[[yname]] <- log1p(yraw)
  }
  attr(mf, "y_raw") <- yraw; mf
}

standardize_split <- function(train_mf, test_mf, formula) {
  Xtr <- model.matrix(formula, train_mf); Xte <- model.matrix(formula, test_mf)
  yname <- names(train_mf)[1]
  cols <- setdiff(colnames(Xtr), "(Intercept)")
  ctr <- colMeans(Xtr[, cols, drop = FALSE]); sds <- apply(Xtr[, cols, drop = FALSE], 2, sd)
  keep <- is.finite(sds) & sds > 1e-12; cols <- cols[keep]; ctr <- ctr[keep]; sds <- sds[keep]
  scal <- function(X) sweep(sweep(X[, cols, drop = FALSE], 2, ctr, "-"), 2, sds, "/")
  list(x_train = scal(Xtr), y_train = train_mf[[yname]],
       x_test = scal(Xte), y_test = test_mf[[yname]], terms = cols)
}

run_dataset <- function(spec, scale_name) {
  mf <- make_model_frame(spec, scale_name); if (is.null(mf)) return(NULL)
  y_raw <- attr(mf, "y_raw"); n <- nrow(mf); yname <- names(mf)[1]
  diag_fit <- lm(spec$formula, data = mf)
  gf <- residual_g_factors(residuals(diag_fit))
  Xd <- model.matrix(spec$formula, data = mf)
  cond <- kappa(scale(Xd[, colnames(Xd) != "(Intercept)", drop = FALSE]), exact = TRUE)
  disp <- dispatch_base(residuals(diag_fit), n)

  methods <- c("OLS", "PMM2", "PMM3", "RTE_CV", "RTE_Stable",
               "PMM2_RTE_Stable", "PMM3_RTE_Stable")
  specs <- lapply(methods, method_spec); names(specs) <- methods
  res_rows <- list(); coef_rows <- list(); ri <- 0L; ci <- 0L

  for (r in seq_len(reps)) {
    set.seed(seed + r)
    tr_idx <- sample.int(n, floor(0.80 * n)); te_idx <- setdiff(seq_len(n), tr_idx)
    tr_mf <- mf[tr_idx, , drop = FALSE]; te_mf <- mf[te_idx, , drop = FALSE]
    sp <- standardize_split(tr_mf, te_mf, spec$formula)
    x_tr <- sp$x_train; y_tr <- sp$y_train; x_te <- sp$x_test
    yraw_te <- y_raw[te_idx]
    ni <- floor(CFG$inner_frac * nrow(x_tr)); inn <- sample.int(nrow(x_tr), ni)
    xin <- x_tr[inn, , drop = FALSE]; yin <- y_tr[inn]
    xva <- x_tr[-inn, , drop = FALSE]; yva <- y_tr[-inn]
    bc <- list(OLS = boot_base_coefs("OLS", xin, yin, B_boot),
               PMM2 = boot_base_coefs("PMM2", xin, yin, B_boot),
               PMM3 = boot_base_coefs("PMM3", xin, yin, B_boot))
    for (m in methods) {
      s <- specs[[m]]
      tune <- tune_method(s, xin, yin, xva, yva, g_spec, B_boot, CFG$delta_main,
                          boot_coefs = bc[[s$base]])
      co <- fit_final(s, x_tr, y_tr, tune)
      pred_model <- if (anyNA(co)) rep(NA_real_, length(te_idx)) else predict_coef(x_te, co)
      pred_orig  <- if (scale_name == "log1p") expm1(pred_model) else pred_model
      ri <- ri + 1L
      res_rows[[ri]] <- data.frame(
        dataset_id = spec$id, dataset = spec$label, scale = scale_name, rep = r,
        method = m, n_total = n, rmse_original = rmse(yraw_te, pred_orig),
        mae_original = mae(yraw_te, pred_orig), selected_k = tune$k,
        selected_g = tune$g, boundary_hit = tune$boundary_hit, ok = !anyNA(co),
        stringsAsFactors = FALSE)
      if (!anyNA(co)) for (j in seq_along(sp$terms)) {
        ci <- ci + 1L
        coef_rows[[ci]] <- data.frame(dataset_id = spec$id, scale = scale_name, rep = r,
          method = m, term = sp$terms[j], estimate = co[j + 1L], stringsAsFactors = FALSE)
      }
    }
  }
  list(results = do.call(rbind, res_rows), coefs = do.call(rbind, coef_rows),
       diag = data.frame(dataset_id = spec$id, dataset = spec$label, scale = scale_name,
         n = n, cond = cond, gamma3 = gf$gamma3, gamma4 = gf$gamma4, g2 = gf$g2,
         g3 = gf$g3, dispatch = disp$base, dispatch_status = disp$status,
         stringsAsFactors = FALSE))
}

cat(sprintf("[03] real-data | reps=%d B=%d g_mode=%s\n", reps, B_boot, g_spec$name))
all_res <- list(); all_coef <- list(); all_diag <- list(); idx <- 0L
for (spec in candidate_specs()) for (sc in c("raw", "log1p")) {
  cat(sprintf("  %s [%s]\n", spec$label, sc))
  out <- run_dataset(spec, sc); if (is.null(out)) next
  idx <- idx + 1L; all_res[[idx]] <- out$results; all_coef[[idx]] <- out$coefs
  all_diag[[idx]] <- out$diag
}
results <- do.call(rbind, all_res); coefs <- do.call(rbind, all_coef)
diags   <- do.call(rbind, all_diag)
write_csv_stamped(results, "realdata_results.csv")
write_csv_stamped(coefs, "realdata_coefficients.csv")

# Per dataset x scale x method: slope-variance trace + median RMSE -----------
coef_var   <- aggregate(estimate ~ dataset_id + scale + method + term, coefs,
                        FUN = function(z) var(z, na.rm = TRUE))
slope_trace <- aggregate(estimate ~ dataset_id + scale + method, coef_var,
                         FUN = function(z) sum(z, na.rm = TRUE))
names(slope_trace)[names(slope_trace) == "estimate"] <- "slope_var_trace"
metric <- aggregate(cbind(rmse_original, mae_original) ~ dataset_id + scale + method,
                    results, FUN = function(z) median(z, na.rm = TRUE))
ok_rate <- aggregate(ok ~ dataset_id + scale + method, results, FUN = mean)
summary_df <- Reduce(function(a, b) merge(a, b, all.x = TRUE),
  list(metric, slope_trace, ok_rate, diags[, c("dataset_id", "scale", "cond",
       "gamma3", "gamma4", "g2", "g3", "dispatch", "dispatch_status")]))
summary_df <- summary_df[order(summary_df$dataset_id, summary_df$scale, summary_df$method), ]
write_csv_stamped(summary_df, "realdata_method_summary.csv")

# Paired ratios vs OLS / RTE_CV on rmse_original -----------------------------
ratio_rows <- list(); k <- 0L
for (ds in unique(results$dataset_id)) for (sc in unique(results$scale)) {
  sub <- results[results$dataset_id == ds & results$scale == sc, ]
  w <- reshape(sub[, c("rep", "method", "rmse_original")], idvar = "rep",
               timevar = "method", direction = "wide")
  names(w) <- sub("rmse_original\\.", "", names(w))
  for (m in setdiff(unique(sub$method), "OLS")) {
    if (!all(c(m, "OLS", "RTE_CV") %in% names(w))) next
    k <- k + 1L
    ratio_rows[[k]] <- data.frame(dataset_id = ds, scale = sc, method = m,
      ratio_to_ols = median(w[[m]] / w[["OLS"]], na.rm = TRUE),
      ratio_to_rtecv = median(w[[m]] / w[["RTE_CV"]], na.rm = TRUE),
      stringsAsFactors = FALSE)
  }
}
write_csv_stamped(do.call(rbind, ratio_rows), "realdata_ratio_summary.csv")

# --- Block D: Portland/Hald (MASS::cement, n=13) — ILLUSTRATIVE only ---------
portland <- local({
  cem <- load_local("MASS_cement_Hald_Portland.csv")
  names(cem) <- make.names(names(cem))
  ycol <- grep("^y$|^Y$|heat", names(cem), ignore.case = TRUE, value = TRUE)[1]
  xcol <- setdiff(names(cem), ycol)
  form <- as.formula(paste(ycol, "~", paste(xcol, collapse = " + ")))
  fit <- lm(form, cem); gf <- residual_g_factors(residuals(fit))
  Xc <- model.matrix(form, cem)
  cond <- kappa(scale(Xc[, colnames(Xc) != "(Intercept)", drop = FALSE]), exact = TRUE)
  Xs <- scale(Xc[, colnames(Xc) != "(Intercept)", drop = FALSE])
  ci <- boot_ci_slopes("OLS", Xs, cem[[ycol]], B = 1000, level = 0.95, seed = seed)
  loo <- t(vapply(seq_len(nrow(cem)), function(i)
    fit_ols_coef(Xs[-i, , drop = FALSE], cem[[ycol]][-i])[-1], numeric(ncol(Xs))))
  data.frame(dataset = "MASS::cement (Portland/Hald)", n = nrow(cem), cond = cond,
    gamma3 = gf$gamma3, gamma4 = gf$gamma4, g2 = gf$g2, g3 = gf$g3,
    var_reduction_pmm3 = 1 - gf$g3, mean_boot_ci_width = mean(ci$width),
    loo_slope_var_trace = sum(apply(loo, 2, var)), status = "illustrative",
    stringsAsFactors = FALSE)
})
write_csv_stamped(portland, "portland_diag.csv")

# --- compact markdown report ------------------------------------------------
local({
  path <- file.path(CFG$paths$results, "realdata_report.md")
  con <- file(path, open = "wt"); on.exit(close(con))
  wl <- function(...) writeLines(sprintf(...), con)
  wl("# PMM-RTE real-data report (Block E supporting + Block D illustrative)")
  writeLines("", con)
  wl("Generated: %s | reps=%d B=%d g_mode=%s | v%s",
     format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), reps, B_boot, g_spec$name, CFG$est_version)
  writeLines("", con)
  for (ds in unique(summary_df$dataset_id)) {
    b <- summary_df[summary_df$dataset_id == ds, ]
    wl("## %s", b$dataset_id[1]); writeLines("", con)
    for (sc in unique(b$scale)) {
      bb <- b[b$scale == sc, ]; d <- bb[1, ]
      wl("### scale=%s  (cond=%.1f, gamma3=%.3f, gamma4=%.3f, g2=%.3f, g3=%.3f, dispatch=%s/%s)",
         sc, d$cond, d$gamma3, d$gamma4, d$g2, d$g3, d$dispatch, d$dispatch_status)
      writeLines("| method | median RMSE | slope-var trace | ok |", con)
      writeLines("|---|---:|---:|---:|", con)
      for (i in seq_len(nrow(bb)))
        writeLines(sprintf("| %s | %.5g | %.5g | %.2f |", bb$method[i],
          bb$rmse_original[i], bb$slope_var_trace[i], bb$ok[i]), con)
      writeLines("", con)
    }
  }
  wl("## Portland/Hald (illustrative, n=%d)", portland$n)
  wl("- cond=%.3g, g2=%.3f, g3=%.3f, potential PMM3 variance reduction 1-g3=%.3f",
     portland$cond, portland$g2, portland$g3, portland$var_reduction_pmm3)
  wl("- mean bootstrap CI width=%.3g, LOO slope-var trace=%.3g (illustrative, no PMM claim; n=13)",
     portland$mean_boot_ci_width, portland$loo_slope_var_trace)
  message("  wrote ", path)
})
cat("[03] done.\n")

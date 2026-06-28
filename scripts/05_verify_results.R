#!/usr/bin/env Rscript
# 05_verify_results.R — assert SPEC §10 acceptance gates (+ defect-1 & Prop-2)
# on the stored summary CSVs. Robust to absent inputs (SKIP). Exits non-zero if a
# HARD gate fails. After the manuscript is frozen, per-cell number checks (the
# Ku-CIT verify_reported_values idiom) are appended below the gate block.

suppressPackageStartupMessages(library(EstemPMM))
source("R/config.R"); invisible(load_project(verbose = FALSE))
RES <- CFG$paths$results
rd  <- function(f) { p <- file.path(RES, f); if (file.exists(p)) read.csv(p, check.names = FALSE) else NULL }

PASS <- 0L; HARD <- 0L; SOFT <- 0L; LOG <- character(0)
chk <- function(label, ok, hard = TRUE) {
  ok <- isTRUE(ok)
  tag <- if (ok) "PASS" else if (hard) "FAIL" else "WARN"
  if (ok) PASS <<- PASS + 1L else if (hard) HARD <<- HARD + 1L else SOFT <<- SOFT + 1L
  line <- sprintf("  [%s] %s", tag, label); LOG <<- c(LOG, line); cat(line, "\n")
}
skip <- function(label) { line <- sprintf("  [SKIP] %s", label); LOG <<- c(LOG, line); cat(line, "\n") }
med  <- function(df, meth, errs, col)
  if (is.null(df)) NA_real_ else stats::median(df[df$method == meth & df$error %in% errs, col], na.rm = TRUE)

asym <- rd("synthetic_asym_summary.csv"); sym <- rd("synthetic_sym_summary.csv")
cta  <- rd("covtransfer_asym.csv");       cts <- rd("covtransfer_sym.csv")
rsum <- rd("realdata_method_summary.csv")
cat("=== PMM-RTE verification gates ===\n")

# G1 — reproducibility stamp
if (!is.null(asym))
  chk("G1 stamp columns present (est_version/phase/seed_base)",
      all(c("est_version", "phase", "seed_base") %in% names(asym))) else skip("G1 (no asym summary)")

# G2 — Gaussian fallback (PMM not harmful / no false gain on normal)
if (!is.null(asym)) {
  chk("G2a PMM2/OLS on normal in [0.85,1.15]",
      { v <- med(asym, "PMM2", "normal", "ratio_ols_med"); is.finite(v) && v >= 0.85 && v <= 1.15 })
  chk("G2b PMM2_RTE_Stable/RTE_CV on normal <= 1.15 (no harm)",
      { v <- med(asym, "PMM2_RTE_Stable", "normal", "ratio_rtecv_med"); is.finite(v) && v <= 1.15 })
} else skip("G2 (no asym summary)")

# G4 — asymmetric gain (HARD headline)
if (!is.null(asym)) {
  v <- med(asym, "PMM2_RTE_Stable", c("gamma2", "exponential"), "ratio_rtecv_med")
  chk(sprintf("G4 median PMM2_RTE_Stable/RTE_CV on {gamma2,exp} = %.3f < 0.85", v),
      is.finite(v) && v < 0.85, hard = TRUE)
} else skip("G4 (no asym summary)")

# Defect-1 — deployable g>=0, no negative-shrinkage selection (HARD)
if (!is.null(asym)) {
  rte <- asym[grepl("RTE", asym$method) & is.finite(asym$median_g), ]
  chk("Defect-1 all RTE median_g >= 0 (no sign-flip region)",
      nrow(rte) > 0 && min(rte$median_g) >= -1e-9, hard = TRUE)
  cat(sprintf("       (boundary g-frac: mean=%.2f over RTE methods)\n",
              mean(asym$boundary_hit_frac[grepl("RTE", asym$method)], na.rm = TRUE)))
} else skip("Defect-1 (no asym summary)")

# Proposition-2 — covariance transfer (HARD, on benign scenarios)
ct <- rbind(cta, cts)
if (!is.null(ct)) {
  ok_trans <- with(ct, all(abs(transferred_cov_ratio - gS_emp) < 0.10, na.rm = TRUE))
  ok_raw   <- with(ct, all(abs(raw_cov_ratio - transferred_cov_ratio) < 0.05, na.rm = TRUE))
  chk("Prop-2 |transferred ratio - g_S| < 0.10 (all benign scenarios)", ok_trans, hard = TRUE)
  chk("Prop-2 raw ~ transferred (operator transfers covariance)", ok_raw, hard = TRUE)
} else skip("Prop-2 (no covtransfer)")

# G5 — symmetric platykurtic gain (soft for the compact PMM3 block)
if (!is.null(sym)) {
  v <- med(sym, "PMM3_RTE_Stable", c("uniform", "tn1.5"), "ratio_rtecv_med")
  chk(sprintf("G5 median PMM3_RTE_Stable/RTE_CV on {uniform,tn1.5} = %.3f < 0.90", v),
      is.finite(v) && v < 0.90, hard = FALSE)
} else skip("G5 (no sym summary)")

# G6 — real-data practical stability (soft)
if (!is.null(rsum)) {
  hit <- FALSE
  for (ds in unique(rsum$dataset_id)) for (sc in unique(rsum$scale)) {
    b <- rsum[rsum$dataset_id == ds & rsum$scale == sc, ]
    ols <- b[b$method == "OLS", ]; pm <- b[b$method == "PMM2", ]
    if (nrow(ols) && nrow(pm) && is.finite(ols$slope_var_trace) && ols$slope_var_trace > 0) {
      red <- 1 - pm$slope_var_trace / ols$slope_var_trace
      pen <- pm$rmse_original / ols$rmse_original - 1
      if (is.finite(red) && is.finite(pen) && red >= 0.10 && pen <= 0.05) hit <- TRUE
    }
  }
  chk("G6 >=1 real dataset with slope-var trace down >=10% and RMSE penalty <=5%", hit, hard = FALSE)
} else skip("G6 (no real-data summary)")

# G7 — no oracle leakage (oracle is a lower bound, must stay reference-only)
if (!is.null(asym)) {
  orc <- asym[grepl("Oracle", asym$method), ]
  chk("G7 oracle present only as reference (median beta-MSE <= matched Stable)",
      nrow(orc) == 0 ||
      all(med(asym, "PMM2_RTE_Oracle", c("gamma2", "exponential"), "median_beta_mse") <=
          med(asym, "PMM2_RTE_Stable", c("gamma2", "exponential"), "median_beta_mse"),
          na.rm = TRUE), hard = FALSE)
} else skip("G7 (no asym summary)")

cat(sprintf("\n=== %d PASS, %d HARD-FAIL, %d WARN ===\n", PASS, HARD, SOFT))
writeLines(c(sprintf("PMM-RTE verification — %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
             LOG, sprintf("%d PASS, %d HARD-FAIL, %d WARN", PASS, HARD, SOFT)),
           file.path(RES, "verify_log.txt"))
if (HARD > 0L) stop(sprintf("Verification failed: %d hard gate(s).", HARD), call. = FALSE)
cat("All hard gates passed.\n")

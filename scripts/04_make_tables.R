#!/usr/bin/env Rscript
# 04_make_tables.R — auto-generate Tables 1-5 (.tex) and Figure 1-4 data (.csv)
# from result CSVs only (numbers never hand-edited). Robust to missing inputs.

suppressPackageStartupMessages(library(EstemPMM))
source("R/config.R"); invisible(load_project(verbose = FALSE)); ensure_dirs()
RES <- CFG$paths$results
rd <- function(f) { p <- file.path(RES, f); if (file.exists(p)) read.csv(p, check.names = FALSE) else NULL }

# --- Table 1: relation to REVSTAT estimators (static) -----------------------
t1 <- data.frame(
  Estimator = c("OLS", "Ridge", "RTE", "PMM2", "PMM3", "PMM2-RTE", "PMM3-RTE"),
  Base = c("OLS", "OLS", "OLS", "PMM2", "PMM3", "PMM2", "PMM3"),
  Shrinkage = c("--", "$g{=}1$", "$L_{RTE}(k,g)$", "--", "--",
                "$L_{RTE}(k,g)$", "$L_{RTE}(k,g)$"),
  `Error assumption` = c("Gaussian", "Gaussian", "Gaussian", "asymmetric",
                         "symmetric platykurtic", "asymmetric", "symmetric platykurtic"),
  Tuning = c("--", "$k$ (CV)", "$k,g$ stability", "--", "--",
             "$k,g$ stability", "$k,g$ stability"),
  Deployable = c("yes", "yes", "yes", "yes", "yes", "yes", "yes"),
  check.names = FALSE)
write_tex(t1, "T1_estimators.tex", caption = "PMM-RTE estimators in relation to the REVSTAT RTE family.",
          label = "estimators", digits = 3, align = "llllll")

# --- Table 2: theoretical PMM efficiency factors ----------------------------
cat("[04] computing theory factor catalog (large-sample) ...\n")
catalog <- dgp_catalog(n_big = 1e6)
write_csv_stamped(catalog, "theory_factor_catalog.csv")
write_tex(build_theory_factor_table(catalog), "T2_theory_factors.tex",
          caption = "Theoretical cumulants and PMM variance-reduction factors $g_2,g_3$ by error family.",
          label = "theory", digits = 3, fit_width = TRUE)

# Representative regime cells for the MAIN-paper tables: the hardest multicollinear
# block (max p) at two bracketing regimes -- small-n/extreme-collinearity and the
# large-n/asymptotic case. The complete factorial grid lives in the supplement CSVs.
# min/max selection is adaptive (self-tests on the screening grid; targets the final
# grid's {n100,rho0.99} and {n500,rho0.90} cells) and falls back to the full set.
main_cells <- function(s) {
  if (!all(c("n", "p", "rho") %in% names(s))) return(s)
  sel <- s$p == max(s$p, na.rm = TRUE) &
    ((s$n == min(s$n, na.rm = TRUE) & s$rho == max(s$rho, na.rm = TRUE)) |
     (s$n == max(s$n, na.rm = TRUE) & s$rho == min(s$rho, na.rm = TRUE)))
  if (any(sel)) s[sel, ] else s
}

# --- Table 3: synthetic MC, asymmetric (headline deployable methods) ---------
mk_synth_table <- function(summary, methods, file, caption, label) {
  if (is.null(summary)) return(invisible(NULL))
  s <- summary[summary$method %in% methods, ]
  s <- main_cells(s)
  s <- s[order(s$error, s$n, s$rho), ]
  tab <- data.frame(
    Scenario = s$scenario_id, Method = s$method,
    `median beta-MSE` = signif(s$median_beta_mse, 4),
    `ratio /RTE_CV` = sprintf("%.3f [%.3f, %.3f]", s$ratio_rtecv_med, s$ratio_rtecv_lo, s$ratio_rtecv_hi),
    `trace Var(beta)` = signif(s$trace_var_slope, 4),
    `sign-flip` = round(s$sign_flip_rate, 3),
    `boundary g-frac` = round(s$boundary_hit_frac, 2),
    check.names = FALSE)
  write_tex(tab, file, caption = caption, label = label, digits = 4, fit_width = TRUE)
}
mk_synth_table(rd("synthetic_asym_summary.csv"),
  c("OLS", "PMM2", "RTE_CV", "RTE_Stable", "PMM2_RTE_Stable", "PMM2_RTE_Oracle"),
  "T3_synth_asymmetric.tex",
  "Synthetic Monte Carlo, asymmetric errors: paired $\\beta$-MSE ratios vs RTE\\_CV under CRN, representative high-collinearity regimes (visualized in Figure~\\ref{fig:asym}).",
  "synth-asym")

# --- Table 4: synthetic MC, symmetric platykurtic (PMM3) --------------------
mk_synth_table(rd("synthetic_sym_summary.csv"),
  c("OLS", "PMM3", "MLE_TN", "RTE_Stable", "PMM3_RTE_Stable", "PMM3_RTE_Oracle"),
  "T4_synth_symmetric.tex",
  "Synthetic Monte Carlo, symmetric platykurtic errors (PMM$_3$-RTE), representative high-collinearity regimes (visualized in Figure~\\ref{fig:sym}).",
  "synth-sym")

# --- Covariance-transfer (Proposition 2) table ------------------------------
ct <- rbind(rd("covtransfer_asym.csv"), rd("covtransfer_sym.csv"))
if (!is.null(ct)) {
  ctab <- data.frame(Scenario = ct$scenario_id, Order = ct$order,
    `raw Cov ratio` = round(ct$raw_cov_ratio, 3),
    `transferred ratio` = round(ct$transferred_cov_ratio, 3),
    `empirical g_S` = round(ct$gS_emp, 3), check.names = FALSE)
  write_tex(ctab, "T2b_covariance_transfer.tex",
    caption = "Proposition 2: covariance transfer at fixed $(k,g)$ on benign scenarios.",
    label = "covtransfer", digits = 3, fit_width = TRUE)
}

# --- Table 5: real-data coefficient stability -------------------------------
rsum <- rd("realdata_method_summary.csv"); rrat <- rd("realdata_ratio_summary.csv")
if (!is.null(rsum)) {
  t5 <- rsum[, c("dataset_id", "scale", "method", "rmse_original", "slope_var_trace",
                 "dispatch", "g2", "g3")]
  write_tex(t5, "T5_realdata_stability.tex",
    caption = "Real-data coefficient-stability (slope-variance trace) and dispatch (visualized in Figure~\\ref{fig:realdata}).",
    label = "realdata", digits = 4, fit_width = TRUE)
}

# --- Figure data ------------------------------------------------------------
utils::write.csv(emit_eigen_curve(),
  file.path(CFG$paths$figures, "fig1_eigen_shrinkage.csv"), row.names = FALSE)
asym <- rd("synthetic_asym_summary.csv")
if (!is.null(asym))
  utils::write.csv(emit_tradeoff_data(asym),
    file.path(CFG$paths$figures, "fig3_tradeoff.csv"), row.names = FALSE)

# Base-R figures (no ggplot2 dependency) -------------------------------------
fig1 <- emit_eigen_curve()
grDevices::pdf(file.path(CFG$paths$figures, "fig1_eigen_shrinkage.pdf"), width = 6, height = 4)
plot(NA, xlim = range(fig1$lambda), ylim = c(0, 1.1), log = "x",
     xlab = expression(lambda[j]), ylab = expression(s[j](k, g)),
     main = "RTE eigen-shrinkage factor")
kg <- unique(fig1[, c("k", "g")])
for (i in seq_len(nrow(kg))) {
  d <- fig1[fig1$k == kg$k[i] & fig1$g == kg$g[i], ]
  lines(d$lambda, d$s, col = i, lwd = 2)
}
legend("bottomright", legend = sprintf("k=%.2g, g=%.2g", kg$k, kg$g),
       col = seq_len(nrow(kg)), lwd = 2, bty = "n", cex = 0.8)
abline(h = 1, lty = 3); grDevices::dev.off()

# Forest-plot + dot-plot figures: these replace the dense synthetic/real-data tables in
# the manuscript main text (the full tables move to the supplementary appendix).
if (!is.null(asym))
  emit_forest_pdf(asym,
    c("OLS", "PMM2", "RTE_CV", "RTE_Stable", "PMM2_RTE_Stable", "PMM2_RTE_Oracle"),
    "PMM2_RTE_Stable", c("exponential", "gamma2", "lognormal", "normal"),
    "fig_asym_forest.pdf")
sym <- rd("synthetic_sym_summary.csv")
if (!is.null(sym))
  emit_forest_pdf(sym,
    c("OLS", "PMM3", "MLE_TN", "RTE_Stable", "PMM3_RTE_Stable", "PMM3_RTE_Oracle"),
    "PMM3_RTE_Stable", c("uniform", "tn1.5", "normal"),
    "fig_sym_forest.pdf")
if (!is.null(rsum))
  emit_realdata_pdf(rsum,
    c("OLS", "PMM2", "PMM3", "RTE_CV", "RTE_Stable", "PMM2_RTE_Stable", "PMM3_RTE_Stable"),
    "fig_realdata.pdf")
cat("[04] tables + figures written to", CFG$paths$tables, "and", CFG$paths$figures, "\n")

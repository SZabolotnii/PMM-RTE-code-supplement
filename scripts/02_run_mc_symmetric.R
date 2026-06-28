#!/usr/bin/env Rscript
# 02_run_mc_symmetric.R — SPEC Block C (compact): PMM3-RTE + MLE-TN on symmetric
# platykurtic errors (uniform, tn1.5). Env as in 01.
# Emits (results/): synthetic_sym_summary.csv, synthetic_sym_results.rds,
#   covtransfer_sym.csv, boundary_diag_sym.csv, pmm3_convergence.csv,
#   synthetic_sym_report.md.

suppressPackageStartupMessages(library(EstemPMM))
source("R/config.R"); invisible(load_project(verbose = FALSE)); ensure_dirs()

methods <- c("OLS", "PMM3", "MLE_TN",
             "RTE_CV", "RTE_Stable", "PMM3_RTE_CV", "PMM3_RTE_Stable",
             "RTE_Oracle", "PMM3_RTE_Oracle")

ps <- phase_settings()
cat(sprintf("[02] symmetric MC | phase=%s g_mode=%s reps=%d test_n=%d B_boot=%d\n",
            ps$phase, g_mode_spec()$name, ps$reps, ps$test_n, ps$B_boot))

mc <- run_mc_block("sym", methods)
ct <- run_covtransfer(order = "pmm3", reps = if (ps$phase == "final") 500L else 300L)

write_csv_stamped(mc$summary, "synthetic_sym_summary.csv")
saveRDS(cfg_stamp(mc$results), file.path(CFG$paths$results, "synthetic_sym_results.rds"))
if (nrow(mc$results) <= 2e5)
  write_csv_stamped(mc$results, "synthetic_sym_results.csv")
write_csv_stamped(ct, "covtransfer_sym.csv")

bd <- mc$summary[grepl("RTE|Ridge", mc$summary$method),
                 c("scenario_id", "method", "median_g", "boundary_hit_frac")]
bd$g_mode <- g_mode_spec()$name
write_csv_stamped(bd, "boundary_diag_sym.csv")

conv <- unique(mc$summary[mc$summary$method == "PMM3",
                          c("scenario_id", "n", "p", "rho", "error", "pmm3_conv_rate")])
write_csv_stamped(conv, "pmm3_convergence.csv")

write_md_report(mc$summary, ct, "sym", "synthetic_sym_report.md")
cat("[02] done.\n")

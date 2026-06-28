#!/usr/bin/env Rscript
# 01_run_mc_asymmetric.R — SPEC Blocks A (Normal RTE baseline) + B (PMM2-RTE).
# Env: PHASE=screening|final, G_MODE=deployable|full_sens|coupled, PARALLEL=0|1.
# Emits (results/): synthetic_asym_summary.csv, synthetic_asym_results.rds,
#   covtransfer_asym.csv, boundary_diag_asym.csv, synthetic_asym_report.md.

suppressPackageStartupMessages(library(EstemPMM))
source("R/config.R"); invisible(load_project(verbose = FALSE)); ensure_dirs()

methods <- c("OLS", "PMM2",
             "Ridge_CV", "Ridge_Stable", "PMM2_Ridge_CV", "PMM2_Ridge_Stable",
             "RTE_CV", "RTE_Stable", "PMM2_RTE_CV", "PMM2_RTE_Stable",
             "RTE_Oracle", "PMM2_RTE_Oracle")

ps <- phase_settings()
cat(sprintf("[01] asymmetric MC | phase=%s g_mode=%s reps=%d test_n=%d B_boot=%d\n",
            ps$phase, g_mode_spec()$name, ps$reps, ps$test_n, ps$B_boot))

mc <- run_mc_block("asym", methods)
ct <- run_covtransfer(order = "pmm2", reps = if (ps$phase == "final") 500L else 300L)

write_csv_stamped(mc$summary, "synthetic_asym_summary.csv")
saveRDS(cfg_stamp(mc$results), file.path(CFG$paths$results, "synthetic_asym_results.rds"))
if (nrow(mc$results) <= 2e5)
  write_csv_stamped(mc$results, "synthetic_asym_results.csv")
write_csv_stamped(ct, "covtransfer_asym.csv")

bd <- mc$summary[grepl("RTE|Ridge", mc$summary$method),
                 c("scenario_id", "method", "median_g", "boundary_hit_frac")]
bd$g_mode <- g_mode_spec()$name
write_csv_stamped(bd, "boundary_diag_asym.csv")

write_md_report(mc$summary, ct, "asym", "synthetic_asym_report.md")
cat("[01] done.\n")

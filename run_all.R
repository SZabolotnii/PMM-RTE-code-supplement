#!/usr/bin/env Rscript
# run_all.R — orchestrate the PMM-RTE pipeline (01 -> 05).
# Env: PHASE=screening|final (default screening), G_MODE=deployable (default),
#      PARALLEL=0|1, REALDATA=1 to include 03 (slower), SYM=1 to include 02.
# Usage: PHASE=screening Rscript scripts/run_all.R

t0 <- Sys.time()
phase <- Sys.getenv("PHASE", "screening")
cat(sprintf("==== PMM-RTE run_all | PHASE=%s G_MODE=%s ====\n",
            phase, Sys.getenv("G_MODE", "deployable")))

steps <- c("scripts/01_run_mc_asymmetric.R")
if (identical(Sys.getenv("SYM", "1"), "1"))      steps <- c(steps, "scripts/02_run_mc_symmetric.R")
if (identical(Sys.getenv("REALDATA", "0"), "1")) steps <- c(steps, "scripts/03_run_realdata.R")
steps <- c(steps, "scripts/04_make_tables.R", "scripts/05_verify_results.R")

for (s in steps) {
  cat(sprintf("\n---- %s ----\n", s))
  st <- Sys.time()
  status <- system2("Rscript", s)
  cat(sprintf("   (%s, %.1f min)\n", s, as.numeric(difftime(Sys.time(), st, units = "mins"))))
  if (!identical(status, 0L)) stop("Step failed: ", s, call. = FALSE)
}
cat(sprintf("\n==== run_all done in %.1f min ====\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

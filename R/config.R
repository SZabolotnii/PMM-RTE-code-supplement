# =============================================================================
# R/config.R — single source of truth for the PMM-RTE pipeline
# Edit THIS file to change study parameters; every other module reads CFG.
# Loader pattern ported from PMM3_vs_MLE-TN/R/config.R.
# =============================================================================

CFG <- list(
  est_version = "0.5.0-pmmrte",
  artifact_stamp = "2026-06-29 REVSTAT submission",
  seed_base   = 20260612L,          # YYYYMMDD; CRN per-rep seed = seed_base + sid*1e6 + rep
  inner_frac  = 0.75,               # train/validation inner split for tuning

  # --- shrinkage grids -------------------------------------------------------
  k_grid = 10^seq(-4, 4, length.out = 17),
  ridge_g = 1,                      # Ridge = RTE special case g = 1
  # DEFECT-1 FIX: g-handling modes (switch via env G_MODE) --------------------
  g_modes = list(
    deployable = list(type = "free",    grid = c(0, 0.1, 0.25, 0.5, 1, 2, 5)),       # g>=0 headline
    full_sens  = list(type = "free",    grid = c(-0.5, -0.25, 0, 0.25, 0.5, 1, 2, 5, 10)),
    coupled    = list(type = "coupled", fun = "liu")                                  # g(k) ablation
  ),
  shrink_tol = 1e-6,                # eigen-admissibility tolerance (drop s_j < -tol or > 1+tol)
  rmse_guard = 0.01,               # legacy slope-norm guard (kept for back-compat diffs)
  delta_grid = c(0.01, 0.03, 0.05),# §6.3 constraint sensitivity
  delta_main = 0.03,

  # --- phases (screening go/no-go -> final publication run) ------------------
  phases = list(
    screening = list(reps = 500L,  test_n = 2000L, B_boot = 100L),
    # reps=1000 with paired CRN matches ~2000 unpaired in precision (screening p~1e-60);
    # B_boot for tuning kept modest (the bootstrap is the compute floor).
    final     = list(reps = 1000L, test_n = 5000L, B_boot = 150L)
  ),

  # --- scenario factors ------------------------------------------------------
  scenarios = list(
    screening = list(np_pairs = list(c(80L, 8L), c(60L, 12L)), rho = c(0.95, 0.99)),
    # Representative final grid (feasible + spans small-n/high-collinearity AND the
    # asymptotic regime so covariance transfer shows in the MAIN tables).
    final     = list(n = c(100L, 500L), p = c(4L, 12L), rho = c(0.9, 0.99)),
    # COMPACT-PMM3 decision (2026-06-27): the sym block (PMM3-RTE) is the agreed
    # *compact* extension, and its n=500 PMM3+MLE-TN cells are the run's only real
    # time sink. Restrict the sym FINAL grid to n=100; PMM3 covariance transfer is
    # still demonstrated separately on the benign n=300-500 scenarios in run_covtransfer.
    final_sym_n = 100L
  ),
  errors = list(
    asym = list(screening = c("normal", "gamma2", "exponential"),
                final     = c("normal", "gamma2", "exponential", "lognormal")),
    sym  = list(screening = c("normal", "uniform", "tn1.5"),       # compact Block C
                final     = c("normal", "uniform", "tn1.5"))
  ),
  beta_pattern_main = "equal_positive",
  beta_patterns     = c("equal_positive", "sparse", "alternating"),

  # --- defect-2 / Proposition-2 (covariance transfer) -----------------------
  covtransfer_kg = list(k = 1.0, g = 1.0),   # FIXED (admissible) (k,g) for the Prop-2 check

  # --- inference / dispatch --------------------------------------------------
  ci = list(B = 500L, method = "percentile"),
  dispatch = list(symmetry_threshold = 0.3, kurtosis_threshold = -0.7,
                  g2_threshold = 0.95, n_min = 50L),

  # --- paths (relative to project root) -------------------------------------
  paths = list(results = "results", tables = "results/tables",
               figures = "results/figures", cache = "results/mc_cache",
               data_raw = "data/raw")
)

# Phase settings (reps/test_n/B_boot) for the active PHASE -------------------
phase_settings <- function(phase = Sys.getenv("PHASE", "screening")) {
  if (!nzchar(phase)) phase <- "screening"
  phase <- match.arg(phase, c("screening", "final"))
  s <- c(list(phase = phase), CFG$phases[[phase]])
  ov <- function(env, cur) { v <- Sys.getenv(env, ""); if (nzchar(v)) as.integer(v) else cur }
  s$reps   <- ov("REPS",   s$reps)        # smoke/custom overrides
  s$test_n <- ov("TEST_N", s$test_n)
  s$B_boot <- ov("B_BOOT", s$B_boot)
  s
}

# g-mode spec for the active G_MODE -----------------------------------------
g_mode_spec <- function(mode = Sys.getenv("G_MODE", "deployable")) {
  if (!nzchar(mode)) mode <- "deployable"
  m <- CFG$g_modes[[mode]]
  if (is.null(m)) stop("Unknown G_MODE: ", mode, call. = FALSE)
  c(list(name = mode), m)
}

# CRN seed for (scenario index, rep) ----------------------------------------
scenario_seed <- function(sid_index, rep) {
  as.integer(CFG$seed_base) + as.integer(sid_index) * 1000000L + as.integer(rep)
}

ensure_dirs <- function() {
  for (p in c(CFG$paths$results, CFG$paths$tables, CFG$paths$figures, CFG$paths$cache))
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

# Source all R/*.R modules except config.R ----------------------------------
load_project <- function(verbose = TRUE) {
  r_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
  r_files <- r_files[basename(r_files) != "config.R"]
  for (f in r_files) {
    if (verbose) message("  Loading: ", basename(f))
    source(f, local = FALSE)
  }
  if (verbose) message("PMM-RTE project loaded (", length(r_files), " modules, v",
                       CFG$est_version, ").")
  invisible(r_files)
}

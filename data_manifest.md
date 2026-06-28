# Data manifest — Ku-PMM-RTE-code-supplement

All datasets are vendored as local CSVs under `data/raw/` so the pipeline has **no
hidden `utils::data()` dependency** on R packages. The package names below are only
the *provenance* of each public dataset.

| file | provenance | role | model |
|---|---|---|---|
| `ISLR2_College.csv` | `ISLR2::College` | real-data repeated-split (supporting) | `Expend ~ Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Grad.Rate` |
| `nlme_Remifentanil.csv` | `nlme::Remifentanil` | real-data repeated-split (supporting) | `conc ~ Time + Rate + Amt + Age + Wt + Ht + LBM + BSA` |
| `sandwich_InstInnovation.csv` | `sandwich::InstInnovation` | real-data repeated-split (supporting) | `value ~ cites + patents + precites + randd + sales + employment + capital + competition + competition4 + acompetition + tobinq + institutions + dedicated + transient + quasiindexed` |
| `MASS_cement_Hald_Portland.csv` | `MASS::cement` | Portland/Hald multicollinearity illustration (`n = 13`, **illustrative only**) | `y ~ x1 + x2 + x3 + x4` |

Synthetic Monte Carlo data is **generated on demand** by `scripts/01_run_mc_asymmetric.R`
and `scripts/02_run_mc_symmetric.R` from the known `beta` and error families configured in
`R/config.R`; it is not stored as static raw data.

All four CSVs are redistributable public datasets shipped in the cited open-source R packages.

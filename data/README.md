# PMM-RTE data manifest

Дата формування: 2026-06-12.

Ця папка містить локальні копії наборів даних і скринінгові артефакти, які використовуються в PMM-RTE роботі. Мета - прибрати приховану залежність від `utils::data()` у R-пакетах і тримати всі data-related файли всередині article track.

## raw

| file | source | role in PMM-RTE work | response/model |
|---|---|---|---|
| `ISLR2_College.csv` | `ISLR2::College` | real-data repeated-split pilot | `Expend ~ Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Grad.Rate` |
| `nlme_Remifentanil.csv` | `nlme::Remifentanil` | real-data repeated-split pilot | `conc ~ Time + Rate + Amt + Age + Wt + Ht + LBM + BSA` |
| `sandwich_InstInnovation.csv` | `sandwich::InstInnovation` | real-data repeated-split pilot | `value ~ cites + patents + precites + randd + sales + employment + capital + competition + competition4 + acompetition + tobinq + institutions + dedicated + transient + quasiindexed` |
| `MASS_cement_Hald_Portland.csv` | `MASS::cement` | Portland/Hald illustrative multicollinearity benchmark | `y ~ x1 + x2 + x3 + x4` |

## screening

| file | purpose |
|---|---|
| `real_data_candidates_for_Kunchenko_RTE_2026-06-12.md` | narrative screening note for candidate real datasets |
| `real_data_screen_external_excel_2026-06-12.csv` | external Excel/open-data screening output |
| `real_data_screen_external_open_2026-06-12.csv` | external open-data screening output |
| `real_data_screen_installed_R_datasets_2026-06-12.csv` | installed R datasets screening output |
| `real_data_screen_installed_R_datasets_clean_2026-06-12.csv` | cleaned installed R datasets screening output |

## Notes

- Synthetic Monte Carlo datasets are generated on demand by `scripts/run_synthetic_pmm_rte_mc.R`; they are not stored as static raw data.
- `scripts/run_realdata_pmm_rte.R` reads the three real-data pilot datasets from `data/raw`.
- Portland/Hald cement is stored because it is used in the theoretical/practical discussion, but `n=13` means it should remain illustrative rather than a PMM proof dataset.

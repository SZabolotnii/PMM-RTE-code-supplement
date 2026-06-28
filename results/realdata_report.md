# PMM-RTE real-data report (Block E supporting + Block D illustrative)

Generated: 2026-06-27 12:13:41 EEST | reps=60 B=40 g_mode=deployable | v0.5.0-pmmrte

## college_expend

### scale=log1p  (cond=14.6, gamma3=0.945, gamma4=4.945, g2=0.871, g3=0.846, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 2560.9 | 0.0021201 | 1.00 |
| PMM2 | 2552.2 | 0.0023302 | 1.00 |
| PMM2_RTE_Stable | 2642.1 | 0.00098367 | 1.00 |
| PMM3 | 2563.6 | 0.0028439 | 1.00 |
| PMM3_RTE_Stable | 3104.7 | 0.0038586 | 1.00 |
| RTE_CV | 2576.6 | 0.0025582 | 1.00 |
| RTE_Stable | 2662.6 | 0.0011027 | 1.00 |

### scale=raw  (cond=14.6, gamma3=4.808, gamma4=42.581, g2=0.481, g3=0.554, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 2909.6 | 5.9458e+05 | 1.00 |
| PMM2 | 2945 | 3.9188e+05 | 1.00 |
| PMM2_RTE_Stable | 2932.7 | 3.9108e+05 | 1.00 |
| PMM3 | 2909.6 | 5.9458e+05 | 1.00 |
| PMM3_RTE_Stable | 2934.9 | 9.5404e+05 | 1.00 |
| RTE_CV | 2932.9 | 2.0024e+06 | 1.00 |
| RTE_Stable | 2934.9 | 9.5404e+05 | 1.00 |

## instinnovation_value

### scale=log1p  (cond=26.7, gamma3=0.209, gamma4=1.381, g2=0.987, g3=0.954, dispatch=OLS/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 3.3325e+05 | 0.053304 | 1.00 |
| PMM2 | 2.2338e+05 | 0.061963 | 1.00 |
| PMM2_RTE_Stable | 74545 | 0.036585 | 1.00 |
| PMM3 | 3.0049e+06 | 16.994 | 1.00 |
| PMM3_RTE_Stable | 15402 | 0.72353 | 1.00 |
| RTE_CV | 3.1436e+05 | 0.54015 | 1.00 |
| RTE_Stable | 1.0889e+05 | 0.026729 | 1.00 |

### scale=raw  (cond=26.7, gamma3=8.287, gamma4=139.436, g2=0.514, g3=0.630, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 10007 | 7.1668e+06 | 1.00 |
| PMM2 | 10094 | 5.0036e+06 | 1.00 |
| PMM2_RTE_Stable | 10087 | 2.6536e+06 | 1.00 |
| PMM3 | 10007 | 7.1668e+06 | 1.00 |
| PMM3_RTE_Stable | 10001 | 1.676e+06 | 1.00 |
| RTE_CV | 9949.1 | 1.1637e+07 | 1.00 |
| RTE_Stable | 10001 | 1.676e+06 | 1.00 |

## remifentanil_conc

### scale=log1p  (cond=86.8, gamma3=1.301, gamma4=5.041, g2=0.760, g3=0.904, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 25.309 | 0.13836 | 1.00 |
| PMM2 | 26.86 | 55.686 | 1.00 |
| PMM2_RTE_Stable | 25.281 | 12.849 | 1.00 |
| PMM3 | 31.553 | 7.5286 | 1.00 |
| PMM3_RTE_Stable | 30.574 | 0.68362 | 1.00 |
| RTE_CV | 25.475 | 0.10411 | 1.00 |
| RTE_Stable | 25.743 | 0.0091583 | 1.00 |

### scale=raw  (cond=86.8, gamma3=3.742, gamma4=25.627, g2=0.493, g3=0.704, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 20.134 | 63.459 | 1.00 |
| PMM2 | 20.227 | 40.698 | 1.00 |
| PMM2_RTE_Stable | 20.374 | 0.99338 | 1.00 |
| PMM3 | 20.179 | 55.921 | 1.00 |
| PMM3_RTE_Stable | 20.299 | 1.1023 | 1.00 |
| RTE_CV | 20.189 | 63.625 | 1.00 |
| RTE_Stable | 20.329 | 1.2199 | 1.00 |

## Portland/Hald (illustrative, n=13)
- cond=37.1, g2=0.970, g3=0.834, potential PMM3 variance reduction 1-g3=0.166
- mean bootstrap CI width=26, LOO slope-var trace=44.4 (illustrative, no PMM claim; n=13)

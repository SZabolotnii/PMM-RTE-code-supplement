# PMM-RTE real-data report (Block E supporting + Block D illustrative)

Generated: 2026-06-28 18:28:43 EEST | reps=120 B=80 g_mode=deployable | v0.5.0-pmmrte

## college_expend

### scale=log1p  (cond=14.6, gamma3=0.945, gamma4=4.945, g2=0.871, g3=0.846, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 2596.2 | 0.0024079 | 1.00 |
| PMM2 | 2561.7 | 0.0025768 | 1.00 |
| PMM2_RTE_Stable | 2660.3 | 0.001005 | 1.00 |
| PMM3 | 2612.8 | 0.25669 | 1.00 |
| PMM3_RTE_Stable | 2834.6 | 0.0053477 | 1.00 |
| RTE_CV | 2596.2 | 0.0026632 | 1.00 |
| RTE_Stable | 2693.5 | 0.0011335 | 1.00 |

### scale=raw  (cond=14.6, gamma3=4.808, gamma4=42.581, g2=0.481, g3=0.554, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 2912 | 6.0505e+05 | 1.00 |
| PMM2 | 2982.7 | 4.0242e+05 | 1.00 |
| PMM2_RTE_Stable | 3025.6 | 4.284e+05 | 1.00 |
| PMM3 | 2912 | 6.0505e+05 | 1.00 |
| PMM3_RTE_Stable | 2994.9 | 1.0119e+06 | 1.00 |
| RTE_CV | 2947.1 | 1.8771e+06 | 1.00 |
| RTE_Stable | 2994.9 | 1.0119e+06 | 1.00 |

## instinnovation_value

### scale=log1p  (cond=26.7, gamma3=0.209, gamma4=1.381, g2=0.987, g3=0.954, dispatch=OLS/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 5.3747e+05 | 0.053661 | 1.00 |
| PMM2 | 5.4079e+05 | 0.063712 | 1.00 |
| PMM2_RTE_Stable | 1.3303e+05 | 0.03578 | 1.00 |
| PMM3 | 7.7753e+06 | 18.834 | 1.00 |
| PMM3_RTE_Stable | 15600 | 0.7301 | 1.00 |
| RTE_CV | 4.8182e+05 | 0.49306 | 1.00 |
| RTE_Stable | 1.9094e+05 | 0.027807 | 1.00 |

### scale=raw  (cond=26.7, gamma3=8.287, gamma4=139.436, g2=0.514, g3=0.630, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 10192 | 6.9146e+06 | 1.00 |
| PMM2 | 10100 | 4.5847e+06 | 1.00 |
| PMM2_RTE_Stable | 10163 | 2.2288e+06 | 1.00 |
| PMM3 | 10192 | 6.9146e+06 | 1.00 |
| PMM3_RTE_Stable | 10048 | 1.6953e+06 | 1.00 |
| RTE_CV | 10026 | 1.2143e+07 | 1.00 |
| RTE_Stable | 10048 | 1.6953e+06 | 1.00 |

## remifentanil_conc

### scale=log1p  (cond=86.8, gamma3=1.301, gamma4=5.041, g2=0.760, g3=0.904, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 25.037 | 0.1111 | 1.00 |
| PMM2 | 26.002 | 66.768 | 1.00 |
| PMM2_RTE_Stable | 25.093 | 18.384 | 1.00 |
| PMM3 | 33.094 | 6.9542 | 1.00 |
| PMM3_RTE_Stable | 30.958 | 0.52291 | 1.00 |
| RTE_CV | 25.244 | 0.084057 | 1.00 |
| RTE_Stable | 25.526 | 0.0091885 | 1.00 |

### scale=raw  (cond=86.8, gamma3=3.742, gamma4=25.627, g2=0.493, g3=0.704, dispatch=PMM2/publication)
| method | median RMSE | slope-var trace | ok |
|---|---:|---:|---:|
| OLS | 19.996 | 68.659 | 1.00 |
| PMM2 | 20.145 | 44.308 | 1.00 |
| PMM2_RTE_Stable | 20.289 | 0.8918 | 1.00 |
| PMM3 | 20.087 | 59.717 | 1.00 |
| PMM3_RTE_Stable | 20.221 | 1.1976 | 1.00 |
| RTE_CV | 19.997 | 56.156 | 1.00 |
| RTE_Stable | 20.238 | 1.1551 | 1.00 |

## Portland/Hald (illustrative, n=13)
- cond=37.1, g2=0.970, g3=0.834, potential PMM3 variance reduction 1-g3=0.166
- mean bootstrap CI width=26, LOO slope-var trace=44.4 (illustrative, no PMM claim; n=13)

<h1 align="center">Impact of Heavy Vehicles on Headway Distributions: Study Using Naturalistic Urban Expressway Trajectories</h1>

<p align="center">
  <a href="https://journals.sagepub.com/doi/abs/10.1177/03611981251324198"><img alt="TRR" src="https://img.shields.io/static/v1?label=TRR&message=Paper&color=green&style=flat-square"></a>&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="https://opensource.org/licenses/MIT"><img alt="License" src="https://img.shields.io/static/v1?label=License&message=MIT&color=rose&style=flat-square"></a>
</p>

---

## Abstract

Headway between vehicles affects safety, level of service, driver behavior, and capacity. This study analyzes **time headways** when **heavy vehicles** are present in a **naturalistic urban expressway** setting. Three leader–follower combinations are examined: **car–car (C–C)**, **car–heavy vehicle (C–HV)**, and **heavy vehicle–car (HV–C)**. **High-resolution trajectories** are derived from videos on **eastbound I-395 in Washington, DC**, covering **three through lanes plus one merging lane**. Headways are analyzed **by lane**. The study finds statistically **different mean headways** across C–C, C–HV, and HV–C; **lognormal** distributions fit well for all three types in many cases; under **severe congestion in the merging lane**, **gamma** is a better fit for **C–C**. The results motivate **lane- and vehicle-pair-specific** treatment of headways in **microscopic simulation**.

## Study summary

The analysis uses **Third Generation Simulation (TGSIM)** <a href="https://data.transportation.gov/Automobiles/Third-Generation-Simulation-Data-TGSIM-I-395-Traje/97n2-kuqi/about_data"><img alt="Data — TGSIM I-395" src="https://img.shields.io/static/v1?label=Data&message=TGSIM%20I-395&color=blue&style=flat-square" style="vertical-align: middle;"></a> <a href="https://journals.sagepub.com/doi/10.1177/03611981241257257"><img alt="Paper — TGSIM TRR methods" src="https://img.shields.io/static/v1?label=Paper&message=TGSIM%20TRR&color=purple&style=flat-square" style="vertical-align: middle;"></a> trajectories on **eastbound I-395** (Washington, DC), separating **passenger cars** from **heavy vehicles** and studying **C–C**, **C–HV**, and **HV–C** time headways by lane (**HV–HV** is rare and omitted as in the paper). **Flow–density** observations are clustered with **Gaussian mixture models** so headway work focuses on **congested** conditions; **time headway** is measured at cross-sections, with **`pedram4.R`** / **`mergeplots.R`** fitting **gamma**, **lognormal**, **Weibull**, and **inverse Gaussian** distributions (CDFs, **K–S**, **AIC/BIC**), **`Flow-Density.ipynb`** building lane-level **flow, density, and speed** time series, and **`OLS - MLE.ipynb`** supporting **OLS/MLE** calibration of distribution parameters against **flow**, **HV share**, and **lane**. In the congested regime, mean headways are about **2.41 s**, **2.57 s**, and **3.55 s** for **C–C**, **C–HV**, and **HV–C**; **lognormal** fits are typical, while **gamma** fits **C–C** better in the **merging lane** under strong congestion. The TGSIM build used in the article may differ from publicly released data.

## Repository contents

| File | Description |
|------|-------------|
| [`pedram4.R`](pedram4.R) | Reads per-lane Excel inputs (lanes H-2–H-5; three sheets per file for C–C, C–HV, HV–C). Fits **Gamma**, **lognormal**, **Weibull**, and **inverse Gaussian** distributions (`fitdistrplus`, `actuar`), compares empirical vs. theoretical **CDFs**, reports **Kolmogorov–Smirnov** distances and **AIC/BIC**, and writes `CDF_Lane*.png` plots plus **`FitResults.csv`**. |
| [`mergeplots.R`](mergeplots.R) | Combines the individual CDF figures into a single high-resolution **`MergedPlotsv2.png`** grid (`gridExtra`). Run after `pedram4.R` once plots exist in the output folder. |
| [`Flow-Density.ipynb`](Flow-Density.ipynb) | Python notebook that aggregates trajectory data into **lane-level flow, density, and speed** over time windows from a CSV (indexed vehicle records with timestamps, lane, and kinematic fields). Useful for **flow–density** summaries of the corridor. |
| [`OLS - MLE.ipynb`](OLS%20-%20MLE.ipynb) | **OLS** (`statsmodels`) and **maximum likelihood** (`scipy.optimize`) routines supporting **distribution-parameter regressions** (flow, HV share, lane) as reported in the paper. |

**Raw trajectory or headway spreadsheets are not distributed in this repo** (only the processing code). Scripts currently use **absolute paths** from the authors’ machines; adjust `file_paths`, `output_folder`, and notebook `read_csv` paths to your environment before running.

## Acknowledgments

Research supported by the **U.S. Department of Transportation** Federal Highway Administration (FHWA) under support related to **TGSIM Data: A Closer Look at The Impacts of Automated Driving Systems on Human Behavior**.

## Requirements

**R**

- Packages: `readxl`, `fitdistrplus`, `actuar`, `png`, `grid`, `gridExtra`, `stringr`

**Python** (recommended: recent 3.x with Jupyter)

- Packages: `pandas`, `numpy`, `statsmodels`, `scipy`

## Citation

If you use this repository, please cite our paper:

```bibtex
@article{beigi2025impact,
  title={Impact of Heavy Vehicles on Headway Distributions: Study Using Naturalistic Urban Expressway Trajectories},
  author={Beigi, Pedram and Hamdar, Samer and Talebpour, Alireza and Mahmassani, Hani},
  journal={Transportation Research Record},
  volume={2679},
  number={7},
  pages={222--235},
  year={2025},
  publisher={SAGE Publications Sage CA: Los Angeles, CA}
}
```

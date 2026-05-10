<h1 align="center">Impact of Heavy Vehicles on Headway Distributions: Study Using Naturalistic Urban Expressway Trajectories</h1>

<p align="center">
  <a href="https://journals.sagepub.com/doi/abs/10.1177/03611981251324198"><img alt="TRR" src="https://img.shields.io/static/v1?label=TRR&message=Paper&color=green&style=flat-square"></a>&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="https://opensource.org/licenses/MIT"><img alt="License" src="https://img.shields.io/static/v1?label=License&message=MIT&color=rose&style=flat-square"></a>
</p>

---

Companion code for [*Transportation Research Record* **2679**(7): 222–235, 2025](https://journals.sagepub.com/doi/abs/10.1177/03611981251324198). **DOI:** [10.1177/03611981251324198](https://doi.org/10.1177/03611981251324198).

## Abstract

Headway between vehicles affects safety, level of service, driver behavior, and capacity. This study analyzes **time headways** when **heavy vehicles** are present in a **naturalistic urban expressway** setting. Three leader–follower combinations are examined: **car–car (C–C)**, **car–heavy vehicle (C–HV)**, and **heavy vehicle–car (HV–C)**. **High-resolution trajectories** are derived from videos on **eastbound I-395 in Washington, DC**, covering **three through lanes plus one merging lane**. Headways are analyzed **by lane**. The study finds statistically **different mean headways** across C–C, C–HV, and HV–C; **lognormal** distributions fit well for all three types in many cases; under **severe congestion in the merging lane**, **gamma** is a better fit for **C–C**. The results motivate **lane- and vehicle-pair-specific** treatment of headways in **microscopic simulation**.

## Keywords

Traffic operations · Microscopic traffic models · Headway distribution · Heavy vehicles · Driver behavior

## Authors

Pedram Beigi<sup>1</sup> (corresponding author: [beigi@gwu.edu](mailto:beigi@gwu.edu)), Samer Hamdar<sup>1</sup>, Alireza Talebpour<sup>2</sup>, Hani Mahmassani<sup>3</sup>

<sup>1</sup>George Washington University, Washington, DC · <sup>2</sup>University of Illinois Urbana-Champaign · <sup>3</sup>Northwestern University Transportation Center, Evanston, IL

## Study site and data

Data come from **Third Generation Simulation (TGSIM)** trajectory extraction on **I-395** (eastbound). Vehicles are grouped as **passenger cars (including SUVs and pickups)** versus **heavy vehicles (trucks and buses)**; **HV–HV** pairs are rare and are set aside as in the paper. The manuscript uses **Gaussian mixture models (GMMs)** on **flow–density** observations to classify traffic states so that subsequent headway analysis can focus on **congested** conditions. **Note:** The TGSIM build used for the publication may **differ** from versions that are or will be **publicly** released.

## Methodology (and mapping to this repo)

- **Time headway:** difference between follower and leader passage times at cross-sections; statistics and distributions by **lane** and **vehicle-pair type** (detailed statistical tests, including Welch’s *t*-tests on means, appear in the paper).
- **`pedram4.R` / `mergeplots.R`:** fit **Gamma**, **lognormal**, **Weibull**, and **inverse Gaussian** laws; **CDF** overlays; **K–S distances** and **AIC/BIC** (goodness-of-fit procedures as in the paper’s Methodology section).
- **`Flow-Density.ipynb`:** compute **lane-level flow, density, and mean speed** in time bins from indexed trajectory CSVs—aligned with constructing **fundamental-diagram-style** aggregates (see flow–density clustering in the paper).
- **`OLS - MLE.ipynb`:** **OLS** and **maximum likelihood** fits used in calibrating **parameter surfaces** relating distribution parameters to **flow rate**, **heavy-vehicle percentage**, and **lane index** (Table 4 in the article).

## Key findings

- Mean headways (**across lanes**, congested regime in the manuscript) are roughly **2.41 s (C–C)**, **2.57 s (C–HV)**, and **3.55 s (HV–C)**—drivers maintain **larger gaps** when interacting with heavy vehicles as leader or follower.
- **Lane position** matters, especially near the **merge lane** where **gamma** better describes **C–C** under strong congestion.

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

# Time-Smoothed Inverse Probability Weighted Estimation of Repeatedly Measured Outcomes

This repository contains the code for the simulation study and data application in the manuscript ["Time-smoothed inverse probability weighted estimation of effects of generalized time-varying treatment strategies on repeated outcomes truncated by death"](https://doi.org/10.48550/arXiv.2509.13971) by Sean McGrath, Takuya Kawahara, Joshua Petimar, Sheryl L. Rifas-Shiman, Iván Díaz, Jason P. Block, and Jessica G. Young.

## Simulation Study

The `simulations` folder contains all code and results related to the simulation study. The subfolder `code` contains all code and `results` contains all results.

### Prerequisites

The code requires the following R packages to be installed: 

| Package      | Version |
|-------------|---------|
| data.table  | 1.16.0  |
| doParallel  | 1.0.17  |
| doRNG       | 1.8.6   |
| foreach     | 1.5.2   |
| RColorBrewer| 1.1-3   |
| scales      | 1.3.0   |
| xtable      | 1.8-4   |

The package version numbers listed above were used in the analyses in the manuscript.

### Running the Simulations and Analyzing Results

The main files for running the simulations are:

| File | Description | Approx. Runtime* |
|------|------------|----------------|
| `run-sims-null.R` | Null scenarios without deaths | 9 hours (parallelized across 20 CPU cores) |
| `run-sims-nonnull.R` | Non-null scenarios without deaths | 4 hours (parallelized across 10 CPU cores) |
| `run-sims-null-deaths.R` | Scenarios with deaths | 4 hours (parallelized across 10 CPU cores) |

All simulation scripts rely on the helper file, ``run-sims-helper.R``. 

The ``analyze-results.R`` file analyzes the results of the simulation studies generates all figures and tables reported in the manuscript.

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
| smoothedIPW | 0.1.0   |
| xtable      | 1.8-4   |

The package version numbers listed above were used in the analyses in the manuscript.

### Running Simulations and Analyzing Results

The main files for running the simulations are:

| File | Simulation Scenarios | Approx. Runtime |
|------|------------|----------------|
| `run-sims-null.R` | Null scenarios without deaths | 4 hours (parallelized across 10 CPU cores) |
| `run-sims-nonnull.R` | Non-null scenarios without deaths | 4 hours (parallelized across 10 CPU cores) |
| `run-sims-null-deaths.R` | Scenarios with deaths | 9 hours (parallelized across 20 CPU cores) |

All simulation scripts rely on the helper file, `sim-utils.R`. Also,
-  `run-sims-null.R` and `run-sims-nonnull.R` additionally call `sim-runner.R`.
-  `run-sims-null-deaths.R` additionally calls `sim-runner-deaths.R`.

The `analyze-results.R` file analyzes the results of the simulation studies and generates all figures and tables reported in the manuscript.

## Data Application

### Prerequisites

The code requires the following R packages to be installed: 

| Package      | Version |
|-------------|---------|
| data.table  | 1.14.2  |
| haven       | 2.5.0   |
| smoothedIPW | 0.1.0   |
| table1      | 1.4.3   |
| xtable      | 1.8-4   |

The package version numbers listed above were used in the analyses in the manuscript.

### Running the Data Application

The data analysis can performed by the following steps:
-  `data-processing.R`: Cleans the dataset.
-  `descriptive-analyses.R`: Performs descriptive analyses of the analytic dataset.
-  `ipw-analyses.R`: Applying the inverse probability weighted estimators to the analytic dataset.

The input dataset read into `data-processing.R` cannot be publicly shared. A data dictionary for this dataset is given in `dictionary.pdf`.
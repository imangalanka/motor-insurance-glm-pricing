# Frequency-Severity GLM – Motor Third Party Liability (MTPL)

## Overview

This project builds a simple frequency-severity GLM rating model for a personal motor portfolio using the French MTPL dataset (freMTPL2freq / freMTPL2sev). The model produces multiplicative rating relativities suitable for use within a risk premium framework, following UK General Insurance actuarial practice.

The pipeline covers the full modelling workflow from raw data ingestion through to exportable rating factors and model objects, with diagnostic outputs at each stage.

## Repository Structure
GLM Modelling/

│

├── Datasets/freMTPL2freq.csv  # Frequency data – one row per policy

├── Datasets/freMTPL2sev.csv   # Severity data – one row per claim

├── glm_mtpl.R                # Main modelling script

├── README.md                 # This file

│

└── outputs/

├── 01_EDA_Frequency_Plots.pdf

├── 02_EDA_Severity_Plots.pdf

├── 03_EDA_Exposure_Profiles.pdf

├── 04_Freq_Model_Diagnostics.pdf

├── 05_Freq_Actual_vs_Predicted.pdf

├── 06_Freq_Decile_Lift.pdf

├── 07_EDA_Severity_by_Factor.pdf

├── 08_Sev_Model_Diagnostics.pdf

├── 09_Sev_Decile_Lift.pdf

├── 10_Rating_Relativities.pdf

├── 11_PurePremium_Distribution.pdf

├── freq_model_coefficients.csv

├── sev_model_coefficients.csv

└── combined_relativities.csv


## Data

The dataset is sourced from the online resource Kaggle (https://www.kaggle.com/datasets/karansarpal/fremtpl2-french-motor-tpl-insurance-claims?resource=download)

| File | Granularity | Key Fields |
|---|---|---|
| freMTPL2freq.csv | One row per policy | IDpol, ClaimNb, Exposure, VehPower, VehAge, DrivAge, BonusMalus, VehBrand, VehGas, Area, Density, Region |
| freMTPL2sev.csv | One row per claim | IDpol, ClaimAmount |

The two files are linked by `IDpol` (policy identifier). A single policy may have multiple rows in the severity file if it generated more than one claim.

## Modelling Approach

### Frequency Model
- **Target:** Claim count per policy
- **Family:** Poisson with log link
- **Offset:** log(Exposure) — ensures predictions are on a claims per car-year basis
- **Variable selection:** Backward stepwise by AIC from a full candidate set

### Severity Model
- **Target:** Average claim amount per policy (total capped claims / claim count)
- **Family:** Gamma with log link
- **Weights:** Claim count, to give greater credibility to policies with more claims
- **Variable selection:** Backward stepwise by AIC from the same candidate set

### Pure Premium
The pure premium (risk premium) for each policy is calculated as:
Pure Premium = Predicted Frequency × Predicted Severity

Rating relativities are then expressed relative to the most populated (reference) level of each factor, holding all other variables at their reference level.

### Rating Factors Considered

| Variable | Description |
|---|---|
| drv_age_band | Driver age, banded into 8 groups (18–22 through 71+) |
| veh_age_band | Vehicle age, banded into 5 groups (0–1 through 11+) |
| veh_power_band | Vehicle power, winsorised to [4,15] and banded into 5 groups |
| bm_band | Bonus-Malus score, banded into 8 groups (50–54 through 151+) |
| veh_fuel | Fuel type (Diesel / Petrol) |
| area | Area code (A–F) |
| density_band | Log population density, cut into 5 equal-exposure quantiles |
| veh_brand | Vehicle brand |
| region | French administrative region |

## Script Structure

| Section | Description |
|---|---|
| 0 | Environment setup – packages, seed, theme, file paths |
| 1 | Data ingestion |
| 2 | Data cleaning, validation, and joining |
| 3 | Factor engineering and reference level assignment |
| 4 | Stratified 70/30 train/test split |
| 5 | Exploratory data analysis |
| 6 | Frequency GLM fitting and variable selection |
| 7 | Frequency model diagnostics and validation |
| 8 | Severity GLM fitting and variable selection |
| 9 | Severity model diagnostics and validation |
| 10 | Pure premium and rating relativities |
| 11 | Portfolio-level pure premium application |
| 12 | Export of model summary tables to CSV |
| 13 | Export of fitted model objects to RDS |
| 14 | Final summary |

## Requirements

**R version:** 4.1 or later recommended

**Packages** (installed automatically on first run if missing):

```r
dplyr, tidyr, readr, ggplot2, gridExtra, scales, statmod, stringr
```

## Usage

1. Clone the repository, unzip the Dataset folder and place `freMTPL2freq.csv` and `freMTPL2sev.csv` in the working directory.
2. Update the `setwd()` path in section 0.5 to match your local directory.
3. Run `glm_mtpl.R` in full. The script is self-contained and will install any missing packages automatically.
4. All outputs are written to the `outputs/` folder, which is created automatically if it does not exist.

After the initial run rds files 'glm_freq.rds' and 'glm_sev.rds' will be created within the output folder. To reload the fitted models in a separate session without re-running the full pipeline:

```r
glm_freq <- readRDS("outputs/glm_freq.rds")
glm_sev  <- readRDS("outputs/glm_sev.rds")
```

## Outputs

### Diagnostic PDFs
Residual plots, Q-Q plots, actual vs predicted comparisons, and decile lift charts are produced for both models and saved as PDFs to the `outputs/` directory.

### CSV Exports
- `freq_model_coefficients.csv` — Poisson model relativities, standard errors, p-values and 95% confidence intervals
- `sev_model_coefficients.csv` — Gamma model relativities in the same format
- `combined_relativities.csv` — Frequency, severity and pure premium relativities for every level of every rating factor

### Model Objects
Fitted model objects are saved as `.rds` files for use in downstream scoring or pricing scripts without re-running the full pipeline.

## Limitations & Next Steps

### Current Limitations
- The frequency and severity models are built seperately and then multiplied to get the pure premium. This assumes that how often a policyholder claims and how much they claim are unrelated which might not always be true
- The dispersion checks in section 6 shows an over dispersion which suggests that a Negative binomial model maybe more suitable than the Poisson model.
- The severity is modelled as an average cost per policy rather than at individual claim level. Eventhough this keeps things simple some details about how claims are distributed within a policy is lost.
- Variable selection for GLMs are done using backward selection at this initial stage however it does not garuntee the best possible set of variables.
- No interactions between variables are been tested at this stage. In motor insurance some combinations can be more material for an example a young driver with high power car can be more riskier than the two factors alone
- The banding boundries were chosen using judgement and a basic credibility check.
- The large loss cap at the 99th percentile is based mainly on the data. In practice reinsurance structure will be taken into account.

### Potential Next Steps
- Negative Binomial frequency model
- Tweediw model instead of two seperate models
- Test combinations of variable and iclude the interaction terms that would improve model fit
- Review and improve current bands
- Include more policy and claim data, also additional variables such as claim history, vehicle characteristics and etc if they become available.

## Notes

- The underlying data are from a French MTPL portfolio. Variable names and geographic fields reflect that origin; the modelling approach and commentary are written in line with UK GI actuarial practice.
- Large individual claims are capped at the 99th percentile of the severity distribution prior to modelling.
- All EDA and model fitting is performed on the training set only. The held-out 30% test set is used exclusively for out-of-sample validation.
- Model outputs require peer review before use in pricing or product filing.


## Author

Imanga Lankathilaka  
Part-Qualified Actuary | FIA Candidate  
[github.com/imangalanka](https://github.com/imangalanka)

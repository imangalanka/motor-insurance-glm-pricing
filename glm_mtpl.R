#===============================================================================
#                                                                            
#  FREQUENCY-SEVERITY GLM ANALYSIS – MOTOR THIRD PARTY LIABILITY (MTPL)      
#                                                                            
#  Purpose  : Build a frequency-severity GLM rating model for a personal     
#             motor portfolio.  The model produces relativities suitable     
#             for use within a risk premium framework.                       
#                                                                            
#  Data     : freMTPL2freq.csv  – one row per policy (French MTPL data)      
#             freMTPL2sev.csv   – one row per individual claim               
#                                                                            
#  Note     : The underlying data are sourced from a French MTPL portfolio.  
#             Variable names and geographic fields reflect that origin;      
#             the modelling approach, commentary and assumptions are written
#             in line with UK General Insurance actuarial practice.          
#                                                                           
#===============================================================================

# ============================================================================
# 0.  ENVIRONMENT SETUP
# 
# This section prepares a clean, reproducible R environment prior to execution
# through;
#
#   1.  Clearing existing workspace objects to prevent object conflicts from 
#       prior sessions
#   2.  Setting global seed value to 42 for full reproducibility across random
#       sampling steps (train/test split)
#   3.  Installing and loading required packages to avoid hard failures on
#       first-run environments
#   4.  Setting a global ggplot theme to ensure visual consistency across all 
#       diagnostic and output plots
#   5.  Defining input file paths and output directory centrally
#
# ============================================================================

# 0.1  Clear workspace
rm(list = ls())
gc()

# 0.2  Set seed for reproducibility 
set.seed(42)

# 0.3  Load required libraries 
#         dplyr / tidyr / readr: data wrangling (preferred pipeline style)
#         ggplot2              : visualisation
#         gridExtra            : multi-panel plots
#         scales               : axis formatting
#         statmod              : Tweedie / GLM residual tools
#         stringr              : String alterations


required_pkgs <- c(
  "dplyr","tidyr","readr","ggplot2","gridExtra",
  "scales","statmod","stringr"
)

# Install missing packages
to_install <- required_pkgs[!required_pkgs %in% installed.packages()[,"Package"]]
if (length(to_install)) {
  install.packages(to_install)
}

# Load all packages
lapply(required_pkgs, library, character.only = TRUE)

# 0.4  Global plot theme -
theme_set(
  theme_bw(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, colour = "grey"),
      axis.title    = element_text(size = 10),
      strip.background = element_rect(fill = "lightgrey"),
      legend.position  = "bottom"
    )
)

# 0.5  File paths 
setwd("C:/Users/imang/Documents/GitHub/GLM Modelling")
PATH_FREQ <- "freMTPL2freq.csv"
PATH_SEV  <- "freMTPL2sev.csv"
DIR_OUT   <- "outputs"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT)


# ============================================================================
# 1.  DATA INGESTION
#
# This section reads the two raw data files that underpin this frequency-
# severity model. Please note the frequency and severity data files are kept
# separate at this stage by design. 
# 
# Tables; 
#    (a) freMTPL2freq  – one row per policy and contains exposure, claim count,
#                        and all other rating factor variables.
#    (b) freMTPL2sev   – one row per individual claim and contains claim 
#                        amounts. A single policy may have multiple rows if 
#                        it has more than one claim.
# 
# Key identifiers is IDpol, which links the two tables (policy identifier).
#      
# Other key fields include;    
#      ClaimNb  : number of claims on the policy year
#      Exposure : fraction of the year at risk (car-years)
#      ClaimAmount: gross incurred amount for the individual claim
#
# Row and column counts for each table are printed for reference.
#
# ============================================================================

cat("\n[1] Loading data...\n")

raw_freq <- readr::read_csv(PATH_FREQ, show_col_types = FALSE)
raw_sev  <- readr::read_csv(PATH_SEV,  show_col_types = FALSE)

cat(sprintf("    Frequency file : %s rows, %s columns\n",
            format(nrow(raw_freq), big.mark = ","), ncol(raw_freq)))
cat(sprintf("    Severity file  : %s rows, %s columns\n",
            format(nrow(raw_sev), big.mark = ","), ncol(raw_sev)))


# ============================================================================
# 2.  DATA CLEANING & VALIDATION
#
# Data are cleaned, validated and logged under the following assumptions and
# transformations. Invalid records are written in error log and removed from
# the modelling dataset.
#
#   
#  1. Frequency Dataset
#       
#       Validation rules, records failing these rules are flagged and removed
#         - Policy ID : unique for each entry
#         - Claim count >= 0
#         - Exposure in the range (0,2]
#         - Driver age in the range [18,100]
#         - Vehicle age > 0
#         - Bonus-Malus score in the range [50,350]
#
#       Transformations applied to clean records:
#         - Vehicle age capped at 20 years to avoid sparse cells at the tail
#         - Standardise text case for fuel type
#         - Vehicle power winsorised to [4, 15] for the same reason
#         - Population density log-transformed (log_density) as raw density
#           spans several orders of magnitude and is better behaved on a
#           log scale as a linear predictor
#         - Binary indicator 'has_claim' created for use in stratified splitting
#   
#
# 2. Severity Dataset
#       
#       Validation rules,records failing these rules are flagged and removed
#         - Claim amount is a positive
#       
#       Transformations applied to clean records
#         - Claims are capped at the 99th percentile of the severity 
#           distribution. This is done to minimise the influence of large 
#           individual losses.
#
# A joined dataset is then created by combining the frequency dataset with the
# severity dataset that is been aggregated to policy level. Policies with no 
# claims will have NA severity. Additionally, average severity per claim is 
# calculated (total claims capped / claim count ) at this stage to be used 
# downstream.
#
# ============================================================================

cat("\n[2] Cleaning and validating data...\n")

# 2.1  Frequency dataset

# Rename columns to conventional variable names
freq_renamed <- raw_freq %>%
  rename(
    policy_id   = IDpol,
    claim_count = ClaimNb,
    exposure    = Exposure,
    veh_power   = VehPower,
    veh_age     = VehAge,
    drv_age     = DrivAge,
    bonus_malus = BonusMalus,
    veh_brand   = VehBrand,
    veh_fuel    = VehGas,
    area        = Area,
    density     = Density,
    region      = Region
  ) 

# Step 1: 
# Identify duplicate policy IDs and log them so the error log captures all 
# offending rows. Only the first occurrence of each policy_id is kept.

dup_log <- freq_renamed %>%
  group_by(policy_id) %>%
  filter(n() > 1) %>%                        # policies with more than one row
  slice(-1) %>%                              # keep first, flag the rest
  ungroup() %>%
  mutate(error_reason = "Duplicate policy ID - removed, first occurrence kept")

# First occurrence of every policy_id is retained
freq_deduped <- freq_renamed %>%
  group_by(policy_id) %>%
  slice(1) %>%
  ungroup()

cat(sprintf("    Duplicate rows removed : %s\n",
            format(nrow(dup_log), big.mark = ",")))

# Step 2: 
# Remaining validation checks on the other variables.

freq_error_log <- bind_rows(
  
  # Invalid claim count: must be non-negative
  freq_deduped %>%
    filter(claim_count < 0) %>%
    mutate(error_reason = "Negative claim count"),
  
  # Invalid exposure: must be in (0, 2]
  freq_deduped %>%
    filter(exposure <= 0 | exposure > 2) %>%
    mutate(error_reason = "Invalid exposure"),
  
  # Invalid driver age: [18, 100]
  freq_deduped %>%
    filter(drv_age < 18 | drv_age > 100) %>%
    mutate(error_reason = "Invalid driver age"),
  
  # Invalid vehicle age: must be non-negative
  freq_deduped %>%
    filter(veh_age < 0) %>%
    mutate(error_reason = "Invalid vehicle age"),
  
  # Invalid Bonus-Malus: must be in [50, 350]
  freq_deduped %>%
    filter(bonus_malus < 50 | bonus_malus > 350) %>%
    mutate(error_reason = "Invalid bonus malus")
)

# Combine multiple error reasons for the same policy
freq_error_log <- freq_error_log %>%
  group_by(policy_id) %>%
  summarise(
    error_reason = paste(unique(error_reason), collapse = "; "),
    across(everything(), first),
    .groups = "drop"
  )

if (nrow(freq_error_log) > 0) {
  warning(
    paste(nrow(freq_error_log),
          "frequency records removed due to validation errors")
  )
}

# Step 3: 
# Apply transformations to clean records only targeting validation failures

freq_clean <- freq_deduped %>%
  anti_join(
    freq_error_log %>% select(policy_id),
    by = "policy_id"
  ) %>%
  mutate(veh_age    = pmin(veh_age, 20L)) %>%
  mutate(veh_fuel   = stringr::str_to_title(veh_fuel)) %>%
  mutate(veh_power  = pmin(pmax(veh_power, 4L), 15L)) %>%
  mutate(log_density = log(density + 1)) %>%
  mutate(has_claim   = as.integer(claim_count > 0))

# Count summary

cat(sprintf("Original rows          : %s\n", format(nrow(raw_freq),
                                                    big.mark = ",")))
cat(sprintf("Duplicate rows removed : %s\n", format(nrow(dup_log),
                                                    big.mark = ",")))
cat(sprintf("Rows after cleaning    : %s\n", format(nrow(freq_clean),
                                                    big.mark = ",")))
cat(sprintf("Rows in error log      : %s\n", format(nrow(freq_error_log),
                                                    big.mark = ",")))
cat(sprintf("Total rows removed     : %s\n",
            format(nrow(raw_freq) - nrow(freq_clean), big.mark = ",")))


# 2.2  Severity dataset
#-------------------------------------------------------------------------------

sev_renamed <- raw_sev %>%
  rename(
    policy_id    = IDpol,
    claim_amount = ClaimAmount
  ) 

# Create error log
sev_error_log <- sev_renamed %>%
  filter(claim_amount <= 0) %>%
  mutate(
    error_reason = "Negative or zero claim amount"
  )

# Create cleaned severity dataset
sev_clean <- sev_renamed %>%
  
  # Remove invalid claim records
  filter(claim_amount > 0) %>%
  
  # Cap extreme claims at 99th percentile
  mutate(
    cap_99 = quantile(
      claim_amount,
      0.99,
      na.rm = TRUE
    ),
    claim_capped = pmin(
      claim_amount,
      cap_99
    )
  )

cat(sprintf("Severity rows after cleaning : %s\n", 
            format(nrow(sev_clean), big.mark = ",")))
cat(sprintf("Error rows logged            : %s\n",
            format(nrow(sev_error_log), big.mark = ",")))
cat(sprintf("Large loss cap (99th pct)    : £%s\n", 
            format(round(sev_clean$cap_99[1]), big.mark = ",")))



# 2.3  Join severity back to frequency file 
#-------------------------------------------------------------------------------

# Aggregate claim amounts to policy level (summing all claims on a policy) 
# then merge.  Policies with no claims will have NA severity, this is expected 
# and handled downstream.

sev_by_policy <- sev_clean %>%
  group_by(policy_id) %>%
  summarise(
    total_claim_amount = sum(claim_amount),
    total_claim_capped = sum(claim_capped),
    n_sev_records      = n(),
    .groups = "drop"
  )

modelling_data <- freq_clean %>%
  left_join(sev_by_policy, by = "policy_id") %>%
  mutate(
    # Average severity per claim (used in severity model)
    avg_severity = total_claim_capped / pmax(claim_count, 1L)
  )

cat(sprintf("    Policies with at least one claim : %s (%.1f%%)\n",
            format(sum(modelling_data$claim_count > 0), big.mark = ","),
            100 * mean(modelling_data$claim_count > 0)))


# ============================================================================
# 3.  BANDING / FACTOR ENGINEERING
# ============================================================================
#
# In this section continuous variables are sorted into discrete bands and 
# reference levels are set for each factor prior to model fitting.
#
#   Reasons for banding continuous variables include;
#     (a) GLMs with categorical predictors are the industry standard for
#         producing auditable, interpretable rating factors.
#     (b) Bands allow the model to fit non-monotone relationships.
#     (c) They can improve stability and interpretability, provided each 
#         band has sufficient exposure and claims credibility.
#
#   Reasons for band boundary selection;
#     (a) Meaningful natural breakpoint based on best judgement
#     (b) Statistically credible, assessed using both exposure and claim counts
#         for frequency, and claim counts / claim cost observations for severity
# 
# Variables banded in this section;
#     - drv_age_band : driver age in 8 bands from 18-22 through 71+
#     - veh_age_band : vehicle age in 5 bands from 0-1 through 11+
#     - veh_power_band : vehicle power in 5 bands from 4-5 through 12+
#     - bm_band : Bonus-Malus score in 8 bands from 50-54 through 151+
#     - density_band : log population density cut into 5 equal exposure 
#                      quantiles (Q1 Rural through Q5 Urban)
#
# For reference level selection the most populated band/catergory has been  
# selected to minimise the standard error on the base level estimate.
#
# ============================================================================

cat("\n[3] Engineering rating factors...\n")

modelling_data <- modelling_data %>%
  mutate(
    
    # Driver age bands (years) --
    drv_age_band = cut(
      drv_age,
      breaks = c(18, 22, 26, 30, 40, 50, 60, 70, 100),
      labels = c("18-22", "23-26", "27-30", "31-40",
                 "41-50", "51-60", "61-70", "71+"),
      right  = TRUE, include.lowest = TRUE
    ),
    
    # Vehicle age bands (years) -
    veh_age_band = cut(
      veh_age,
      breaks = c(0, 1, 3, 6, 10, 20),
      labels = c("0-1", "2-3", "4-6", "7-10", "11+"),
      right  = TRUE, include.lowest = TRUE
    ),
    
    # Vehicle power bands --
    veh_power_band = cut(
      veh_power,
      breaks = c(4, 5, 7, 9, 11, 15),
      labels = c("4-5", "6-7", "8-9", "10-11", "12+"),
      right  = TRUE, include.lowest = TRUE
    ),
    
    # Bonus-Malus bands 
    #    The BM scale proxies claims history.  Levels below 100 indicate
    #    bonus; above 100 indicate malus.
    bm_band = cut(
      bonus_malus,
      breaks = c(50, 54, 60, 70, 80, 100, 120, 150, 350),
      labels = c("50-54", "55-60", "61-70", "71-80",
                 "81-100", "101-120", "121-150", "151+"),
      right  = TRUE, include.lowest = TRUE
    ),
    
    # Population density band (log scale) --
    density_band = cut(
      log_density,
      breaks = quantile(log_density, probs = seq(0, 1, 0.2)),
      labels = c("Q1 Rural", "Q2", "Q3", "Q4", "Q5 Urban"),
      right  = TRUE, include.lowest = TRUE
    )
  )

# Set reference levels – chosen as the most populated band to
# minimise standard errors on the base level estimate

get_ref_level <- function(x){
  names(which.max(table(x)))
}

modelling_data <- modelling_data %>%
  mutate(
    drv_age_band   = relevel(drv_age_band, ref = get_ref_level(drv_age_band)),
    veh_age_band   = relevel(veh_age_band, ref = get_ref_level(veh_age_band)),
    veh_power_band = relevel(veh_power_band, ref = get_ref_level(veh_power_band)),
    bm_band        = relevel(bm_band, ref = get_ref_level(bm_band)),
    veh_fuel       = factor(veh_fuel),
    veh_fuel       = relevel(veh_fuel, ref = get_ref_level(veh_fuel)),
    area           = factor(area),
    area           = relevel(area, ref = get_ref_level(area)),
    density_band   = relevel(density_band, ref = get_ref_level(density_band)),
    veh_brand      = factor(veh_brand),
    veh_brand      = relevel(veh_brand, ref = get_ref_level(veh_brand)),
    region         = factor(region),
    region         = relevel(region, ref = get_ref_level(region))
  )


# ============================================================================
# 4.  TRAIN / TEST SPLIT
# ============================================================================
#
# This section splits the modelling dataset into train and test datasets 
# at 70/30 proportion. The split is stratified on 'has_claim' indicator 
# to ensure the training and test sets are directly comparable and neither 
# is materially richer or poorer in claims than the underlying portfolio
#
# ============================================================================

cat("\n[4] Creating train/test split (70/30 stratified)...\n")

set.seed(42)
train_idx <- modelling_data %>%
  mutate(row_id = row_number()) %>%
  group_by(has_claim) %>%
  slice_sample(prop = 0.70) %>%
  pull(row_id)

train <- modelling_data[ train_idx, , drop = FALSE]
test  <- modelling_data[-train_idx, , drop = FALSE]


cat(sprintf("    Training set : %s policies (%.1f%%)\n",format(nrow(train),
            big.mark = ","), 100 * nrow(train) / nrow(modelling_data)))
cat(sprintf("    Test set     : %s policies (%.1f%%)\n", format(nrow(test),  
            big.mark = ","), 100 * nrow(test)  / nrow(modelling_data)))
cat(sprintf("    Train claim rate : %.3f\n", 
            weighted.mean(train$claim_count, train$exposure)))
cat(sprintf("    Test  claim rate : %.3f\n", 
            weighted.mean(test$claim_count,  test$exposure)))


# ============================================================================
# 5.  EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================================
#
#  This section performs exploratory data analysis to understand the univariate
#  distribution of each rating factor and it's unadjusted relationship with
#  claim frequency. Note that the EDA is performed only on the training set 
#  to avoid data leakage.
# 
#  The following outputs are created;
#     
#     1. Portfolio summaries for  headline exposure, claim count and frequency 
#        statistics for training set.
#     2. Observed claim frequency plots - single bubble plot per rating factor 
#        showing unadjusted frequency vs the portfolio average. Helpful to 
#        assess whether each variable has a signal worth modelling and to 
#        validate banding decisions from section 3.
#     3. Severity distribution - linear and log-log histrograms of individual 
#        claim amounts plots created. Used to visually justify the GLM family 
#        choice in section 8.
#     4. Exposure profiles – bar charts showing earned car-years by driver
#        age and vehicle age band.  Used to assess credibility of observed
#        frequencies in 5.2.
#
# Outputs saved to:
#    outputs/01_EDA_Frequency_Plots.pdf
#    outputs/02_EDA_Severity_Plots.pdf
#    outputs/03_EDA_Exposure_Profiles.pdf
#
# ============================================================================

cat("\n[5] Performing exploratory data analysis...\n")

# Helper function created to summarise observed frequency by a grouping 
# variable

# Training data split for severity data to avoid exposing test information
sev_train <- sev_clean %>%
  semi_join(train, by = "policy_id")

obs_freq_summary <- function(data, group_var) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      earned_exposure = sum(exposure),
      claim_count     = sum(claim_count),
      obs_frequency   = claim_count / earned_exposure,
      .groups = "drop"
    )
}

# 5.1  Portfolio summary 

cat("\n    Portfolio Summary (Training Set) \n")
cat(sprintf("    Total earned car-years  : %s\n",
            format(round(sum(train$exposure)), big.mark = ",")))
cat(sprintf("    Total claim count       : %s\n",
            format(sum(train$claim_count), big.mark = ",")))
cat(sprintf("    Observed claim frequency: %.4f\n",
            sum(train$claim_count) / sum(train$exposure)))
cat(sprintf("    Mean claim severity     : €%s\n",
            format(round(mean(sev_train$claim_amount)), big.mark = ",")))
cat(sprintf("    Total incurred claims   : €%s\n",
            format(round(sum(train$total_claim_capped, na.rm = TRUE)),
                   big.mark = ",")))


# 5.2  Frequency by key rating factors

plot_obs_freq <- function(data, group_var, x_label) {
  
  plot_data <- obs_freq_summary(data, group_var) %>%
    filter(!is.na(.data[[group_var]]),
           !is.na(obs_frequency),
           earned_exposure > 0)
  
  portfolio_freq <- sum(data$claim_count, na.rm = TRUE) /
    sum(data$exposure, na.rm = TRUE)
  
  ggplot(
    plot_data,
    aes(
      x = .data[[group_var]],
      y = obs_frequency,
      size = earned_exposure
    )
  ) +
    geom_point(colour = "blue", alpha = 0.8) +
    geom_hline(
      yintercept = portfolio_freq,
      linetype = "dashed",
      colour = "red",
      linewidth = 0.6
    ) +
    scale_size_continuous(
      name = "Earned\nExposure",
      range = c(2, 8),
      labels = scales::comma
    ) +
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.001),
      expand = expansion(mult = c(0.12, 0.08))
    ) +
    labs(
      title    = paste("Observed Claim Frequency by", x_label),
      subtitle = "Red dashed line = portfolio average; bubble size = exposure",
      x        = x_label,
      y        = "Claim Frequency (claims per car-year)"
    ) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

p_freq_drvage  <- plot_obs_freq(train, "drv_age_band","Driver Age Band")
p_freq_vehage  <- plot_obs_freq(train, "veh_age_band","Vehicle Age Band")
p_freq_pwr     <- plot_obs_freq(train, "veh_power_band","Vehicle Power Band")
p_freq_bm      <- plot_obs_freq(train, "bm_band", "Bonus-Malus Band")
p_freq_fuel    <- plot_obs_freq(train, "veh_fuel","Fuel Type")
p_freq_area    <- plot_obs_freq(train, "area", "Area Code")
p_freq_density <- plot_obs_freq(train, "density_band","Population Density Band")
p_freq_brand   <- plot_obs_freq(train, "veh_brand", "Vehicle Brand")
p_freq_region  <- plot_obs_freq(train, "region","Region")

# 5.3  Severity distribution 

# Linear histogram
p_sev_hist <- sev_train %>%
  filter(claim_amount <= quantile(claim_amount, 0.99)) %>%
  ggplot(aes(x = claim_amount)) +
  geom_histogram(bins = 60, fill = "blue", colour = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Distribution of Individual Claim Amounts",
    subtitle = "Capped at 99th percentile for display",
    x        = "Claim Amount (€)",
    y        = "Count"
  )

# Log-log histogram
p_sev_loglog <- sev_train %>%
  ggplot(aes(x = claim_amount)) +
  geom_histogram(bins = 60, fill = "orange", colour = "white") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title    = "Claim Amount Distribution (log-log scale)",
    subtitle = "Log-log scale highlights heavy tail behaviour",
    x        = "Claim Amount (€, log scale)",
    y        = "Count (log scale)"
  )

# 5.4  Exposure profile by key factors 

# Diver age
p_exp_drvage <- train %>%
  group_by(drv_age_band) %>%
  summarise(earned_exposure = sum(exposure), .groups = "drop") %>%
  ggplot(aes(x = drv_age_band, y = earned_exposure)) +
  geom_col(fill = "green") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Exposure Profile: Driver Age",
       x = "Driver Age Band", y = "Earned Car-Years") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Vehicle age
p_exp_vehage <- train %>%
  group_by(veh_age_band) %>%
  summarise(earned_exposure = sum(exposure), .groups = "drop") %>%
  ggplot(aes(x = veh_age_band, y = earned_exposure)) +
  geom_col(fill = "green") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Exposure Profile: Vehicle Age",
       x = "Vehicle Age Band", y = "Earned Car-Years")

# 5.5  Save EDA plots to file -

pdf(file.path(DIR_OUT, "01_EDA_Frequency_Plots.pdf"), width = 12, height = 8)
print(p_freq_drvage)
print(p_freq_vehage)
print(p_freq_pwr)
print(p_freq_bm)
print(p_freq_fuel)
print(p_freq_area)
print(p_freq_density)
print(p_freq_brand)
print(p_freq_region)
dev.off()

pdf(file.path(DIR_OUT, "02_EDA_Severity_Plots.pdf"), width = 10, height = 5)
gridExtra::grid.arrange(p_sev_hist, p_sev_loglog, ncol = 2)
dev.off()

pdf(file.path(DIR_OUT, "03_EDA_Exposure_Profiles.pdf"), width = 10, height = 5)
gridExtra::grid.arrange(p_exp_drvage, p_exp_vehage, ncol = 2)
dev.off()

cat("    EDA plots saved to outputs/ directory\n")


# ============================================================================
# 6.  FREQUENCY MODEL
# ============================================================================
#
# Frequency model specifics
#  
#   Target   : Claim count (ClaimNb)
#   Family   : Poisson with log link
#   Offset   : log(Exposure), allowing the model to estimate claim frequency 
#              (claims per car-year) rather than raw claim counts.
#
#   By including log(Exposure) as an offset, the linear predictor becomes;
#     
#       log(E[claims]) = log(Exposure) + Xb
#
#   Poisson family is used as the starting point, might consider
#   other distributions as future refinements depending on the overall fit. 
#
#   Variable selection rationale:
#     - drv_age_band   : primary driver characteristic; young and elderly
#                        drivers are well established higher-risk groups in
#                        motor insurance.
#     - veh_age_band   : captures differences in claim frequency associated 
#                        with vehicle age, technology, usage patterns
#                        and ownership characteristics.
#     - veh_power_band : captures differences in claim propensity associated 
#                        with vehicle performance characteristics
#     - bm_band        : historically one of the most predictive rating factors 
#                        in motor insurance, reflecting prior claims experience 
#                        and driving behaviour.
#     - veh_fuel       : diesel/petrol behavioural differences
#     - area           : geographic risk differences across regions
#     - density_band   : urbanicity proxy, denser areas correlates with higher 
#                        TPL exposure and accident frequency
#
# This section is structured as follows;
#
#   1. Full model fit and dispersion diagnostics
#   2. Coefficients exponentiated to produce rating relativities. A relativity
#      above 1.0 indicates higher predicted frequency than the reference level
#      and below 1.0 indicates lower.
#   3. Sequential ANOVA Type I, likelihood-ratio chi-square tests to assess 
#      whether each rating factor contributes significantly to model fit. 
#
# ============================================================================

cat("\n[6] Fitting frequency GLM...\n")

# 6.1  Full frequency model — all candidate variables
freq_formula_full <- claim_count ~
  drv_age_band   +
  veh_age_band   +
  veh_power_band +
  bm_band        +
  veh_fuel       +
  area           +
  density_band   +
  veh_brand      +
  region         +
  offset(log(exposure))

glm_freq_full <- glm(
  formula = freq_formula_full,
  family  = poisson(link = "log"),
  data    = train
)

cat("\n    Full model ANOVA (Chi-square tests, sequential):\n")
freq_anova_full <- anova(glm_freq_full, test = "Chisq")
print(freq_anova_full)

# 6.2  Backward stepwise variable selection by AIC
glm_freq <- step(glm_freq_full, direction = "backward", trace = 1)

cat("\n    Final frequency formula selected:\n")
print(formula(glm_freq))

# 6.3  Final model diagnostics and summary
cat("\n    Frequency model fitted\n")
cat(sprintf("    Null deviance      : %.1f on %d df\n",
            glm_freq$null.deviance, glm_freq$df.null))
cat(sprintf("    Residual deviance  : %.1f on %d df\n",
            glm_freq$deviance, glm_freq$df.residual))
cat(sprintf("    AIC                : %.1f\n", AIC(glm_freq)))

# Dispersion checks
deviance_disp <- glm_freq$deviance / glm_freq$df.residual
deviance_msg  <- if (deviance_disp > 1.5) {
  "(evidence of over-dispersion – alternative count models may be considered)"
} else if (deviance_disp < 0.8) {
  "(evidence of under-dispersion – review data structure and Pearson dispersion)"
} else {
  "(dispersion broadly consistent with Poisson assumptions)"
}

pearson_disp <- sum(residuals(glm_freq, type = "pearson")^2) / glm_freq$df.residual
pearson_msg  <- if (pearson_disp > 1.5) {
  "(evidence of over-dispersion – alternative count models may be considered)"
} else if (pearson_disp < 0.8) {
  "(evidence of under-dispersion – review data structure and residual diagnostics)"
} else {
  "(dispersion broadly consistent with Poisson assumptions)"
}

cat(sprintf("    Deviance dispersion ratio : %.3f  %s\n", deviance_disp, deviance_msg))
cat(sprintf("    Pearson dispersion ratio  : %.3f  %s\n", pearson_disp, pearson_msg))

# Relativities table
coef_freq_table <- summary(glm_freq)$coefficients
freq_summary <- data.frame(
  term        = rownames(coef_freq_table),
  estimate    = exp(coef_freq_table[, "Estimate"]),
  conf.low    = exp(coef_freq_table[, "Estimate"] - 1.96 * coef_freq_table[, "Std. Error"]),
  conf.high   = exp(coef_freq_table[, "Estimate"] + 1.96 * coef_freq_table[, "Std. Error"]),
  std.error   = coef_freq_table[, "Std. Error"],
  p.value     = coef_freq_table[, "Pr(>|z|)"],
  z.statistic = coef_freq_table[, "z value"]
)

cat("\n    Frequency model relativities (exponentiated coefficients):\n")
print(freq_summary %>%
        mutate(across(where(is.numeric), ~round(., 4))) %>%
        select(term, relativity = estimate, std.error, p.value, conf.low, conf.high))

# ANOVA on final selected model
cat("\n    Final model ANOVA (Chi-square tests, sequential):\n")
freq_anova_final <- anova(glm_freq, test = "Chisq")
print(freq_anova_final)


# ============================================================================
# 7.  FREQUENCY MODEL DIAGNOSTICS & VALIDATION
# ============================================================================
#
# This section assesses the fit and predictive performance of the frequency 
# model through a series of diagnostic test on the training dataset and 
# additional out-of-sample tests performed on the test data
#
# The tests conducted and their respective outputs are as follows;
#
#   1. Residual diagnostics (training set)
#       - Deviance residual vs fitted values to check for systematic bias or
#         or patterns in residuals. 
#       - Q-Q plot of deviance residuals to check whether the residual 
#         distribution is consistent with the Poisson assumptions.
#   
#   2. Actual vs predicted frequency by rating factor (training set)
#      Observed and fitted claim frequencies are compared within each band for
#      every rating factor.
#
#   3. Out-of-sample prediction (test set)
#      The model is applied to the held-out 30% test set. Actual and predicted
#      claim rates are compared at portfolio level.
#
#   4. Decile lift chart (test set)
#      Test policies are ranked into deciles by predicted claim frequency. Actual
#      frequency is then plotted against predicted within each decile.
#
# Outputs are saved to;
#     output/04_Freq_Model_Diagnostics.pdf
#     output/05_Freq_Actual_vs_Predicted.pdf
#     output/06_Freq_Decile_Lift.pdf
#
# ============================================================================

cat("\n[7] Running frequency model diagnostics...\n")

# 7.1  Deviance residuals 

train_freq_diag <- train %>%
  mutate(
    fitted_freq  = fitted(glm_freq),
    dev_resid    = residuals(glm_freq, type = "deviance"),
    pearson_res  = residuals(glm_freq, type = "pearson"),
    fitted_rate  = fitted_freq / exposure
  )

# Sample down for plotting only - keeps the full dataset intact
train_freq_diag_sample <- train_freq_diag %>%
  slice_sample(n = 5000)

p_resid_fitted <- ggplot(train_freq_diag_sample,
                         aes(x = fitted_rate, y = dev_resid)) +
  geom_point(alpha = 0.15, size = 0.6, colour = "blue") +
  geom_smooth(method = "loess", se = FALSE, colour = "red",
              linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_log10(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Frequency Model: Deviance Residuals vs Fitted Values",
    subtitle = "Random scatter around zero indicates good model fit",
    x        = "Predicted Annual Claim Frequency",
    y        = "Deviance Residual"
  )


p_resid_qq <- ggplot(train_freq_diag, aes(sample = dev_resid)) +
  stat_qq(alpha = 0.3, size = 0.5, colour = "blue") +
  stat_qq_line(colour = "red") +
  labs(
    title    = "Frequency Model: Q-Q Plot of Deviance Residuals",
    subtitle = "Points should lie close to the diagonal for good fit",
    x        = "Theoretical Quantiles",
    y        = "Sample Quantiles"
  )



# 7.2  Actual vs predicted by rating factor

avp_by_factor <- function(data, fitted_col, group_var, x_label) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      earned_exposure = sum(exposure),
      actual_freq     = sum(claim_count) / sum(exposure),
      predicted_freq  = sum(.data[[fitted_col]]) / sum(exposure),
      .groups = "drop"
    ) %>%
    mutate(across(all_of(group_var), as.factor)) %>%
    pivot_longer(cols = c(actual_freq, predicted_freq),
                 names_to = "type", values_to = "frequency") %>%
    mutate(type = case_when(
      type == "actual_freq" ~ "Actual",
      type == "predicted_freq" ~ "Predicted",
      TRUE ~ type
      )) %>%
    ggplot(aes(x = .data[[group_var]], y = frequency,
               colour = type, group = type)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Actual" = "blue",
                                   "Predicted" = "red"),
                        name = NULL) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(
      title = paste("Actual vs Predicted Frequency:", x_label),
      x     = x_label,
      y     = "Claim Frequency"
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

train_freq_diag2 <- train %>%
  mutate(fitted_freq = fitted(glm_freq))


p_avp_drvage  <- avp_by_factor(train_freq_diag2, "fitted_freq",
                               "drv_age_band",   "Driver Age Band")
p_avp_bm      <- avp_by_factor(train_freq_diag2, "fitted_freq",
                               "bm_band",        "Bonus-Malus Band")
p_avp_vehage  <- avp_by_factor(train_freq_diag2, "fitted_freq",
                               "veh_age_band",   "Vehicle Age Band")
p_avp_area    <- avp_by_factor(train_freq_diag2, "fitted_freq",
                               "area",           "Area Code")


# 7.3  Out-of-sample prediction on test set

test_freq_pred <- test %>%
  mutate(
    pred_freq   = predict(glm_freq, newdata = test, type = "response"),
    pred_rate   = pred_freq / exposure
  )


# Actual vs predicted at portfolio level
cat(sprintf("    Test set actual  claim rate : %.4f\n",
            sum(test_freq_pred$claim_count) / sum(test_freq_pred$exposure)))
cat(sprintf("    Test set predicted claim rate: %.4f\n",
            sum(test_freq_pred$pred_freq) / sum(test_freq_pred$exposure)))


# 7.4  Decile lift chart

p_decile_freq <- test_freq_pred %>%
  mutate(decile = ntile(pred_rate, 10)) %>%
  group_by(decile) %>%
  summarise(
    actual    = sum(claim_count) / sum(exposure),
    predicted = sum(pred_freq)   / sum(exposure),
    .groups   = "drop"
  ) %>%
  pivot_longer(cols = c(actual, predicted),
               names_to = "type", values_to = "frequency") %>%
  mutate(type = stringr::str_to_title(type)) %>%
  ggplot(aes(x = decile, y = frequency, colour = type, group = type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Actual" = "blue",
                                 "Predicted" = "red"),
                      name = NULL) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title    = "Frequency Model: Decile Lift Chart (Test Set)",
    subtitle = "Policies ranked by predicted frequency decile (test set)",
    x        = "Predicted Frequency Decile (1 = Lowest Risk)",
    y        = "Claim Frequency"
  )


# 7.5  Save frequency diagnostic plots

pdf(file.path(DIR_OUT, "04_Freq_Model_Diagnostics.pdf"),
    width = 12, height = 10)
gridExtra::grid.arrange(p_resid_fitted, p_resid_qq, ncol = 2)
dev.off()

pdf(file.path(DIR_OUT, "05_Freq_Actual_vs_Predicted.pdf"),
    width = 14, height = 10)
gridExtra::grid.arrange(p_avp_drvage, p_avp_bm,
                        p_avp_vehage, p_avp_area, ncol = 2)
dev.off()

pdf(file.path(DIR_OUT, "06_Freq_Decile_Lift.pdf"), width = 9, height = 5)
print(p_decile_freq)
dev.off()

cat("    Frequency diagnostic plots saved\n")


# ============================================================================
# 8.  SEVERITY MODEL
# ============================================================================
#
#  The severity model is fitted to policies with at least one claim.
#
#  Target : Average claim amount per policy (total_claim_capped /
#             claim_count) – sometimes called "average cost per claim"
#             (ACPC) in UK parlance.
#
#  Family : Gamma with log link
#    - Gamma is the canonical choice for claim severity in UK GI:
#      it is right-skewed, strictly positive, and has variance
#      proportional to the mean squared (consistent with observed data).
#    - The log link ensures multiplicative structure consistent with
#      the frequency model (needed to combine into pure premium).
#
#  Variable selection:
#    A full model is fitted first containing all candidate rating factors
#    (the same set considered in the frequency model). Backward stepwise
#    selection via step() is then applied, retaining only variables that
#    improve AIC.
#
#  Note on variable selection:
#    Not all frequency drivers are necessarily severity drivers.  The
#    Bonus-Malus score, for instance, is a strong frequency driver but
#    may have a weaker severity relationship.  Variables are assessed
#    individually via univariate analysis and the ANOVA table.
#
# This section includes the following;
#     1. Univariate severity EDA, observed average severity by factor
#     2. Full model fit and backward variable selection via step()
#     3. Final model diagnostics, relativities, and ANOVA
#
# ============================================================================

cat("\n[8] Fitting severity GLM...\n")

# Training data – policies with at least one claim
train_sev <- train %>%
  filter(claim_count > 0, !is.na(avg_severity), avg_severity > 0)

cat(sprintf("    Severity training records : %s\n",
            format(nrow(train_sev), big.mark = ",")))

# 8.1  Univariate severity exploration

obs_sev_summary <- function(data, group_var) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      claim_count  = sum(claim_count),
      total_amount = sum(total_claim_capped),
      avg_sev      = total_amount / claim_count,
      .groups      = "drop"
    )
}

plot_obs_sev <- function(data, group_var, x_label) {
  obs_sev_summary(data, group_var) %>%
    ggplot(aes(x = .data[[group_var]], y = avg_sev, size = claim_count)) +
    geom_point(colour = "orange", alpha = 0.8) +
    geom_hline(yintercept = mean(data$avg_severity, na.rm = TRUE),
               linetype = "dashed", colour = "red", linewidth = 0.6) +
    scale_size_continuous(name = "Claim\nCount", labels = scales::comma,
                          range = c(2, 8)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = paste("Observed Average Severity by", x_label),
      subtitle = "Red dashed line = overall mean severity; bubble size = claim count",
      x        = x_label,
      y        = "Average Claim Amount (€)"
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

p_sev_drvage  <- plot_obs_sev(train_sev, "drv_age_band", "Driver Age Band")
p_sev_vehage  <- plot_obs_sev(train_sev, "veh_age_band", "Vehicle Age Band")
p_sev_pwr     <- plot_obs_sev(train_sev, "veh_power_band", "Vehicle Power Band")
p_sev_fuel    <- plot_obs_sev(train_sev, "veh_fuel", "Fuel Type")
p_sev_area    <- plot_obs_sev(train_sev, "area", "Area Code")
p_sev_bm      <- plot_obs_sev(train_sev, "bm_band", "Bonus-Malus Band")
p_sev_density <- plot_obs_sev(train_sev,"density_band","Population Density Band")
p_sev_brand   <- plot_obs_sev(train_sev, "veh_brand", "Vehicle Brand")
p_sev_region  <- plot_obs_sev(train_sev, "region","Region")

pdf(file.path(DIR_OUT,"07_EDA_Severity_by_Factor.pdf"), width = 16, height = 24)
gridExtra::grid.arrange(p_sev_drvage, p_sev_vehage,p_sev_pwr, p_sev_fuel,
                        p_sev_area, p_sev_bm, p_sev_density, p_sev_brand,
                        p_sev_region, ncol = 2)
dev.off()


# 8.2  Variable selection via step

# Full model including all candidate variables — step() will drop those
# that do not improve AIC, avoiding the need to hardcode the final formula.

sev_formula_full <- avg_severity ~
  drv_age_band   +
  veh_age_band   +
  veh_power_band +
  bm_band        +
  veh_fuel       +
  area           +
  density_band   +
  veh_brand      +
  region  

glm_sev_full <- glm(
  formula = sev_formula_full,
  family  = Gamma(link = "log"),
  data    = train_sev,
  weights = claim_count    # weight by claim count for credibility
)

cat("\n    Full Severity Model ANOVA (Chi-square tests, sequential):\n")
sev_anova_full <- anova(glm_sev_full, test = "Chisq")
print(sev_anova_full)

glm_sev <- step(glm_sev_full, direction = "backward", trace = 1)

cat("\n    Final severity formula:\n")
print(formula(glm_sev))


# 8.3  Final severity model diagnostic summary

cat("    Severity model fitted\n")
cat(sprintf("    Null deviance     : %.1f on %d df\n",
            glm_sev$null.deviance, glm_sev$df.null))
cat(sprintf("    Residual deviance : %.1f on %d df\n",
            glm_sev$deviance, glm_sev$df.residual))
cat(sprintf("    AIC               : %.1f\n", AIC(glm_sev)))

# Gamma dispersion estimate (phi)
dispersion_sev <- summary(glm_sev)$dispersion
cat(sprintf("    Gamma dispersion (phi): %.4f\n", dispersion_sev))

coef_sev_table <- summary(glm_sev)$coefficients

sev_summary <- data.frame(
                term        = rownames(coef_sev_table),
                estimate    = exp(coef_sev_table[,"Estimate"]),
                conf.low    = exp(coef_sev_table[,"Estimate"] 
                                - 1.96 * coef_sev_table[,"Std. Error"]),
                conf.high   = exp(coef_sev_table[,"Estimate"] 
                                + 1.96 * coef_sev_table[,"Std. Error"]),
                std.err     = coef_sev_table[,"Std. Error"],
                p.value     = coef_sev_table[, "Pr(>|t|)"],
                t.statistic = coef_sev_table[, "t value"]
)

cat("\n    Severity model relativities (exponentiated coefficients):\n")
print(sev_summary %>%
        mutate(across(where(is.numeric), ~round(., 4))) %>%
        select(term, relativity = estimate, std.err, p.value, conf.low, conf.high))

cat("\n    ANOVA (Chi-square tests, sequential):\n")
sev_anova_final <- anova(glm_sev, test = "Chisq")
print(sev_anova_final)


# ============================================================================
# 9.  SEVERITY MODEL DIAGNOSTICS
# ============================================================================
#
#  This section assesses the fit and predictive performance of the severity
#  model through residual diagnostics on the training data and out-of-sample
#  validation on the held-out test set.
#
#  The tests conducted and their respective outputs are as follows:
#
#    1. Residual diagnostics (training set)
#       - Deviance residuals vs log(fitted values) to check for systematic
#         bias or non-linear patterns in the residuals.
#       - Q-Q plot of deviance residuals to assess whether the residual
#         distribution is consistent with the Gamma assumptions.
#
#    2. Decile lift chart (test set)
#       Test policies with at least one claim are ranked into deciles by
#       predicted average severity. Actual and predicted average claim
#       amounts are compared within each decile to assess rank-ordering
#       and calibration of the model out-of-sample.
#
#  Note: an actual vs predicted by rating factor plot (as produced for
#  the frequency model in section 7) is not included here. Severity is
#  modelled at claim level on a filtered subset, so factor-level
#  comparisons are better captured through the univariate EDA in 8.1
#  and the decile lift chart below.
#
#  Outputs are saved to:
#      output/08_Sev_Model_Diagnostics.pdf
#      output/09_Sev_Decile_Lift.pdf
#
# ============================================================================

cat("\n[9] Running severity model diagnostics...\n")

train_sev_diag <- train_sev %>%
  mutate(
    fitted_sev  = fitted(glm_sev),
    dev_resid   = residuals(glm_sev, type = "deviance"),
    pearson_res = residuals(glm_sev, type = "pearson")
  )

p_sev_resid <- ggplot(train_sev_diag,
                      aes(x = log(fitted_sev), y = dev_resid)) +
  geom_point(alpha = 0.2, size = 0.8, colour = "orange") +
  geom_smooth(method = "loess", se = FALSE, colour = "red",
              linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title    = "Severity Model: Deviance Residuals vs Fitted Values",
    x        = "log(Fitted Average Severity)",
    y        = "Deviance Residual"
  )

p_sev_qq <- ggplot(train_sev_diag, aes(sample = dev_resid)) +
  stat_qq(alpha = 0.3, size = 0.7, colour = "orange") +
  stat_qq_line(colour = "red") +
  labs(
    title = "Severity Model: Q-Q Plot of Deviance Residuals",
    x     = "Theoretical Quantiles",
    y     = "Sample Quantiles"
  )

# Actual vs Predicted severity – out of sample (test with claims)
test_sev <- test %>%
  filter(claim_count > 0, !is.na(avg_severity), avg_severity > 0)

test_sev_pred <- test_sev %>%
  mutate(pred_sev = predict(glm_sev, newdata = test_sev, type = "response"))

p_sev_avp <- test_sev_pred %>%
  mutate(decile = ntile(pred_sev, 10)) %>%
  group_by(decile) %>%
  summarise(
    actual    = sum(total_claim_capped) / sum(claim_count),
    predicted = sum(pred_sev * claim_count) / sum(claim_count),
    .groups   = "drop"
  ) %>%
  pivot_longer(cols = c(actual, predicted),
               names_to = "type", values_to = "severity") %>%
  mutate(type = stringr::str_to_title(type)) %>%
  ggplot(aes(x = decile, y = severity, colour = type, group = type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Actual" = "orange",
                                 "Predicted" = "red"),
                      name = NULL) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Severity Model: Decile Lift Chart (Test Set)",
    x     = "Predicted Severity Decile (1 = Lowest)",
    y     = "Average Claim Amount (€)"
  )

pdf(file.path(DIR_OUT, "08_Sev_Model_Diagnostics.pdf"), width = 12, height = 5)
gridExtra::grid.arrange(p_sev_resid, p_sev_qq, ncol = 2)
dev.off()

pdf(file.path(DIR_OUT, "09_Sev_Decile_Lift.pdf"), width = 9, height = 5)
print(p_sev_avp)
dev.off()

cat("    Severity diagnostic plots saved\n")


# ============================================================================
# 10.  PURE PREMIUM & RISK RELATIVITIES
# ============================================================================
#
#  The risk premium (pure premium) for a policy is calculated as:
#
#      Risk Premium = Predicted Frequency × Predicted Severity
#
#  This is then expressed relative to the base level (reference class)
#  to produce multiplicative rating factors.
#
# ============================================================================

cat("\n[10] Computing pure premium relativities...\n")

# 10.1  Base-level pure premium

# Helper function for reference level = first level after relevel() in section 3
ref_level <- function(x) levels(x)[1]

base_policy <- train[1, ]

base_policy$drv_age_band   <- factor(ref_level(train$drv_age_band),
                                     levels = levels(train$drv_age_band))
base_policy$veh_age_band   <- factor(ref_level(train$veh_age_band),
                                     levels = levels(train$veh_age_band))
base_policy$veh_power_band <- factor(ref_level(train$veh_power_band),
                                     levels = levels(train$veh_power_band))
base_policy$bm_band        <- factor(ref_level(train$bm_band),
                                     levels = levels(train$bm_band))
base_policy$veh_fuel       <- factor(ref_level(train$veh_fuel),
                                     levels = levels(train$veh_fuel))
base_policy$area           <- factor(ref_level(train$area),
                                     levels = levels(train$area))
base_policy$density_band   <- factor(ref_level(train$density_band),
                                     levels = levels(train$density_band))
base_policy$veh_brand      <- factor(ref_level(train$veh_brand),
                                     levels = levels(train$veh_brand))
base_policy$region         <- factor(ref_level(train$region),
                                     levels = levels(train$region))

# Set exposure to 1 so frequency prediction is annualised
base_policy$exposure       <- 1.0

base_freq <- predict(glm_freq, newdata = base_policy, type = "response")
base_sev  <- predict(glm_sev, newdata = base_policy, type = "response")
base_pp   <- base_freq * base_sev

cat(sprintf("    Base-level predicted frequency : %.4f\n", base_freq))
cat(sprintf("    Base-level predicted severity  : £%.2f\n", base_sev))
cat(sprintf("    Base-level pure premium        : £%.2f\n", base_pp))


# 10.2  Driver age relativities

drv_age_rel <- data.frame(drv_age_band = levels(train$drv_age_band)) %>%
  mutate(
    drv_age_band   = factor(drv_age_band, levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band),
                            levels = levels(train$veh_power_band)),
    bm_band        = factor(ref_level(train$bm_band), 
                            levels = levels(train$bm_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    veh_brand      = factor(ref_level(train$veh_brand), 
                            levels = levels(train$veh_brand)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq      = predict(glm_freq, newdata = ., type = "response"),
    pred_sev       = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp        = pred_freq * pred_sev,
    rel_freq       = pred_freq / base_freq,
    rel_sev        = pred_sev  / base_sev,
    rel_pp         = pred_pp   / base_pp
  )

cat("\n    Driver Age Relativities:\n")
print(drv_age_rel %>%
        select(drv_age_band, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.3  Vehicle age relativities

veh_age_rel <- data.frame(veh_age_band = levels(train$veh_age_band)) %>%
  mutate(
    veh_age_band   = factor(veh_age_band,levels = levels(train$veh_age_band)),
    drv_age_band   = factor(ref_level(train$drv_age_band), 
                            levels = levels(train$drv_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band),
                            levels = levels(train$veh_power_band)),
    bm_band        = factor(ref_level(train$bm_band), 
                            levels = levels(train$bm_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    veh_brand      = factor(ref_level(train$veh_brand), 
                            levels = levels(train$veh_brand)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq      = predict(glm_freq, newdata = ., type = "response"),
    pred_sev       = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp        = pred_freq * pred_sev,
    rel_freq       = pred_freq / base_freq,
    rel_sev        = pred_sev  / base_sev,
    rel_pp         = pred_pp   / base_pp
  )

cat("\n    Vehicle Age Relativities:\n")
print(veh_age_rel %>%
        select(veh_age_band, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.4  Vehicle power relativities

veh_power_rel <- data.frame(veh_power_band = levels(train$veh_power_band)) %>%
  mutate(
    veh_power_band = factor(veh_power_band,
                            levels = levels(train$veh_power_band)),
    drv_age_band   = factor(ref_level(train$drv_age_band),
                            levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    bm_band        = factor(ref_level(train$bm_band), 
                            levels = levels(train$bm_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    veh_brand      = factor(ref_level(train$veh_brand),
                            levels = levels(train$veh_brand)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq = predict(glm_freq, newdata = ., type = "response"),
    pred_sev  = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp   = pred_freq * pred_sev,
    rel_freq  = pred_freq / base_freq,
    rel_sev   = pred_sev  / base_sev,
    rel_pp    = pred_pp   / base_pp
  )

cat("\n    Vehicle Power Relativities:\n")
print(veh_power_rel %>%
        select(veh_power_band, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))


# 10.5  Bonus-Malus relativities

bm_rel <- data.frame(bm_band = levels(train$bm_band)) %>%
  mutate(
    bm_band        = factor(bm_band, levels = levels(train$bm_band)),
    drv_age_band   = factor(ref_level(train$drv_age_band),
                            levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band),
                            levels = levels(train$veh_power_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    veh_brand      = factor(ref_level(train$veh_brand),
                            levels = levels(train$veh_brand)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq = predict(glm_freq, newdata = ., type = "response"),
    pred_sev  = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp   = pred_freq * pred_sev,
    rel_freq  = pred_freq / base_freq,
    rel_sev   = pred_sev  / base_sev,
    rel_pp    = pred_pp   / base_pp
  )

cat("\n    Bonus-Malus Relativities:\n")
print(bm_rel %>%
        select(bm_band, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.6  Vehicle Brand relativities

veh_brand_rel <- data.frame(veh_brand = levels(train$veh_brand)) %>%
  mutate(
    veh_brand      = factor(veh_brand, levels = levels(train$veh_brand)),
    drv_age_band   = factor(ref_level(train$drv_age_band),
                            levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band),
                            levels = levels(train$veh_power_band)),
    bm_band        = factor(ref_level(train$bm_band), 
                            levels = levels(train$bm_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq = predict(glm_freq, newdata = ., type = "response"),
    pred_sev  = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp   = pred_freq * pred_sev,
    rel_freq  = pred_freq / base_freq,
    rel_sev   = pred_sev  / base_sev,
    rel_pp    = pred_pp   / base_pp
  )

cat("\n    Vehicle Brand Relativities:\n")
print(veh_brand_rel %>%
        select(veh_brand, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.7  Vehicle Fuel relativities

veh_fuel_rel <- data.frame(veh_fuel = levels(train$veh_fuel)) %>%
  mutate(
    veh_fuel       = factor(veh_fuel, levels = levels(train$veh_fuel)),
    drv_age_band   = factor(ref_level(train$drv_age_band),
                            levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band),
                            levels = levels(train$veh_power_band)),
    bm_band        = factor(ref_level(train$bm_band),
                            levels = levels(train$bm_band)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    veh_brand      = factor(ref_level(train$veh_brand),
                            levels = levels(train$veh_brand)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq = predict(glm_freq, newdata = ., type = "response"),
    pred_sev  = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp   = pred_freq * pred_sev,
    rel_freq  = pred_freq / base_freq,
    rel_sev   = pred_sev  / base_sev,
    rel_pp    = pred_pp   / base_pp
  )

cat("\n    Vehicle Fuel Relativities:\n")
print(veh_fuel_rel %>%
        select(veh_fuel, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.8  Area relativities

area_rel <- data.frame(area = levels(train$area)) %>%
  mutate(
    area           = factor(area, levels = levels(train$area)),
    drv_age_band   = factor(ref_level(train$drv_age_band),   
                            levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band), 
                            levels = levels(train$veh_power_band)),
    bm_band        = factor(ref_level(train$bm_band),
                            levels = levels(train$bm_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    veh_brand      = factor(ref_level(train$veh_brand), 
                            levels = levels(train$veh_brand)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq = predict(glm_freq, newdata = ., type = "response"),
    pred_sev  = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp   = pred_freq * pred_sev,
    rel_freq  = pred_freq / base_freq,
    rel_sev   = pred_sev  / base_sev,
    rel_pp    = pred_pp   / base_pp
  )

cat("\n    Area Relativities:\n")
print(area_rel %>%
        select(area, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.9  Density Band relativities

density_rel <- data.frame(density_band = levels(train$density_band)) %>%
  mutate(
    density_band   = factor(density_band, levels = levels(train$density_band)),
    drv_age_band   = factor(ref_level(train$drv_age_band),
                            levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band), 
                            levels = levels(train$veh_power_band)),
    bm_band        = factor(ref_level(train$bm_band),
                            levels = levels(train$bm_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    veh_brand      = factor(ref_level(train$veh_brand),
                            levels = levels(train$veh_brand)),
    region         = factor(ref_level(train$region),
                            levels = levels(train$region)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq = predict(glm_freq, newdata = ., type = "response"),
    pred_sev  = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp   = pred_freq * pred_sev,
    rel_freq  = pred_freq / base_freq,
    rel_sev   = pred_sev  / base_sev,
    rel_pp    = pred_pp   / base_pp
  )

cat("\n    Density Band Relativities:\n")
print(density_rel %>%
        select(density_band, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.10  Region relativities

region_rel <- data.frame(region = levels(train$region)) %>%
  mutate(
    region         = factor(region, levels = levels(train$region)),
    drv_age_band   = factor(ref_level(train$drv_age_band),
                            levels = levels(train$drv_age_band)),
    veh_age_band   = factor(ref_level(train$veh_age_band),
                            levels = levels(train$veh_age_band)),
    veh_power_band = factor(ref_level(train$veh_power_band),
                            levels = levels(train$veh_power_band)),
    bm_band        = factor(ref_level(train$bm_band),
                            levels = levels(train$bm_band)),
    veh_fuel       = factor(ref_level(train$veh_fuel),
                            levels = levels(train$veh_fuel)),
    area           = factor(ref_level(train$area),
                            levels = levels(train$area)),
    density_band   = factor(ref_level(train$density_band),
                            levels = levels(train$density_band)),
    veh_brand      = factor(ref_level(train$veh_brand), 
                            levels = levels(train$veh_brand)),
    exposure       = 1.0
  ) %>%
  mutate(
    pred_freq = predict(glm_freq, newdata = ., type = "response"),
    pred_sev  = predict(glm_sev,  newdata = ., type = "response"),
    pred_pp   = pred_freq * pred_sev,
    rel_freq  = pred_freq / base_freq,
    rel_sev   = pred_sev  / base_sev,
    rel_pp    = pred_pp   / base_pp
  )

cat("\n    Region Relativities:\n")
print(region_rel %>%
        select(region, rel_freq, rel_sev, rel_pp) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 10.11  Relativity charts 

plot_relativities <- function(rel_data, x_var, x_label) {
  rel_data %>%
    select({{ x_var }}, rel_freq, rel_sev, rel_pp) %>%
    pivot_longer(cols = c(rel_freq, rel_sev, rel_pp),
                 names_to  = "component",
                 values_to = "relativity") %>%
    mutate(component = case_when(
                          component == "rel_freq" ~ "Frequency",
                          component == "rel_sev"  ~ "Severity",
                          component == "rel_pp"   ~ "Pure Premium")) %>%
    ggplot(aes(x = .data[[x_var]], y = relativity,
               colour = component, group = component)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
    scale_colour_manual(
      values = c("Frequency"    = "blue",
                 "Severity"     = "orange",
                 "Pure Premium" = "green"),
      name = "Component"
    ) +
    labs(
      title = paste("Rating Relativities by", x_label),
      subtitle = "Reference level = 1.0 (dashed line)",
      x = x_label,
      y = "Relativity"
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

p_rel_drvage   <- plot_relativities(drv_age_rel,"drv_age_band",
                                    "Driver Age Band")
p_rel_vehage   <- plot_relativities(veh_age_rel,"veh_age_band", 
                                    "Vehicle Age Band")
p_rel_vehpower <- plot_relativities(veh_power_rel, "veh_power_band", 
                                    "Vehicle Power Band")
p_rel_bm       <- plot_relativities(bm_rel,"bm_band","Bonus-Malus Band")
p_rel_brand    <- plot_relativities(veh_brand_rel, "veh_brand","Vehicle Brand")
p_rel_fuel     <- plot_relativities(veh_fuel_rel, "veh_fuel","Fuel Type")
p_rel_area     <- plot_relativities(area_rel, "area","Area")
p_rel_density  <- plot_relativities(density_rel,"density_band", 
                                    "Population Density Band")
p_rel_region   <- plot_relativities(region_rel, "region", "Region")

pdf(file.path(DIR_OUT, "10_Rating_Relativities.pdf"), width = 16, height = 24)
gridExtra::grid.arrange(
  p_rel_drvage,
  p_rel_vehage,
  p_rel_vehpower,
  p_rel_bm,
  p_rel_brand,
  p_rel_fuel,
  p_rel_area,
  p_rel_density,
  p_rel_region,
  ncol = 2
)
dev.off()

cat("    Relativity plots saved\n")


# ============================================================================
# 11.  COMBINED PURE PREMIUM – PORTFOLIO APPLICATION
# ============================================================================
#
#  Apply frequency and severity models to the full dataset to produce
#  a predicted pure premium for every policy.  This is the key output
#  that feeds into the pricing framework.
#
# ============================================================================

cat("\n[11] Computing portfolio-level pure premiums...\n")

modelling_data <- modelling_data %>%
  mutate(
    pred_freq  = predict(glm_freq, newdata = modelling_data, type = "response"),
    pred_sev   = predict(glm_sev,  newdata = modelling_data, type = "response"),
    pred_pp    = pred_freq * pred_sev,
    pred_rate  = pred_freq / exposure
  )

# Summary of predicted pure premium vs actual loss cost
pp_summary <- modelling_data %>%
  summarise(
    total_exposure     = sum(exposure),
    total_claims       = sum(claim_count),
    total_incurred     = sum(total_claim_capped, na.rm = TRUE),
    actual_freq        = total_claims / total_exposure,
    actual_loss_cost   = total_incurred / total_exposure,
    pred_freq          = sum(pred_freq) / total_exposure,
    pred_loss_cost     = sum(pred_pp) / total_exposure,
    loss_cost_ratio    = pred_loss_cost / actual_loss_cost
  )

cat(sprintf("    Portfolio predicted frequency  : %.4f (actual: %.4f)\n",
            pp_summary$pred_freq, pp_summary$actual_freq))
cat(sprintf("    Portfolio predicted loss cost  : £%.2f (actual: £%.2f)\n",
            pp_summary$pred_loss_cost, pp_summary$actual_loss_cost))
cat(sprintf("    Predicted / Actual loss cost   : %.3f\n",
            pp_summary$loss_cost_ratio))

# Distribution of pure premiums 

p_pp_dist <- ggplot(modelling_data, aes(x = pred_pp)) +
  geom_histogram(bins = 60, fill = "green", colour = "white", alpha = 0.8) +
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, quantile(modelling_data$pred_pp, 0.99))) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Distribution of Predicted Pure Premiums",
    subtitle = "Capped at 99th percentile for display",
    x        = "Predicted Pure Premium (€)",
    y        = "Policy Count"
  )

pdf(file.path(DIR_OUT, "11_PurePremium_Distribution.pdf"),
    width = 8, height = 5)
print(p_pp_dist)
dev.off()


# ============================================================================
# 12.  MODEL SUMMARY TABLES (EXPORT)
# ============================================================================
#
#  This section exports three summary tables to CSV for downstream use
#  and peer review.
#
#     1. Frequency model coefficients - exponentiated relativities,
#        standard errors, p-values and 95% confidence intervals for
#        each term in the final Poisson model.
#
#     2. Severity model coefficients — same structure as above for
#        the final Gamma model.
#
#     3. Combined relativity table — frequency, severity and pure
#        premium relativities for every level of every rating factor,
#        held at reference level for all other variables.
#
#  Outputs:
#      outputs/freq_model_coefficients.csv
#      outputs/sev_model_coefficients.csv
#      outputs/combined_relativities.csv
#
# ============================================================================

cat("\n[12] Exporting model summary tables...\n")

# 12.1  Frequency model coefficients

freq_summary %>%
  mutate(model = "Frequency (Poisson)") %>%
  rename(
    Factor     = term,
    Relativity = estimate,
    Std_Error  = std.error,
    P_Value    = p.value,
    CI_Lower   = conf.low,
    CI_Upper   = conf.high,
    Z_Value    = z.statistic   
  ) %>%
  select(model,Factor,Relativity,Std_Error,P_Value,CI_Lower,CI_Upper,Z_Value) %>%
    mutate(across(where(is.numeric), ~round(., 4))) %>%
    write.csv(file.path(DIR_OUT, "freq_model_coefficients.csv"),
              row.names = FALSE)


# 12.2  Severity model coefficients 

sev_summary %>%
  mutate(model = "Severity(Gamma)") %>%
  rename(
    Factor     = term,
    Relativity = estimate,
    Std_Error  = std.err,
    P_Value    = p.value,
    CI_Lower   = conf.low,
    CI_Upper   = conf.high,
    T_Value    = t.statistic   
  ) %>%
  select(model,Factor,Relativity,Std_Error,P_Value,CI_Lower,CI_Upper,T_Value) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  write.csv(file.path(DIR_OUT, "sev_model_coefficients.csv"),
            row.names = FALSE)

# 12.3  Combined relativity table

build_rel_table <- function(rel_data, variable_name, level_col) {
  rel_data %>%
    transmute(
      Variable        = variable_name,
      Level           = as.character(.data[[level_col]]),
      Freq_Relativity = round(rel_freq, 4),
      Sev_Relativity  = round(rel_sev,  4),
      PP_Relativity   = round(rel_pp,   4)
    )
}

all_rel <- bind_rows(
  build_rel_table(drv_age_rel,"drv_age_band", "drv_age_band"),
  build_rel_table(veh_age_rel,"veh_age_band", "veh_age_band"),
  build_rel_table(veh_power_rel, "veh_power_band", "veh_power_band"),
  build_rel_table(bm_rel, "bm_band", "bm_band"),
  build_rel_table(veh_fuel_rel, "veh_fuel","veh_fuel"),
  build_rel_table(area_rel,"area","area"),
  build_rel_table(density_rel,"density_band", "density_band"),
  build_rel_table(veh_brand_rel, "veh_brand", "veh_brand"),
  build_rel_table(region_rel, "region", "region")
)

write.csv(
  all_rel,
  file.path(DIR_OUT, "combined_relativities.csv"),
  row.names = FALSE
)

cat(sprintf("    Combined relativity table exported: %s rows across %s factors\n",
            nrow(all_rel),
            length(unique(all_rel$Variable))))


# ============================================================================
# 13.  MODEL OBJECT EXPORT
# ============================================================================
#
#  The fitted frequency and severity model objects are saved as .rds files.
#  This allows the models to be reloaded in a separate scoring or pricing
#  script without re-running the full pipeline.
#
#  To reload:
#      glm_freq <- readRDS("outputs/glm_freq.rds")
#      glm_sev  <- readRDS("outputs/glm_sev.rds")
#
#  Outputs:
#      outputs/glm_freq.rds
#      outputs/glm_sev.rds
#
# ============================================================================

cat("\n[13] Saving model objects...\n")

saveRDS(glm_freq, file.path(DIR_OUT, "glm_freq.rds"))
saveRDS(glm_sev,  file.path(DIR_OUT, "glm_sev.rds"))

cat("    Model objects saved as .rds files\n")


# ============================================================================
# 14.  FINAL SUMMARY
# ============================================================================

cat("\n")
cat("=====================================================================\n")
cat("  FREQUENCY-SEVERITY GLM – FINAL MODEL SUMMARY\n")
cat("=====================================================================\n\n")

cat("  FREQUENCY MODEL (Poisson, log link)\n")
cat("  \n")
cat(sprintf("  Training records    : %s\n", format(nrow(train), big.mark=",")))
cat(sprintf("  Null deviance       : %.1f\n", glm_freq$null.deviance))
cat(sprintf("  Residual deviance   : %.1f\n", glm_freq$deviance))
cat(sprintf("  Deviance explained: %.1f%%\n",
            100 * (1 - glm_freq$deviance / glm_freq$null.deviance)))
cat(sprintf("  AIC                 : %.1f\n", AIC(glm_freq)))

cat("  SEVERITY MODEL (Gamma, log link)\n")
cat("  \n")
cat(sprintf("  Training records    : %s\n", format(nrow(train_sev), big.mark=",")))
cat(sprintf("  Null deviance       : %.1f\n", glm_sev$null.deviance))
cat(sprintf("  Residual deviance   : %.1f\n", glm_sev$deviance))
cat(sprintf("  Deviance explained: %.1f%%\n",
            100 * (1 - glm_sev$deviance / glm_sev$null.deviance)))
cat(sprintf("  AIC                 : %.1f\n\n", AIC(glm_sev)))

cat("  OUTPUTS\n")
cat("  \n")
cat("  outputs/01_EDA_Frequency_Plots.pdf\n")
cat("  outputs/02_EDA_Severity_Plots.pdf\n")
cat("  outputs/03_EDA_Exposure_Profiles.pdf\n")
cat("  outputs/04_Freq_Model_Diagnostics.pdf\n")
cat("  outputs/05_Freq_Actual_vs_Predicted.pdf\n")
cat("  outputs/06_Freq_Decile_Lift.pdf\n")
cat("  outputs/07_EDA_Severity_by_Factor.pdf\n")
cat("  outputs/08_Sev_Model_Diagnostics.pdf\n")
cat("  outputs/09_Sev_Decile_Lift.pdf\n")
cat("  outputs/10_Rating_Relativities.pdf\n")
cat("  outputs/11_PurePremium_Distribution.pdf\n")
cat("  outputs/freq_model_coefficients.csv\n")
cat("  outputs/sev_model_coefficients.csv\n")
cat("  outputs/combined_relativities.csv\n")
cat("  outputs/glm_freq.rds\n")
cat("  outputs/glm_sev.rds\n")
cat("  \n")
cat("  STATUS: Model fitted and validated.  Output requires peer review\n")
cat("          before use in pricing or product filing.\n")
cat("=====================================================================\n")

# END OF SCRIPT ################################
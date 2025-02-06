# Download packages---------

packages <- c("dplyr", "ggplot2", "broom", "tidyr","srvyr", "NHANES", "kableExtra")
package.check <- lapply(
  packages,
  FUN = function(x) {if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
library(stargazer)
library(survey)
library(ipumsr)
library(lavaan)



# Load data ------

ddi_new <- read_ipums_ddi("nhis_00005.xml")
data_ipums_new <- read_ipums_micro(ddi_new)

## Apply filters ------
filtered_data <- data_ipums_new %>%
  filter(
    YEAR >= 2013 & YEAR <= 2018,
    AGE <= 85,
    SEX <= 2,
    HEALTH <= 5,
    EDUC <= 503,
    (HOURSWRK != 0 & HOURSWRK <= 95),
    (ASAD != 6 & ASAD <= 4),
    (WORFREQ != 0 & WORFREQ <= 5),
    (DEPFREQ != 0 & DEPFREQ <= 5),
    (MARST >= 10 & MARST <= 50),
  )

filtered_data$HOURSWRK[filtered_data$HOURSWRK >= 95] <- 95

## Regroup Education ------
filtered_data$EDUCATION = filtered_data$EDUC
no_school <- c(102)  
elementary <- c(104, 105, 106, 107, 108, 109)  
middle_school <- c(110, 111)  
no_hs_diploma <- c(113, 114, 115, 116)  
hs_complete <- c(200, 201, 202)  
no_co_diploma <- c(300, 301, 302, 303)  
bachelors <- c(400)  
graduate <- c(500, 501, 502, 503, 504, 505)  

filtered_data$EDUCATION[filtered_data$EDUCATION %in% no_school] = 1
filtered_data$EDUCATION[filtered_data$EDUCATION %in% elementary] = 2
filtered_data$EDUCATION[filtered_data$EDUCATION %in% middle_school] = 3
filtered_data$EDUCATION[filtered_data$EDUCATION %in% no_hs_diploma] = 4
filtered_data$EDUCATION[filtered_data$EDUCATION %in% hs_complete] = 5
filtered_data$EDUCATION[filtered_data$EDUCATION %in% no_co_diploma] = 6
filtered_data$EDUCATION[filtered_data$EDUCATION %in% bachelors] = 7
filtered_data$EDUCATION[filtered_data$EDUCATION %in% graduate] = 8

filtered_data$HEALTH_STATUS = filtered_data$HEALTH
filtered_data$SADNESS = filtered_data$ASAD  # 0-4
filtered_data$ANXIETY_MED = filtered_data$WORRX    # 1-2
filtered_data$DEPRESSION_MED = filtered_data$DEPRX # 1-2

filtered_data$WORK_HOURS = filtered_data$HOURSWRK
filtered_data$WORK_HOURS[filtered_data$WORK_HOURS >= 95] = 95

## Create work hours categories ------
filtered_data$WORK_HOURS_CAT <- NA
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS <= 20] = 1
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 20 & filtered_data$WORK_HOURS <= 35] = 2
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 35 & filtered_data$WORK_HOURS <= 40] = 3
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 40 & filtered_data$WORK_HOURS <= 60] = 4
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 60] = 5

## Recode all variables and create new ones within the data frame ------
filtered_data <- filtered_data %>%
  mutate(
    # Education grouping
    EDUCATION = case_when(
      EDUC %in% c(102) ~ 1,
      EDUC %in% c(104, 105, 106, 107, 108, 109) ~ 2,
      EDUC %in% c(110, 111) ~ 3,
      EDUC %in% c(113, 114, 115, 116) ~ 4,
      EDUC %in% c(200, 201, 202) ~ 5,
      EDUC %in% c(300, 301, 302, 303) ~ 6,
      EDUC %in% c(400) ~ 7,
      EDUC %in% c(500, 501, 502, 503, 504, 505) ~ 8,
      TRUE ~ NA_real_
    ),
 
    # Health Status
    HEALTH_STATUS = HEALTH,
    
    # Mental Health Indicators
    SADNESS = ASAD,
    
    # Binary coding for depression frequency
      DEPRESS_FREQ = case_when(
      DEPFREQ >= 1 & DEPFREQ <= 4 ~ 1,
      DEPFREQ == 5 ~ 0,
      TRUE ~ NA_real_
    ),

    # Work Hours
    WORK_HOURS = if_else(HOURSWRK >= 95, 95, HOURSWRK),
    
    # Work Hours Categories
    WORK_HOURS_CAT = case_when(
      WORK_HOURS <= 20 ~ 1,
      WORK_HOURS > 20 & WORK_HOURS <= 35 ~ 2,
      WORK_HOURS > 35 & WORK_HOURS <= 40 ~ 3,
      WORK_HOURS > 40 & WORK_HOURS <= 60 ~ 4,
      WORK_HOURS > 60 ~ 5,
      TRUE ~ NA_real_
    ),
    
    # Marital Status
    MARITAL = case_when(
      MARST %in% c(10, 11, 12, 13) ~ 1,
      MARST == 20 ~ 2,
      MARST == 30 ~ 3,
      MARST == 40 ~ 4,
      MARST == 50 ~ 5,
      TRUE ~ NA_real_
    )
  )

## Create final dataset with numeric variables ------
final_data <- dplyr::select(filtered_data, YEAR, SERIAL,AGE,SEX,MARITAL, EDUCATION, WORK_HOURS, WORK_HOURS_CAT,
HEALTH_STATUS, SADNESS,SAMPWEIGHT, STRATA,PSU)

## Examine distributions ------
ggplot(filtered_data, aes(WORK_HOURS)) +
  geom_histogram(binwidth = 0.2, fill = '#BCD1BC', color = "#BCD1BC") +
  labs(title = "Working Hours Distribution", 
       x = "Working Hours", 
       y = "Count")

ggplot(filtered_data, aes(log(WORK_HOURS))) +
  geom_histogram(binwidth = 0.2, fill = '#BCD1BC', color = "#BCD1BC") +
  labs(title = "Log-transformed Working Hours Distribution", 
       x = "Log(Working Hours)", 
       y = "Count")



# Multivariate and Logistic Regression Models with Grouped Working Hours ------

# Prepare data with proper coding and work hour groups
regression_data <- filtered_data %>%
  filter(WORK_HOURS >= 5) %>%
  mutate(
    # Create work hour categories
    work_group = case_when(
      WORK_HOURS < 35 ~ "Part-time",         # Part-time
      WORK_HOURS <= 40 ~ "Standard",         # Standard full-time (reference)
      WORK_HOURS <= 48 ~ "Moderate-Over",    # Moderate overtime
      WORK_HOURS <= 60 ~ "High-Over",        # High overtime
      TRUE ~ "Excessive-Over"                 # Excessive overtime
    ),
    # Convert to factor with Standard as reference
    work_group = relevel(factor(work_group), ref = "Standard"),
    
    # Other variables
    sex_dummy = as.numeric(SEX - 1),
    education_num = as.numeric(EDUCATION),
    health_num = as.numeric(HEALTH_STATUS),
    age_num = as.numeric(AGE),
    marital_num = as.numeric(MARITAL),
    sadness_num = as.numeric(ASAD),
    depress_binary = DEPRESS_FREQ
  )

# Count PSUs per stratum and filter
psu_count <- regression_data %>%
  group_by(STRATA) %>%
  summarize(n_psus = n_distinct(PSU))

valid_strata <- psu_count %>%
  filter(n_psus > 1) %>%
  pull(STRATA)

regression_data <- regression_data %>%
  filter(STRATA %in% valid_strata)

# Create survey design object
nhis_design <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data,
  nest = TRUE
)

## Fit models ------

### 1. Linear model for Sadness ------
model_sadness <- svyglm(
  sadness_num ~ 
    work_group + 
    age_num + 
    sex_dummy + 
    education_num + 
    marital_num,
  design = nhis_design
)

### 2. Logistic regression for Depression ------
model_depression <- svyglm(
  depress_binary ~ 
    work_group + 
    age_num + 
    sex_dummy + 
    education_num + 
    marital_num,
  design = nhis_design,
  family = quasibinomial()
)

# Get test results first
f_test_sadness <- regTermTest(model_sadness, "work_group")
wald_test_depression <- regTermTest(model_depression, "work_group")

# Create table with test results
stargazer(model_sadness, model_depression,
          type = "html",
          out = "regression_results_1.doc",
          title = "Regression Results",
          column.labels = c("Sadness", "Depression"),
          model.names = FALSE,
          dep.var.labels = c("Mental Health Outcomes"),
          add.lines = list(
            c("Model Type", "Linear", "Logistic"),
            c("Joint test statistic", 
              paste("F =", round(f_test_sadness$Ftest, 2)),
              paste("Chi2 =", round(wald_test_depression$Ftest, 2))),
            c("Joint test p-value", 
              round(f_test_sadness$p, 3),
              round(wald_test_depression$p, 3))
          ),
          covariate.labels = c("Part-time (<35h)",
                               "Moderate Overtime (41-48h)",
                               "High Overtime (49-60h)",
                               "Excessive Overtime (>60h)",
                               "Age",
                               "Sex",
                               "Education Level",
                               "Marital Status"))


## Heterogeneity Analysis for sex--------------

# Function to run models for subgroup with PSU filtering
run_subgroup_models <- function(data, subgroup_var) {
  subgroup_levels <- unique(data[[subgroup_var]])
  results_list <- list()
  
  for(level in subgroup_levels) {
    # Subset data
    subset_data <- subset(data, get(subgroup_var) == level)
    
    # Count PSUs per stratum in subset
    psu_count <- subset_data %>%
      group_by(STRATA) %>%
      summarize(n_psus = n_distinct(PSU))
    
    # Keep only strata with multiple PSUs
    valid_strata <- psu_count %>%
      filter(n_psus > 1) %>%
      pull(STRATA)
    
    # Filter data to valid strata
    subset_data <- subset_data %>%
      filter(STRATA %in% valid_strata)
    
    # Create survey design for subset
    subset_design <- svydesign(
      id = ~PSU,
      strata = ~STRATA,
      weights = ~SAMPWEIGHT,
      data = subset_data,
      nest = TRUE
    )
    
    # Fit models
    model_sadness <- svyglm(
      sadness_num ~ work_group + age_num + education_num  + marital_num,
      design = subset_design
    )
    
    model_depression <- svyglm(
      depress_binary ~ work_group + age_num + education_num  + marital_num,
      design = subset_design,
      family = quasibinomial()
    )
    
    # Get test statistics
    f_test_sadness <- regTermTest(model_sadness, "work_group")
    wald_test_depression <- regTermTest(model_depression, "work_group")
    
    # Store results
    results_list[[as.character(level)]] <- list(
      sadness = model_sadness,
      depression = model_depression,
      f_test = f_test_sadness,
      wald_test = wald_test_depression,
      n = nrow(subset_data)
    )
  }
  return(results_list)
}

# Run analyses by sex
sex_results <- run_subgroup_models(regression_data, "sex_dummy")

# Create summary tables for sex analysis
stargazer(
  sex_results[["0"]]$sadness, sex_results[["1"]]$sadness,
  type = "html",
  out = "regression_results_2.doc",
  title = "Heterogeneity Analysis by Sex - Sadness Model",
  column.labels = c("Female", "Male"),
  add.lines = list(
    c("F-statistic", 
      round(sex_results[["0"]]$f_test$Ftest, 2),
      round(sex_results[["1"]]$f_test$Ftest, 2)),
    c("p-value",
      round(sex_results[["0"]]$f_test$p, 3),
      round(sex_results[["1"]]$f_test$p, 3)),
    c("Observations",
      sex_results[["0"]]$n,
      sex_results[["1"]]$n)
  )
)

stargazer(
  sex_results[["0"]]$depression, sex_results[["1"]]$depression,
  type = "html",
  out = "regression_results_3.doc",
  title = "Heterogeneity Analysis by Sex - Depression Model",
  column.labels = c("Female", "Male"),
  add.lines = list(
    c("Chi-square",
      round(sex_results[["0"]]$wald_test$Ftest, 2),
      round(sex_results[["1"]]$wald_test$Ftest, 2)),
    c("p-value",
      round(sex_results[["0"]]$wald_test$p, 3),
      round(sex_results[["1"]]$wald_test$p, 3)),
    c("Observations",
      sex_results[["0"]]$n,
      sex_results[["1"]]$n)
  )
)

### Chow Test of Sex Heterogeneity -------------

# Count PSUs per stratum for each sex group
psu_count_female <- subset(regression_data, sex_dummy == 0) %>%
  group_by(STRATA) %>%
  summarize(n_psus = n_distinct(PSU))

psu_count_male <- subset(regression_data, sex_dummy == 1) %>%
  group_by(STRATA) %>%
  summarize(n_psus = n_distinct(PSU))

# Get valid strata for each group
valid_strata_female <- psu_count_female %>%
  filter(n_psus > 1) %>%
  pull(STRATA)

valid_strata_male <- psu_count_male %>%
  filter(n_psus > 1) %>%
  pull(STRATA)

# Filter data for each group
regression_data_female <- subset(regression_data, sex_dummy == 0 & STRATA %in% valid_strata_female)
regression_data_male <- subset(regression_data, sex_dummy == 1 & STRATA %in% valid_strata_male)

# Create pooled dataset with only valid strata
regression_data_pooled <- rbind(regression_data_female, regression_data_male)

# Create survey designs with filtered data
design_female <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data_female,
  nest = TRUE
)

design_male <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data_male,
  nest = TRUE
)

# Create pooled design
design_pooled <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data_pooled,
  nest = TRUE
)

#### Chow Test for Sadness Model -------------
# Run separate models
model_female <- svyglm(
  sadness_num ~ work_group + age_num + education_num + marital_num,
  design = design_female
)

model_male <- svyglm(
  sadness_num ~ work_group + age_num + education_num + marital_num,
  design = design_male
)

# Run pooled model
model_pooled <- svyglm(
  sadness_num ~ work_group + age_num + education_num + marital_num,
  design = design_pooled
)

# Calculate RSS and perform Chow test for sadness model
rss_female <- sum(residuals(model_female)^2)
rss_male <- sum(residuals(model_male)^2)
rss_pooled <- sum(residuals(model_pooled)^2)

n_female <- nobs(model_female)
n_male <- nobs(model_male)
k <- length(coef(model_pooled))

rss_ur <- rss_female + rss_male
rss_r <- rss_pooled
df1 <- k
df2 <- n_female + n_male - 2*k
f_stat <- ((rss_r - rss_ur)/df1) / (rss_ur/df2)
p_value <- 1 - pf(f_stat, df1, df2)

# Print Chow test results for sadness model
cat("\nChow Test Results for Sex Heterogeneity (Sadness Model):\n")
cat("F-statistic:", f_stat, "\n")
cat("Degrees of freedom:", df1, "and", df2, "\n")
cat("p-value:", p_value, "\n")


#### Chow Test for Depression Model -------------
# Run separate models for depression
model_female_dep <- svyglm(
  depress_binary ~ work_group + age_num + education_num + marital_num,
  design = design_female,
  family = quasibinomial()
)

model_male_dep <- svyglm(
  depress_binary ~ work_group + age_num + education_num + marital_num,
  design = design_male,
  family = quasibinomial()
)

# Run pooled model for depression
model_pooled_dep <- svyglm(
  depress_binary ~ work_group + age_num + education_num + marital_num,
  design = design_pooled,
  family = quasibinomial()
)

# Calculate RSS and perform Chow test for depression model
rss_female_dep <- sum(residuals(model_female_dep)^2)
rss_male_dep <- sum(residuals(model_male_dep)^2)
rss_pooled_dep <- sum(residuals(model_pooled_dep)^2)

n_female_dep <- nobs(model_female_dep)
n_male_dep <- nobs(model_male_dep)
k_dep <- length(coef(model_pooled_dep))

rss_ur_dep <- rss_female_dep + rss_male_dep
rss_r_dep <- rss_pooled_dep
df1_dep <- k_dep
df2_dep <- n_female_dep + n_male_dep - 2*k_dep
f_stat_dep <- ((rss_r_dep - rss_ur_dep)/df1_dep) / (rss_ur_dep/df2_dep)
p_value_dep <- 1 - pf(f_stat_dep, df1_dep, df2_dep)

# Print Chow test results for depression model
cat("\nChow Test Results for Sex Heterogeneity (Depression Model):\n")
cat("F-statistic:", f_stat_dep, "\n")
cat("Degrees of freedom:", df1_dep, "and", df2_dep, "\n")
cat("p-value:", p_value_dep, "\n")



## Heterogeneity Analysis for education --------------
# Calculate the total number of observations
total_obs <- nrow(regression_data)
target_size <- total_obs / 2

# Order education levels by number of observations
education_dist <- regression_data %>%
  group_by(EDUCATION) %>%
  summarize(count = n()) %>%
  arrange(EDUCATION)

# Find the cutoff point that creates the most equal split
running_sum <- 0
cutoff_level <- 0

for(i in 1:nrow(education_dist)) {
  running_sum <- running_sum + education_dist$count[i]
  if(running_sum >= target_size) {
    # Check which split would be more equal
    diff_if_include <- abs((running_sum) - target_size)
    diff_if_exclude <- abs((running_sum - education_dist$count[i]) - target_size)
    
    if(diff_if_include < diff_if_exclude) {
      cutoff_level <- education_dist$EDUCATION[i]
    } else {
      cutoff_level <- education_dist$EDUCATION[i-1]
    }
    break
  }
}

# Create binary groups using the identified cutoff
regression_data <- regression_data %>%
  mutate(
    education_binary = case_when(
      EDUCATION <= cutoff_level ~ 0,
      EDUCATION > cutoff_level ~ 1,
      TRUE ~ NA_real_
    )
  )

# Run the heterogeneity analysis
education_results <- run_subgroup_models(regression_data, "education_binary")

# Create summary tables for education analysis
stargazer(
  education_results[["0"]]$sadness, education_results[["1"]]$sadness,
  type = "html",
  out = "regression_results_4.doc",
  title = "Heterogeneity Analysis by Education - Sadness Model",
  column.labels = c("Low Education", "High Education"),
  add.lines = list(
    c("F-statistic", 
      round(education_results[["0"]]$f_test$Ftest, 2),
      round(education_results[["1"]]$f_test$Ftest, 2)),
    c("p-value",
      round(education_results[["0"]]$f_test$p, 3),
      round(education_results[["1"]]$f_test$p, 3)),
    c("Observations",
      education_results[["0"]]$n,
      education_results[["1"]]$n)
  )
)

stargazer(
  education_results[["0"]]$depression, education_results[["1"]]$depression,
  type = "html",
  out = "regression_results_5.doc",
  title = "Heterogeneity Analysis by Education - Depression Model",
  column.labels = c("Low Education", "High Education"),
  add.lines = list(
    c("Chi-square",
      round(education_results[["0"]]$wald_test$Ftest, 2),
      round(education_results[["1"]]$wald_test$Ftest, 2)),
    c("p-value",
      round(education_results[["0"]]$wald_test$p, 3),
      round(education_results[["1"]]$wald_test$p, 3)),
    c("Observations",
      education_results[["0"]]$n,
      education_results[["1"]]$n)
  )
)


### Chow test of Education Heterogeneity ---------

# First, count PSUs per stratum for each education group
psu_count_low <- subset(regression_data, education_binary == 0) %>%
  group_by(STRATA) %>%
  summarize(n_psus = n_distinct(PSU))

psu_count_high <- subset(regression_data, education_binary == 1) %>%
  group_by(STRATA) %>%
  summarize(n_psus = n_distinct(PSU))

# Get valid strata (those with multiple PSUs) for each group
valid_strata_low <- psu_count_low %>%
  filter(n_psus > 1) %>%
  pull(STRATA)

valid_strata_high <- psu_count_high %>%
  filter(n_psus > 1) %>%
  pull(STRATA)

# Filter data for each group
regression_data_low <- subset(regression_data, education_binary == 0 & STRATA %in% valid_strata_low)
regression_data_high <- subset(regression_data, education_binary == 1 & STRATA %in% valid_strata_high)

# Create survey designs with filtered data
design_low <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data_low,
  nest = TRUE
)

design_high <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data_high,
  nest = TRUE
)

#### Chow test for Sadness Model ----------
# Run separate regressions
model_low_edu <- svyglm(
  sadness_num ~ work_group + age_num + sex_dummy + health_num + marital_num,
  design = design_low
)

model_high_edu <- svyglm(
  sadness_num ~ work_group + age_num + sex_dummy + health_num + marital_num,
  design = design_high
)

# Create pooled dataset and design
regression_data_pooled <- rbind(regression_data_low, regression_data_high)
design_pooled <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data_pooled,
  nest = TRUE
)

# Run pooled model
model_pooled <- svyglm(
  sadness_num ~ work_group + age_num + sex_dummy + health_num + marital_num,
  design = design_pooled
)

# Calculate RSS for each model
rss_low <- sum(residuals(model_low_edu)^2)
rss_high <- sum(residuals(model_high_edu)^2)
rss_pooled <- sum(residuals(model_pooled)^2)

# Calculate degrees of freedom
n_low <- nobs(model_low_edu)
n_high <- nobs(model_high_edu)
k <- length(coef(model_pooled))

# Calculate F-statistic
rss_ur <- rss_low + rss_high
rss_r <- rss_pooled
df1 <- k
df2 <- n_low + n_high - 2*k
f_stat <- ((rss_r - rss_ur)/df1) / (rss_ur/df2)

# Calculate p-value
p_value <- 1 - pf(f_stat, df1, df2)

# Print results
cat("\nChow Test Results for Education Heterogeneity (Sadness Model):\n")
cat("F-statistic:", f_stat, "\n")
cat("Degrees of freedom:", df1, "and", df2, "\n")
cat("p-value:", p_value, "\n")


#### Chow test for Depression Model ----------
# Run separate regressions for depression
model_low_edu_dep <- svyglm(
  depress_binary ~ work_group + age_num + sex_dummy + health_num + marital_num,
  design = design_low,
  family = quasibinomial()
)

model_high_edu_dep <- svyglm(
  depress_binary ~ work_group + age_num + sex_dummy + health_num + marital_num,
  design = design_high,
  family = quasibinomial()
)

# Run pooled model for depression
model_pooled_dep <- svyglm(
  depress_binary ~ work_group + age_num + sex_dummy + health_num + marital_num,
  design = design_pooled,
  family = quasibinomial()
)

# Calculate RSS for depression models
rss_low_dep <- sum(residuals(model_low_edu_dep)^2)
rss_high_dep <- sum(residuals(model_high_edu_dep)^2)
rss_pooled_dep <- sum(residuals(model_pooled_dep)^2)

# Calculate degrees of freedom for depression model
n_low_dep <- nobs(model_low_edu_dep)
n_high_dep <- nobs(model_high_edu_dep)
k_dep <- length(coef(model_pooled_dep))

# Calculate F-statistic for depression model
rss_ur_dep <- rss_low_dep + rss_high_dep
rss_r_dep <- rss_pooled_dep
df1_dep <- k_dep
df2_dep <- n_low_dep + n_high_dep - 2*k_dep
f_stat_dep <- ((rss_r_dep - rss_ur_dep)/df1_dep) / (rss_ur_dep/df2_dep)

# Calculate p-value for depression model
p_value_dep <- 1 - pf(f_stat_dep, df1_dep, df2_dep)

# Print results for depression model
cat("\nChow Test Results for Education Heterogeneity (Depression Model):\n")
cat("F-statistic:", f_stat_dep, "\n")
cat("Degrees of freedom:", df1_dep, "and", df2_dep, "\n")
cat("p-value:", p_value_dep, "\n")


## Mediation Test for Sadness -------------

# Function to perform Sobel test with survey weights for categorical predictors
sobel_test_categorical <- function(model_m, model_y, treatment_levels) {
  results <- list()
  
  for(level in treatment_levels) {
    # Get coefficients
    a <- coef(model_m)[level]
    b <- coef(model_y)["HEALTH_STATUS"]  # Effect of mediator on outcome
    
    # Get standard errors
    se_a <- sqrt(diag(vcov(model_m)))[level]
    se_b <- sqrt(diag(vcov(model_y)))["HEALTH_STATUS"]
    
    # Calculate indirect effect
    indirect_effect <- a * b
    se_indirect <- sqrt(b^2 * se_a^2 + a^2 * se_b^2)
    z_score <- indirect_effect / se_indirect
    p_value <- 2 * (1 - pnorm(abs(z_score)))
    
    results[[level]] <- list(
      indirect_effect = indirect_effect,
      se = se_indirect,
      z_score = z_score,
      p_value = p_value
    )
  }
  return(results)
}

# Run models for health status mediation
# Path a: work hours -> health
model_health <- svyglm(
  HEALTH_STATUS ~ work_group + age_num + sex_dummy + education_num,
  design = nhis_design
)

# Path b and c': Full model with mediator
model_sadness_full <- svyglm(
  sadness_num ~ work_group + HEALTH_STATUS + age_num + sex_dummy + education_num,
  design = nhis_design
)

# Get treatment levels (excluding reference category)
treatment_levels <- grep("work_group", names(coef(model_health)), value = TRUE)

# Perform Sobel test
sobel_results <- sobel_test_categorical(
  model_health, 
  model_sadness_full, 
  treatment_levels
)

# Create results table
results_df <- do.call(rbind, lapply(names(sobel_results), function(level) {
  res <- sobel_results[[level]]
  data.frame(
    Work_Hours_Category = level,
    Indirect_Effect = res$indirect_effect,
    Standard_Error = res$se,
    Z_Score = res$z_score,
    P_Value = res$p_value,
    row.names = NULL
  )
}))

# Calculate total effects
total_effects <- coef(model_sadness_full)[treatment_levels] + 
  results_df$Indirect_Effect

# Add total effects to results
results_df$Total_Effect <- total_effects
results_df$Proportion_Mediated <- results_df$Indirect_Effect / total_effects

# Print updated results
print(knitr::kable(results_df, 
                   caption = "Complete Mediation Analysis Results",
                   digits = 3))



## Mediation Test for Depression Level -----------
# Run models for health status mediation  
# Path a: work hours -> health
model_health <- svyglm(
  HEALTH_STATUS ~ work_group + age_num + sex_dummy + education_num,
  design = nhis_design
)

# Path b and c': Full model with mediator
model_depression_full <- svyglm(
  depress_binary ~ work_group + HEALTH_STATUS + age_num + sex_dummy + education_num,
  design = nhis_design,
  family = quasibinomial()
)

# Get treatment levels
treatment_levels <- grep("work_group", names(coef(model_health)), value = TRUE)

# Perform Sobel test
sobel_results <- sobel_test_categorical(
  model_health, 
  model_depression_full, 
  treatment_levels
)

# Create results table
results_df <- do.call(rbind, lapply(names(sobel_results), function(level) {
  res <- sobel_results[[level]]
  data.frame(
    Work_Hours_Category = level,
    Indirect_Effect = res$indirect_effect,
    Standard_Error = res$se,
    Z_Score = res$z_score,
    P_Value = res$p_value,
    row.names = NULL
  )
}))

# Calculate total effects
total_effects <- coef(model_depression_full)[treatment_levels] + 
  results_df$Indirect_Effect

# Add total effects to results
results_df$Total_Effect <- total_effects
results_df$Proportion_Mediated <- results_df$Indirect_Effect / total_effects

# Print updated results
print(knitr::kable(results_df, 
                   caption = "Complete Mediation Analysis Results",
                   digits = 3))



# Data Visualization ------

## Working hour and sadness level by sex ------

# Prepare the data
plot_data <- filtered_data %>%
  filter(!is.na(WORK_HOURS), !is.na(ASAD), !is.na(SEX)) %>%
  mutate(
    # Create work hour groups
    work_group = cut(WORK_HOURS, 
                     breaks = c(0, 35, 40, 48, 60, Inf),
                     labels = c("Part-time", "Standard", "Moderate-Overtime", "High-Overtime", "Excessive-Overtime"),
                     include.lowest = TRUE),
    sex_label = factor(ifelse(SEX == 1, "Male", "Female"))
  ) %>%
  group_by(work_group, sex_label) %>%
  summarize(
    mean_sadness = mean(ASAD, na.rm = TRUE),
    se_sadness = sd(ASAD, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  )

# Create the plot
ggplot(plot_data, aes(x = work_group, y = mean_sadness, fill = sex_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_sadness - se_sadness, 
                    ymax = mean_sadness + se_sadness),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_fill_manual(values = c("Male" = "#BCD1BC", "Female" = "pink")) +
  labs(
    title = "Average Sadness Level by Working Hours and Sex",
    x = "Working Hours per Week",
    y = "Average Sadness Level",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  scale_y_continuous(
    limits = c(0, max(plot_data$mean_sadness + plot_data$se_sadness) * 1.1)
  )


## Working hour and depression level by sex ------

# Prepare the data
plot_data <- filtered_data %>%
  filter(!is.na(WORK_HOURS), !is.na(DEPFREQ), !is.na(SEX)) %>%
  mutate(
    # Create work hour groups
    work_group = cut(WORK_HOURS, 
                     breaks = c(0, 35, 40, 48, 60, Inf),
                     labels = c("Part-time", "Standard", "Moderate-Overtime", "High-Overtime", "Excessive-Overtime"),
                     include.lowest = TRUE),
    sex_label = factor(ifelse(SEX == 1, "Male", "Female"))
  ) %>%
  group_by(work_group, sex_label) %>%
  summarize(
    mean_dep = mean(DEPFREQ, na.rm = TRUE),
    se_dep = sd(DEPFREQ, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  )

# Create the plot
ggplot(plot_data, aes(x = work_group, y = mean_dep, fill = sex_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_dep - se_dep, 
                    ymax = mean_dep + se_dep),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_fill_manual(values = c("Male" = "#BCD1BC", "Female" = "pink")) +
  labs(
    title = "Average Depression Frequency by Working Hours and Sex",
    x = "Working Hours per Week",
    y = "Average Depression Frequency",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  scale_y_continuous(
    limits = c(0, max(plot_data$mean_dep + plot_data$se_dep) * 1.1)
  )


## Working hour and sadness level by edu level ------

# Prepare the data with fewer education categories
plot_data <- filtered_data %>%
  filter(!is.na(WORK_HOURS), !is.na(ASAD), !is.na(EDUCATION)) %>%
  # Only include complete education levels
  filter(EDUCATION %in% c(5, 7, 8)) %>%  # High School, Bachelors, Graduate
  mutate(
    # Create work hour groups
    work_group = cut(WORK_HOURS, 
                     breaks = c(0, 35, 40, 48, 60, Inf),
                     labels = c("Part-time", "Standard", "Moderate-Overtime", "High-Overtime", "Excessive-Overtime"),
                     include.lowest = TRUE),
    # Create simplified education labels
    educ_label = case_when(
      EDUCATION == 5 ~ "High School",
      EDUCATION == 7 ~ "Bachelors",
      EDUCATION == 8 ~ "Graduate"
    ) %>% factor(levels = c("High School", "Bachelors", "Graduate"))
  ) %>%
  group_by(work_group, educ_label) %>%
  summarize(
    mean_sadness = mean(ASAD, na.rm = TRUE),
    se_sadness = sd(ASAD, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  )

# Create the plot
ggplot(plot_data, aes(x = work_group, y = mean_sadness, fill = educ_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_sadness - se_sadness, 
                    ymax = mean_sadness + se_sadness),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_fill_manual(values = c("High School" = "royalblue", 
                               "Bachelors" = "#BCD1BC", 
                               "Graduate" = "pink")) +
  labs(
    title = "Average Sadness Level by Working Hours and Education Level",
    x = "Working Hours per Week",
    y = "Average Sadness Level",
    fill = "Education Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  scale_y_continuous(
    limits = c(0, max(plot_data$mean_sadness + plot_data$se_sadness) * 1.1)
  )


## Working hour and depression level by edu level ------

# Prepare the data with fewer education categories
plot_data <- filtered_data %>%
  filter(!is.na(WORK_HOURS), !is.na(DEPFREQ), !is.na(EDUCATION)) %>%
  # Only include complete education levels
  filter(EDUCATION %in% c(5, 7, 8)) %>%  # High School, Bachelors, Graduate
  mutate(
    # Create work hour groups
    work_group = cut(WORK_HOURS, 
                     breaks = c(0, 35, 40, 48, 60, Inf),
                     labels = c("Part-time", "Standard", "Moderate-Overtime", "High-Overtime", "Excessive-Overtime"),
                     include.lowest = TRUE),
    # Create simplified education labels
    educ_label = case_when(
      EDUCATION == 5 ~ "High School",
      EDUCATION == 7 ~ "Bachelors",
      EDUCATION == 8 ~ "Graduate"
    ) %>% factor(levels = c("High School", "Bachelors", "Graduate"))
  ) %>%
  group_by(work_group, educ_label) %>%
  summarize(
    mean_depression = mean(DEPFREQ, na.rm = TRUE),
    se_depression = sd(DEPFREQ, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  )

# Create the plot
ggplot(plot_data, aes(x = work_group, y = mean_depression, fill = educ_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_depression - se_depression, 
                    ymax = mean_depression + se_depression),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_fill_manual(values = c("High School" = "royalblue", 
                               "Bachelors" = "#BCD1BC", 
                               "Graduate" = "pink")) +
  labs(
    title = "Average Depression Level by Working Hours and Education Level",
    x = "Working Hours per Week",
    y = "Average Depression Level",
    fill = "Education Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  scale_y_continuous(
    limits = c(0, max(plot_data$mean_depression + plot_data$se_depression) * 1.1)
  )


## Regression Results Visualisation ------

### 1. Forest Plot of Regression Coefficients ------

# Extract coefficients from the models
sadness_coef <- tidy(model_sadness, conf.int = TRUE)
depression_coef <- tidy(model_depression, conf.int = TRUE)

# Prepare data for plotting
sadness_coef$model <- "Sadness (Linear)"
depression_coef$model <- "Depression (Logistic)"

# Combine coefficients
combined_coefs <- rbind(
  sadness_coef, 
  depression_coef
)

# Filter out intercept and only keep key predictors
key_predictors <- c(
  "log_work_hours", 
  "age_num", 
  "sex_dummy", 
  "education_num", 
  "health_num", 
  "marital_num"
)
combined_coefs_filtered <- combined_coefs[combined_coefs$term %in% key_predictors,]

# Create forest plot
ggplot(combined_coefs_filtered, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Regression Coefficients Across Mental Health Models",
    x = "Coefficient Estimate",
    y = "Predictors"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




### 2. Odds Ratio Plot for Logistic Regression Models ------

# Get odds ratios and CIs for depression model
depression_or <- exp(coef(model_depression))
depression_ci <- exp(confint(model_depression))

# Create data frame
plot_data <- data.frame(
  work_group = names(depression_or),
  OR = depression_or,
  CI_Lower = depression_ci[,1],
  CI_Upper = depression_ci[,2]
)

# Filter to only include work hour groups and clean up names
plot_data <- plot_data[grep("work_group", plot_data$work_group),]
plot_data$work_group <- factor(
  c("Part-time (<35h)",
    "Moderate Overtime (41-48h)",
    "High Overtime (49-60h)", 
    "Excessive Overtime (>60h)"),
  levels = c("Part-time (<35h)",
             "Moderate Overtime (41-48h)",
             "High Overtime (49-60h)",
             "Excessive Overtime (>60h)")
)

# Create odds ratio plot
ggplot(plot_data, aes(x = work_group, y = OR)) +
  geom_point(color = "pink") +
  geom_line(group = 1, color = "pink") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "pink") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10() +
  labs(
    title = "Odds Ratios for Depression by Work Hours",
    x = "Work Hour Group",
    y = "Odds Ratio (log scale)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Descriptive Statistics ------

# First convert to data frame
regression_df <- as.data.frame(regression_data)

# Create the groups for working hours
work_hours_by_group <- regression_df %>%
  mutate(
    Part_time = ifelse(HOURSWRK < 35, HOURSWRK, NA),
    Standard = ifelse(HOURSWRK >= 35 & HOURSWRK <= 40, HOURSWRK, NA),
    Moderate_Over = ifelse(HOURSWRK > 40 & HOURSWRK <= 48, HOURSWRK, NA),
    High_Over = ifelse(HOURSWRK > 48 & HOURSWRK <= 60, HOURSWRK, NA),
    Excessive_Over = ifelse(HOURSWRK > 60, HOURSWRK, NA)
  )

# Combine with original variables
final_df <- regression_df %>%
  select(WORK_HOURS, WORK_HOURS_CAT, ASAD,DEPFREQ, DEPRESS_FREQ, 
         EDUCATION,MARITAL, HEALTH, AGE, SEX) %>%
  cbind(work_hours_by_group[,c("Part_time", "Standard", "Moderate_Over", "High_Over", "Excessive_Over")])

# Generate descriptive statistics
stargazer(final_df, 
          type = "html",
          title = "Descriptive Statistics",
          digits = 2,
          summary.stat = c("n", "mean", "sd", "min", "median", "max"),
          out = "descriptive_statistics.doc")

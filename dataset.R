### Download packages ####
packages <- c("dplyr", "ggplot2", "broom", "tidyr","srvyr", "NHANES", "kableExtra")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#### Load data ####
ddi_new <- read_ipums_ddi("nhis_00005.xml")
data_ipums_new <- read_ipums_micro(ddi_new)
head(data_ipums_new)

#### Apply filters ####
filtered_data <- data_ipums_new %>%
  filter(
    YEAR >= 2013 & YEAR <= 2018,
    AGE <= 85,
    SEX <= 2,
    HEALTH <= 5,
    EMPSTAT <= 200,
    EDUC <= 503,
    (HOURSWRK != 0 & HOURSWRK <= 95),
    (ASAD != 6 & ASAD <= 4),
    (WORFREQ != 0 & WORFREQ <= 5),
    (DEPFREQ != 0 & DEPFREQ <= 5),
    (MARST >= 10 & MARST <= 50),
    (WORFEELEVL != 0 & WORFEELEVL <= 3),  # Include only valid worry level responses
    (DEPFEELEVL != 0 & DEPFEELEVL <= 3)    # Include only valid depression level responses
  )

filtered_data$HOURSWRK[filtered_data$HOURSWRK >= 95] <- 95
head(filtered_data)

#### 1. Regroup Education ####
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

#### 2. Regroup Employment Status ####
filtered_data$EMPLOYMENT = filtered_data$EMPSTAT
employed <- c(100, 110, 111, 112, 120, 121, 122)
unemployed <- c(200, 210, 211, 212, 213, 214, 215, 216, 217, 220)
filtered_data$EMPLOYMENT[filtered_data$EMPLOYMENT %in% employed] = 1
filtered_data$EMPLOYMENT[filtered_data$EMPLOYMENT %in% unemployed] = 2

filtered_data$HEALTH_STATUS = filtered_data$HEALTH
filtered_data$SADNESS = filtered_data$ASAD  # 0-4
filtered_data$ANXIETY_MED = filtered_data$WORRX    # 1-2
filtered_data$DEPRESSION_MED = filtered_data$DEPRX # 1-2

filtered_data$WORK_HOURS = filtered_data$HOURSWRK
filtered_data$WORK_HOURS[filtered_data$WORK_HOURS >= 95] = 95

#### Create work hours categories ####
filtered_data$WORK_HOURS_CAT <- NA
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS <= 20] = 1
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 20 & filtered_data$WORK_HOURS <= 35] = 2
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 35 & filtered_data$WORK_HOURS <= 40] = 3
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 40 & filtered_data$WORK_HOURS <= 60] = 4
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 60] = 5

#### Recode all variables and create new ones within the data frame ####
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
    
    # Employment grouping
    EMPLOYMENT = case_when(
      EMPSTAT %in% c(100, 110, 111, 112, 120, 121, 122) ~ 1,
      EMPSTAT %in% c(200, 210, 211, 212, 213, 214, 215, 216, 217, 220) ~ 2,
      TRUE ~ NA_real_
    ),
    
    # Health Status
    HEALTH_STATUS = HEALTH,
    
    # Mental Health Indicators
    SADNESS = ASAD,
    
    # Binary coding for worry and depression frequency
    WORRY_FREQ = case_when(
      WORFREQ >= 1 & WORFREQ <= 4 ~ 1,
      WORFREQ == 5 ~ 0,
      TRUE ~ NA_real_
    ),
    
    DEPRESS_FREQ = case_when(
      DEPFREQ >= 1 & DEPFREQ <= 4 ~ 1,
      DEPFREQ == 5 ~ 0,
      TRUE ~ NA_real_
    ),
    
    WORRY_LEVEL = WORFEELEVL,
    DEPRESS_LEVEL = DEPFEELEVL,
    
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

#### Create final dataset with numeric variables ####
final_data <- filtered_data %>%
  select(
    YEAR,
    SERIAL,
    AGE,
    SEX,
    MARITAL,
    EDUCATION,
    EMPLOYMENT,
    WORK_HOURS,
    WORK_HOURS_CAT,
    HEALTH_STATUS,
    SADNESS,
    WORRY_FREQ,
    WORRY_LEVEL,
    DEPRESS_FREQ,
    DEPRESS_LEVEL,
    SAMPWEIGHT,
    STRATA,
    PSU
  )

head(final_data)

#### Examine distributions ####
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

ggplot(filtered_data, aes(DEPRESS_FREQ)) +
  geom_histogram(binwidth = 0.2, fill = '#BCD1BC', color = "#BCD1BC") +
  labs(title = "Depression", 
       x = "Depression", 
       y = "Count")

ggplot(filtered_data, aes(WORRY_FREQ)) +
  geom_histogram(binwidth = 0.2, fill = '#BCD1BC', color = "#BCD1BC") +
  labs(title = "Worrieness", 
       x = "Worrieness", 
       y = "Count")



#### Multivariate and Logistic Regression Model with Logistic Working Hours ####

# Prepare data with proper binary and ordinal coding
regression_data <- filtered_data %>%
  filter(WORK_HOURS >= 5) %>%
  mutate(
    log_work_hours = log(WORK_HOURS),
    sex_dummy = as.numeric(SEX - 1),
    education_num = as.numeric(EDUCATION),
    health_num = as.numeric(HEALTH_STATUS),
    age_num = as.numeric(AGE),
    marital_num = as.numeric(MARITAL),
    sadness_num = as.numeric(ASAD),
    worry_binary = WORRY_FREQ,
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

# 1. Regular linear model for Sadness
model_sadness <- svyglm(
  sadness_num ~ 
    log_work_hours + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num +
    marital_num,
  design = nhis_design
)

# 2. Logistic regression for Worry
model_worry <- svyglm(
  worry_binary ~ 
    log_work_hours + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num +
    marital_num,
  design = nhis_design,
  family = quasibinomial()
)

# 3. Logistic regression for Depression
model_depression <- svyglm(
  depress_binary ~ 
    log_work_hours + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num +
    marital_num,
  design = nhis_design,
  family = quasibinomial()
)

# Create regression output table
stargazer(model_sadness, model_worry, model_depression,
          type = "text",
          title = "Regression Results",
          column.labels = c("Sadness", "Worry", "Depression"),
          column.separate = c(1,1,1),
          model.names = FALSE,
          dep.var.labels = c("Mental Health Outcomes"),
          add.lines = list(
            c("Model Type", "Linear", "Logistic", "Logistic")),
          covariate.labels = c("Log(Working Hours)",
                               "Age",
                               "Sex (Female)",
                               "Education Level",
                               "Health Status",
                               "Marital Status"))

# For logistic models, calculate odds ratios
get_or_ci <- function(model) {
  coef <- coef(model)
  se <- sqrt(diag(vcov(model)))
  ci_lower <- exp(coef - 1.96 * se)
  ci_upper <- exp(coef + 1.96 * se)
  or <- exp(coef)
  return(data.frame(OR = or, CI_Lower = ci_lower, CI_Upper = ci_upper))
}

# Get odds ratios for binary outcome models
or_worry <- get_or_ci(model_worry)
or_depression <- get_or_ci(model_depression)

# Print odds ratios
print("Odds Ratios and 95% CI for Worry Model:")
print(or_worry)
print("Odds Ratios and 95% CI for Depression Model:")
print(or_depression)



#### Multivariate and Logistic Regression Models with Grouped Working Hours ####

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
    worry_binary = WORRY_FREQ,
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

# Fit models
# 1. Linear model for Sadness
model_sadness <- svyglm(
  sadness_num ~ 
    work_group + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num +
    marital_num,
  design = nhis_design
)

# 2. Logistic regression for Worry
model_worry <- svyglm(
  worry_binary ~ 
    work_group + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num +
    marital_num,
  design = nhis_design,
  family = quasibinomial()
)

# 3. Logistic regression for Depression
model_depression <- svyglm(
  depress_binary ~ 
    work_group + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num +
    marital_num,
  design = nhis_design,
  family = quasibinomial()
)

# Create regression output table
stargazer(model_sadness, model_worry, model_depression,
          type = "text",
          title = "Regression Results",
          column.labels = c("Sadness", "Worry", "Depression"),
          column.separate = c(1,1,1),
          model.names = FALSE,
          dep.var.labels = c("Mental Health Outcomes"),
          add.lines = list(
            c("Model Type", "Linear", "Logistic", "Logistic")),
          covariate.labels = c("Part-time (<35h)",
                               "Moderate Overtime (41-48h)",
                               "High Overtime (49-60h)",
                               "Excessive Overtime (>60h)",
                               "Age",
                               "Sex (Female)",
                               "Education Level",
                               "Health Status",
                               "Marital Status"))

# For logistic models, calculate odds ratios
get_or_ci <- function(model) {
  coef <- coef(model)
  se <- sqrt(diag(vcov(model)))
  ci_lower <- exp(coef - 1.96 * se)
  ci_upper <- exp(coef + 1.96 * se)
  or <- exp(coef)
  return(data.frame(OR = or, CI_Lower = ci_lower, CI_Upper = ci_upper))
}

# Get odds ratios for binary outcome models
or_worry <- get_or_ci(model_worry)
or_depression <- get_or_ci(model_depression)

# Print odds ratios
print("Odds Ratios and 95% CI for Worry Model:")
print(or_worry)
print("Odds Ratios and 95% CI for Depression Model:")
print(or_depression)



#### Visualisations ####

library(gridExtra)  # for arranging multiple plots

# 1. Scatterplot with fitted lines for Sadness
p1 <- ggplot(regression_data, aes(x = log_work_hours, y = sadness_num)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship between Working Hours and Sadness",
       x = "Log(Working Hours)",
       y = "Sadness Level") +
  theme_minimal()

# 2. Predicted probabilities for medication use
# Create prediction data
pred_data <- data.frame(
  log_work_hours = seq(min(regression_data$log_work_hours), 
                       max(regression_data$log_work_hours), 
                       length.out = 100),
  age_num = mean(regression_data$age_num),
  sex_dummy = mean(regression_data$sex_dummy),
  education_num = mean(regression_data$education_num),
  health_num = mean(regression_data$health_num)
)

# Get predictions for anxiety
pred_data$pred_anxiety <- predict(model_anxiety, 
                                  newdata = pred_data, 
                                  type = "response")

# Get predictions for depression
pred_data$pred_depression <- predict(model_depression, 
                                     newdata = pred_data, 
                                     type = "response")

# Plot predicted probabilities
p2 <- ggplot(pred_data, aes(x = log_work_hours)) +
  geom_line(aes(y = pred_anxiety, color = "Anxiety Med"), size = 1) +
  geom_line(aes(y = pred_depression, color = "Depression Med"), size = 1) +
  labs(title = "Predicted Probabilities of Medication Use",
       x = "Log(Working Hours)",
       y = "Predicted Probability",
       color = "Medication Type") +
  theme_minimal()

# 3. Coefficient plot
# Prepare coefficient data
coef_data <- data.frame(
  Variable = rep("Log(Working Hours)", 3),
  Model = c("Sadness", "Anxiety Med", "Depression Med"),
  Coefficient = c(coef(model_sadness)[2], 
                  coef(model_anxiety)[2], 
                  coef(model_depression)[2]),
  SE = c(sqrt(diag(vcov(model_sadness)))[2],
         sqrt(diag(vcov(model_anxiety)))[2],
         sqrt(diag(vcov(model_depression)))[2])
)

p3 <- ggplot(coef_data, aes(x = Model, y = Coefficient)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96*SE, 
                    ymax = Coefficient + 1.96*SE), 
                width = 0.2) +
  labs(title = "Working Hours Coefficients Across Models",
       x = "",
       y = "Coefficient Estimate") +
  theme_minimal() +
  coord_flip()

# Arrange plots
grid.arrange(p1, p2, p3, ncol = 2)

# Save plots if needed
ggsave("working_hours_mental_health.png", 
       arrangeGrob(p1, p2, p3, ncol = 2), 
       width = 12, height = 8)
















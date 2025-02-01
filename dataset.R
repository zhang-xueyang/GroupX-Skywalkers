#download packages
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

# Load data
ddi_new <- read_ipums_ddi("nhis_00003.xml")
data_ipums_new <- read_ipums_micro(ddi_new)
head(data_ipums_new)

# Apply filters using similar format
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
    (WORRX != 0 & WORRX <= 2),
    (DEPRX != 0 & DEPRX <= 2)
  )

# Recode any specific values
filtered_data$HOURSWRK[filtered_data$HOURSWRK >= 95] <- 95
head(filtered_data)

# 1. Regroup Education (numeric)
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

# 2. Regroup Employment Status (numeric)
filtered_data$EMPLOYMENT = filtered_data$EMPSTAT
employed <- c(100, 110, 111, 112, 120, 121, 122)
unemployed <- c(200, 210, 211, 212, 213, 214, 215, 216, 217, 220)
filtered_data$EMPLOYMENT[filtered_data$EMPLOYMENT %in% employed] = 1
filtered_data$EMPLOYMENT[filtered_data$EMPLOYMENT %in% unemployed] = 2

# 3. Keep Health Status numeric (1-5)
filtered_data$HEALTH_STATUS = filtered_data$HEALTH

# 4. Keep Mental Health Indicators numeric
filtered_data$SADNESS = filtered_data$ASAD  # 0-4

# 5. Keep Medication Use numeric
filtered_data$ANXIETY_MED = filtered_data$WORRX    # 1-2
filtered_data$DEPRESSION_MED = filtered_data$DEPRX # 1-2

# 6. Work Hours
filtered_data$WORK_HOURS = filtered_data$HOURSWRK
filtered_data$WORK_HOURS[filtered_data$WORK_HOURS >= 95] = 95

# Create work hours categories numerically
filtered_data$WORK_HOURS_CAT <- NA
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS <= 20] = 1
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 20 & filtered_data$WORK_HOURS <= 35] = 2
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 35 & filtered_data$WORK_HOURS <= 40] = 3
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 40 & filtered_data$WORK_HOURS <= 60] = 4
filtered_data$WORK_HOURS_CAT[filtered_data$WORK_HOURS > 60] = 5

# Create final dataset with numeric variables
final_data <- filtered_data %>%
  select(
    YEAR,
    SERIAL,
    AGE,
    SEX,
    EDUCATION,
    EMPLOYMENT,
    WORK_HOURS,
    WORK_HOURS_CAT,
    HEALTH_STATUS,
    SADNESS,
    ANXIETY_MED,
    DEPRESSION_MED,
    SAMPWEIGHT,
    STRATA,
    PSU
  )

head(final_data)

## Examine the distribution
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

ggplot(filtered_data, aes(DEPRESSION_MED)) +
  geom_histogram(binwidth = 0.5, fill = '#BCD1BC', color = "#BCD1BC") +
  labs(title = "Depression", 
       x = "Depression", 
       y = "Count")

ggplot(filtered_data, aes(ANXIETY_MED)) +
  geom_histogram(binwidth = 0.5, fill = '#BCD1BC', color = "#BCD1BC") +
  labs(title = "Anxiety", 
       x = "Anxiety", 
       y = "Count")




#### Multivariate Linear Regression Model ####

# Create cleaned dataset for regression
regression_data <- filtered_data %>%
  filter(WORK_HOURS >= 5) %>%
  mutate(
    log_work_hours = log(WORK_HOURS),
    sex_dummy = SEX - 1  # Converts 1,2 to 0,1
  )

# Create survey design object
nhis_design <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data,
  nest = TRUE
)

# Fit three survey-weighted regression models
# 1. Sadness Level as dependent variable
model_sadness <- svyglm(
  SADNESS ~ 
    log_work_hours + 
    AGE + 
    sex_dummy + 
    EDUCATION + 
    HEALTH_STATUS,
  design = nhis_design
)

# 2. Anxiety Medication as dependent variable
model_anxiety <- svyglm(
  ANXIETY_MED ~ 
    log_work_hours + 
    AGE + 
    sex_dummy + 
    EDUCATION + 
    HEALTH_STATUS,
  design = nhis_design
)

# 3. Depression Medication as dependent variable
model_depression <- svyglm(
  DEPRESSION_MED ~ 
    log_work_hours + 
    AGE + 
    sex_dummy + 
    EDUCATION + 
    HEALTH_STATUS,
  design = nhis_design
)

# View summaries
summary(model_sadness)
summary(model_anxiety)
summary(model_depression)

# Create regression output table for all three models
library(stargazer)
stargazer(model_sadness, model_anxiety, model_depression,
          type = "text",
          title = "Survey-Weighted Regression Results",
          column.labels = c("Sadness", "Anxiety Med", "Depression Med"),
          covariate.labels = c("Log(Working Hours)",
                               "Age",
                               "Sex (Female)",
                               "Education Level",
                               "Health Status"),
          dep.var.labels = c("Mental Health Outcomes"))

# Calculate pseudo R-squared for each model
r2_sadness <- 1 - model_sadness$deviance/model_sadness$null.deviance
r2_anxiety <- 1 - model_anxiety$deviance/model_anxiety$null.deviance
r2_depression <- 1 - model_depression$deviance/model_depression$null.deviance

print(paste("Pseudo R-squared (Sadness):", round(r2_sadness, 3)))
print(paste("Pseudo R-squared (Anxiety):", round(r2_anxiety, 3)))
print(paste("Pseudo R-squared (Depression):", round(r2_depression, 3)))



#### Probit and Logit Model ####

# Prepare data with proper binary and ordinal coding
regression_data <- filtered_data %>%
  filter(WORK_HOURS >= 5) %>%
  mutate(
    log_work_hours = log(WORK_HOURS),
    sex_dummy = as.numeric(SEX - 1),
    
    # Recode anxiety med (1=No, 2=Yes) to (0=No, 1=Yes)
    anxiety_med_binary = case_when(
      ANXIETY_MED == 1 ~ 0,
      ANXIETY_MED == 2 ~ 1
    ),
    
    # Recode depression med (1=No, 2=Yes) to (0=No, 1=Yes)
    depression_med_binary = case_when(
      DEPRESSION_MED == 1 ~ 0,
      DEPRESSION_MED == 2 ~ 1
    ),
    
    # Other numeric conversions
    education_num = as.numeric(EDUCATION),
    health_num = as.numeric(HEALTH_STATUS),
    age_num = as.numeric(AGE),
    sadness_num = as.numeric(ASAD)
  )

# Create survey design object
nhis_design <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weights = ~SAMPWEIGHT,
  data = regression_data,
  nest = TRUE
)

# 1. Regular linear model for Sadness (as it's on a 0-4 scale)
model_sadness <- svyglm(
  sadness_num ~ 
    log_work_hours + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num,
  design = nhis_design
)

# 2. Logistic regression for Anxiety Medication
model_anxiety <- svyglm(
  anxiety_med_binary ~ 
    log_work_hours + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num,
  design = nhis_design,
  family = quasibinomial()
)

# 3. Logistic regression for Depression Medication
model_depression <- svyglm(
  depression_med_binary ~ 
    log_work_hours + 
    age_num + 
    sex_dummy + 
    education_num + 
    health_num,
  design = nhis_design,
  family = quasibinomial()
)

# View summaries
summary(model_sadness)
summary(model_anxiety)
summary(model_depression)

# Create regression output table
library(stargazer)
stargazer(model_sadness, model_anxiety, model_depression,
          type = "text",
          title = "Regression Results",
          column.labels = c("Sadness (Linear)", "Anxiety Med (Logit)", "Depression Med (Logit)"),
          covariate.labels = c("Log(Working Hours)",
                               "Age",
                               "Sex (Female)",
                               "Education Level",
                               "Health Status"),
          dep.var.labels = c("Mental Health Outcomes"))

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
or_anxiety <- get_or_ci(model_anxiety)
or_depression <- get_or_ci(model_depression)

# Print odds ratios
print("Odds Ratios and 95% CI for Anxiety Medication Model:")
print(or_anxiety)
print("Odds Ratios and 95% CI for Depression Medication Model:")
print(or_depression)



#### plots ####
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
















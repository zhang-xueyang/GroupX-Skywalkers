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

## Working hour and sadness level by sex ##

# Prepare the data
plot_data <- filtered_data %>%
  filter(!is.na(WORK_HOURS), !is.na(ASAD), !is.na(SEX)) %>%
  mutate(
    # Create work hour groups
    work_group = cut(WORK_HOURS, 
                     breaks = c(0, 20, 35, 40, 48, 60, Inf),
                     labels = c("≤20", "21-35", "36-40", "41-48", "49-60", ">60"),
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

# Optional: Add sample sizes to the plot data
print("Sample sizes for each group:")
print(plot_data %>% select(work_group, sex_label, n))


## Working hour and sadness level by edu level ##

# Prepare the data
plot_data <- filtered_data %>%
  filter(!is.na(WORK_HOURS), !is.na(ASAD), !is.na(EDUCATION)) %>%
  mutate(
    # Create work hour groups
    work_group = cut(WORK_HOURS, 
                     breaks = c(0, 20, 35, 40, 48, 60, Inf),
                     labels = c("≤20", "21-35", "36-40", "41-48", "49-60", ">60"),
                     include.lowest = TRUE),
    # Create meaningful education labels
    educ_label = case_when(
      EDUCATION == 1 ~ "No School",
      EDUCATION == 2 ~ "Elementary",
      EDUCATION == 3 ~ "Middle School",
      EDUCATION == 4 ~ "Some High School",
      EDUCATION == 5 ~ "High School",
      EDUCATION == 6 ~ "Some College",
      EDUCATION == 7 ~ "Bachelors",
      EDUCATION == 8 ~ "Graduate"
    )
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
  scale_fill_brewer(palette = "Set2") +  # Use a color palette suitable for multiple categories
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
    legend.position = "right"
  ) +
  scale_y_continuous(
    limits = c(0, max(plot_data$mean_sadness + plot_data$se_sadness) * 1.1)
  )

# Optional: Add sample sizes to the plot data
print("Sample sizes for each group:")
print(plot_data %>% select(work_group, educ_label, n))



## Reduced edu level ##

# Prepare the data with fewer education categories
plot_data <- filtered_data %>%
  filter(!is.na(WORK_HOURS), !is.na(ASAD), !is.na(EDUCATION)) %>%
  # Only include complete education levels
  filter(EDUCATION %in% c(5, 7, 8)) %>%  # High School, Bachelors, Graduate
  mutate(
    # Create work hour groups
    work_group = cut(WORK_HOURS, 
                     breaks = c(0, 20, 35, 40, 48, 60, Inf),
                     labels = c("≤20", "21-35", "36-40", "41-48", "49-60", ">60"),
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

# Print sample sizes
print("Sample sizes for each group:")
print(plot_data %>% select(work_group, educ_label, n))










## Sadness levels ##

# Version with percentage labels
hist_with_labels <- filtered_data %>%
  group_by(ASAD) %>%
  summarize(count = n()) %>%
  mutate(percentage = count/sum(count) * 100) %>%
  ggplot(aes(x = factor(ASAD), y = percentage)) +
  geom_bar(stat = "identity", fill = "#BCD1BC") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5) +
  labs(
    title = "Distribution of Sadness Levels",
    x = "Sadness Level (0 = None to 4 = Highest)",
    y = "Percentage"
  ) +
  theme_minimal() +
  ylim(0, max(filtered_data %>% 
                group_by(ASAD) %>% 
                summarize(count = n()) %>% 
                mutate(percentage = count/sum(count) * 100) %>% 
                pull(percentage)) * 1.2)

# By sex with percentage labels - corrected version
hist_by_sex_labels <- filtered_data %>%
  mutate(sex_label = ifelse(SEX == 1, "Male", "Female")) %>%  # Create labels first
  group_by(sex_label, ASAD) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(sex_label) %>%
  mutate(percentage = count/sum(count) * 100) %>%
  ggplot(aes(x = factor(ASAD), y = percentage, fill = sex_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  scale_fill_manual(values = c("Male" = "#BCD1BC", "Female" = "pink")) +
  labs(
    title = "Distribution of Sadness Levels by Sex",
    x = "Sadness Level (0 = None to 4 = Highest)",
    y = "Percentage",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  ylim(0, max(filtered_data %>% 
                mutate(sex_label = ifelse(SEX == 1, "Male", "Female")) %>%
                group_by(sex_label, ASAD) %>%
                summarize(count = n()) %>%
                group_by(sex_label) %>%
                mutate(percentage = count/sum(count) * 100) %>%
                pull(percentage)) * 1.2)

# Display both plots side by side
grid.arrange(hist_with_labels, hist_by_sex_labels, ncol = 2)

# Print summary statistics
summary_stats <- filtered_data %>%
  group_by(SEX) %>%
  summarize(
    mean_sadness = mean(ASAD, na.rm = TRUE),
    sd_sadness = sd(ASAD, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(sex_label = ifelse(SEX == 1, "Male", "Female"))

print("Summary Statistics:")
print(summary_stats)








# 1. Forest Plot of Regression Coefficients

# Extract coefficients from the models
sadness_coef <- tidy(model_sadness, conf.int = TRUE)
worry_coef <- tidy(model_worry, conf.int = TRUE)
depression_coef <- tidy(model_depression, conf.int = TRUE)

# Prepare data for plotting
sadness_coef$model <- "Sadness (Linear)"
worry_coef$model <- "Worry (Logistic)"
depression_coef$model <- "Depression (Logistic)"

# Combine coefficients
combined_coefs <- rbind(
  sadness_coef, 
  worry_coef, 
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




# 2. Odds Ratio Plot for Logistic Regression Models

# Prepare odds ratio data
worry_or <- or_worry[rownames(or_worry) %in% c("work_groupPart-time", "work_groupModerate-Over", "work_groupHigh-Over", "work_groupExcessive-Over"), ]
depression_or <- or_depression[rownames(or_depression) %in% c("work_groupPart-time", "work_groupModerate-Over", "work_groupHigh-Over", "work_groupExcessive-Over"), ]

# Add model names
worry_or$model <- "Worry"
depression_or$model <- "Depression"
worry_or$work_group <- rownames(worry_or)
depression_or$work_group <- rownames(depression_or)

# Combine odds ratio data
combined_or <- rbind(worry_or, depression_or)

# Create odds ratio plot
ggplot(combined_or, aes(x = work_group, y = OR, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10() +
  labs(
    title = "Odds Ratios for Mental Health Outcomes by Work Hours",
    x = "Work Hour Group",
    y = "Odds Ratio (log scale)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
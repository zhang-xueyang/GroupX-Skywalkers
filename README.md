# BUSI70501 Group Project
## Effect of Working Hours on Mental Health: Empirical Evidence in the USA [Group X]

## Overview
This project analyzes the relationship between working hours and mental health outcomes (depression and sadness) using the IPUMS National Health Interview Survey (NHIS) data from 2013 to 2018. The analysis examines how different working patterns affect mental well-being, with particular attention to gender and education-level heterogeneity.

## Data
- **IPUMS series**: NHIS

  **Dependent Variable**:   
  - Depression (DEPFREQ): How often feel depressed (categorized from 1 to 5, where 1 indicates feeling depressed daily and 5 means never feel depressed)      
  - Sadness (ASAD): How often feel sad, past 30 days (adults)(categorized from 0 to 4, where 0 indicates none of the time and 4 means all of the time)      

  **Explanatory Variable**:      
  - Working hours (HOURSWRK): Total hours worked last week or usually      

  **Control Variables**:      
  - Education attainment (EDUC): categorical variable to control for the effect of education      
  - Age (AGE): continuous variable to control for the impact of age-related differences      
  - Sex (SEX)：binary variable to control for gender differences in work-life pressures      
  - Marital status (MARSTAT): married individuals may have different work commitments compared to single individuals 

  **Mediation Variable**:
  - Health status (HEALTH)   

- **Countries**: USA
- **Years**: 2013-2018
- **Access to Raw Data**: https://nhis.ipums.org/nhis-action/variables/group

## Code
To run the code, first clone the repository into your local envinronment. The main codes of our project are in **Group X Code Scripts.Rmd**, with data files used compiled in **Data** and relevant outputs stored in **Output**. In **Data**, **nhis_00005.dat** and **nhis_00005.xml** are the two main data files downloaded directly from IPUMS: the former is the raw data file and the latter is the ddi document. For **Group X Code Scripts.Rmd**, you have to set the correct working directory and download all necessary packages as indicated in the beginning of the file. Also, please ensure the codes are ran sequentially with a R version no older than 4.4.2 to avoid any unexpected error.

# Introduction to Health Analytics Group Project X

## Description of data
In this section you should describe what data you use for your project with enough detail that anyone could download it and run your analysis.
- **IPUMS series**: NHIS
**Dependent Variable**: mental health
 Depression (DEPFREQ): how often feel depressed (categorized from 0 to 4, where 0 indicates feeling depressed daily and 4 means never feel depressed)
 Anxiety (WORFREQ): how often feel worried, nervous or anxious (categorized from 0 to 4, where 0 indicates feeling anxious daily and 4 means never feel anxious)
**Explanatory Variable**: 
 working hours (HOURSWRK): Total hours worked last week or usually
**Control Variables**: 
 educational attainment (EDUC): categorical variable to control for the effect of education
 age (AGE): continuous variable to control for the impact of age-related differences
 sex (SEX)：binary variable to control for gender differences in work-life pressures
 health status (HEALTH)
 marital status (MARSTAT): married individuals may have different work commitments compared to single individuals
- **Countries**: USA
- **Years**: 2013-2018
- **How to access the data**: https://nhis.ipums.org/nhis-action/variables/group

## Description of how to run the code
Download the R script file and 2 dataset files (ddi & dat) in a same directory and run the code

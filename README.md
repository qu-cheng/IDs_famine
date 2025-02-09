# IDs_Famine

This repository contains data and code for reproducing the analyses in Elevated risk of infectious diseases in adulthood after prenatal or early postnatal exposure to the Great Chinese Famine

## Data

- 11.by_sex_pref_disease.data.csv: the reported case count of cases for each of the 11 studied infectious diseases reported in Sichuan Province during the period from 2005–2022 by age, sex, year of diagnosis, and prefecture of residence.
- provincial_model_data.rds and prefecture_model_data.rds: the nested data used for fitting Bayesian APC models at the provincial and prefecture levels, respectively.
- pop.by_pref_age_sex_2005-2022.csv: the population size stratified by age, sex, year, and prefecture of residence.
- Famine intensity by pref.csv: the cohort size shrinkage index (CSSI) representing famine intensity of 21 prefectures. 
- F2 time by prefecture population data.csv: the  birth year ranges of F2 for different prefectures.
- Sichuan_City.shp: the base map of Sichuan Province, China.


## Code

This folder contains code for the analyses and figures:

- 01_packages_data_functions.R: R script to load packages, preprocess data, define functions
- 02_provincial_level_by_sex.R: R script to fit the Bayesian APC model at the provincial level 
- 03_pref_level.R: R script to fit the Bayesian APC model at the prefecture level
- 04_meta_analysis.R: R script to fit the meta-regression model
- 05_results_visualization.R: R script to visualize the main results
- 06_supp_figs.R: R script to visualize the supplementary results
  

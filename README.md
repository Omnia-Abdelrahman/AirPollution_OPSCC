# AirPollution_OPSCC Project

This repository contains the final project for the MPH practicum at Boston University, titled **"Air Pollution and Recurrence Risk in HPV-Positive Oropharyngeal Squamous Cell Carcinoma (OPSCC)"** by Omnia Abdelrahman.

## Summary

Oropharyngeal squamous cell carcinoma (OPSCC), particularly HPV-positive cases, generally has a favorable prognosis. However, environmental factors such as long-term air pollution exposure may influence recurrence and overall survival. This project examines the role of pollutants, specifically PM2.5, NO2, and VOCs, in modifying cancer outcomes among HPV-positive OPSCC patients.

The analysis includes retrospective data from 791 patients treated at the University of Pennsylvania between 2007 and 2023. Using geocoded ZIP codes and EPA/CDC pollution datasets, the study assesses the impact of regional exposure levels on recurrence and mortality. Socioeconomic status, smoking, and comorbidities were also evaluated as modifiers of risk.

## Files Included

- `code/`: R scripts for survival modeling, Cox regression, LASSO, and visualization  
- `sas/`: SAS script for propensity score matching  
- `poster/`: Final PDF poster summarizing the study findings  
- [Click here to view the full poster (PDF)](poster/OPSCC_AirPollution_Poster.pdf)


## Data and Methods

- **Population:** 791 HPV-positive OPSCC patients
- **Exposure:** Long-term levels of PM2.5, NO2, and VOCs assigned via ZIP-code linkage to national pollution data
- **Statistical Methods:**
  - Kaplan-Meier survival analysis
  - Cox proportional hazards models
  - Propensity score matching and inverse probability weighting
  - LASSO regression for variable selection
  - Spatial stratification of regional exposure

## Tools Used

- R: `survival`, `survminer`, `ggplot2`, `glmnet`, `corrplot`, `officer`, `flextable`
- SAS: Propensity score estimation and adjustment

## Files Included

- `code/`: R scripts for survival modeling, Cox regression, LASSO, and visualization
- `sas/`: SAS script for propensity score matching
- `poster/`: Final PDF poster summarizing the study findings

## Author

Omnia Abdelrahman  
BDS, MPH | Boston University

## Contact

For questions or collaboration inquiries, please open an issue or contact via email.

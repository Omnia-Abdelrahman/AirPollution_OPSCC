# AirPollution_OPSCC Project

This repository contains my MPH practicum project at Boston University titled  
**"Air Pollution and Recurrence Risk in HPV-Positive Oropharyngeal Squamous Cell Carcinoma (OPSCC)"**.

## Project Overview

While HPV-positive OPSCC generally has favorable outcomes, this study investigates whether long-term air pollution exposure modifies recurrence and survival risks. Leveraging clinical and environmental data, I assessed the impact of PM2.5, NO₂, and VOC exposure on 791 patients treated at the University of Pennsylvania between 2007 and 2023.

## Analysis Summary

- **Study Design:** Retrospective cohort  
- **Population:** 791 HPV-positive OPSCC patients  
- **Exposure Assessment:** EPA & CDC pollution estimates (ZIP-code level)  
- **Variables:** Included SES, smoking, comorbidities as covariates/modifiers  
- **Key Models Used:**
  - Cox proportional hazards
  - Kaplan-Meier survival curves
  - LASSO regression for variable selection
  - Propensity score matching (SAS 9.4)
  - Inverse probability weighting  
  - Stratified analyses for spatial variation

## Tools & Code

- **R 4.3.1:** `survival`, `survminer`, `glmnet`, `ggplot2`, `corrplot`, `officer`, `flextable`  
- **SAS 9.4:** Propensity score modeling, matching diagnostics  
- **Output:** Clean, annotated scripts in `code/` and `sas/` folders

## Repository Structure

- `code/` – R scripts for modeling and visualization  
- `sas/` – SAS script for matching and sensitivity checks  
- `poster/` – [View Final Poster (PDF)](OPSCC_AirPollution_Poster.pdf)

## What I Learned

- Applied causal inference techniques in a real-world oncology dataset  
- Integrated clinical and environmental datasets for outcome prediction  
- Improved data cleaning, modeling efficiency, and reporting in both R and SAS  
- Developed reproducible workflows and publication-ready figures/tables

## Author

**Omnia Abdelrahman, BDS, MPH**  
Graduate Student in Epidemiology & Biostatistics, Boston University

## Contact

Have questions or want to collaborate?  
Email: [omnia@bu.edu](mailto:omnia@bu.edu)  
Open an issue or connect with me on [LinkedIn](https://www.linkedin.com/in/omnia-abdelrahman/)

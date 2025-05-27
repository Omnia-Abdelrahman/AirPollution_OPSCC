#PART3: #COX FOR ALL VARIABLES (Clinical – SE – Env)
library(survival)
library(survminer)
library(car)
library(officer)
library(flextable)

my_data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")


my_data$pm_cat <- relevel(factor(my_data$pm_cat), ref = "0")   
my_data$no2_cat <- relevel(factor(my_data$no2_cat), ref = "0")  
my_data$voc_cat <- relevel(factor(my_data$voc_cat), ref = "0")  
my_data$aqi_cat <- relevel(factor(my_data$aqi_cat), ref = "1")  
my_data$SES <- factor(ifelse(my_data$SES == 1, 2, my_data$SES)) 
my_data$SES <- relevel(factor(my_data$SES), ref = 2) 
my_data$rx <- relevel(factor(my_data$rx), ref = 1)
my_data$clm <- factor(ifelse(my_data$clm %in% c(1, 2, 3), 3, my_data$clm))
my_data$clm <- relevel(factor(my_data$clm), ref = "0")
my_data <- subset(my_data, ms_cat != 0)
my_data$ms_cat <- relevel(factor(my_data$ms_cat), ref = 2)
my_data$eclo1 <- relevel(factor(my_data$eclo1), ref = 1)
my_data$smk_cat <- relevel(factor(my_data$smk_cat), ref = 1)
my_data$HPV <- relevel(factor(my_data$HPV), ref = 2)
my_data$epao <- relevel(factor(my_data$epao), ref = 1)

#my unique code
# Cox models for short-term recurrence (within 36 months) including environmental variables
cox_short_term_clinical <- coxph(Surv(ttr, rec) ~ COPD + priorhnc + ms_cat + clm + rx + epao + eclo1 + pm_cat + no2_cat + voc_cat + aqi_cat, 
                                 data = my_data, subset = (ttr <= 36))

cox_short_term_se <- coxph(Surv(ttr, rec) ~ age + SES + HPV + smk_cat + pm_cat + no2_cat + voc_cat + aqi_cat, 
                           data = my_data, subset = (ttr <= 36))
# Cox models for long-term recurrence (beyond 36 months) including environmental variables
cox_long_term_clinical <- coxph(Surv(ttr, rec) ~ COPD + priorhnc + ms_cat + clm + rx + epao + eclo1 + pm_cat + no2_cat + voc_cat + aqi_cat, 
                                data = my_data, subset = (ttr > 36))

cox_long_term_se <- coxph(Surv(ttr, rec) ~ age + SES + HPV + smk_cat + pm_cat + no2_cat + voc_cat + aqi_cat, 
                          data = my_data, subset = (ttr > 36))
# Cox model for overall survival (using TTD) including environmental variables
cox_os_clinical <- coxph(Surv(ttd, death) ~ COPD + priorhnc + ms_cat + clm + rx + epao + eclo1 + pm_cat + no2_cat + voc_cat + aqi_cat, 
                         data = my_data)

cox_os_se <- coxph(Surv(ttd, death) ~ age + SES + HPV + smk_cat + pm_cat + no2_cat + voc_cat + aqi_cat, 
                   data = my_data)

# A function to convert the model summary into a flextable with confidence intervals
make_flextable <- function(model, title) {
  tbl <- as.data.frame(summary(model)$coefficients)
  conf_int <- as.data.frame(confint(model))  # Extract confidence intervals
  tbl$Variable <- rownames(tbl)
  rownames(tbl) <- NULL
  tbl <- cbind(tbl, conf_int)  # Combine coefficients and confidence intervals
  tbl <- tbl[, c("Variable", "exp(coef)", "2.5 %", "97.5 %", "coef", "se(coef)", "z", "Pr(>|z|)")]  # Reorder columns
  tbl_flex <- flextable(tbl)
  tbl_flex <- set_header_labels(tbl_flex, `exp(coef)` = "Hazard Ratio", `2.5 %` = "CI Lower", `97.5 %` = "CI Upper",
                                coef = "Coefficient", se.coef. = "Standard Error", z = "Z-value", `Pr(>|z|)` = "p-value", Variable = "Variable")
  tbl_flex <- add_header_row(tbl_flex, values = title, colwidths = 8)  # Adjust column width for confidence intervals
  return(tbl_flex)
}

# Create the word document
doc <- read_docx()
doc <- body_add_flextable(doc, make_flextable(cox_short_term_clinical, "Short-term Recurrence (Clinical Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_short_term_se, "Short-term Recurrence (Socioeconomic/Environmental Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_long_term_clinical, "Long-term Recurrence (Clinical Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_long_term_se, "Long-term Recurrence (Socioeconomic/Environmental Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_os_clinical, "Overall Survival (Clinical Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_os_se, "Overall Survival (Socioeconomic/Environmental Variables)"))

print(doc, target = "/Users/omniaabdelrahman/Desktop/CoxS.docx")


#COX FOR MODERATE PM2.5
library(survival)
library(survminer)
library(car)
library(officer)
library(flextable)

my_data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")
moderate_pm_data <- subset(my_data, pm_cat == 1)

# Set references in variables that might be problematic 

moderate_pm_data$SES <- factor(ifelse(moderate_pm_data$SES == 1, 2, moderate_pm_data$SES)) 
moderate_pm_data$SES <- relevel(factor(moderate_pm_data$SES), ref = 2) 
moderate_pm_data$rx <- factor(ifelse(moderate_pm_data$rx == 2, 3, moderate_pm_data$rx)) 
moderate_pm_data$rx <- relevel(factor(moderate_pm_data$rx), ref = 1)
moderate_pm_data$clm <- factor(ifelse(moderate_pm_data$clm %in% c(1, 2, 3), 3, moderate_pm_data$clm))
moderate_pm_data$clm <- relevel(factor(moderate_pm_data$clm), ref = "0")
moderate_pm_data <- subset(moderate_pm_data, ms_cat != 0)
moderate_pm_data$ms_cat <- relevel(factor(moderate_pm_data$ms_cat), ref = 2)
moderate_pm_data$eclo1 <- relevel(factor(moderate_pm_data$eclo1), ref = 1)
moderate_pm_data$smk_cat <- relevel(factor(moderate_pm_data$smk_cat), ref = 1)
moderate_pm_data$HPV <- relevel(factor(moderate_pm_data$HPV), ref = 2)
moderate_pm_data$epao <- relevel(factor(moderate_pm_data$epao), ref = 1)

# Cox models for short-term recurrence (within 36 months)
cox_short_term_clinical <- coxph(Surv(ttr, rec) ~ COPD + priorhnc + ms_cat + clm + rx + epao + eclo1, 
                                 data = moderate_pm_data, subset = (ttr <= 36))
cox_short_term_se <- coxph(Surv(ttr, rec) ~ age + SES + HPV + smk_cat, 
                           data = moderate_pm_data, subset = (ttr <= 36))
# Cox models for long-term recurrence (beyond 36 months)
cox_long_term_clinical <- coxph(Surv(ttr, rec) ~ COPD + priorhnc + ms_cat + clm + rx + epao + eclo1, 
                                data = moderate_pm_data, subset = (ttr > 36))
cox_long_term_se <- coxph(Surv(ttr, rec) ~ age + SES + HPV + smk_cat, 
                          data = moderate_pm_data, subset = (ttr > 36))
# Cox model for overall survival (using TTD)
cox_os_clinical <- coxph(Surv(ttd, death) ~ COPD + priorhnc + ms_cat + clm + rx + epao + eclo1, 
                         data = moderate_pm_data)
cox_os_se <- coxph(Surv(ttd, death) ~ age + SES + HPV + smk_cat, 
                   data = moderate_pm_data)

# a function to convert the model summary into a flextable
make_flextable <- function(model, title) {
  tbl <- as.data.frame(summary(model)$coefficients)
  conf_int <- as.data.frame(confint(model))  # Extract confidence intervals
  tbl$Variable <- rownames(tbl)
  rownames(tbl) <- NULL
  tbl <- cbind(tbl, conf_int)  # Combine coefficients and confidence intervals
  tbl <- tbl[, c("Variable", "exp(coef)", "2.5 %", "97.5 %", "coef", "se(coef)", "z", "Pr(>|z|)")]  # Reorder columns
  tbl_flex <- flextable(tbl)
  tbl_flex <- set_header_labels(tbl_flex, `exp(coef)` = "Hazard Ratio", `2.5 %` = "CI Lower", `97.5 %` = "CI Upper",
                                coef = "Coefficient", se.coef. = "Standard Error", z = "Z-value", `Pr(>|z|)` = "p-value", Variable = "Variable")
  tbl_flex <- add_header_row(tbl_flex, values = title, colwidths = 8)  # Adjust column width for confidence intervals
  return(tbl_flex)
}

# Create the word document
doc <- read_docx()
doc <- body_add_flextable(doc, make_flextable(cox_short_term_clinical, "Short-term Recurrence (Clinical Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_short_term_se, "Short-term Recurrence (Socioeconomic/Environmental Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_long_term_clinical, "Long-term Recurrence (Clinical Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_long_term_se, "Long-term Recurrence (Socioeconomic/Environmental Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_os_clinical, "Overall Survival (Clinical Variables)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_os_se, "Overall Survival (Socioeconomic/Environmental Variables)"))

print(doc, target = "/Users/omniaabdelrahman/Desktop/CoxS.docx") 


#OVERALL SURVIVAL OF DIFFERNET PREDICTORS IN A SUBSET
# Load necessary libraries
library(survival)
library(survminer)

# Subset data to only include moderate PM2.5 exposure (pm_cat == 1)
moderate_pm_data <- subset(my_data, pm_cat == 1)

# Kaplan-Meier for SES (Survival Outcome in Moderate PM Group)
km_ses <- survfit(Surv(ttd, death) ~ SES, data = moderate_pm_data)
ggsurvplot(km_ses, data = moderate_pm_data, 
           pval = TRUE, 
           conf.int = FALSE,  
           risk.table = TRUE, 
           xlab = "Time (months)", 
           ylab = "Overall Survival Probability",
           title = "Kaplan-Meier Curve for SES (Moderate PM2.5 Group)",
           legend.labs = c("Low (Medicaid, MHI < $30k)", "Moderate (Medicare, MHI $30k - $60k)", "High (BCBS, MHI > $60k)"),
           legend.title = "SES")

# Kaplan-Meier for Smoking Status (Survival Outcome in Moderate PM Group)
km_smoking <- survfit(Surv(ttd, death) ~ smk_cat, data = moderate_pm_data)
ggsurvplot(km_smoking, data = moderate_pm_data, 
           pval = TRUE, 
           conf.int = FALSE,  
           risk.table = TRUE, 
           xlab = "Time (months)", 
           ylab = "Overall Survival Probability",
           title = "Kaplan-Meier Curve for Smoking Status (Moderate PM2.5 Group)",
           legend.labs = c("Never (pack-years = 0)", "Light (pack-years ≤ 10 or former)", "Heavy/Current (pack-years > 10)"),
           legend.title = "Smoking Status")

# Kaplan-Meier for Margin Status (Survival Outcome in Moderate PM Group)
km_margin <- survfit(Surv(ttd, death) ~ ms_cat, data = moderate_pm_data)
ggsurvplot(km_margin, data = moderate_pm_data, 
           pval = TRUE, 
           conf.int = FALSE,  
           risk.table = TRUE, 
           xlab = "Time (months)", 
           ylab = "Overall Survival Probability",
           title = "Kaplan-Meier Curve for Margin Status (Moderate PM2.5 Group)",
           legend.labs = c("Not assessed", "Positive (involved margin, carcinoma < 2mm)", "Negative (no carcinoma within 2mm)"),
           legend.title = "Margin Status")

# Kaplan-Meier for Closest Margin (clm) in Moderate PM Group
km_clm <- survfit(Surv(ttd, death) ~ clm, data = moderate_pm_data)
ggsurvplot(km_clm, data = moderate_pm_data, 
           pval = TRUE, 
           conf.int = FALSE,  
           risk.table = TRUE, 
           xlab = "Time (months)", 
           ylab = "Overall Survival Probability",
           title = "Kaplan-Meier Curve for Closest Margin (Moderate PM2.5 Group)",
           legend.labs = c("Unspecified", "Mucosal", "Deep", "Both mucosal and deep"),
           legend.title = "Closest Margin")

# Kaplan-Meier for epao in Moderate PM Group
km_epao <- survfit(Surv(ttd, death) ~ epao, data = moderate_pm_data)
ggsurvplot(km_epao, data = moderate_pm_data, 
           pval = TRUE, 
           conf.int = FALSE,  
           risk.table = TRUE, 
           xlab = "Time (months)", 
           ylab = "Overall Survival Probability",
           title = "Kaplan-Meier Curve for Epao (Moderate PM2.5 Group)",
           legend.labs = c("T0-T1-T2, N0-N1 M0 I", "T0-T1-T2, N2 M0 II", "T3-T4, N2 M0 III"),
           legend.title = "Epao") 


#INTERACTION CODE
library(survival)
library(flextable)
my_data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")
data <- my_data  
my_data$pm_cat <- relevel(factor(my_data$pm_cat), ref = "0")   
my_data$no2_cat <- relevel(factor(my_data$no2_cat), ref = "0")  
my_data$voc_cat <- relevel(factor(my_data$voc_cat), ref = "0")  
my_data$aqi_cat <- relevel(factor(my_data$aqi_cat), ref = "1")  
my_data$SES <- factor(ifelse(my_data$SES == 1, 2, my_data$SES)) 
my_data$SES <- relevel(factor(my_data$SES), ref = 2) 
my_data$rx <- relevel(factor(my_data$rx), ref = 1)
my_data$clm <- factor(ifelse(my_data$clm %in% c(1, 2, 3), 3, my_data$clm))
my_data$clm <- relevel(factor(my_data$clm), ref = "0")
my_data <- subset(my_data, ms_cat != 0)
my_data$ms_cat <- relevel(factor(my_data$ms_cat), ref = 2)
my_data$eclo1 <- relevel(factor(my_data$eclo1), ref = 1)
my_data$smk_cat <- relevel(factor(my_data$smk_cat), ref = 1)
my_data$HPV <- relevel(factor(my_data$HPV), ref = 2)
my_data$epao <- relevel(factor(my_data$epao), ref = 1)

# Define the Cox models with interaction terms for OS (Overall Survival)
cox_pm25_interaction_os <- coxph(Surv(ttd, death) ~ w_av_pm * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)
cox_no2_interaction_os <- coxph(Surv(ttd, death) ~ w_av_no2 * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)
cox_voc_interaction_os <- coxph(Surv(ttd, death) ~ w_av_voc * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)
cox_aqi_interaction_os <- coxph(Surv(ttd, death) ~ w_av_aqi * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)

# Define the Cox models with interaction terms for RFS (Recurrence-Free Survival)
cox_pm25_interaction_rfs <- coxph(Surv(ttr, rec) ~ w_av_pm * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)
cox_no2_interaction_rfs <- coxph(Surv(ttr, rec) ~ w_av_no2 * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)
cox_voc_interaction_rfs <- coxph(Surv(ttr, rec) ~ w_av_voc * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)
cox_aqi_interaction_rfs <- coxph(Surv(ttr, rec) ~ w_av_aqi * smk_cat + age + SES + ms_cat + clm + rx + COPD + epao, data = data)

# Function to convert model summary into a flextable with confidence intervals and p-values
make_flextable <- function(model, title) {
  tbl <- as.data.frame(summary(model)$coefficients)
  conf_int <- as.data.frame(confint(model))  # Extract confidence intervals
  tbl$Variable <- rownames(tbl)
  rownames(tbl) <- NULL
  tbl <- cbind(tbl, conf_int)  # Combine coefficients and confidence intervals
  tbl <- tbl[, c("Variable", "exp(coef)", "2.5 %", "97.5 %", "se(coef)", "z", "Pr(>|z|)")]
  colnames(tbl) <- c("Variable", "Hazard Ratio", "CI Lower", "CI Upper", "Standard Error", "Z-value", "p-value")
  tbl_flex <- flextable(tbl)
  tbl_flex <- set_header_labels(tbl_flex, 
                                `exp(coef)` = "Hazard Ratio", 
                                `2.5 %` = "CI Lower", 
                                `97.5 %` = "CI Upper", 
                                `se(coef)` = "Standard Error", 
                                `Pr(>|z|)` = "p-value", 
                                Variable = "Variable")
  tbl_flex <- add_header_row(tbl_flex, values = title, colwidths = 7)
  return(tbl_flex)
}

# Create the tables for each environmental factor and adjusted models with interaction
doc <- read_docx()
doc <- body_add_flextable(doc, make_flextable(cox_pm25_interaction_os, "PM2.5 Interaction Model (OS)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_no2_interaction_os, "NO2 Interaction Model (OS)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_voc_interaction_os, "VOC Interaction Model (OS)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_aqi_interaction_os, "AQI Interaction Model (OS)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_pm25_interaction_rfs, "PM2.5 Interaction Model (RFS)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_no2_interaction_rfs, "NO2 Interaction Model (RFS)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_voc_interaction_rfs, "VOC Interaction Model (RFS)"))
doc <- body_add_par(doc, " ")
doc <- body_add_flextable(doc, make_flextable(cox_aqi_interaction_rfs, "AQI Interaction Model (RFS)"))

print(doc, target = "/Users/omniaabdelrahman/Desktop/Cox_Models_Interaction_Smoking_AirPollution_OS_RFS.docx")

#KMs of all interactions
library(survival)
library(survminer)  

# Define the Surv object for OS (Overall Survival) and RFS (Recurrence-Free Survival)
os_surv <- Surv(time = data$ttd, event = data$death)
rfs_surv <- Surv(time = data$ttr, event = data$rec)

# Kaplan-Meier for pm_cat and smoking interaction (OS) with adjusted risk table
km_fit_pm_os <- survfit(os_surv ~ smk_cat + pm_cat, data = data)
ggsurvplot(km_fit_pm_os, data = data, conf.int = FALSE, pval = TRUE, risk.table = TRUE,
           title = "Kaplan-Meier Curve for OS: Smoking & PM2.5 Category Interaction",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.labs = c("Never, Low PM2.5", "Never, Moderate PM2.5", 
                           "Light, Low PM2.5", "Light, Moderate PM2.5", 
                           "Heavy, Low PM2.5", "Heavy, Moderate PM2.5"),
           legend.title = "Smoking & PM2.5 Categories",
           tables.height = 0.3,             
           risk.table.fontsize = 4,  
           tables.theme = theme_survminer(font.tickslab = 12))  

# Kaplan-Meier for pm_cat and smoking interaction (RFS) with adjusted risk table
km_fit_pm_rfs <- survfit(rfs_surv ~ smk_cat + pm_cat, data = data)
ggsurvplot(km_fit_pm_rfs, data = data, conf.int = FALSE, pval = TRUE, risk.table = TRUE,
           title = "Kaplan-Meier Curve for RFS: Smoking & PM2.5 Category Interaction",
           xlab = "Time (Months)", ylab = "Recurrence-Free Survival Probability",
           legend.labs = c("Never, Low PM2.5", "Never, Moderate PM2.5", 
                           "Light, Low PM2.5", "Light, Moderate PM2.5", 
                           "Heavy, Low PM2.5", "Heavy, Moderate PM2.5"),
           legend.title = "Smoking & PM2.5 Categories",
           tables.height = 0.3,  
           risk.table.fontsize = 4,  
           tables.theme = theme_survminer(font.tickslab = 12))  

# Kaplan-Meier for no2_cat and smoking interaction (OS) with adjusted risk table
km_fit_no2_os <- survfit(os_surv ~ smk_cat + no2_cat, data = data)
ggsurvplot(km_fit_no2_os, data = data, conf.int = FALSE, pval = TRUE, risk.table = TRUE,
           title = "Kaplan-Meier Curve for OS: Smoking & NO2 Category Interaction",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.labs = c("Never, Low NO2", "Never, Moderate NO2",
                           "Light, Low NO2", "Light, Moderate NO2",
                           "Heavy, Low NO2", "Heavy, Moderate NO2"),
           legend.title = "Smoking & NO2 Categories",
           tables.height = 0.3,  
           risk.table.fontsize = 4,  
           tables.theme = theme_survminer(font.tickslab = 12))  

# Kaplan-Meier for voc_cat and smoking interaction (OS) with adjusted risk table
km_fit_voc_os <- survfit(os_surv ~ smk_cat + voc_cat, data = data)
ggsurvplot(km_fit_voc_os, data = data, conf.int = FALSE, pval = TRUE, risk.table = TRUE,
           title = "Kaplan-Meier Curve for OS: Smoking & VOC Category Interaction",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.labs = c("Never, Low VOC", "Never, Moderate VOC", "Never, High VOC",
                           "Light, Low VOC", "Light, Moderate VOC", "Light, High VOC",
                           "Heavy, Low VOC", "Heavy, Moderate VOC", "Heavy, High VOC"),
           legend.title = "Smoking & VOC Categories",
           tables.height = 0.3,  
           risk.table.fontsize = 4,  
           tables.theme = theme_survminer(font.tickslab = 12))  

# Kaplan-Meier for aqi_cat and smoking interaction (OS) with adjusted risk table
km_fit_aqi_os <- survfit(os_surv ~ smk_cat + aqi_cat, data = data)
ggsurvplot(km_fit_aqi_os, data = data, conf.int = FALSE, pval = TRUE, risk.table = TRUE,
           title = "Kaplan-Meier Curve for OS: Smoking & AQI Category Interaction",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.labs = c("Never, Moderate AQI", "Never, Unhealthy AQI",
                           "Light, Moderate AQI", "Light, Unhealthy AQI",
                           "Heavy, Moderate AQI", "Heavy, Unhealthy AQI"),
           legend.title = "Smoking & AQI Categories",
           tables.height = 0.3,  
           risk.table.fontsize = 4,  
           tables.theme = theme_survminer(font.tickslab = 12))  

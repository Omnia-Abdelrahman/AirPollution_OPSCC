library(glmnet)
library(survival)
my_data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

# Ensure x_cat is a factor and set the reference group
# Relevel categorical variables with numerical codes
my_data$sex <- relevel(factor(my_data$sex), ref = 1)  # Male = 1
my_data$SES <- factor(ifelse(my_data$SES == 1, 2, my_data$SES)) 
my_data$SES <- relevel(factor(my_data$SES), ref = 1)  # Low SES = 1
my_data$smk_cat <- relevel(factor(my_data$smk_cat), ref = 1)  # Non-smoker = 1
my_data$HPV <- relevel(factor(my_data$HPV), ref = 2)  # Negative = 2
my_data$rx <- relevel(factor(my_data$rx), ref = 1)  # surgery without adjuvant = 1
my_data$cci <- relevel(factor(my_data$cci), ref = "0")  # CCI- first level = 0
my_data$MI <- relevel(factor(my_data$MI), ref = "0")  # No MI = 0
my_data$COPD <- relevel(factor(my_data$COPD), ref = "0")  # No COPD = 0
my_data$other_t <- relevel(factor(my_data$other_t), ref = "0")  # No history of tumors = 0
my_data$DM <- relevel(factor(my_data$DM), ref = "0")  # No DM = 0
my_data$priorhnc <- relevel(factor(my_data$priorhnc), ref = "0")  # No history of HNC = 0
my_data$eclo1 <- relevel(factor(my_data$eclo1), ref = 1)  # Clinical TNM stage1
my_data <- subset(my_data, ms_cat != 0)
my_data$ms_cat <- relevel(factor(my_data$ms_cat), ref = 2)  # negative margin status = 0
my_data$clm <- factor(ifelse(my_data$clm %in% c(1, 2, 3), 3, my_data$clm))
my_data$clm <- relevel(factor(my_data$clm), ref = "0")  # no deep or mucosal margin type = 0
my_data$lvi_2 <- relevel(factor(my_data$lvi_2), ref = "0")  # No lymphatic invasion = 0
my_data$pni <- relevel(factor(my_data$pni), ref = "0")  # No nodal invasion = 0
my_data$ece <- relevel(factor(my_data$ece), ref = "0")  # No extra capsular extension = 0
my_data$epao <- relevel(factor(my_data$epao), ref = 1)  # Pathologic TNM istage1
my_data$rec <- relevel(factor(my_data$rec), ref = "0")  # No recurrence = 0
my_data$death <- relevel(factor(my_data$death), ref = "0")  # Alive = 0
my_data$survival <- relevel(factor(my_data$survival), ref = "1")  # 0 = Dead
my_data$pm_cat <- relevel(factor(my_data$pm_cat), ref = "0")  # Low exposure = 1
my_data$no2_cat <- relevel(factor(my_data$no2_cat), ref = "0")  # Low exposure = 1
my_data$voc_cat <- relevel(factor(my_data$voc_cat), ref = "0")  # Low exposure = 1
my_data$aqi_cat <- relevel(factor(my_data$aqi_cat), ref = "1")  # Moderate = 1


summary(my_data)


# THEN Remove rows with missing values in the SPECIFIC necessary variables
complete_data <- my_data[complete.cases(my_data[, c("ttr", "rec", "ms_cat", "epao", "eclo1", "cci", "rx", 
                                                    "w_av_pm", "w_av_no2", "w_av_voc", "w_av_aqi", "age", 
                                                    "sex", "SES", "smk_cat", "HPV", "MI", "COPD", 
                                                    "other_t", "DM", "priorhnc", "clm", "lvi_2", "pni", 
                                                    "ece")]), ]

# Do this first you will thank me later
summary(complete_data$ttr)
# Remove rows with non-positive event times
complete_data <- complete_data[complete_data$ttr > 0, ]
# Do this first you will thank me later
summary(complete_data$ttd)
# Remove rows with non-positive event times
complete_data <- complete_data[complete_data$ttd > 0, ]


# GENERAL USE x / y / fit 
X_ <- model.matrix(Surv(ttr, rec) ~ ms_cat + epao + eclo1 + cci + rx + w_av_pm + w_av_no2 + w_av_voc + w_av_aqi + age +
                     sex + SES + smk_cat + HPV + MI + COPD + other_t +
                     DM + priorhnc + clm + lvi_2 + pni + ece, data = complete_data)[, -1]
y_ <- Surv(complete_data$ttr, complete_data$rec == 1)
cv.lasso_* <- cv.glmnet(X, y, family = "cox", alpha = 1)


#double check not necessary unless error
dim(X_clinic)
length(y_clinic)


# Again X / Y / Fit
x_clinic <- model.matrix(Surv(ttr, rec) ~ ms_cat + epao + eclo1 + cci + rx + HPV + MI + COPD + other_t 
                         + pm_cat + no2_cat + voc_cat + aqi_cat+ DM + priorhnc + clm + lvi_2 + pni + ece, data = complete_data)[, -1]
y_clinic <- Surv(complete_data$ttr, complete_data$rec == 1)
cv.lasso_clinical <- cv.glmnet(x_clinic, y_clinic, family = "cox", alpha = 1)


w_av_pm + w_av_no2 + w_av_voc + w_av_aqi + age +
  +                        sex + SES + smk_cat + pm_cat + no2_cat + voc_cat + aqi_cat

# Extract the best lambda
best_lambda_clinical <- cv.lasso_clinical$lambda.min
print(best_lambda_clinical)

# Get the coefficients for the best lambda
coef(cv.lasso_clinical, s = "lambda.min")
plot(cv.lasso_clinical)

# IN CASE IT SAYS IT DOES NOT FIT Adjust margins
par(mar = c(5, 4, 4, 2) + 0.1) 
plot(cv.lasso_clinical)

# Reset graphics device
dev.off()
plot(cv.lasso_clinical)

# Save plot as a PNG file
png("cv_lasso_clinical_plot.png")
plot(cv.lasso_clinical)
dev.off()
# Get the coefficients for the best lambda
coefficients <- coef(cv.lasso_clinical, s = "lambda.min")
print(coefficients)

# Calculate hazard ratios (HR = exp(coefficient))
coefficients <- coef(cv.lasso_clinical, s = "lambda.min")
coefficients_matrix <- as.matrix(coefficients)
non_zero_coefs <- coefficients_matrix[coefficients_matrix != 0, ]
hazard_ratios <- exp(non_zero_coefs)
hr_table <- data.frame(Variable = rownames(coefficients_matrix)[coefficients_matrix != 0],
                       Coefficient = non_zero_coefs,
                       Hazard_Ratio = hazard_ratios)
print(hr_table)
#let’s get the fun out out of this plot
library(glmnet)
library(ggplot2)
library(tidyr)
library(dplyr)

lasso_coefs <- as.matrix(coef(cv.lasso_clinical$glmnet.fit))
lambda_values <- cv.lasso_clinical$glmnet.fit$lambda

# Coefficients should match the number of variables and lambdas
coef_df <- as.data.frame(t(lasso_coefs))  # Transpose the coefficients
coef_df$lambda <- lambda_values  # Add lambda values

# Create a named vector for the descriptive names based on your variable list
# Complete named vector for all variables, including updated CCI levels
descriptive_names <- c(
  "sex" = "Male",
  "sex2" = "Female",
  "age" = "Age",
  "SES" = "Low/Moderate SES",
  "SES3" = "High SES",
  "smk_cat" = "Non-smoker",
  "smk_cat2" = "Light Smoker",
  "smk_cat3" = "Heavy or Current Smoker",
  "HPV" = "HPV Negative",
  "HPV1" = "HPV Positive",
  "rx" = "Surgery Without Adjuvant",
  "rx2" = "Surgery and Radiotherapy",
  "rx3" = "Surgery and Chemoradiotherapy",
  "cci1" = "Charlson Comorbidity Index Level 1",
  "cci2" = "Charlson Comorbidity Index Level 2",
  "cci3" = "Charlson Comorbidity Index Level 3",
  "MI" = "No Myocardial Infarction",
  "MI1" = "Yes Myocardial Infarction",
  "COPD" = "No COPD",
  "COPD1" = "COPD",
  "other_t" = "No Other Tumors",
  "other_t1" = "History of Other Tumors",
  "DM" = "No Diabetes Mellitus",
  "DM1" = "Diabetes Mellitus",
  "priorhnc" = "No history of Head and Neck Cancer",
  "priorhnc1" = "Prior Head and Neck Cancer",
  "eclo12" = "Clinical TNM Stage 2",
  "eclo13" = "Clinical TNM Stage 3",
  "ms_cat1" = "Positive Margin",
  "clm" = "Closest Margin Type (No Deep or Mucosal Involvement)",
  "clm1" = "Mucosal Margin",
  "clm2" = "Deep Margin",
  "clm3" = "Margin Mucosal and Deep",
  "lvi_21" = "No Lymphatic Invasion",
  "lvi_22" = "Lymphatic Invasion",
  "pni1" = "Perineural Invasion (Yes)",
  "pni2" = "Perineural Invasion (No)",
  "ece1" = "Extra Capsular Extension",
  "epao1" = "Pathologic TNM Stage 1",
  "epao2" = "Pathologic TNM Stage 2",
  "epao3" = "Pathologic TNM Stage 3",
  "rec" = "No Recurrence",
  "death" = "Alive",
  "pm_cat" = "Low PM Exposure",
  "pm_cat1" = "Moderate PM Exposure",
  "no2_cat" = "Low NO2 Exposure",
  "no2_cat1" = "Moderate NO2 Exposure",
  "voc_cat" = "Low VOC Exposure",
  "voc_cat1" = "Moderate VOC Exposure",
  "voc_cat2" = "High VOC Exposure",
  "aqi_cat" = "Moderate AQI Exposure",
  "aqi_cat2" = "Moderate AQI Exposure",
  "w_av_pm" = "Average Conc PM2.5 µg/m3",
  "w_av_no2" = "Average Conc NO2 PPb",
  "w_av_voc" = "Average Conc VOC PPbC",
  "w_av_aqi" = "Average AQI",
  
  "aqi_cat1" = "Unhealthy AQI Exposure"
)

# Now proceed with renaming the variables in your LASSO plot using these descriptive names:
coef_long <- coef_df %>%
  gather(key = "variable", value = "coefficient", -lambda) %>%
  mutate(variable = recode(variable, !!!descriptive_names))

# Create the plot with the labels positioned at the bottom
ggplot(coef_long, aes(x = lambda, y = coefficient, color = variable)) +
  geom_line() +
  scale_x_log10() +  # Log scale for lambda
  labs(
    title = "LASSO Coefficient Paths for Clinical Variables", 
    x = "Lambda (Log Scale)", 
    y = "Coefficient"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",      # Move legend to the bottom
    legend.title = element_blank(),  # Optional: remove "variable" title in legend
    legend.text = element_text(size = 7)
  )







library(survival)   # For survival analysis
library(survminer)  # For Kaplan-Meier plots
data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

# ------------------ Overall Survival (OS) ------------------

# COPD for OS
km_copd_os <- survfit(Surv(ttd, death) ~ COPD, data = data)
ggsurvplot(km_copd_os, data = data, pval = TRUE, risk.table = TRUE, 
           title = "Kaplan-Meier Curve for Overall Survival (COPD)",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.title = "COPD Status", legend.labs = c("No COPD", "COPD"))

# SES for OS
km_ses_os <- survfit(Surv(ttd, death) ~ SES, data = data)
ggsurvplot(km_ses_os, data = data, pval = TRUE, risk.table = TRUE, 
           title = "Kaplan-Meier Curve for Overall Survival (SES)",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.title = "SES", legend.labs = c("Low", "Moderate", "High"))

# Smoking Status (smk_cat) for OS
km_smoking_os <- survfit(Surv(ttd, death) ~ smk_cat, data = data)
ggsurvplot(km_smoking_os, data = data, pval = TRUE, risk.table = TRUE, 
           title = "Kaplan-Meier Curve for Overall Survival (Smoking Status)",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.title = "Smoking Status", legend.labs = c("Never", "Light", "Heavy"))

# ------------------ Recurrence-Free Survival (RFS) ------------------

# COPD for RFS
km_copd_rfs <- survfit(Surv(ttd, rec) ~ COPD, data = data)
ggsurvplot(km_copd_rfs, data = data, pval = TRUE, risk.table = TRUE, 
           title = "Kaplan-Meier Curve for Recurrence-Free Survival (COPD)",
           xlab = "Time (Months)", ylab = "Recurrence-Free Survival Probability",
           legend.title = "COPD Status", legend.labs = c("No COPD", "COPD"))

# SES for RFS
km_ses_rfs <- survfit(Surv(ttd, rec) ~ SES, data = data)
ggsurvplot(km_ses_rfs, data = data, pval = TRUE, risk.table = TRUE, 
           title = "Kaplan-Meier Curve for Recurrence-Free Survival (SES)",
           xlab = "Time (Months)", ylab = "Recurrence-Free Survival Probability",
           legend.title = "SES", legend.labs = c("Low", "Moderate", "High"))

# Smoking Status (smk_cat) for RFS
km_smoking_rfs <- survfit(Surv(ttd, rec) ~ smk_cat, data = data)
ggsurvplot(km_smoking_rfs, data = data, pval = TRUE, risk.table = TRUE, 
           title = "Kaplan-Meier Curve for Recurrence-Free Survival (Smoking Status)",
           xlab = "Time (Months)", ylab = "Recurrence-Free Survival Probability",
           legend.title = "Smoking Status", legend.labs = c("Never", "Light", "Heavy"))

library(dplyr)
library(ggplot2)
my_data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

#Counting pts
state_counts <- my_data %>%
  group_by(state) %>%
  summarise(count = n())

#assigning
my_data$region <- case_when(
  
  my_data$state %in% c("Pennsylvania", "New Jersey", "Delaware") & my_data$state %in% state_counts$state[state_counts$count > 5] ~ "Mid-Atlantic Region",
  
  my_data$state %in% c("Florida") & my_data$state %in% state_counts$state[state_counts$count > 5] ~ "Southern Coastal Region",
  
  # Dispersed Population for states with 5 or fewer patients or not in the above regions
  TRUE ~ "Dispersed Population"
)

ggplot(my_data, aes(x = lng, y = lat, color = factor(region))) +
  geom_point() +
  labs(color = 'Region') +
  theme_minimal()

write.csv(my_data, "/Users/omniaabdelrahman/Desktop/Book1R_clusters.csv", row.names = FALSE)




#PART6  
library(survival)
library(survminer)
my_data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

my_data$rgn <- factor(my_data$rgn, levels = c(1, 2, 3), 
                      labels = c("Mid-Atlantic", "Southern Coastal", "Dispersed"))

my_data$pm_cat <- factor(my_data$pm_cat, levels = c(0, 1), 
                         labels = c("Low PM2.5", "Moderate PM2.5"))

### Kaplan-Meier Analysis ###

# OS
os_surv <- Surv(time = my_data$ttd, event = my_data$death)
km_fit_rgn_pm_os <- survfit(os_surv ~ rgn + pm_cat, data = my_data)

# Plot 
ggsurvplot(km_fit_rgn_pm_os, data = my_data, conf.int = FALSE, pval = TRUE, risk.table = TRUE,
           title = "Kaplan-Meier Curve for OS: Region & PM2.5 Category Interaction",
           xlab = "Time (Months)", ylab = "Overall Survival Probability",
           legend.labs = c("Mid-Atlantic, Low PM2.5", "Mid-Atlantic, Moderate PM2.5", 
                           "Southern Coastal, Low PM2.5", "Southern Coastal, Moderate PM2.5", 
                           "Dispersed, Low PM2.5", "Dispersed, Moderate PM2.5"),
           legend.title = "Region & PM2.5 Categories",
           tables.height = 0.3,             
           risk.table.fontsize = 4,  
           tables.theme = theme_survminer(font.tickslab = 12))

#RFS
rfs_surv <- Surv(time = my_data$ttr, event = my_data$rec)
km_fit_rgn_pm_rfs <- survfit(rfs_surv ~ rgn + pm_cat, data = my_data)

ggsurvplot(km_fit_rgn_pm_rfs, data = my_data, conf.int = FALSE, pval = TRUE, risk.table = TRUE,
           title = "Kaplan-Meier Curve for RFS: Region & PM2.5 Category Interaction",
           xlab = "Time (Months)", ylab = "Recurrence-Free Survival Probability",
           legend.labs = c("Mid-Atlantic, Low PM2.5", "Mid-Atlantic, Moderate PM2.5", 
                           "Southern Coastal, Low PM2.5", "Southern Coastal, Moderate PM2.5", 
                           "Dispersed, Low PM2.5", "Dispersed, Moderate PM2.5"),
           legend.title = "Region & PM2.5 Categories",
           tables.height = 0.3,             
           risk.table.fontsize = 4,  
           tables.theme = theme_survminer(font.tickslab = 12))

ree Survival: Region & PM2.5 Interaction")





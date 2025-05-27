library(survival)
library(survminer)
data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")
custom_colors <- c("#FFB6C1", "#B19CD9")  
# Kaplan-Meier analysis for Overall Survival (OS)
km_os <- survfit(Surv(time = data$ttd, event = data$death) ~ pm_cat, data = data)
ggsurvplot(km_os, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE,  # No confidence interval shading
           palette = custom_colors,  
           legend.labs = c("Low PM2.5", "Moderate PM2.5")
           title = "Kaplan-Meier Curve for Overall Survival",
           xlab = "Time (Months)", 
           ylab = "Overall Survival Probability",
           xlim = c(0, 180))  # Limit the time to 15 years (180 months)
# Kaplan-Meier analysis for Recurrence-Free Survival (RFS)
km_rfs <- survfit(Surv(time = data$ttr, event = data$rec) ~ pm_cat, data = data)
ggsurvplot(km_rfs, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE,  
           palette = custom_colors
           legend.labs = c("Low PM2.5", "Moderate PM2.5"),  
           title = "Kaplan-Meier Curve for Recurrence-Free Survival",
           xlab = "Time (Months)", 
           ylab = "Recurrence-Free Survival Probability",
           xlim = c(0, 180))  # Limit the time to 15 years (180 months)
# log-rank test 
log_rank_os <- survdiff(Surv(time = data$ttd, event = data$death) ~ pm_cat, data = data)
print(log_rank_os)
log_rank_rfs <- survdiff(Surv(time = data$ttr, event = data$rec) ~ pm_cat, data = data)
print(log_rank_rfs)

#ALL OTHER KMs
library(survival)
library(survminer)
data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")
no2_colors <- c("#1E90FF", "#4682B4")  # Ocean blue (low), Royal blue (high)
voc_colors <- c("#FFD700", "#F08080", "#FF4500")  # Yellow (low), Light coral (moderate), Dark orange (high)
aqi_colors <- c("#98FB98", "#006400")  # Pastel green (low), Dark green (high)
# ----------------- NO2 -----------------
# Kaplan-Meier analysis for NO2 (Overall Survival)
km_os_no2 <- survfit(Surv(time = data$ttd, event = data$death) ~ no2_cat, data = data)
ggsurvplot(km_os_no2, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE, 
           palette = no2_colors,  
           legend.labs = c("Low NO2", "Moderate NO2"),  
           title = "Kaplan-Meier Curve for Overall Survival (NO2)",
           xlab = "Time (Months)", 
           ylab = "Overall Survival Probability",
           xlim = c(0, 180))
# Kaplan-Meier analysis for NO2 (Recurrence-Free Survival)
km_rfs_no2 <- survfit(Surv(time = data$ttr, event = data$rec) ~ no2_cat, data = data)
ggsurvplot(km_rfs_no2, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE,  
           palette = no2_colors,  
           legend.labs = c("Low NO2", "Moderate NO2"),  
           title = "Kaplan-Meier Curve for Recurrence-Free Survival (NO2)",
           xlab = "Time (Months)", 
           ylab = "Recurrence-Free Survival Probability",
           xlim = c(0, 180))
# ----------------- VOC -----------------
# Kaplan-Meier analysis for VOC (Overall Survival)
km_os_voc <- survfit(Surv(time = data$ttd, event = data$death) ~ voc_cat, data = data)
ggsurvplot(km_os_voc, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE,  
           palette = voc_colors,  
           legend.labs = c("Low VOC", "Moderate VOC", "High VOC"),  
           title = "Kaplan-Meier Curve for Overall Survival (VOC)",
           xlab = "Time (Months)", 
           ylab = "Overall Survival Probability",
           xlim = c(0, 180))
# Kaplan-Meier analysis for VOC (Recurrence-Free Survival)
km_rfs_voc <- survfit(Surv(time = data$ttr, event = data$rec) ~ voc_cat, data = data)
ggsurvplot(km_rfs_voc, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE,  
           palette = voc_colors,  
           legend.labs = c("Low VOC", "Moderate VOC", "High VOC"),  
           title = "Kaplan-Meier Curve for Recurrence-Free Survival (VOC)",
           xlab = "Time (Months)", 
           ylab = "Recurrence-Free Survival Probability",
           xlim = c(0, 180))
# ----------------- AQI -----------------
# Kaplan-Meier analysis for AQI (Overall Survival)
km_os_aqi <- survfit(Surv(time = data$ttd, event = data$death) ~ aqi_cat, data = data)
ggsurvplot(km_os_aqi, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE,  
           palette = aqi_colors,  
           legend.labs = c("Moderate AQI", "Unhealthy AQI"),  
           title = "Kaplan-Meier Curve for Overall Survival (AQI)",
           xlab = "Time (Months)", 
           ylab = "Overall Survival Probability",
           xlim = c(0, 180))
# Kaplan-Meier analysis for AQI (Recurrence-Free Survival)
km_rfs_aqi <- survfit(Surv(time = data$ttr, event = data$rec) ~ aqi_cat, data = data)
ggsurvplot(km_rfs_aqi, data = data, pval = TRUE, 
           risk.table = TRUE, 
           conf.int = FALSE,  
           palette = aqi_colors,  
           legend.labs = c("Moderate AQI", "Unhealthy AQI"),  
           title = "Kaplan-Meier Curve for Recurrence-Free Survival (AQI)",
           xlab = "Time (Months)", 
           ylab = "Recurrence-Free Survival Probability",
           xlim = c(0, 180))
# Log-rank tests for all variables
log_rank_no2 <- survdiff(Surv(time = data$ttd, event = data$death) ~ no2_cat, data = data)
print(log_rank_no2)
log_rank_voc <- survdiff(Surv(time = data$ttd, event = data$death) ~ voc_cat, data = data)
print(log_rank_voc)
log_rank_aqi <- survdiff(Surv(time = data$ttd, event = data$death) ~ aqi_cat, data = data)
print(log_rank_aqi)

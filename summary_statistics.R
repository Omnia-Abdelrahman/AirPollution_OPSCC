library(officer)
data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

# Continuous variables summary
continuous_summary <- data.frame(
  Variable = c("Age", "PM2.5", "NO2", "VOC", "AQI", "Time to Recurrence", "Time to Death"),
  Mean = c(mean(data$age, na.rm = TRUE),
           mean(data$w_av_pm, na.rm = TRUE),
           mean(data$w_av_no2, na.rm = TRUE),
           mean(data$w_av_voc, na.rm = TRUE),
           mean(data$w_av_aqi, na.rm = TRUE),
           mean(data$ttr, na.rm = TRUE),
           mean(data$ttd, na.rm = TRUE)),
  Median = c(median(data$age, na.rm = TRUE),
             median(data$w_av_pm, na.rm = TRUE),
             median(data$w_av_no2, na.rm = TRUE),
             median(data$w_av_voc, na.rm = TRUE),
             median(data$w_av_aqi, na.rm = TRUE),
             median(data$ttr, na.rm = TRUE),
             median(data$ttd, na.rm = TRUE)),
  IQR = c(IQR(data$age, na.rm = TRUE),
          IQR(data$w_av_pm, na.rm = TRUE),
          IQR(data$w_av_no2, na.rm = TRUE),
          IQR(data$w_av_voc, na.rm = TRUE),
          IQR(data$w_av_aqi, na.rm = TRUE),
          IQR(data$ttr, na.rm = TRUE),
          IQR(data$ttd, na.rm = TRUE))
)

# Categorical variables counts
categorical_summary <- data.frame(
  Variable = c("Sex", "Smoking", "HPV", "Socioeconomic Status", "Therapy Mode", "Charlson Comorbidities", "MI", 
               "COPD", "Other Tumors", "Diabetes", "Former Head & Neck Cancer", "AJCC 8th Stage", 
               "Margin Status", "Closest Margin Type", "Lymphovascular Invasion", "Perineural Invasion", 
               "Extra Capsular Extension", "Recurrence", "Death", "Survival"),
  Category = c("Male:Female", "Never:Light/Quit:Heavy/Current", "Positive:Others", "Low:Moderate:High", 
               "Surgery:Surg+Radio:CRT", "0:1:2:>=3", "No:Yes", "No:Yes", "No:Yes", "No:Yes", 
               "No:Yes", "1:2:3:4", "Not assessed:Positive:Negative", "Unspecified:Mucosal:Deep:Both", 
               "No:Yes:Not described", "No:Yes:Not described", "No:Yes", "No:Yes", "No:Yes", 
               "Dead:Alive:Lost to follow-up"),
  Count = c(paste(table(data$sex), collapse = ":"),
            paste(table(data$smk_cat), collapse = ":"),
            paste(table(data$HPV), collapse = ":"),
            paste(table(data$SES), collapse = ":"),
            paste(table(data$rx), collapse = ":"),
            paste(table(data$cci), collapse = ":"),
            paste(table(data$MI), collapse = ":"),
            paste(table(data$COPD), collapse = ":"),
            paste(table(data$other_t), collapse = ":"),
            paste(table(data$DM), collapse = ":"),
            paste(table(data$priorhnc), collapse = ":"),
            paste(table(data$eclo1), collapse = ":"),
            paste(table(data$ms_cat), collapse = ":"),
            paste(table(data$clm), collapse = ":"),
            paste(table(data$lvi_2), collapse = ":"),
            paste(table(data$pni), collapse = ":"),
            paste(table(data$ece), collapse = ":"),
            paste(table(data$rec), collapse = ":"),
            paste(table(data$death), collapse = ":"),
            paste(table(data$survival), collapse = ":"))
)

# Create a new Word document
doc <- read_docx()
# Add the continuous summary table
doc <- doc %>%
  body_add_par("Continuous Variables Summary", style = "heading 1") %>%
  body_add_table(continuous_summary, style = "table_template")
# Add the categorical summary table
doc <- doc %>%
  body_add_par("Categorical Variables Summary", style = "heading 1") %>%
  body_add_table(categorical_summary, style = "table_template")

print(doc, target = "/Users/omniaabdelrahman/Desktop/summary_statistics.docx")

#boxplots
library(ggplot2)
data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

boxplot(data$w_av_pm, main = "Boxplot of PM2.5", ylab = "PM2.5 (µg/m³)")
boxplot(data$w_av_no2, main = "Boxplot of NO2", ylab = "NO2 (ppb)")
boxplot(data$w_av_voc, main = "Boxplot of VOC", ylab = "VOC (ppb)")
boxplot(data$w_av_aqi, main = "Boxplot of AQI", ylab = "AQI")

plot(density(data$w_av_pm, na.rm = TRUE), main = "Density Plot of PM2.5", xlab = "PM2.5 (µg/m³)")
plot(density(data$w_av_no2, na.rm = TRUE), main = "Density Plot of NO2", xlab = "NO2 (ppb)")
plot(density(data$w_av_voc, na.rm = TRUE), main = "Density Plot of VOC", xlab = "VOC (ppb)")
plot(density(data$w_av_aqi, na.rm = TRUE), main = "Density Plot of AQI", xlab = "AQI")
#Correlation Matrix
library(corrplot)
data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

continuous_vars <- data[, c("age", "SES", "smk_cat", "HPV", "rx", "cci", "MI", "COPD", 
                            "other_t", "DM", "priorhnc", "eclo1", "ms_cat", "clm", "lvi_2", 
                            "pni", "ece", "epao", "ttr", "rec", "ttd", "death", "survival", 
                            "w_av_pm", "w_av_no2", "w_av_voc", "w_av_aqi", "pm_cat", 
                            "no2_cat", "voc_cat", "aqi")]

cor_matrix <- cor(continuous_vars, use = "complete.obs")

# Define the custom color palette to stop at pastel purple
custom_colors <- colorRampPalette(c("#FFB6C1", "#B0E0E6", "#B19CD9"))(200)  # Pastel red to pastel purple
corrplot(cor_matrix, method = "circle", type = "lower", tl.cex = 0.8, diag = FALSE, 
         col = custom_colors, cl.pos = "r", cl.length = 5)


#corrected code for the correlation matrix # Load necessary libraries
library(corrplot)
data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

continuous_vars <- data[, c("w_av_pm", "w_av_no2", "w_av_voc", "w_av_aqi", "ttr", "ttd", "age")]

# Pearson correlation for continuous vars
cor_matrix <- cor(continuous_vars, use = "complete.obs")

descriptive_names <- c(
  "w_av_pm" = "Avg PM2.5 Conc.",
  "w_av_no2" = "Avg NO2 Conc.",
  "w_av_voc" = "Avg VOC Conc.",
  "w_av_aqi" = "Avg AQI",
  "ttr" = "Time to Recurrence",
  "ttd" = "Time to Death",
  "age" = "Age"
)

colnames(cor_matrix) <- descriptive_names[colnames(cor_matrix)]
rownames(cor_matrix) <- descriptive_names[rownames(cor_matrix)]

custom_colors <- colorRampPalette(c("#FFB6C1", "#B0E0E6", "#B19CD9"))(200)  # Pastel red to pastel purple

#I use this as a standard
corrplot(cor_matrix, method = "circle", type = "lower", tl.cex = 0.7, diag = FALSE, 
         col = custom_colors, cl.pos = "b", cl.length = 5, tl.srt = 0, tl.col = "black", mar = c(2, 2, 2, 2))

# Cramer
library(corrplot)
library(vcd)

data <- read.csv("/Users/omniaabdelrahman/Desktop/Book1R.csv")

categorical_vars <- data[, c("sex", "SES", "smk_cat", "HPV", "rx", "cci", "MI", "COPD", 
                             "other_t", "DM", "priorhnc", "eclo1", "ms_cat", "clm", "lvi_2", 
                             "pni", "ece", "epao", "rec", "death", "survival", "pm_cat", 
                             "no2_cat", "voc_cat", "aqi_cat")]

descriptive_names <- c(
  "sex" = "Sex",
  "SES" = "Socioeconomic Status",
  "smk_cat" = "Smoking Category",
  "HPV" = "HPV Status",
  "rx" = "Treatment Type",
  "cci" = "Charlson Comorbidity Index",
  "MI" = "Myocardial Infarction",
  "COPD" = "COPD",
  "other_t" = "Other Tumors",
  "DM" = "Diabetes Mellitus",
  "priorhnc" = "Prior Head and Neck Cancer",
  "eclo1" = "Clinical TNM Stage",
  "ms_cat" = "Margin Status",
  "clm" = "Closest Margin Type",
  "lvi_2" = "Lymphatic Invasion",
  "pni" = "Perineural Invasion",
  "ece" = "Extra Capsular Extension",
  "epao" = "Pathologic TNM Stage",
  "rec" = "Recurrence Status",
  "death" = "Death Status",
  "survival" = "Survival Time",
  "pm_cat" = "PM2.5 Exposure Category",
  "no2_cat" = "NO2 Exposure Category",
  "voc_cat" = "VOC Exposure Category",
  "aqi_cat" = "AQI Exposure Category"
)

cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chisq <- chisq.test(tbl, correct = FALSE)$statistic
  n <- sum(tbl)
  min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
  v <- sqrt(chisq / (n * min_dim))
  return(v)
}

# Create a matrix to store the Cramér’s V values
n <- ncol(categorical_vars)
cramers_v_matrix <- matrix(NA, n, n)

# Fill in the matrix with Cramér’s V values
for (i in 1:n) {
  for (j in 1:n) {
    cramers_v_matrix[i, j] <- cramers_v(categorical_vars[, i], categorical_vars[, j])
  }
}

# Assign descriptive variable names to the matrix
colnames(cramers_v_matrix) <- descriptive_names
rownames(cramers_v_matrix) <- descriptive_names

custom_colors <- colorRampPalette(c("#FFF0F5", "#ADD8E6", "#9370DB"))(200)  # Whitish pink to blue to obvious purple
par(mar = c(5, 5, 5, 5))  # Adjust margins to center the plot

# Heatmap magic
corrplot(cramers_v_matrix, method = "color", tl.cex = 0.6, tl.col = "black", 
         addgrid.col = NA, cl.pos = "b", col = custom_colors, is.corr = FALSE) 

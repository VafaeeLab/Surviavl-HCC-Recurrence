# Data
library(dplyr)
library(tidyverse)
dat_train <-
  readxl::read_xlsx("RevisedData/imputed_data_train.xlsx") %>%
  filter(DaysToRecurrence >= 90) %>% # only +90 values in the duration column
  select(-c(Hospital, MRN, DOB)) %>%
  # renaming the columns to avoid very long names
  rename(
    LiverDisease = UnderlyingLiverDisease_NASH_0_HepB_1_HepC_2_Alcohol_3_Other_4_,
    NAFLD = NAFLDVsNonNAFLD,
    Viral = ViralVs0nviral,
    Age = AgeAtResection_years_,
    Size = sizeOfLargestLesion_cm_,
    Sex = SexCodedM_0_F_1,
    Ethnicity = Ethnicity_0Aus_1Asian_2African3European4Other_,
    LVI = Lymphatic_vascularInvasion,
    PortalHTN = PortalHTN_HPVG_5mmhg_,
    HBsAg = HBsAgPositive,
    HepC = ChronicHepatitisC,
    TreatDiabetes = DiabetesTreatmentPriorToResectionY1N0,
    TreatHTN = TreatmentOfHTNPriorToResectionY1N0
  ) %>%
  select(-c(NAFLD, Viral, HBsAg, HepC, TreatDiabetes, TreatHTN))

dat_test <-
  readxl::read_xlsx("RevisedData/binarized_data_test.xlsx") %>%
  filter(DaysToRecurrence >= 90) %>% # only +90 values in the duration column
  select(-c(Hospital, MRN, DOB)) %>%
  # renaming the columns to avoid very long names
  rename(
    LiverDisease = UnderlyingLiverDisease_NASH_0_HepB_1_HepC_2_Alcohol_3_Other_4_,
    NAFLD = NAFLDVsNonNAFLD,
    Viral = ViralVs0nviral,
    Age = AgeAtResection_years_,
    Size = sizeOfLargestLesion_cm_,
    Sex = SexCodedM_0_F_1,
    Ethnicity = Ethnicity_0Aus_1Asian_2African3European4Other_,
    LVI = Lymphatic_vascularInvasion,
    PortalHTN = PortalHTN_HPVG_5mmhg_,
    HBsAg = HBsAgPositive,
    HepC = ChronicHepatitisC,
    TreatDiabetes = DiabetesTreatmentPriorToResectionY1N0,
    TreatHTN = TreatmentOfHTNPriorToResectionY1N0
  ) %>%
  select(-c(NAFLD, Viral, HBsAg, HepC, TreatDiabetes, TreatHTN))

dat_train[, 3:ncol(dat_train)] <-
  lapply(dat_train[3:ncol(dat_train)], factor) ## as.factor() could also be used
dat_test[, 3:ncol(dat_train)] <-
  lapply(dat_test[3:ncol(dat_train)], factor) ## as.factor() could also be used

# Modelling
## Kaplan Meier Analysis:
library(survival)
library(ggplot2)
library(survminer)

dat_train_yr <-
  dat_train %>% mutate(YearsToRecurrence = DaysToRecurrence / 365.25)
km_fit <-
  survfit(Surv(YearsToRecurrence, Recurrence) ~ 1, data = dat_train_yr)

pdf("KM-0.pdf", width = 6, height = 6)
ggsurvplot(
  km_fit,
  data = dat_train_yr,
  palette =
    c("#0000a7"),# custom color palettes
  break.time.by = 1,
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  conf.int = TRUE,
  # Add confidence interval
  risk.table = TRUE,
  # Add risk table
  ggtheme = theme_bw() # Change ggplot2 theme
)

dev.off()

summary(km_fit,time=c(2, 5))$surv

## Kaplan Meier Analysis with group:
# ---------------------------------------------------------------------------------------#
# LiverDesease
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    LiverDisease,
  data = dat_train_yr
)

pdf("KM_LiverDisease.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Underlying liver disease",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  legend.labs =
    c("NASH", "HepB", "HepC", "Alcohol", "Other"),
  # Change legend labels
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# Age
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    Age,
  data = dat_train_yr
)

pdf("KM_Age.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Age 65 years",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# PriorTACE
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    PriorTACE,
  data = dat_train_yr
)

pdf("KM_PriorTACE.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Prior TACE",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# bili
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    bili,
  data = dat_train_yr
)

pdf("KM_bili.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Bilirubin",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# albumin
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    albumin,
  data = dat_train_yr
)

pdf("KM_albumin.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Albumin",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# INR
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    INR,
  data = dat_train_yr
)

pdf("KM_INR.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  palette =
    c("#0000a7", "#c1272d"),# custom color palettes
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "INR",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw(), # Change ggplot2 theme
  legend.title="",
  legend.labs =
    c("INR <= 1", "INR > 1.1"),
)
dev.off()

# ---------------------------------------------------------------------------------------#
# PlateletCount
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    PlateletCount,
  data = dat_train_yr
)

pdf("KM_PlateletCount.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Platelet count",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# AFP
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    AFP,
  data = dat_train_yr
)

pdf("KM_AFP.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "AFP",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# numberOfLesions
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    numberOfLesions,
  data = dat_train_yr
)

pdf("KM_numberOfLesions.pdf",
  width = 6,
  height = 6
)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Number of lesions > 1",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# Size
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    Size,
  data = dat_train_yr
)

pdf("KM_Size.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  palette =
    c("#0000a7", "#c1272d"),# custom color palettes
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Size of the largest lesion >= 5cm",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  legend.labs =
    c("Size < 5cm", "Size >= 5cm"),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw(), # Change ggplot2 theme
  legend.title=""
)
dev.off()

# ---------------------------------------------------------------------------------------#
# satellite
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    satellite,
  data = dat_train_yr
)

pdf("KM_satellite.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Satellite lesions",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# cirrhosis
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    cirrhosis,
  data = dat_train_yr
)
pdf("KM_cirrhosis.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  palette =
    c("#0000a7", "#c1272d"),# custom color palettes
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Cirrhosis",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  legend.labs =
    c("No cirrhosis", "Cirrhosis"),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw(), # Change ggplot2 theme
  legend.title=""
)
dev.off()

# ---------------------------------------------------------------------------------------#
# Sex
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    Sex,
  data = dat_train_yr
)

pdf("KM_Sex.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Gender",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  legend.labs =
    c("Female", "Male"),
  # Change legend labels
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# Ethnicity
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    Ethnicity,
  data = dat_train_yr
)

pdf("KM_Ethnicity.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Ethnicity",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  legend.labs =
    c("Caucasian", "Asian", "Others"),
  # Change legend labels
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# LVI
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    LVI,
  data = dat_train_yr
)

pdf("KM_LVI.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  palette =
    c("#0000a7", "#c1272d"),# custom color palettes
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Lymphovascular invasion",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  legend.labs =
    c("No LVI", "LVI"),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw(), # Change ggplot2 theme
  legend.title=""
)
dev.off()

# ---------------------------------------------------------------------------------------#
# PortalHTN
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    PortalHTN,
  data = dat_train_yr
)

pdf("KM_PortalHTN.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Portal HTN HPVG 5mmhg",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# ALT
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    ALT,
  data = dat_train_yr
)

pdf("KM_ALT.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "ALT",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# BMI
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    BMI,
  data = dat_train_yr
)

pdf("KM_BMI.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "BMI",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# DM
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    DM,
  data = dat_train_yr
)

pdf("KM_DM.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "DM",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# Hypertension
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    Hypertension,
  data = dat_train_yr
)

pdf("KM_Hypertension.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "Hypertension",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# eGFR
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    eGFR,
  data = dat_train_yr
)

pdf("KM_eGFR.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "eGFR",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# IHD
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    IHD,
  data = dat_train_yr
)

pdf("KM_IHD.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "IHD",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()

# ---------------------------------------------------------------------------------------#
# CVS
km_trt_fit <- survfit(
  Surv(YearsToRecurrence, Recurrence) ~
    CVS,
  data = dat_train_yr
)

pdf("KM_CVS.pdf", width = 6, height = 6)
ggsurvplot(
  km_trt_fit,
  data = dat_train_yr,
  size = 1,
  # change line size
  conf.int = TRUE,
  # Add confidence interval
  pval = TRUE,
  # Add p-value
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Risk table color by groups
  title = "CVS",
  xlab = "Time in years",
  xlim = c(0, 20),
  ylim = c(0, 1),
  risk.table.height = 0.3,
  # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()


## Semi-parametric Cox-PH Model:
### Fit univariate Cox model
library(forestmodel)
covariates <- colnames(dat_train)[3:ncol(dat_train)]

univ_formulas <-
  sapply(covariates, function(x) {
    as.formula(paste("Surv(DaysToRecurrence, Recurrence)~", x))
  })

univ_models <- lapply(univ_formulas, function(x) {
  coxph(x, data = dat_train)
})
lapply(univ_models, function(x) {
  summary(x)
})


pdf("Univariate.pdf", width = 10, height = 10)
forest_model(
  model_list = univ_models,
  covariates = covariates,
  merge_models = T
)
dev.off()

### Fit multivariate Cox Model
cox <- coxph(
  Surv(DaysToRecurrence, Recurrence) ~
    bili + albumin + INR + PlateletCount +
    numberOfLesions + Size + satellite +
    cirrhosis + Ethnicity + LVI +
    PortalHTN + ALT + DM,
  data = dat_train
)
summary(cox)

write.csv(tidy(cox), file = "cox.csv")

cox_fit <- survfit(cox)

# autoplot(cox_fit)

pdf("KM-cox.pdf", width = 6, height = 6)

ggsurvplot(
  cox_fit,
  data = dat_train,
  # size = 1,                 # change line size
  # palette =
  #   c("#2E9FDF"),# custom color palettes  c("#E7B800", "#2E9FDF")
  conf.int = TRUE,
  # Add confidence interval
  # pval = TRUE,              # Add p-value
  risk.table = TRUE,
  # Add risk table
  title = "Cox-PH survival curve",
  xlab = "Time in years",
  ylim = c(0.25, 1),
  xlim = c(0, 7200),
  # risk.table.col = "strata",# Risk table color by groups
  # legend.labs =
  #   c("Male", "Female"),    # Change legend labels
  # risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw() # Change ggplot2 theme
)
dev.off()


## The impact of covariates over time:
# (Aalen's additive regression model for censored data)
aa_fit <-
  aareg(Surv(DaysToRecurrence, Recurrence) ~ ., data = dat_train)
aa_fit

pdf("cox_time.pdf", width = 12.5, height = 8)
library(ggfortify)
autoplot(aa_fit)

dev.off()


## C-index for train and test sets:
library(Hmisc)
# For train set
SurvObj_train <-
  Surv(dat_train$DaysToRecurrence, dat_train$Recurrence)
predicted_train <- predict(cox, newdata = dat_train)
c_index_result_train <-
  rcorr.cens(x = -predicted_train, S = SurvObj_train) # concordance(cox)

# For test set
SurvObj_test <- Surv(dat_test$DaysToRecurrence, dat_test$Recurrence)
predicted_test <- predict(cox, newdata = dat_test)
c_index_result_test <-
  rcorr.cens(x = -predicted_test, S = SurvObj_test) # concordance(cox)

paste(
  "In the Cox model: the C-Index is ",
  round(c_index_result_train[1], 3),
  " for the train set, ",
  "and the C-index is ",
  round(c_index_result_test[1], 3),
  " for the test set."
)


## Survival-based Random Forest model:
### ranger model
unique.death.times_train <- dat_train %>%
  filter(Recurrence == 1) %>%
  select(DaysToRecurrence) %>%
  unlist() %>%
  unique() %>%
  sort()
unique.death.times_test <- dat_test %>%
  filter(Recurrence == 1) %>%
  select(DaysToRecurrence) %>%
  unlist() %>%
  unique() %>%
  sort()

library(ranger)
r_fit_all <- ranger(
  Surv(DaysToRecurrence, Recurrence) ~ .,
  data = dat_train,
  mtry = 4,
  importance = "permutation",
  splitrule = "extratrees",
  verbose = TRUE
)

# Average the survival models
death_times_all <- r_fit_all$unique.death.times
surv_prob_all <- data.frame(r_fit_all$survival)
avg_prob_all <- colMeans(surv_prob_all)

# Predict on the test set
r_pred_all_test <- predict(r_fit_all, data = dat_test)

# Extract survival probabilities
surv_prob_all_test <- data.frame(r_pred_all_test$survival)
avg_prob_all_test <- colMeans(surv_prob_all_test)


r_fit_sign_uni <- ranger(
  Surv(DaysToRecurrence, Recurrence) ~ bili + albumin + INR + PlateletCount +
    numberOfLesions + Size + satellite +
    cirrhosis + Ethnicity + LVI +
    PortalHTN + ALT + DM,
  data = dat_train,
  mtry = 4,
  importance = "permutation",
  splitrule = "extratrees",
  verbose = TRUE
)
# Average the survival models
death_times_sign_uni <- r_fit_sign_uni$unique.death.times
surv_prob_sign_uni <- data.frame(r_fit_sign_uni$survival)
avg_prob_sign_uni <- colMeans(surv_prob_sign_uni)

# Predict on the test set
r_pred_sign_uni_test <- predict(r_fit_sign_uni, data = dat_test)

# Extract survival probabilities
surv_prob_sign_uni_test <- data.frame(r_pred_sign_uni_test$survival)
avg_prob_sign_uni_test <- colMeans(surv_prob_sign_uni_test)

r_fit_sign <- ranger(
  Surv(DaysToRecurrence, Recurrence) ~
    INR + Size + cirrhosis + LVI,
  data = dat_train,
  mtry = 4,
  importance = "permutation",
  splitrule = "extratrees",
  verbose = TRUE
)

# Average the survival models
death_times_sign <- r_fit_sign$unique.death.times
surv_prob_sign <- data.frame(r_fit_sign$survival)
avg_prob_sign <- colMeans(surv_prob_sign)

# Predict on the test set
r_pred_sign_test <- predict(r_fit_sign, data = dat_test)

# Extract survival probabilities
surv_prob_sign_test <- data.frame(r_pred_sign_test$survival)
avg_prob_sign_test <- colMeans(surv_prob_sign_test)

pdf("RF_patients.pdf", width = 8, height = 6)
# Plot the survival models for each patient
plot(
  r_fit_sign$unique.death.times,
  r_fit_sign$survival[1, ],
  type = "l",
  ylim = c(0, 1),
  col = "red",
  xlab = "Days",
  ylab = "survival",
  main = "Patient Survival Curves"
)


cols <- colors()
for (n in sample(c(2:dim(dat_train)[1]), 40)) {
  lines(
    r_fit_sign$unique.death.times,
    r_fit_sign$survival[n, ],
    type = "l",
    col = cols[n]
  )
}
lines(death_times_sign, avg_prob_sign, lwd = 2)
legend(4000, 0.8, legend = c("Average = black"))

dev.off()


#### ranger in MLR package
##### All features
library(mlr) # install.packages("mlr")
task_mlr <-
  makeSurvTask(
    data = dat_train,
    target = c("DaysToRecurrence", "Recurrence")
  )

surv_lrn_mlr <-
  makeLearner("surv.ranger", id = "rng") # lrns = listLearners()

mod <- train(surv_lrn_mlr, task_mlr)

mod
Pred_train <- predict(mod, newdata = dat_train)
Pred_train
Pred_test <- predict(mod, newdata = dat_test)
Pred_test

c_index_result_train_RF <-
  performance(Pred_train) # listMeasures(task_mlr)
c_index_result_test_RF <- performance(Pred_test)

paste(
  "In the Random Forest model: the C-Index is ",
  round(c_index_result_train_RF[1], 3),
  " for the train set, ",
  "and the C-index is ",
  round(c_index_result_test_RF[1], 3),
  " for the test set."
)

#### ranger in MLR package
##### Significant features of the univariate cox
library(mlr) # install.packages("mlr")
dat_train_sign_uni <- dat_train %>% select(
  c(
    DaysToRecurrence,
    Recurrence,
    bili,
    albumin,
    INR,
    PlateletCount,
    numberOfLesions,
    Size,
    satellite,
    cirrhosis,
    Ethnicity,
    LVI,
    PortalHTN,
    ALT,
    DM
  )
)
dat_test_sign_uni <- dat_test %>% select(
  c(
    DaysToRecurrence,
    Recurrence,
    bili,
    albumin,
    INR,
    PlateletCount,
    numberOfLesions,
    Size,
    satellite,
    cirrhosis,
    Ethnicity,
    LVI,
    PortalHTN,
    ALT,
    DM
  )
)

task_mlr_sign_uni <-
  makeSurvTask(
    data = dat_train_sign_uni,
    target = c("DaysToRecurrence", "Recurrence")
  )

surv_lrn_mlr <-
  makeLearner("surv.ranger", id = "rng") # lrns = listLearners()

mod_sign_uni <- train(surv_lrn_mlr, task_mlr_sign_uni)

Pred_train_sign_uni <-
  predict(mod_sign_uni, newdata = dat_train_sign_uni)
Pred_train_sign_uni

Pred_test_sign_uni <-
  predict(mod_sign_uni, newdata = dat_test_sign_uni)
Pred_test_sign_uni

c_index_result_train_RF_sign_uni <-
  performance(Pred_train_sign_uni) # listMeasures(task_mlr)
c_index_result_test_RF_sign_uni <- performance(Pred_test_sign_uni)

paste(
  "In the Random Forest model: the C-Index is ",
  round(c_index_result_train_RF_sign_uni[1], 3),
  " for the train set, ",
  "and the C-index is ",
  round(c_index_result_test_RF_sign_uni[1], 3),
  " for the test set."
)


#### ranger in MLR package
##### Significant features of the multivariate cox
library(mlr) # install.packages("mlr")
dat_train_sign <-
  dat_train %>% select(c(DaysToRecurrence, Recurrence, INR, Size, cirrhosis, LVI))
dat_test_sign <-
  dat_test %>% select(c(DaysToRecurrence, Recurrence, INR, Size, cirrhosis, LVI))

task_mlr_sign <-
  makeSurvTask(
    data = dat_train_sign,
    target = c("DaysToRecurrence", "Recurrence")
  )

surv_lrn_mlr <-
  makeLearner("surv.ranger", id = "rng") # lrns = listLearners()

mod_sign <- train(surv_lrn_mlr, task_mlr_sign)

Pred_train_sign <- predict(mod_sign, newdata = dat_train_sign)
Pred_train_sign

Pred_test_sign <- predict(mod_sign, newdata = dat_test_sign)
Pred_test_sign

c_index_result_train_RF_sign <-
  performance(Pred_train_sign) # listMeasures(task_mlr)
c_index_result_test_RF_sign <- performance(Pred_test_sign)

paste(
  "In the Random Forest model: the C-Index is ",
  round(c_index_result_train_RF_sign[1], 3),
  " for the train set, ",
  "and the C-index is ",
  round(c_index_result_test_RF_sign[1], 3),
  " for the test set."
)


## Comparison of the three models, Kaplan Meier, Cox-PH, and Random Forest:
### Train set
# Set up for ggplot
km_fit <-
  survfit(Surv(DaysToRecurrence, Recurrence) ~ 1, data = dat_train)
kmi <- rep("KM", length(km_fit$time))
km_df <- data.frame(km_fit$time, km_fit$surv, kmi)
names(km_df) <- c("Time", "Surv", "Model")

coxi <- rep("Cox", length(cox_fit$time))
cox_df <- data.frame(cox_fit$time, cox_fit$surv, coxi)
names(cox_df) <- c("Time", "Surv", "Model")

rfi_all <- rep("RF_all", length(unique.death.times_train))
rf_df_all <-
  data.frame(unique.death.times_train, avg_prob_all, rfi_all)
names(rf_df_all) <- c("Time", "Surv", "Model")

rfi_sign_uni <- rep("RF_sign", length(unique.death.times_train))
rf_df_sign_uni <-
  data.frame(unique.death.times_train, avg_prob_sign_uni, rfi_sign_uni)
names(rf_df_sign_uni) <- c("Time", "Surv", "Model")

xlim_max <- max(c(km_df$Time, cox_df$Time, rf_df_sign_uni$Time))

plot_df <- rbind(km_df, cox_df, rf_df_sign_uni) %>%
  split(.$Model) %>%
  map(~ add_row(
    .,
    Time = xlim_max,
    Surv = NA,
    Model = unique(.$Model)
  )) %>%
  bind_rows() %>%
  arrange(by_group = T) %>%
  fill(Surv, .direction = "down") %>%
  ungroup()

pdf("model_comp_sign_train.pdf",
  width = 8,
  height = 6
)

ggplot(plot_df, aes(x = Time, y = Surv, color = Model)) +
  geom_line() +
  labs(x = "Time in days", y = "Survival Probability", title = "Comparison of Cox-PH and Random Forest models_train") +
  coord_cartesian(xlim = c(0, xlim_max), ylim = c(0, 1))

dev.off()

xlim_max <-
  max(c(km_df$Time, cox_df$Time, rf_df_sign_uni$Time, rf_df_all$Time))

plot_df <- rbind(km_df, cox_df, rf_df_sign_uni, rf_df_all) %>%
  split(.$Model) %>%
  map(~ add_row(
    .,
    Time = xlim_max,
    Surv = NA,
    Model = unique(.$Model)
  )) %>%
  bind_rows() %>%
  arrange(by_group = T) %>%
  fill(Surv, .direction = "down") %>%
  ungroup()

pdf("model_comp_train.pdf", width = 8, height = 6)

ggplot(plot_df, aes(x = Time, y = Surv, color = Model)) +
  geom_line() +
  labs(x = "Time in days", y = "Survival Probability", title = "Comparison of Cox-PH and Random Forest models_train") +
  coord_cartesian(xlim = c(0, xlim_max), ylim = c(0, 1))
dev.off()


### Test set
# Set up for ggplot
km_fit_test <-
  survfit(Surv(DaysToRecurrence, Recurrence) ~ 1, data = dat_test)
kmi_test <- rep("KM", length(km_fit_test$time))
km_df_test <-
  data.frame(km_fit_test$time, km_fit_test$surv, kmi_test)
names(km_df_test) <- c("Time", "Surv", "Model")

# Fit survival curves on the test set
cox_fit_test <- survfit(cox, newdata = dat_test)

# Calculate the average survival at each time point for the test set
avg_surv_test <- colMeans(as.data.frame(t(cox_fit_test$surv)))

coxi_test <- rep("Cox", length(cox_fit_test$time))
cox_df_test <-
  data.frame(cox_fit_test$time, avg_surv_test, coxi_test)
names(cox_df_test) <- c("Time", "Surv", "Model")

rfi_all_test <- rep("RF_all", length(avg_prob_all_test))
rf_df_all_test <-
  data.frame(unique.death.times_train, avg_prob_all_test, rfi_all_test)
names(rf_df_all_test) <- c("Time", "Surv", "Model")

rfi_sign_uni_test <- rep("RF_sign", length(avg_prob_sign_uni_test))
rf_df_sign_uni_test <-
  data.frame(
    unique.death.times_train,
    avg_prob_sign_uni_test,
    rfi_sign_uni_test
  )
names(rf_df_sign_uni_test) <- c("Time", "Surv", "Model")

xlim_max <-
  max(c(km_df_test$Time, cox_df_test$Time, rf_df_sign_uni_test$Time))

plot_df <- rbind(km_df_test, cox_df_test, rf_df_sign_uni_test) %>%
  split(.$Model) %>%
  map(~ add_row(
    .,
    Time = xlim_max,
    Surv = NA,
    Model = unique(.$Model)
  )) %>%
  bind_rows() %>%
  arrange(by_group = T) %>%
  fill(Surv, .direction = "down") %>%
  ungroup()

pdf("model_comp_sign_test.pdf",
  width = 8,
  height = 6
)

ggplot(plot_df, aes(x = Time, y = Surv, color = Model)) +
  geom_line() +
  labs(x = "Time in days", y = "Survival Probability", title = "Comparison of Cox-PH and Random Forest models_test") +
  coord_cartesian(xlim = c(0, xlim_max), ylim = c(0, 1))

dev.off()

xlim_max <-
  max(
    c(
      km_df_test$Time,
      cox_df_test$Time,
      rf_df_sign_uni_test$Time,
      rf_df_all_test$Time
    )
  )

plot_df <-
  rbind(km_df_test, cox_df_test, rf_df_sign_uni, rf_df_all) %>%
  split(.$Model) %>%
  map(~ add_row(
    .,
    Time = xlim_max,
    Surv = NA,
    Model = unique(.$Model)
  )) %>%
  bind_rows() %>%
  arrange(by_group = T) %>%
  fill(Surv, .direction = "down") %>%
  ungroup()

pdf("model_comp_test.pdf", width = 8, height = 6)

ggplot(plot_df, aes(x = Time, y = Surv, color = Model)) +
  geom_line() +
  labs(x = "Time in days", y = "Survival Probability", title = "Comparison of Cox-PH and Random Forest models_test") +
  coord_cartesian(xlim = c(0, xlim_max), ylim = c(0, 1))

dev.off()


# A comparison of test/train models

xlim_max <-
  max(
    c(
      km_df$Time,
      km_df_test$Time,
      cox_df_test$Time,
      cox_df$Time,
      rf_df_sign_uni$Time,
      rf_df_sign_uni_test$Time
    )
  )

km_df$Model <- "KM-train"
km_df_test$Model <- "KM-test"

cox_df$Model <- "Cox-train"
cox_df_test$Model <- "Cox-test"

rf_df_sign_uni$Model <- "RF-train"
rf_df_sign_uni_test$Model <- "RF-test"

# Define a custom color palette for train and test models
library(RColorBrewer)

# Define a custom color palette using RColorBrewer
custom_palette <- c("KM-train" = brewer.pal(8, "Blues")[3], "KM-test" = brewer.pal(8, "Blues")[5],
                    "Cox-train" = brewer.pal(8, "Reds")[3], "Cox-test" = brewer.pal(8, "Reds")[5],
                    "RF-train" = brewer.pal(8, "Greens")[3], "RF-test" = brewer.pal(8, "Greens")[5])

plot_df <-
  rbind(
    km_df,
    km_df_test,
    cox_df,
    cox_df_test,
    rf_df_sign_uni,
    rf_df_sign_uni_test
  ) %>%
  split(.$Model) %>%
  map(~ add_row(
    .,
    Time = xlim_max,
    Surv = NA,
    Model = unique(.$Model)
  )) %>%
  bind_rows() %>%
  arrange(by_group = T) %>%
  fill(Surv, .direction = "down") %>%
  ungroup()

pdf("model_comp_all.pdf", width = 8, height = 6)

ggplot(plot_df, aes(x = Time, y = Surv, color = Model)) +
  geom_line() +
  scale_color_manual(values = custom_palette) +  # Specify the custom color palette
  labs(x = "Time in days", y = "Survival Probability", title = "Comparison of Cox-PH and Random Forest models") +
  coord_cartesian(xlim = c(0, xlim_max), ylim = c(0, 1))

dev.off()

## Parametric survival models (Accelerated Failure Time models (AFT)):
### Only intercept models:
library("muhaz")
library("data.table")
library("flexsurv")
kernel_haz_est <-
  muhaz(dat_train$DaysToRecurrence, dat_train$Recurrence)
kernel_haz <- data.table(
  time = kernel_haz_est$est.grid,
  est = kernel_haz_est$haz.est,
  method = "Kernel density"
)


dists <- c(
  "exp", "weibull", "gamma",
  "lognormal", "llogis", "gengamma"
)
dists_long <- c(
  "Exponential",
  "Weibull",
  "Gamma",
  "Lognormal",
  "Log-logistic",
  "Generalized gamma"
)

parametric_haz <- vector(mode = "list", length = length(dists))
for (i in 1:length(dists)) {
  fit <- flexsurvreg(Surv(DaysToRecurrence, Recurrence) ~ 1,
    data = dat_train,
    dist = dists[i]
  )

  parametric_haz[[i]] <-
    summary(fit,
      type = "hazard",
      ci = FALSE,
      tidy = TRUE
    )
  parametric_haz[[i]]$method <- dists_long[i]
}

parametric_haz <- rbindlist(parametric_haz)
haz <- rbind(kernel_haz, parametric_haz)
haz[, method := factor(method,
  levels = c(
    "Kernel density",
    dists_long
  )
)]
n_dists <- length(dists)

pdf("parametric.pdf", width = 8, height = 6)

ggplot(haz, aes(
  x = time,
  y = est,
  col = method,
  linetype = method
)) +
  geom_line() +
  labs(x = "Time in days", y = "Hazard", title = "Comparison of parametric hazard curves with Kaplan Meier curve") +
  scale_colour_manual(
    name = "",
    values = c("black", rainbow(n_dists))
  ) +
  scale_linetype_manual(
    name = "",
    values = c(1, rep_len(2:6, n_dists))
  )
dev.off()

# Agoramétrie Surveys

#0) Packages 

library(haven)
library(summarytools)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(classInt)
library(janitor)
library(reshape2)
library(plm)
library(stargazer)
library(sandwich)
library(lmtest)
library(tidyr)
library(FactoMineR)
library(explor)
library(factoextra)
library(flexmix)
library(psych)
library(ltm)
library(scales)

#I) Datasets 

data_1985 <- read_sas("Structures de l'opinion (1977 - 1991)/1985/fr.cdsp.ddi.agora1985.sas7bdat")
data_1986 <- read_sas("Structures de l'opinion (1977 - 1991)/1986/fr.cdsp.ddi.agora1986.sas7bdat")
data_1987 <- read_sas("Structures de l'opinion (1977 - 1991)/1987/fr.cdsp.ddi.agora1987.sas7bdat")
data_1987 <- data_1987[-1, ]
data_1988 <- read_sas("Structures de l'opinion (1977 - 1991)/1988/fr.cdsp.ddi.agora1988.sas7bdat")
data_1988 <- data_1988[-c(1:3), ]

exposure <- read_delim("Cesium 137 et Iode 131 data (IPSN).csv", delim = ";")


#II) Variables 

#A) Outcome variables

# Construct more nuclear plants ? 

data_1985$NuclearPlants <- data_1985$c4
data_1986$NuclearPlants <- data_1986$c4
data_1987 <- data_1987 %>%
  mutate(NuclearPlants = ifelse(data_1987$c4 == 5.31017013119972e-315, NA, data_1987$c4))
data_1988$NuclearPlants <- data_1988$c4

# Building nuclear power plants was a good thing ? (Retrospective appreciation)

data_1985$RetroNuclearPlants <- data_1985$c168
data_1986$RetroNuclearPlants <- data_1986$c168

data_1987 <- data_1987 %>%
  mutate(RetroNuclearPlants = ifelse(data_1987$c168 == 5.31017013119972e-315, NA, data_1987$c168))

data_1988$RetroNuclearPlants <- data_1988$c168

# Nuclear experts are very serious people ?

data_1985$NuclearExpertise <- data_1985$n19
data_1986 <- data_1986 %>%
  mutate(NuclearExpertise = ifelse(data_1986$n19 == 9, NA, data_1986$n19))

data_1987 <- data_1987 %>%
  mutate(NuclearExpertise = ifelse(n19 %in% c(9, 5.31017013119972e-315), NA, n19))

data_1988 <- data_1988 %>%
  mutate(NuclearExpertise = ifelse(data_1988$n19 == 9, NA, data_1988$n19))

freq(data_1988$NuclearExpertise)

# Nuclear waste is a serious problem ?

data_1985$NuclearWaste <- data_1985$n3
data_1986 <- data_1986 %>%
  mutate(NuclearWaste = ifelse(data_1986$n3 == 9, NA, data_1986$n3))

data_1987 <- data_1987 %>%
  mutate(NuclearWaste = ifelse(n3 %in% c(9, 5.31017013119972e-315), NA, n3))

data_1988 <- data_1988 %>%
  mutate(NuclearWaste = ifelse(data_1988$n3 == 9, NA, data_1988$n3))

# Finding a site in France for radioactive waste ?

data_1985$NuclearWasteLocation <- data_1985$n35
data_1986 <- data_1986 %>%
  mutate(NuclearWasteLocation = ifelse(data_1986$n35 == 9, NA, data_1986$n35))

data_1987 <- data_1987 %>%
  mutate(NuclearWasteLocation = ifelse(n35 %in% c(9, 5.30498947741318e-315), NA, n35))
freq(data_1987$NuclearWasteLocation)

data_1988 <- data_1988 %>%
  mutate(NuclearWasteLocation = ifelse(data_1988$n35 == 9, NA, data_1988$n35))

# Radioactive waste can be safely stored ?

data_1985$NuclearWasteSafety <- data_1985$n39
data_1986 <- data_1986 %>%
  mutate(NuclearWasteSafety = ifelse(data_1986$n39 == 9, NA, data_1986$n39))
data_1987 <- data_1987 %>%
  mutate(NuclearWasteSafety = ifelse(n39 %in% c(9, 5.30757980430645e-315), NA, n39))
freq(data_1987$NuclearWasteSafety)

# Missing 1988 

# Nuclear Support Index (PCA)

# 1985

cor(data_1985$NuclearPlants, data_1985$RetroNuclearPlants, use = "complete.obs") # 0,64
cor(data_1985$NuclearPlants, data_1985$NuclearExpertise, use = "complete.obs")   # 0,29
cor(data_1985$NuclearExpertise, data_1985$RetroNuclearPlants, use = "complete.obs") # 0,34

cronbach_1985 <- data_1985 %>%
  dplyr::select(NuclearPlants, RetroNuclearPlants, NuclearExpertise, NuclearWaste, NuclearWasteLocation, NuclearWasteSafety)

cronbach.alpha(cronbach_1985, CI = TRUE, na.rm = TRUE) #0.7

normalization_1985 <- scale(cronbach_1985)
head(normalization_1985)

PCA_1985 <- princomp(normalization_1985)
PCA_1985$loadings[, 1:2]
summary(PCA_1985) # 1st component : 35% of total data variance

fviz_pca_var(PCA_1985, col.var = "black")

data_1985$NuclearSupportIndex <- PCA_1985$scores 
data_1985$NuclearSupportIndex <- rescale(data_1985$NuclearSupportIndex, to = c(0, 1))

mean(data_1985$NuclearSupportIndex)

# 1986

cronbach_1986 <- data_1986 %>%
  dplyr::select(NuclearPlants, RetroNuclearPlants, NuclearExpertise, NuclearWaste, NuclearWasteLocation, NuclearWasteSafety)

cronbach.alpha(cronbach_1986, CI = TRUE, na.rm = TRUE) #0.71

normalization_1986 <- scale(cronbach_1986)
head(normalization_1986)

PCA_1986 <- princomp(na.omit(normalization_1986))
PCA_1986$loadings[, 1:2]
summary(PCA_1986) # 1st component : 42% of total data variance

fviz_pca_var(PCA_1986, col.var = "black")

rows_used86 <- complete.cases(normalization_1986)
data_1986$NuclearSupportIndex <- NA
data_1986$NuclearSupportIndex[rows_used86] <- PCA_1986$scores[, 1]  # first component

data_1986$NuclearSupportIndex <- rescale(data_1986$NuclearSupportIndex, to = c(0, 1))

mean(data_1986$NuclearSupportIndex, na.rm = TRUE)

rm(cronbach_1985, cronbach_1986, normalization_1985, normalization_1986, rows_used85)

# 1987 

cronbach_1987 <- data_1987 %>%
  dplyr::select(NuclearPlants, RetroNuclearPlants, NuclearExpertise, NuclearWaste, NuclearWasteLocation, NuclearWasteSafety)

cronbach.alpha(cronbach_1987, CI = TRUE, na.rm = TRUE) #0.57

normalization_1987 <- scale(cronbach_1987)
head(normalization_1987)

PCA_1987 <- princomp(na.omit(normalization_1987))
PCA_1987$loadings[, 1:2]
summary(PCA_1987) # 1st component : 39% of total data variance

fviz_pca_var(PCA_1987, col.var = "black")

rows_used87 <- complete.cases(normalization_1987)
data_1987$NuclearSupportIndex <- NA
data_1987$NuclearSupportIndex[rows_used87] <- PCA_1987$scores[, 1]  # first component

data_1987$NuclearSupportIndex <- rescale(data_1987$NuclearSupportIndex, to = c(0, 1))

mean(data_1987$NuclearSupportIndex, na.rm = TRUE)

rm(cronbach_1985, cronbach_1986, normalization_1985, normalization_1986, rows_used86,
   cronbach_1987, normalization_1987, rows_used87)

# 1988 

cronbach_1988 <- data_1988 %>%
  dplyr::select(NuclearPlants, RetroNuclearPlants, NuclearExpertise, NuclearWaste, NuclearWasteLocation)

cronbach.alpha(cronbach_1988, CI = TRUE, na.rm = TRUE) #0.71

normalization_1988 <- scale(cronbach_1988)
head(normalization_1988)

PCA_1988 <- princomp(na.omit(normalization_1988))
PCA_1988$loadings[, 1:2]
summary(PCA_1988) # 1st component : 39% of total data variance

fviz_pca_var(PCA_1988, col.var = "black")

rows_used88 <- complete.cases(normalization_1988)
data_1988$NuclearSupportIndex <- NA
data_1988$NuclearSupportIndex[rows_used88] <- PCA_1988$scores[, 1]  # first component

data_1988$NuclearSupportIndex <- rescale(data_1988$NuclearSupportIndex, to = c(0, 1))


mean(data_1988$NuclearSupportIndex, na.rm = TRUE)

#B) Control variables

# Gender

data_1985$Women <- ifelse(data_1985$sexe == 2, 1, 0)
data_1986$Women <- ifelse(data_1986$sexe == 2, 1, 0)
data_1987$Women <- ifelse(data_1987$sexe == 2, 1, 0)

freq(data_1986$Women)

# Age in 9 brackets 

data_1985$Age <- data_1985$age9
data_1986$Age <- data_1986$age9
data_1987$Age <- data_1987$age9

freq(data_1987$age9)
freq(data_1986$Age)

head(data_1987)

# Diploma

data_1985 <- data_1985 %>%
  mutate(
    Diploma = case_when(
      diplome %in% c(1, 2) ~ "Low",     # Aucun, CEP
      diplome %in% c(3, 4) ~ "Medium",  # CAP, Brevet
      diplome %in% c(5, 6) ~ "High"     # Bac, diplôme sup.
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High"))
  )

data_1986 <- data_1986 %>%
  mutate(
    Diploma = case_when(
      diplome %in% c(1, 2) ~ "Low",     # Aucun, CEP
      diplome %in% c(3, 4) ~ "Medium",  # CAP, Brevet
      diplome %in% c(5, 6) ~ "High"     # Bac, diplôme sup.
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High"))
  )

data_1987 <- data_1987 %>%
  mutate(
    Diploma = case_when(
      diplome %in% c(1, 2) ~ "Low",     # Aucun, CEP
      diplome %in% c(3, 4) ~ "Medium",  # CAP, Brevet
      diplome %in% c(5, 6) ~ "High"     # Bac, diplôme sup.
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High"))
  )


freq(data_1987$Diploma)

# Occupation

data_1985 <- data_1985 %>%
  mutate(
    Occupation = case_when(
      prof_8 == 1 ~ "Farmer",
      prof_8 == 2 ~ "Craftman",
      prof_8 == 3 ~ "Executive",
      prof_8 == 4 ~ "PI",
      prof_8 == 5 ~ "Employee",
      prof_8 == 6 ~ "Worker",
      prof_8 == 7 ~ "Pensioner",
      prof_8 == 8 ~ "Other"
    ),
    Occupation = factor(Occupation),
    Occupation = relevel(Occupation, ref = "Worker")
  )

data_1986 <- data_1986 %>%
  mutate(
    Occupation = case_when(
      prof_8 == 1 ~ "Farmer",
      prof_8 == 2 ~ "Craftman",
      prof_8 == 3 ~ "Executive",
      prof_8 == 4 ~ "PI",
      prof_8 == 5 ~ "Employee",
      prof_8 == 6 ~ "Worker",
      prof_8 == 7 ~ "Pensioner",
      prof_8 == 8 ~ "Other"
    ),
    Occupation = factor(Occupation),
    Occupation = relevel(Occupation, ref = "Worker")
  )

data_1987 <- data_1987 %>%
  mutate(
    Occupation = case_when(
      prof_8 == 1 ~ "Farmer",
      prof_8 == 2 ~ "Craftman",
      prof_8 == 3 ~ "Executive",
      prof_8 == 4 ~ "PI",
      prof_8 == 5 ~ "Employee",
      prof_8 == 6 ~ "Worker",
      prof_8 == 7 ~ "Pensioner",
      prof_8 == 8 ~ "Other"
    ),
    Occupation = factor(Occupation),
    Occupation = relevel(Occupation, ref = "Worker")
  )

freq(data_1987$Occupation)

# Income Brackets

freq(data_1985$revenu9)

data_1985 <- data_1985 %>%
  mutate(
    Income = case_when(
      revenu9 == 1 ~ "< 4000F",
      revenu9 == 2 ~ "4000 - 5000F",
      revenu9 == 3 ~ "5000 - 6000F",
      revenu9 == 4 ~ "6000 - 7000F",
      revenu9 == 5 ~ "7000 - 8000F",
      revenu9 == 6 ~ "8000 - 10k F",
      revenu9 == 7 ~ "10k - 15k F",
      revenu9 == 8 ~ "> 15k F",
      revenu9 == 999 ~ NA_character_
    ),
    Income = factor(Income, levels = c(
      "< 4000F", "4000 - 5000F", "5000 - 6000F", "6000 - 7000F",
      "7000 - 8000F", "8000 - 10k F", "10k - 15k F", "> 15k F"
    ))
  )

data_1986 <- data_1986 %>%
  mutate(
    Income = case_when(
      revenu9 == 1 ~ "< 4000F",
      revenu9 == 2 ~ "4000 - 5000F",
      revenu9 == 3 ~ "5000 - 6000F",
      revenu9 == 4 ~ "6000 - 7000F",
      revenu9 == 5 ~ "7000 - 8000F",
      revenu9 == 6 ~ "8000 - 10k F",
      revenu9 == 7 ~ "10k - 15k F",
      revenu9 == 8 ~ "> 15k F",
      revenu9 == 9 ~ NA_character_
    ),
    Income = factor(Income, levels = c(
      "< 4000F", "4000 - 5000F", "5000 - 6000F", "6000 - 7000F",
      "7000 - 8000F", "8000 - 10k F", "10k - 15k F", "> 15k F"
    ))
  )

data_1987 <- data_1987 %>%
  mutate(
    Income = case_when(
      revenu9 == 1 ~ "< 4000F",
      revenu9 == 2 ~ "4000 - 5000F",
      revenu9 == 3 ~ "5000 - 6000F",
      revenu9 == 4 ~ "6000 - 7000F",
      revenu9 == 5 ~ "7000 - 8000F",
      revenu9 == 6 ~ "8000 - 10k F",
      revenu9 == 7 ~ "10k - 15k F",
      revenu9 == 8 ~ "> 15k F",
      revenu9 == 9 ~ NA_character_
    ),
    Income = factor(Income, levels = c(
      "< 4000F", "4000 - 5000F", "5000 - 6000F", "6000 - 7000F",
      "7000 - 8000F", "8000 - 10k F", "10k - 15k F", "> 15k F"
    ))
  )

freq(data_1985$Income) # Warning : 20% N.A
freq(data_1986$Income) # Warning : 19% N.A
freq(data_1987$Income) # Warning : 12% N.A

# Assets 

data_1985 <- data_1985 %>%
  mutate(
    HomeOwnership = ifelse(proprio13 == 888, NA, ifelse(proprio13 == 2, 1, 0))
  )

data_1986 <- data_1986 %>%
  mutate(
    HomeOwnership = ifelse(proprio13 == 888, NA, ifelse(proprio13 == 2, 1, 0))
  )

freq(data_1986$HomeOwnership)

data_1987 <- data_1987 %>%
  mutate(
    HomeOwnership = ifelse(proprio13 == 888, NA, ifelse(proprio13 == 2, 1, 0))
  )

data_1985 <- data_1985 %>%
  mutate(
    Savings = ifelse(proprio22 == 888, NA, ifelse(proprio22 == 2, 1, 0))
  )

data_1986 <- data_1986 %>%
  mutate(
    Savings = ifelse(proprio22 == 888, NA, ifelse(proprio22 == 2, 1, 0))
  )

freq(data_1986$Savings)

data_1985 <- data_1985 %>%
  mutate(
    FinancialAssets = ifelse(proprio1 == 888, NA, ifelse(proprio1 == 2, 1, 0))
  )

data_1986 <- data_1986 %>%
  mutate(
    FinancialAssets = ifelse(proprio1 == 888, NA, ifelse(proprio1 == 2, 1, 0))
  )

freq(data_1986$FinancialAssets)

data_1987 <- data_1987 %>%
  mutate(
    FinancialAssets = ifelse(proprio1 == 888, NA, ifelse(proprio1 == 2, 1, 0))
  )

freq(data_1987$FinancialAssets)

#C) Departement variable

freq(data_1985$departement) 
freq(data_1986$departement) 
freq(data_1987$departement)

data_1985$code_dep <- gsub("·", "", data_1985$departement)  # Remove separators
data_1985$code_dep <- ifelse(nchar(data_1985$departement) == 1, 
                               paste0("0", data_1985$departement), 
                               data_1985$departement)

data_1986$code_dep <- gsub("·", "", data_1986$departement)  # Remove separators
data_1986$code_dep <- ifelse(nchar(data_1986$departement) == 1, 
                               paste0("0", data_1986$departement), 
                               data_1986$departement)

data_1987$code_dep <- gsub("·", "", data_1987$departement)  # Remove separators
data_1987$code_dep <- ifelse(nchar(data_1987$departement) == 1, 
                               paste0("0", data_1987$departement), 
                               data_1987$departement)

freq(data_1987$code_dep)

exposure$code_dep <- gsub("·", "", exposure$code_dep)  # Remove separators
exposure$code_dep <- ifelse(nchar(exposure$code_dep) == 1, 
                               paste0("0", exposure$code_dep), 
                               exposure$code_dep)


data_1985 <- data_1985 %>%
  dplyr::select(NuclearPlants:code_dep) %>%
  mutate(Year = as.factor(1985))

data_1986 <- data_1986 %>%
  dplyr::select(NuclearPlants:code_dep) %>%
  mutate(Year = as.factor(1986))

data_1987 <- data_1987 %>%
  dplyr::select(NuclearPlants:code_dep) %>%
  mutate(Year = as.factor(1987))

data_panel <- bind_rows(data_1985, data_1986, data_1987)

data_panel <- data_panel %>%
  left_join(
    exposure %>% dplyr::select(code_dep, departement, `Cesium 137`, `Iode 131`),
    by = "code_dep"
  )

data_panel$Cesium <- data_panel$`Cesium 137`
data_panel$Iodine <- data_panel$`Iode 131`

data_panel <- data_panel %>%
  mutate(Post = ifelse(Year == 1986, 1, 0))

freq(data_panel$Cesium)

#III) Regression Analysis

ols <- lm(NuclearPlants ~ Women + as.factor(Age) + Diploma + Income + Occupation + HomeOwnership + Savings + FinancialAssets, data = data_1985)
ols2 <- lm(RetroNuclearPlants ~ Women + as.factor(Age) + Diploma + Income + Occupation + HomeOwnership + Savings + FinancialAssets, data = data_1985)

ols3 <- lm(NuclearSupportIndex ~ Cesium, data = data_panel, subset = Year == 1986)

stargazer(ols,
  type = "text",
  se = list(sqrt(diag(vcovHC(ols, type = "HC1")))),
  title = "Heteroskedasticity-Robust OLS Regression",
  digits = 3)

stargazer(ols2,
  type = "text",
  se = list(sqrt(diag(vcovHC(ols2, type = "HC1")))),
  title = "Heteroskedasticity-Robust OLS Regression",
  digits = 3)

stargazer(ols3,
  type = "text",
  se = list(sqrt(diag(vcovHC(ols3, type = "HC1")))),
  title = "Heteroskedasticity-Robust OLS Regression",
  digits = 3)

summary(ols3)

data_panel$CesiumZone <- gsub("·", "", data_panel$`Cesium 137`)       # remove dots
data_panel$CesiumZone <- trimws(data_panel$CesiumZone)                # remove extra spaces
data_panel$CesiumZone <- factor(data_panel$CesiumZone, levels = c("Zone 1", "Zone 2", "Zone 3", "Zone 4"))

freq(data_panel$CesiumZone)

ols_panel <- lm(NuclearPlants ~ CesiumZone * Post + Post + code_dep, data = data_panel)

stargazer(ols_panel, type = "text")

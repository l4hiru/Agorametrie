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

#I) Datasets 

data_1985 <- read_sas("Structures de l'opinion (1977 - 1991)/1985/fr.cdsp.ddi.agora1985.sas7bdat")
data_1986 <- read_sas("Structures de l'opinion (1977 - 1991)/1986/fr.cdsp.ddi.agora1986.sas7bdat")


#II) Variables 

#A) Construct more nuclear plants ? 

data_1985$NuclearPlants <- data_1985$c4
data_1986$NuclearPlants <- data_1986$c4

freq(data_1986$NuclearPlants)

#B) Building nuclear power plants was a good thing ? (Retrospective appreciation)

data_1985$RetroNuclearPlants <- data_1985$c168
data_1986$RetroNuclearPlants <- data_1986$c168

freq(data_1986$RetroNuclearPlants)

#C) Control variables

# Gender

data_1985$Women <- ifelse(data_1985$sexe == 2, 1, 0)
data_1986$Women <- ifelse(data_1986$sexe == 2, 1, 0)

freq(data_1986$Women)

# Age in 9 brackets 

data_1985$Age <- data_1985$age9
data_1986$Age <- data_1986$age9

freq(data_1986$Age)

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


freq(data_1986$Diploma)

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

freq(data_1986$Occupation)

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

freq(data_1985$Income) # Warning : 20% N.A
freq(data_1986$Income) # Warning : 19% N.A

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

#D) Departements

freq(data_1985$departement) 
freq(data_1986$departement) 


#III) Regression Analysis

ols <- lm(NuclearPlants ~ Women + as.factor(Age) + Diploma + Income + Occupation + HomeOwnership + Savings + FinancialAssets, data = data_1985)
ols2 <- lm(RetroNuclearPlants ~ Women + as.factor(Age) + Diploma + Income + Occupation + HomeOwnership + Savings + FinancialAssets, data = data_1985)

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
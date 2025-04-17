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

#II) Variables 

#A) Construct more nuclear plants ? 

data_1985$NuclearPlants <- data_1985$c4

freq(data_1985$NuclearPlants)

#B) Building nuclear power plants was a good thing ? (Retrospective appreciation)

data_1985$RetroNuclearPlants <- data_1985$c168

freq(data_1985$RetroNuclearPlants)


#Created 2-22 By Mary As the LME model for Table 1

#install relevant packages
install.packages("readxl")
library(readxl)
install.packages("nlme")
library(nlme)
library(ggplot2)
library(dplyr)
library(knitr)
install.packages("tidyverse")
library(tidyverse)

#import data
metflex <- read_xlsx("Metflex_updated_transformed.xlsx")

#rename variables
metflex <- metflex %>%
  #rename(Insulin = 'Insulin (pg/mL)')
  #rename(carbox_gmin = 'Frayn Carb Oxidation rate (g/min)')
  #rename(fatox_gmin = 'Frayn Fat Oxidation rate (g/min)')
  rename(subject = 'Subject.No')
  #rename(RMR = 'RMR Kcal/min')
  #rename(j_AUC_fatox = 'Johnson Sb[AUC Fat oxidation]')
  #rename(j_AUC_carbox = 'Johnson Su[AUC carb oxidation]')
  
#subset data to just baseline----
metflex_baseline <- metflex %>%
  filter(TIME == 0)

metflex_baseline <- metflex_baseline %>%
  filter(Week == 0)

#counting variables----
TREAT_table <- table(metflex_baseline$TREAT)
TREAT_table

NBC_table <- table(metflex_baseline$NBC)
NBC_table

anyNA(metflex_baseline$RER)
anyNA(metflex_baseline$RMR)
anyNA(metflex_baseline$carbox_gmin)
anyNA(metflex_baseline$fatox_gmin)



#subset data by Diet group----
metflex_TAD <- metflex %>%
  filter(TREAT == "TAD")

metflex_DGA <- metflex %>%
  filter(TREAT == "DGA")

#make NBC a factor----

metflex_baseline$NBC_factor <- factor(metflex_baseline$NBC)
is.factor("NBC_factor")

metflex_baseline$Week <- factor(metflex_baseline$Week)


#lme for table 1----

#set up correlation structure
cor_structure <- corAR1(form = ~ 1 | subject/Week)

#note that all outcome variables used raw data
lme_RMR <- lme('RMR Kcal/min' ~  TIME + NBC * Week ,
               data = metflex, 
               random = ~1 | subject/Week, 
               correlation = cor_structure, 
               na.action = na.omit)

summary(lme_RMR)

lme_RER <- lme(RER ~  TIME + TREAT * Week ,
               data = metflex, 
               random = ~1 | subject/Week, 
               correlation = cor_structure, 
               na.action = na.omit)
summary(lme_RER)

lme_fatox_gmin <- lme(fatox_gmin ~ TIME + TREAT * Week ,
                      data = metflex, 
                      random = ~1 | subject/Week, 
                      correlation = cor_structure, 
                      na.action = na.omit)
summary(lme_fatox_gmin)

lme_carbox_gmin <- lme(carbox_gmin ~ TIME + TREAT * Week ,
                       data = metflex, 
                       random = ~1 | subject/Week, 
                       correlation = cor_structure, 
                       na.action = na.omit)
summary(lme_carbox_gmin)

#get rmr mean by week
mean_by_week <- aggregate(RMR ~ Week, data = metflex, FUN = function(x) c(mean = mean(x), sd = sd(x)))
print(mean_by_week)

summary(metflex_baseline$fatox_gmin)
summary(metflex_baseline$carbox_gmin)

sd(metflex_baseline$fatox_gmin)
sd(metflex_baseline$carbox_gmin)






#lme for insulin test----

metflex_noVB <- metflex %>%
  filter(NBC == "HB" | NBC == "LB")

#rename
metflex_noVB <- metflex_noVB %>%
  #rename(HOMAIR = 'HOMA-IR (>3.67)') %>%
  filter(TIME == 0)

#Make week a factor
metflex_noVB$Week <- factor(metflex_noVB$Week)
is.factor(metflex_noVB$Week)

##set up correlation structure
cor_structure <- corAR1(form = ~ 1 | subject/Week)

#note that all outcome variables used raw data
lme_HOMAIR <- lme(HOMAIR ~  Week * NBC,
               data = metflex_noVB, 
               random = ~1| subject/Week, 
               correlation = cor_structure, 
               na.action = na.omit)

summary(lme_HOMAIR)


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
install.packages("openxlsx")
library(openxlsx)
library(psych)
install.packages("SuppDists")
library(SuppDists)

getwd()
setwd("C:/Users/marya/OneDrive/Documents/Research/Lipid Met Flex")

#import data
metflex <- read_xlsx("Metflex_updated_transformed.xlsx")
hormones <- read_xlsx("Estradiol progesterone and DHEAS.xlsx")
menopause <- read_xlsx("Menopausal status.xlsx")

#Prep Data by matching column names----
#rename group column in menopause
menopause <- menopause %>%
  rename(Group_2 = 'Group')

#rename Week column in metflex
metflex <- metflex %>%
  rename(Week_2 = 'Week of MICE_CART_imputed NEFA.TFA.BC')

hormones <- hormones %>%
  rename(Week_2 = 'Week')

hormones$Week_2 <- sub("Week ", "", hormones$Week_2)


#Merge Datasets----
#Merge menopause
metflex_menopause <- metflex %>%
  left_join(menopause, by= "Subject.No")

#Final Merge
metflex_merged <- merge(metflex_menopause, hormones, by= c("Subject.No", "Week_2"))

#Export and review excel
write.xlsx(metflex_merged, file= "C:/Users/marya/OneDrive/Documents/Research/Lipid Met Flex")


#Testing categorical menopause data----
#filter by baseline
merged_baseline <- metflex_merged %>%
  filter(Week == 0)%>%
  filter(TIME == 0)%>%
  filter(NBC %in% c("HB", "LB"))

#frequency of Menopause status by NBC
freq_bymeno <- table(merged_baseline$NBC, merged_baseline$`DEXA menopausal status`)
print(freq_bymeno)

#Chi sqaured
NBC_meno_chi2 <- chisq.test(freq_bymeno)
print(NBC_meno_chi2)
#error message, checking chi sq assumptions
NBC_meno_chi2$expected
#expected is under 5 so we can't use the chisq test

#fishers exact for when at least one expected value is below 5
fisher.test(freq_bymeno)

#there was no significant difference in pre or post menopause by burner groups


#Testing continuous hormone data----

#rename variables
metflex_merged <- metflex_merged %>%
  #rename(carbox_gmin = 'Frayn Carb Oxidation rate (g/min)')
  #rename(fatox_gmin = 'Frayn Fat Oxidation rate (g/min)')
  #rename(RMR = 'RMR Kcal/min')
  rename(DHEAS = 'DHEAS (ug/dL)') %>%
  rename(BF_burned = '%BF')%>%
  rename(Estradiol = 'Estradiol (pg/mL)') %>%
  rename(Progesterone = 'Progesterone (ng/mL)')

#make hormones dataset numeric
metflex_merged$DHEAS <- as.numeric(metflex_merged$DHEAS)
summary(metflex_merged$DHEAS)
is.numeric(metflex_merged$DHEAS)

metflex_merged$Estradiol <- as.numeric(metflex_merged$Estradiol)
summary(metflex_merged$Estradiol)
is.numeric(metflex_merged$Estradiol)

metflex_merged$Progesterone <- as.numeric(metflex_merged$Progesterone)
summary(metflex_merged$Progesterone)
is.numeric(metflex_merged$Progesterone)

#look at data
describe(merged_baseline$DHEAS)
ggplot(merged_baseline, aes(x = DHEAS, y = BF_burned))+ geom_point() + geom_smooth(method = lm)

describe(merged_baseline$Estradiol)
ggplot(merged_baseline, aes(x = Estradiol, y = BF_burned))+ geom_point() + geom_smooth(method = lm)

describe(merged_baseline$Progesterone)
ggplot(merged_baseline, aes(x = Progesterone, y = BF_burned))+ geom_point() + geom_smooth(method = lm)

#none of them in the raw form demonstrate a linear relationship with the outcome variable (#bf) switching to JMP to try log and Johnson normalization
#normalization looked better with johnson transformation but still couldn't get it to show a linear relationship with %BF

#correlation between x and y
corr_DHEAS <- cor(metflex_merged$BF_burned, metflex_merged$DHEAS, method = "spearman")
print(corr_DHEAS)


#LME Hormones----
#didn't try bc there was not a linear relationship bt x and y but included so the code was here
#filter data
merged_filtered_time <- metflex_merged %>%
  filter(TIME == 0) %>%
  filter(NBC %in% c("HB", "LB"))

#set up correlation structure
cor_structure <- corAR1(form = ~ 1 | Subject.No/Week_2)

#run LME
lme_RMR <- lme(RMR ~  Week_2 + NBC * DHEAS,
               data = merged_filtered_time, 
               random = ~1 | Subject.No/Week_2, 
               correlation = cor_structure, 
               na.action = na.omit)

summary(lme_RMR)

#LME insulin resistance ----
#rename
metflex_merged <- metflex_merged %>%
  rename(homa = 'MCT HOMA-IR')%>%
  rename(subject = Subject.No)
  
#run LME
cor_structure <- corAR1(form = ~ 1 | subject/Week)

lme_Homair <- lme(homa ~  TIME + NBC * Week,
               data = metflex_merged, 
               random = ~1 | subject/Week, 
               correlation = cor_structure, 
               na.action = na.omit)

summary(lme_Homair)

describe(metflex_merged$'MCT HOMA-IR')


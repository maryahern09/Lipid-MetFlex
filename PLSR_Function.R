#November 2023 PLSR created by Mary

#load relevant packages
install.packages("pls")
library(pls)
install.packages("tidyverse")
library(tidyverse)
install.packages("read.xl")
library(readxl)
install.packages("writexl")
library(writexl)

#Import datasets ----

excel_filepath <- "C:/Users/marya/OneDrive/Documents/Research/Lipid Met Flex/Absorption, elimination and AUC for lipidomic data FL95.xlsx"
data_absorption <- read_excel(excel_filepath, sheet = "Absorption")
data_AUC <- read_excel(excel_filepath, sheet = "AUC")
data_elimination <- read_excel(excel_filepath, sheet = "Elimination")


#Set up Data ----

data_absorption <- data_absorption %>%
  rename(pct_burned = `% burned...5`)
data_AUC <- data_AUC %>%
  rename(pct_burned = `% burned...5`)
data_elimination <- data_elimination %>%
  rename(pct_burned = `% burned...5`)

#check
head(data_absorption)
head(data_AUC)
head(data_elimination)

#subset with just the metabolites
data_absorption_subset <- data_absorption %>%
  select("pct_burned", contains(c("NEFA", "BC.", "TFA")), starts_with("C")) 

# PLSR  ----

#determine number of components 
pls_fit = plsr(data_absorption_subset$pct_burned ~., data = data_absorption_subset, 
              scale = TRUE, validation = "LOO")

summary(pls_fit)
## select lowest RMSEP

# Dataset training and testing ----
dataset <- data_absorption_subset
train <- dataset[1:131,] 
y_test <- dataset[132, "pct_burned"] 
test <- dataset[132, -which(names(dataset) == "pct_burned") ]

#Model ----
model <- plsr(pct_burned ~ ., ncomp = 10, data = data_absorption_subset, scale=TRUE, validation="LOO")

#plsr_pred = predict(model, test, ncomp= 10)
  
# next steps 
# export to an excel then re-import then model in a plot by the categorical variables 

#export model to excel
# Extract model coefficients
coefficients <- coef(model)

# Create a data frame from the coefficients
coefficients_df <- data.frame(variable = names(coefficients), coefficient = coefficients)

# Specify the Excel file path
excel_file_path <- "path/to/your/exported_model.xlsx"

# Write the data frame to Excel
write_xlsx(coefficients_df, path = excel_file_path)

plot(RMSEP(model_2))
plot(RMSEP(pls_fit))

model_2 <- plsr(pct_burned ~ ., ncomp = 110, data = data_absorption_subset, scale=TRUE, validation="LOO")



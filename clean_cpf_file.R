library(readxl)
library(data.table)
library(stringr)
library(dplyr)
library(plotly)
library(ggplot2)

# read in data
cpf_data_2023_4 <- read_excel("/Users/AGaffney/Documents/CFP excel/CPF Study Data 2023_copy_2025_April.xlsx", sheet = "2023-24")
cpf_data_2025 <- read_excel("/Users/AGaffney/Documents/CFP excel/CPF Study Data 2023_copy_2025_April.xlsx", sheet = "2025")
cpf_data <- rbind(cpf_data_2023_4,cpf_data_2025)
cpf_data <- as.data.table(cpf_data)
str(cpf_data)

# tidy data
cpf_data <- cpf_data %>%
  mutate(
    Consent = as.factor(Consent),
    `Consent Month` = factor(`Consent Month`, levels = month.abb),
    Weight = as.numeric(gsub("kg|g","",tolower(Weight))),
    `HRCP Diagnosis` = as.factor(`HRCP Diagnosis`),
    `HRCP Diagnosis Date` = as.Date(`HRCP Diagnosis Date`),
    `CP Diagnosis Date` = as.Date(`CP Diagnosis Date`),
    `CP Diagnosis` = as.factor(`CP Diagnosis`),
    


  )






p <- ggplot(cpf_data, aes(x = Weight)) +
  geom_histogram() +
  ylab("Weight")
ggplotly(p)

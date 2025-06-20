library(data.table)
library(readxl)
library(dplyr)
library(lubridate)

# load cpf_data using clean_cpf_file.Rmd
q_data <- read.csv("/Users/AGaffney/Documents/Questionnaires/Data/questionnaire_june.csv")
q_data <- as.data.table(q_data)

# remove 19- from record id from redcap
q_data[, record_id := gsub("19-", "", record_id)]

cpf_data[, cp_diagnosis_date := as.Date(cp_diagnosis_date, format = "%m/%d/%Y")]

###########################################
## Check for CP Diagnosis > 3 Months Ago #
##               &&                      #
## Questionnaire not carried out already #
###########################################

send_questionnaire <- cpf_data %>%
  mutate(study_number = as.character(study_number)) %>%  # Convert Study number to character
  left_join(q_data, by = c("study_number" = "record_id")) %>% 
  filter(cp_diagnosis_date <= as.Date(Sys.Date() %m-% months(3))) %>% # Filter those who's date diagnosis is greater than or equal to 3 months from now
  filter(!(parental_questionnaire_complete == 2)) %>% # filter those who have already completed 
  select("study_number", "name", "mrn")

send_questionnaire

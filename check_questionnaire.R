library(data.table)
library(readxl)
library(dplyr)
library(lubridate)

cpf_data_2023_4 <- read_excel("/Users/AGaffney/Documents/CFP excel/CPF Study Data 2023_copy_2025_April.xlsx", sheet = "2023-24")
cpf_data_2025 <- read_excel("/Users/AGaffney/Documents/CFP excel/CPF Study Data 2023_copy_2025_April.xlsx", sheet = "2025")
cpf_data <- rbind(cpf_data_2023_4,cpf_data_2025)
cpf_data <- as.data.table(cpf_data)
q_data <- read.csv("/Users/AGaffney/Documents/Questionnaires/Data/questionnaires_q2_2025.csv")

cpf_data <- as.data.table(cpf_data)
q_data <- as.data.table(q_data)

# remove 19- from record id from redcap
q_data[, record_id := gsub("19-", "", record_id)]

cpf_data[, `CP Diagnosis Date` := as.Date(`CP Diagnosis Date`, format = "%m/%d/%Y")]

###########################################
## Check for CP Diagnosis > 3 Months Ago #
##               &&                      #
## Questionnaire not carried out already #
###########################################

send_questionnaire <- cpf_data %>%
  mutate(`Study number` = as.character(`Study number`)) %>%  # Convert Study number to character
  left_join(q_data, by = c("Study number" = "record_id")) %>% 
  filter(`CP Diagnosis Date` <= as.Date(Sys.Date() %m-% months(3))) %>% # Filter those who's date diagnosis is greater than or equal to 3 months from now
  filter(!(parental_questionnaire_complete == 2)) %>% # filter those who have already completed 
  select("Study number", "Name", "MRN")

send_questionnaire

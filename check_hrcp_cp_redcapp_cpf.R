library(readxl)

redcap_data <- read.csv("/Users/AGaffney/Documents/CFP excel/Data/2025_q2/redcap_hrcp_2025_q2.csv")
cpf_data <- read_excel("/Users/AGaffney/Documents/CFP excel/Data/2025_q2/data_q2_2025.xlsx", sheet = "2023-24")

redcap_data <- as.data.table(redcap_data)
cpf_data <- as.data.table(cpf_data)

# remove 19- from record id from redcapp
redcap_data[, record_id := gsub("19-", "", record_id)]

# CP Diagnosis
redcap_data[general_cp == 1,]
cpf_data[`CP Diagnosis` == 'Yes',]

# HRCP Diagnosis
redcap_data[general_hr == 1,]

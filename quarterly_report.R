cpf_data_2023_4 <- read_excel("/Users/AGaffney/Documents/CPF excel/CPF Study Data 2023_copy_2025_June.xlsx", sheet = "2023-24")
cpf_data_2025 <- read_excel("/Users/AGaffney/Documents/CPF excel/CPF Study Data 2023_copy_2025_June.xlsx", sheet = "2025")
cpf_data <- rbind(cpf_data_2023_4,cpf_data_2025)
cpf_data <- as.data.table(cpf_data)
setnames(cpf_data, old = colnames(cpf_data), new = c(
  "study_number",
  "consent",
  "consent_comment",
  "consent_month",
  "consent_year",
  "name",
  "dob",
  "mrn",
  "ga_at_birth",
  "birth_weight",
  "hrcp_diagnosis",
  "hrcp_diagnosis_date",
  "cga_hrcp_diagnosis",
  "cp_diagnosis",
  "cp_diagnosis_date",
  "cga_cp_diagnosis",
  "diagnosis_comments",
  "crus",
  "gma_writhing_age",
  "gma_fidgety_age",
  "aims_3m",
  "hine_3m",
  "physio_3m_date",
  "consultant_3m_visit",
  "hine_6m",
  "aims_6m",
  "physio_6m",
  "consultant_6m",
  "consultant_meeting_yn",
  "physio_9m_visit",
  "hine_9m",
  "aims_9m",
  "consultant_9m_visit",
  "hine_12m",
  "aims_12m",
  "cdnt"
))
cpf_data[, `:=`(
  consent = as.factor(consent),
  consent_month = factor(consent_month, levels = month.abb),
  birth_weight = as.numeric(gsub("kg|g", "", tolower(birth_weight))),
  hrcp_diagnosis = as.factor(hrcp_diagnosis),
  hrcp_diagnosis_date = as.Date(hrcp_diagnosis_date),
  cp_diagnosis = as.factor(cp_diagnosis),
  cp_diagnosis_date = as.Date(cp_diagnosis_date)
)]


#1 consented this quarter
# Q2 2025
cpf_data_qr <- cpf_data[consent_month %in% c("Apr", "May", "June") &
                          consent_year == 2025 & 
                          consent == "Yes"]


# number consented this quarter
nrow(cpf_data_qr)

# Prepare and copy to clipboard for easy pasting to word
write.table(
  cpf_data_qr[, .(Name = name, MRN = mrn)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

#2 total consented to date
total_consented <- cpf_data[consent == "Yes"]

# total number consented
nrow(total_consented)

write.table(
  total_consented[, .(Name = name, MRN = mrn, `Consent Comment` = consent_comment)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

total_consented_no_sn <- total_consented[is.na(study_number),]

write.table(
  total_consented_no_sn[, .(Name = name, MRN = mrn)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

#3 total families with forms
total_has_forms <- cpf_data[consent == "Has Forms"]

nrow(total_has_forms)

write.table(
  total_has_forms[, .(Name = name, MRN = mrn)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

#4 total declined 
total_declined <- cpf_data[consent == "Declined"]

nrow(total_declined)

write.table(
  total_declined[, .(Name = name, MRN = mrn)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

#5 GM performed in the NICU
gm_nicu <- cpf_data_qr[!is.na(gma_writhing_age)]

nrow(gm_nicu)

write.table(
  gm_nicu[, .(Name = name, MRN = mrn, `GMA Writhing Age` = gma_writhing_age)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

#6 babies with CS




#7 HRCP quarter
hrcp_qr <- cpf_data[hrcp_diagnosis == "Yes" & format(hrcp_diagnosis_date, "%m") %in% c("04", "05", "06")]

write.table(
  hrcp_qr[, .(Name = name, MRN = mrn)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

#8 Age at hrcp diag
hrcp_qr[, dob := as.Date(dob)]
# Extract weeks and days from "40+4"
hrcp_qr[, c("ga_weeks", "ga_days") := tstrsplit(ga_at_birth, "\\+", fixed = FALSE)]

# Convert to numeric
hrcp_qr[, `:=`(
  ga_weeks = as.integer(ga_weeks),
  ga_days = as.integer(ga_days)
)]

# 1. Fractional GA at birth in weeks
hrcp_qr[, ga_fractional_weeks := ga_weeks + ga_days / 7]

# 2. Chronological age in weeks
hrcp_qr[, chronological_age_weeks := as.numeric(hrcp_diagnosis_date - dob) / 7]

# 3. Total gestational age at diagnosis (weeks & months)
hrcp_qr[, total_ga_weeks := ga_fractional_weeks + chronological_age_weeks]
hrcp_qr[, total_ga_months := total_ga_weeks / 4.345]

# 4. Corrected age since birth (prematurity-adjusted) in weeks & months
hrcp_qr[, corrected_age_weeks := chronological_age_weeks - (40 - ga_fractional_weeks)]
hrcp_qr[, corrected_age_months := corrected_age_weeks / 4.345]

# Optional: Round nicely
hrcp_qr[, `:=`(
  total_ga_weeks = round(total_ga_weeks, 2),
  total_ga_months = round(total_ga_months, 2),
  corrected_age_weeks = round(corrected_age_weeks, 2),
  corrected_age_months = round(corrected_age_months, 2)
)]

hrcp_qr$corrected_age_weeks

# 5. cp diagnosis internal pathway
cp_qr <- cpf_data[cp_diagnosis == "Yes" & format(cp_diagnosis_date, "%m") %in% c("04", "05", "06")]

nrow(cp_qr)

write.table(
  cp_qr[, .(Name = name, MRN = mrn)],
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

# conversion HRCP - CP
hrcp_cp_conversion <- cpf_data[cp_diagnosis == "Yes" &
                                 hrcp_diagnosis == "Yes & " &
                                 format(cp_diagnosis_date, "%m") %in% c("04", "05", "06")]

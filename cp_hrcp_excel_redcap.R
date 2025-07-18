cp_hrcp_redcap <- read.csv("/Users/AGaffney/Documents/CPF excel/CerebralPalsyEDI-HRCPAndCPAge_DATA_2025-07-08_1041.csv")
cp_hrcp_redcap <- as.data.table(cp_hrcp_redcap)

# remove 19- from record id from redcap
cp_hrcp_redcap[, record_id := gsub("19-", "", record_id)]

# Compare excel cp to redcap
cp_excel <- cpf_data[!is.na(study_number) & cp_diagnosis == "Yes"]
cp_redcap <- cp_hrcp_redcap[general_cp == 1]

# Find values in cp_hrcp_redcap not in cpf_data
cp_redcap[!record_id %in% cp_excel$study_number, record_id]

# compare HRCPCP sheet to excel and redcap
# Join to add study_number from cp_excel to hrcpcp_updated by MRN
setnames(hrcpcp_updated, "MRN", "mrn")
# Convert the CP and HRCP column to lowercase
hrcpcp_updated[, HRCP := tolower(HRCP)]
hrcpcp_updated[, CP := tolower(CP)]

hrcpcp_updated[cpf_data[, .(mrn, study_number)], 
               study_number := i.study_number, 
               on = "mrn"]

cp_hrcpcp_updated <- hrcpcp_updated[!is.na(study_number) & CP == "yes"]

cp_excel[!study_number %in% cp_hrcpcp_updated$study_number, study_number]
hrcpcp_updated[!study_number %in% hrcpcp_updated$study_number, study_number]

################
# Compare HRCP #
################

hrcp_excel <- cpf_data[!is.na(study_number) & hrcp_diagnosis == "Yes"]
hrcp_redcap <- cp_hrcp_redcap[general_hr == 1]

# Find values in cp_hrcp_redcap not in cpf_data
hrcp_redcap[!record_id %in% hrcp_excel$study_number, record_id]

hrcp_hrcpcp_updated <- hrcpcp_updated[!is.na(study_number) & HRCP == "yes"]

################################################################################
#### HRCP: Mean CGA at Diagnosis (in months) ####

# Step 1: Extract year from diagnosis date
hrcp_excel[, hrcp_year := year(hrcp_diagnosis_date)]

# Step 2: Convert "weeks+days" (e.g. "56+3") to numeric weeks, then to months
hrcp_excel[!is.na(cga_hrcp_diagnosis),
           cga_hrcp_months := {
             parts <- tstrsplit(cga_hrcp_diagnosis, "\\+")
             weeks <- as.numeric(parts[[1]])
             days <- as.numeric(parts[[2]])
             total_weeks <- weeks + days / 7
             total_weeks / 4.345
           }]

# Step 3: Mean by year
hrcp_mean_by_year <- hrcp_excel[!is.na(cga_hrcp_months),
                                .(mean_cga_months = round(mean(cga_hrcp_months), 2)),
                                by = hrcp_year]

# Step 4: Total mean
hrcp_mean_total <- hrcp_excel[!is.na(cga_hrcp_months),
                              .(hrcp_year = "Total", mean_cga_months = round(mean(cga_hrcp_months), 2))]

# Combine
hrcp_mean_cga <- rbind(hrcp_mean_by_year, hrcp_mean_total, fill = TRUE)

print(hrcp_mean_cga)


#### CP: Mean CGA at Diagnosis (in months) ####

# Step 1: Extract year from CP diagnosis date
cp_excel[, cp_year := year(cp_diagnosis_date)]

# Step 2: Convert "weeks+days" to months
cp_excel[!is.na(cga_cp_diagnosis),
         cga_cp_months := {
           parts <- tstrsplit(cga_cp_diagnosis, "\\+")
           weeks <- as.numeric(parts[[1]])
           days <- as.numeric(parts[[2]])
           total_weeks <- weeks + days / 7
           total_weeks / 4.345
         }]

# Step 3: Mean by year
cp_mean_by_year <- cp_excel[!is.na(cga_cp_months),
                            .(mean_cga_months = round(mean(cga_cp_months), 2)),
                            by = cp_year]

# Step 4: Total mean
cp_mean_total <- cp_excel[!is.na(cga_cp_months),
                          .(cp_year = "Total", mean_cga_months = round(mean(cga_cp_months), 2))]

# Combine
cp_mean_cga <- rbind(cp_mean_by_year, cp_mean_total, fill = TRUE)

print(cp_mean_cga)

#### HRCP Counts ####
hrcp_counts_year <- hrcp_excel[!is.na(hrcp_year),
                               .(count_hrcp = .N),
                               by = hrcp_year]

hrcp_counts_total <- hrcp_excel[!is.na(hrcp_diagnosis_date),
                                .(hrcp_year = "Total", count_hrcp = .N)]

hrcp_counts <- rbind(hrcp_counts_year, hrcp_counts_total, fill = TRUE)
print(hrcp_counts)


#### CP Counts ####
cp_counts_year <- cp_excel[!is.na(cp_year),
                           .(count_cp = .N),
                           by = cp_year]

cp_counts_total <- cp_excel[!is.na(cp_diagnosis_date),
                            .(cp_year = "Total", count_cp = .N)]

cp_counts <- rbind(cp_counts_year, cp_counts_total, fill = TRUE)
print(cp_counts)

#### Enrolment COunts ####

# Filter: must have study_number and consent_year
enrolled <- cpf_data[!is.na(study_number) & !is.na(consent_year)]

# Count per consent_year
enrol_counts_year <- enrolled[, .(count_enrolled = .N), by = consent_year]

# Add total
enrol_counts_total <- enrolled[, .(consent_year = "Total", count_enrolled = .N)]

# Combine
enrol_counts <- rbind(enrol_counts_year, enrol_counts_total, fill = TRUE)

print(enrol_counts)

library(readxl)
library(dplyr)
library(tibble)
library(janitor)
library(lubridate)

neonate_data <- read_excel("/Users/AGaffney/Documents/Quarterly report/01.04.2025  30.06.2025paeds11 opd clinic activity.xls", sheet = "Sheet1")
physio_data <- read_excel("/Users/AGaffney/Documents/Quarterly report/Physio  01.04.2025 30.06.2025.xls", sheet = "Sheet1")
# load cpf_data using clean_cpf_file.Rmd

View(neonate_data)
View(physio_data)

# remove first x rows of data - NA
neonate_data <- tail(neonate_data, -7)
physio_data <- tail(physio_data, -2)

# first row to colnames
neonate_data <- neonate_data %>%
  row_to_names(row_number = 1)

physio_data <- physio_data %>%
  row_to_names(row_number = 1)

# Rename columns if they are duplicates
colnames(neonate_data) <- make.names(colnames(neonate_data), unique = TRUE)
colnames(physio_data) <- make.names(colnames(physio_data), unique = TRUE)

# Format dates
# Appointment Date
## Neonate
neonate_data$Appointment.Date <- as.numeric(neonate_data$Appointment.Date)
neonate_data$Appointment.Date <- as.POSIXct(neonate_data$Appointment.Date * 86400, 
                                            origin = "1899-12-30", 
                                            tz = "Europe/Dublin")
neonate_data$Appointment.Date <- format(neonate_data$Appointment.Date, "%d/%m/%Y %H:%M:%S")

## Physio
physio_data$Appointment.Date <- as.numeric(physio_data$Appointment.Date)
physio_data$Appointment.Date <- as.POSIXct(physio_data$Appointment.Date * 86400, 
                                            origin = "1899-12-30", 
                                            tz = "Europe/Dublin")
physio_data$Appointment.Date <- format(physio_data$Appointment.Date, "%d/%m/%Y %H:%M:%S")

# DOB
## Neonate
neonate_data$Date.of.Birth <- as.character(neonate_data$Date.of.Birth) # Ensure it's character
neonate_data$Date.of.Birth <- trimws(neonate_data$Date.of.Birth)  # Remove leading/trailing spaces
neonate_data$Date.of.Birth <- suppressWarnings(as.numeric(neonate_data$Date.of.Birth))
sum(is.na(neonate_data$Date.of.Birth))  # Count number of NA values
neonate_data$Date.of.Birth <- as.Date(neonate_data$Date.of.Birth, origin = "1899-12-30")
neonate_data$Date.of.Birth <- format(neonate_data$Date.of.Birth, "%d/%m/%Y")

## DOB
physio_data$DOB <- as.Date(as.numeric(physio_data$DOB), origin = "1899-12-30")

# ############# Try without consent first to see if I get same numbers
# ####### FOR PHYSIO APPT BEING WITHIN 12-20 WEEK CGA AT APPT
# physio_data_12_20 <- physio_data %>% filter(CGA.WEEKS >= 12 & CGA.WEEKS <= 20)
# length(unique(physio_data_12_20$mrn))
# 
# ####### FOR NEONATE APPT BEING WITHIN 12-20 WEEK CGA AT APPT
# neonate_data_12_20 <- neonate_data %>% filter(CGA.Weeks >= 12 & CGA.Weeks <= 20)
# length(unique(neonate_data_12_20$mrn))

##############

# Get consented infants
consented_infants <- cpf_data[!is.na(cpf_data$study_number), ]
consented_infants <- consented_infants %>% select(study_number,mrn) %>% arrange(study_number)
length(unique(consented_infants$mrn))


# tolower names so they match
colnames(neonate_data) <- tolower(colnames(neonate_data))
colnames(physio_data) <- tolower(colnames(physio_data))

## Neonate consented
neonate_consented <- neonate_data %>% inner_join(consented_infants, by = "mrn")
length(unique(neonate_data$mrn))
length(unique(neonate_consented$mrn))

## Physio consented
physio_consented <- physio_data %>% inner_join(consented_infants, by = "mrn")
length(unique(physio_data$mrn))
length(unique(physio_consented$mrn))

# Take only columns of importance - multiple with same column names
neonate_sub <- neonate_consented %>% select(mrn, name, date.of.birth, cga.weeks, study_number)
neonate_sub$cga.weeks <- as.numeric(neonate_sub$cga.weeks)

physio_sub <- physio_consented %>% select(mrn, name, dob, cga.weeks, study_number)
physio_sub$cga.weeks <- as.numeric(physio_sub$cga.weeks)

physio_sub %>% filter(cga.weeks >= 12 & cga.weeks <= 20)
neonate_sub %>% filter(cga.weeks >= 12 & cga.weeks <= 20)

# setnames for the join
setnames(physio_sub, old = c("cga.weeks"), new = c("cga.weeks.physio"))
setnames(neonate_sub, old = c("cga.weeks"), new = c("cga.weeks.neonate"))

# Add a flag for "In Neonate" in the neonate dataframe
neonate_sub <- neonate_sub %>%
  mutate(flag_neonate = "1")

# Add a flag for "In Physio" in the physio dataframe
physio_sub <- physio_sub %>%
  mutate(flag_physio = "1")

# Perform a full join on MRN and merge the flags
joined_sub <- full_join(neonate_sub, physio_sub, by = "mrn") %>%
  mutate(
    # Create a final flag based on the presence of flags in both datasets
    Flag = case_when(
      flag_neonate == "1" & !is.na(flag_physio) ~ "In both",  # If flag is "1" in both
      flag_neonate == "1" & is.na(flag_physio) ~ "Only in Neonate",  # If only in Neonate
      is.na(flag_neonate) & flag_physio == "1" ~ "Only in Physio",  # If only in Physio
      TRUE ~ "Not in either"  # Shouldn't occur as it's a full join
    )
  )

# # Filter for both appts being 12-20 weeks CGA
# joined_sub_12_20 <- joined_sub %>% 
#   filter((CGA.Weeks >= 12 & CGA.Weeks <= 20) & (CGA.WEEKS >= 12 & CGA.WEEKS <= 20))
# length(unique(joined_sub_12_20$mrn))


####### FOR JUST PHYSIO APPT BEING WITHIN 12-20 WEEK CGA AT APPT
physio_CGA_12_20 <- joined_sub %>% filter(cga.weeks.physio >= 12 & cga.weeks.physio <= 20)
length(unique(physio_CGA_12_20$mrn))

physio_CGA_12_20 %>%
  filter(Flag == 'In both') %>%
  pull(mrn) %>%
  unique() %>%
  length()

physio_CGA_12_20 %>%
  filter(Flag == 'In both') %>%
  pull(mrn) %>%
  unique() 

####### FOR JUST NEONATE APPT BEING WITHIN 12-20 WEEK CGA AT APPT
neonate_CGA_12_20 <- joined_sub %>% filter(cga.weeks.neonate >= 12 & cga.weeks.neonate <= 20)
length(unique(neonate_CGA_12_20$mrn))

neonate_CGA_12_20 %>%
  filter(Flag == 'In both') %>%
  pull(mrn) %>%
  unique() %>%
  length()

neonate_CGA_12_20 %>%
  filter(Flag == 'In both') %>%
  pull(mrn) %>%
  unique() 

neonate_CGA_12_20 %>%
  filter(Flag == 'Only in Neonate') %>%
  pull(mrn) %>%
  unique() 

###### FOR EITHER NEONATE OR PHYSIO BEING WITHIN 12-20 WEEK CGA AT APPT
joined_sub_filter_age <- joined_sub %>%
  filter((cga.weeks.physio >= 12 & cga.weeks.physio <= 20) | (cga.weeks.neonate >= 12 & cga.weeks.neonate <= 20))
length(unique(joined_sub_filter_age$mrn))

physio_joined_sub_filter_age <- joined_sub_filter_age %>%
  filter(Flag %in% c("In both", "Only in Physio"))
length(unique(physio_joined_sub_filter_age$mrn))

physio_joined_sub_filter_age %>% select(study_number.x,study_number.y,name.x,name.y,mrn) %>% distinct()

neonate_joined_sub_filter_age <- joined_sub_filter_age %>%
  filter(Flag %in% c("In both", "Only in Neonate"))
length(unique(physio_joined_sub_filter_age$mrn))

neonate_joined_sub_filter_age %>% select(study_number.x,study_number.y,name.x,name.y,mrn) %>% distinct()

#################################################
#   Calculate how many should be seen in Q      #
#   12-16 weeks CGA for infants under 4 months  #
#################################################

## PHYSIO
# Define Q1 2025 date range
q1_start <- as.Date("2025-04-01")
q1_end   <- as.Date("2025-06-30")

# Look at all first - not just consented
# DOB format
#cpf_data$dob <- as.Date(as.numeric(cpf_data$dob), origin = "1899-12-30")

# format gestational age - get rid of days
cpf_data$GA_Weeks <- sub("\\+.*", "", cpf_data$ga_at_birth)
cpf_data$GA_Weeks <- as.numeric(cpf_data$GA_Weeks)

# Step 1: Calculate corrected age offset (how many weeks premature)
CGA_cal <- cpf_data %>%
  mutate(
    prematurity_offset_weeks = 40 - GA_Weeks,
    
    # Step 2: Calculate the start and end of the 12–16 week corrected age window
    CGA_12w_date = dob + weeks(prematurity_offset_weeks + 12),
    CGA_16w_date = dob + weeks(prematurity_offset_weeks + 16),
    
    # Step 3: Check if this range overlaps with Q1 2025
    in_Q1_2025 = (CGA_12w_date <= q1_end) & (CGA_16w_date >= q1_start)
  )

# Step 4: Filter infants whose 12–16 week corrected age window overlaps with quarter window
infants_q1_12_16_cga <- CGA_cal %>% filter(in_Q1_2025)

# View the results
infants_q1_12_16_cga

# Now check to see how many of these are consented
infants_q1_12_16_cga_consented <- infants_q1_12_16_cga %>% filter(!is.na(study_number))

length(unique(infants_q1_12_16_cga_consented$mrn))

q1_12_16_cga_consented_mrn <- unique(infants_q1_12_16_cga_consented$mrn)
physio_CGA_12_20_mrn <- unique(physio_CGA_12_20$mrn)

# In both
intersect(q1_12_16_cga_consented_mrn, physio_CGA_12_20_mrn)

# only in physio - seen at 2 and 7 weeks neonate
setdiff(physio_CGA_12_20_mrn, q1_12_16_cga_consented_mrn)

# only in cpf - haven't seen physio
setdiff(q1_12_16_cga_consented_mrn, physio_CGA_12_20_mrn)


## NEONATE
neonate_CGA_12_20_mrn <- unique(neonate_CGA_12_20$mrn)

# In both
intersect(q1_12_16_cga_consented_mrn, neonate_CGA_12_20_mrn)

# only in neonate 
setdiff(neonate_CGA_12_20_mrn, q1_12_16_cga_consented_mrn)

# only in cpf - haven't seen neonate
setdiff(q1_12_16_cga_consented_mrn, neonate_CGA_12_20_mrn)


############################
## COMPARE TO PHYSIO LIST ##
############################

physio_list <- data.frame(
  Name = c(
  ),
  mrn = c(
  ),
  stringsAsFactors = FALSE
)

# In both
intersect(physio_CGA_12_20$mrn, physio_list$mrn)

# Only in physio list
setdiff(physio_list$mrn, physio_CGA_12_20$mrn)

# Only in my list
setdiff(physio_CGA_12_20$mrn, physio_list$mrn)


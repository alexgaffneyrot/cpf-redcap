# At the time the baby is discharged from hospital, 
# how mature are they (in weeks and days)?"
# What this function does:
# 1. Takes the baby's gestational age at birth.
# 2. Adds the number of days that have passed since birth.
# 3. Tells you how many weeks+days old the baby would be gestationally now.

calculate_cga_discharge <- function(dob, ga_weeks, ga_days, discharge_date) {
  # Convert input dates
  dob <- as.Date(dob, format = "%d/%m/%Y")
  discharge_date <- as.Date(discharge_date, format = "%d/%m/%Y")
  
  # Total days since birth
  days_since_birth <- as.numeric(discharge_date - dob)
  
  # GA at birth in days
  ga_at_birth_days <- (ga_weeks * 7) + ga_days
  
  # Total CGA in days
  cga_total_days <- ga_at_birth_days + days_since_birth
  
  # Convert to weeks + days
  cga_weeks <- cga_total_days %/% 7
  cga_days <- cga_total_days %% 7
  
  # Convert to decimal weeks (rounded to 2 decimal places)
  cga_decimal_weeks <- round(cga_total_days / 7, 2)
  
  return(list(
    text_format = paste0(cga_weeks, "+", cga_days),
    decimal_format = cga_decimal_weeks
  ))
}

## Define the dob, dischage dates + GA 
dob <- as.Date("", format="%d/%m/%Y")
discharge <- as.Date("", format="%d/%m/%Y")
ga_weeks <- 42
ga_days <- 2

## Chronological Age @ Discharge
# Calculate the difference in days
diff_days <- as.numeric(difftime(discharge, dob, units = "days"))

# Convert the difference in days to weeks (as a decimal)
diff_weeks <- diff_days / 7

# Print the result
## Chronological Age @ Discharge/Appt
print(diff_weeks)

## Gestational Age (under 40 wks still)
calculate_cga_discharge(dob,ga_weeks,ga_days,discharge)




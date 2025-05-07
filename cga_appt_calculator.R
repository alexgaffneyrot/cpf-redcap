# At the time of a follow-up appointment (like HRCP or CP assessment), 
# how old is the baby, compared to when they were supposed to be born?"
# What this function does:
# 1. Estimates the original due date (since they were born early).
# 2. Measures time from due date to appointment date.
# 3. Gives corrected age for developmental milestones.

calculate_cga <- function(dob, ga_weeks, ga_days, appointment_date) {
  # Convert input dates from "dd/mm/yyyy" format to Date objects
  dob <- as.Date(dob, format = "%d/%m/%Y")
  appointment_date <- as.Date(appointment_date, format = "%d/%m/%Y")
  
  # GA at birth in days
  ga_at_birth_days <- (ga_weeks * 7) + ga_days
  
  # Due date = DOB + (280 - GA at birth in days)
  due_date <- dob + (280 - ga_at_birth_days)
  
  # CGA = appointment date - due date
  cga_total_days <- as.numeric(appointment_date - due_date)
  
  # Convert to weeks + days
  cga_weeks <- cga_total_days %/% 7
  cga_days <- cga_total_days %% 7
  
  # Convert to months (approx)
  cga_months <- round(cga_total_days / 30.44, 2)  # 30.44 is average days/month
  
  
  cga_weeks_days_months <- list(
      CGA_weeks_days = paste0(cga_weeks, "+", cga_days),
      CGA_months = cga_months
    )
  
  return(cga_weeks_days_months)
}

# dob, weeks, days, appointment date
calculate_cga("22/10/2023",35,4,"21/02/2024")



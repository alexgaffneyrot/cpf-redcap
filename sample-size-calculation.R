### R Functions for Sample Size Calculation in Clinical Study

# 1 EEG Longitudinal Analysis - Mixed Effects Model
# This function calculates the required sample size for EEG analysis using longitudinal mixed-effects models.
# Parameters:
# - effect_size: The expected effect size (Cohen's d)
# - power: Desired statistical power (default is 80%)
# - alpha: Significance level (default is 5%)
# - num_measurements: Number of repeated measurements (default is 5)
# - dropout_rate: Expected dropout rate (default is 10%)
effect_size = 0.4
power = 0.95
alpha = 0.05
num_measurements = 6
dropout_rate = 0.35

sample_size_longitudinal <- function(effect_size, power, alpha, num_measurements, dropout_rate) {
  library(longpower)
  model <- lmmpower(effect = effect_size, t = num_measurements, power = power, alpha = alpha) # lmmpower function is specifically designed for longitudinal studies, allowing us to calculate power and sample size by modeling time-dependent trajectories.
  adjusted_n <- ceiling(model + model * dropout_rate) # round up to ensure full participant covarage
  return(adjusted_n)
}
sample_size_longitudinal(effect_size, power, alpha, num_measurements, dropout_rate)

# 2️ Neuro-specific Protein Analysis (Two-group comparison)
# This function estimates the sample size required to detect differences in mean neuro-specific protein levels.
# Parameters:
# - mean_diff: The expected mean difference between groups
# - sd: The standard deviation of the measurements
# - power: Desired statistical power (default is 80%)
# - alpha: Significance level (default is 5%)

sample_size_protein <- function(mean_diff = 0.3, sd = 0.5, power = 0.95, alpha = 0.05) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(power)
  n_per_group <- ((z_alpha + z_beta)^2 * (2 * sd^2)) / (mean_diff^2)
  return(ceiling(n_per_group))
}


# 3️ Executive Function Longitudinal Analysis
# This function calculates the required sample size for analyzing executive function trajectories over time.
# Parameters:
# - effect_size: The expected effect size (Cohen's d)
# - power: Desired statistical power (default is 80%)
# - alpha: Significance level (default is 5%)
# - num_timepoints: Number of repeated measurements (default is 6)

sample_size_exec_function <- function(effect_size, power = 0.8, alpha = 0.05, num_timepoints = 6) {
  library(longpower)
  model <- lmmpower(effect = effect_size, t = num_timepoints, power = power, alpha = alpha)
  return(ceiling(model))
}


# 4️ Machine Learning Validation
# This function estimates the required sample size for validating a machine learning model.
# Parameters:
# - num_features: The number of features in the model
# - effect_size: The expected effect size (Cohen's d)
# - power: Desired statistical power (default is 80%)
# - alpha: Significance level (default is 5%)

sample_size_ml <- function(num_features, effect_size, power = 0.8, alpha = 0.05) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(power)
  min_samples <- ((z_alpha + z_beta)^2 * (1 + num_features) / effect_size^2)
  return(ceiling(min_samples))
}


# 5️ Ophthalmological Biomarker Detection (Proportion Test)
# This function estimates the sample size required to detect differences in biomarker presence (gaze fixation, morphology).
# Parameters:
# - p1: Proportion in group 1
# - p2: Proportion in group 2
# - power: Desired statistical power (default is 80%)
# - alpha: Significance level (default is 5%)

sample_size_opthalmology <- function(p1, p2, power = 0.8, alpha = 0.05) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(power)
  pooled_p <- (p1 + p2) / 2
  n_per_group <- ((z_alpha + z_beta)^2 * pooled_p * (1 - pooled_p)) / ((p1 - p2)^2)
  return(ceiling(n_per_group))
}


# 6️ AUROC Analysis - Predictive Modelling
# This function estimates the required sample size for AUROC analysis.
# Parameters:
# - auroc: Area under the curve (0.5 to 1.0)
# - power: Desired statistical power (default is 80%)
# - alpha: Significance level (default is 5%)

sample_size_auroc <- function(auroc, power = 0.8, alpha = 0.05) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(power)
  p1 <- 0.5
  p2 <- auroc
  n_per_group <- ((z_alpha + z_beta)^2 * (p1 * (1 - p1) + p2 * (1 - p2))) / ((p1 - p2)^2)
  return(ceiling(n_per_group))
}


# 7️ Trajectory Analysis (Regression Splines)
# This function estimates the required sample size for regression spline trajectory analysis.
# Parameters:
# - effect_size: The expected effect size
# - num_knots: Number of knots for splines
# - power: Desired statistical power

sample_size_spline <- function(effect_size, num_knots = 4, power = 0.8, alpha = 0.05) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(power)
  n <- ((z_alpha + z_beta)^2 * num_knots) / effect_size^2
  return(ceiling(n))
}


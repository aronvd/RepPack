
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(texreg)
library(haven)
library(stargazer)
library(tidyr)


# Define global variables
sample <- "bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1"
yrs <- "year_3rd >= 2006 & year_3rd <= 2010"
vce <- "cluster(cityno)"
out <- c("kommheard", "kommgotten", "kommused", "sportsclub", "sport_hrs", "oweight")
controls <- c("female", "siblings", "born_germany", "parent_nongermany", "newspaper", "art_at_home", "academictrack", "sportsclub_4_7", "music_4_7")
further <- c("sport1hrs", "sport2hrs", "sport3hrs", "sport_alt2", "health1", "obese", "eversmoked", "currentsmoking", "everalc", "alclast7")
hte <- c("female", "urban", "newspaper", "art_at_home", "academictrack", "sportsclub_4_7")
fe3 <- c("year_3rd", "bula_3rd", "cityno")
fe1 <- c("year_1st", "bula_1st", "cityno")
fe_now <- c("year_3rd", "bula", "cityno")
age <- "age >= 5 & age <= 12"

# Read the .dta file using haven
data <- read_dta(file.path(DATA_IN, "MSZ_main-data.dta"))

# Combine all variables for summary statistics
all_vars <- c("age", "female", "urban", "academictrack", "newspaper", "art_at_home", out, "tbula_3rd", "treat")


# Calculate summary statistics for all variables
summary_stats <- data %>%
  filter(eval(parse(text = sample)) & eval(parse(text = yrs))) %>%
  summarise(across(all_vars, list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

#Renaming of variables with underscores to avoid problem with latter pivot longer function in the remaining
summary_stats <- summary_stats %>%
  rename_with(~ gsub("^art_at_home", "artathome", .x), starts_with("art_at_home")) %>%
  rename_with(~ gsub("^sport_hrs", "sportshrs", .x), starts_with("sport_hrs")) %>%
  rename_with(~ gsub("^tbula_3rd", "tbula3rd", .x), starts_with("tbula_3rd"))

# Reshape summary statistics to a cleaner format
summary_stats_clean <- summary_stats %>%
  pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_") %>%
  select(variable, mean, sd, min, max)

# Convert summary_stats_clean to a regular data frame
summary_stats_clean_df <- as.data.frame(summary_stats_clean)

# Save summary statistics to a .tex file using stargazer
stargazer(summary_stats_clean_df, type = "latex", out = file.path(MY_TAB, "summary.tex"), summary = FALSE, rownames = FALSE)


# Table 2: Evaluation of Sports Club Voucher Program: Main DD Results
results <- list()
for (x in out) {
  model1 <- lm(as.formula(paste(x, "~ treat + tbula_3rd + tcoh")), data = data, subset = eval(parse(text = sample)) & eval(parse(text = yrs)))
  model2 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd")), data = data, subset = eval(parse(text = sample)) & eval(parse(text = yrs)))
  model3 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = sample)) & eval(parse(text = yrs)))
  results[[x]] <- list(model1, model2, model3)
}


# Save main DD results to a .tex file using stargazer
stargazer(results, type = "latex", out = file.path(MY_TAB, "main.tex"), star.cutoffs = c(0.1, 0.05, 0.01))


# Table 3: Sports Club Membership Across Child Ages
results_age <- list()
for (x in c("ll6", "ll7", "ll8", "ll9", "ll10", "ll11", "ll12")) {
  model <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = sample)) & eval(parse(text = yrs)) & nonmiss_p == 1)
  results_age[[x]] <- model
}

# Save sports club membership results to a .tex file using stargazer
stargazer(results_age, type = "latex", out = file.path(MY_TAB, "main_parents2.tex"), star.cutoffs = c(0.1, 0.05, 0.01))


# Table 4: Robustness
robust_results <- list()
for (x in out) {
  model1 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = sample)) & year_3rd >= 2007 & year_3rd <= 2010)
  model2 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = sample)) & year_3rd >= 2000 & year_3rd <= 2010)
  model3 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = sample)) & year_3rd >= 2006 & year_3rd <= 2011)
  model4 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = sample)) & year_3rd >= 2006 & year_3rd <= 2009)
  model5 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = bula_3rd %in% c(13, 16) & eval(parse(text = yrs)) & target == 1 & nonmiss == 1)
  model6 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = bula_3rd %in% c(4, 13) & eval(parse(text = yrs)) & target == 1 & nonmiss == 1)
  model7 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = yrs)) & target == 1)
  model8 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = yrs)) & eval(parse(text = sample)) & duration > 10 & duration < Inf & female_check == 1 & deutsch_check == 1 & dob_check == 1)
  model9 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = yrs)) & eval(parse(text = sample)) & sib_part == 0 & year_3rd >= 2009 & year_3rd <= 2010)
  model10 <- lm(as.formula(paste(x, "~ treat + year_3rd + bula_3rd + cityno")), data = data, subset = eval(parse(text = yrs)) & eval(parse(text = sample)) & anz_osiblings == 0)
  robust_results[[x]] <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
}

# Save robustness results to a .tex file using stargazer
stargazer(robust_results, type = "latex", out = file.path(MY_TAB, "robust_p1.tex"), star.cutoffs = c(0.1, 0.05, 0.01))


# Additional tables and analyses can be translated similarly...

# Close the log file
#sink()

# Exit (not needed in R, script 

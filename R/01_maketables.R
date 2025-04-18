
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(texreg)
library(haven)
library(stargazer)
library(tidyr)
library(lmtest)
library(clubSandwich)


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

# Create logical vector for sample & year filter
filter_cond <- with(data, eval(parse(text = sample)) & eval(parse(text = yrs)))

# Flattened model list and corresponding outcome labels
model_list <- list()
model_labels <- c()

for (x in out) {
  # Build models with factor() for categorical variables
  model1 <- lm(as.formula(paste(x, "~ treat + tbula_3rd + tcoh")), data = data, subset = filter_cond)
  model2 <- lm(as.formula(paste(x, "~ treat + factor(year_3rd) + factor(bula_3rd)")), data = data, subset = filter_cond)
  model3 <- lm(as.formula(paste(x, "~ treat + factor(year_3rd) + factor(bula_3rd) + factor(cityno)")), data = data, subset = filter_cond)
  
  # Store all three models
  model_list <- c(model_list, list(model1, model2, model3))
  
  # Label each block of models
  model_labels <- c(model_labels, rep(x, 3))
}

# Group labels for each model in the table
group_labels <- unique(model_labels)

# Use stargazer to output LaTeX table (one long table with all models)
stargazer(model_list,
          type = "latex",
          out = file.path(MY_TAB, "main.tex"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          keep = "treat",
          covariate.labels = c("Voucher"),
          title = "Evaluation of Sports Club Voucher Program: Main DD Results",
          column.labels = rep(c("Spec 1", "Spec 2", "Spec 3"), length(group_labels)),
          column.separate = rep(3, length(group_labels)),
          dep.var.labels.include = FALSE,
          dep.var.labels = group_labels,
          omit.stat = c("f", "ser", "adj.rsq", "rsq"),
          no.space = TRUE
)

# Save main DD results to a .tex file using stargazer
stargazer(results, type = "latex", out = file.path(MY_TAB, "main.tex"), star.cutoffs = c(0.1, 0.05, 0.01))

#  Table 3: Sports Club Membership Across Child Ages

# Flattened model list and corresponding outcome labels
model_list <- list()
model_labels <- c()

# Define the filter conditions
filter_cond <- with(data, eval(parse(text = sample)) & eval(parse(text = yrs)))
filter_cond_restricted <- with(data, eval(parse(text = sample)) & eval(parse(text = yrs)) & nonmiss_p == 1)


###1st column### (Suppressed section as regressions take a while)

# # Initialize results table
# results_all <- data.frame(Age = integer(), Coef = numeric(), SE = numeric(), N = integer(), stringsAsFactors = FALSE)
# 
# for (i in 6:12) {
#   outcome_var <- paste0("LL_sport", i)
#   formula_str <- paste(outcome_var, "~ treat + factor(year_3rd) + factor(bula_3rd) + factor(cityno)")
# 
#   # Subset data beforehand to keep everything consistent
#   subset_data <- data[filter_cond, ]
# 
#   # Fit model on subset
#   model <- lm(as.formula(formula_str), data = subset_data)
# 
#   # Compute clustered SEs using cityno from subset
#   clustered_se <- coeftest(model, vcov = vcovCR(model, cluster = subset_data$cityno, type = "CR2"))
# 
#   # Extract and store
#   coef_val <- clustered_se["treat", "Estimate"]
#   se_val   <- clustered_se["treat", "Std. Error"]
#   nobs     <- nobs(model)
# 
#   results_all <- rbind(
#     results_all,
#     data.frame(Age = i, Coef = round(coef_val, 3), SE = round(se_val, 3), N = nobs)
#   )
# }
# 
# print(results_all)


###2nd column###

# Initialize results table
results_parentsample <- data.frame(Age = integer(), Coef = numeric(), SE = numeric(), N = integer(), stringsAsFactors = FALSE)

for (i in 6:12) {
  outcome_var <- paste0("LL_sport", i)
  formula_str <- paste(outcome_var, "~ treat + factor(year_3rd) + factor(bula_3rd) + factor(cityno)")

  # Subset data beforehand to keep everything consistent
  #filter_cond_restricted <- with(data, eval(parse(text = sample)) & eval(parse(text = yrs)) & nonmiss_p == 1)
  subset_data <- data[filter_cond_restricted, ]

  # Fit model on subset
  model <- lm(as.formula(formula_str), data = subset_data)

  # Compute clustered SEs using cityno from subset
  clustered_se <- coeftest(model, vcov = vcovCR(model, cluster = subset_data$cityno, type = "CR2"))

  # Extract and store
  coef_val <- clustered_se["treat", "Estimate"]
  se_val   <- clustered_se["treat", "Std. Error"]
  nobs     <- nobs(model)

  results_parentsample <- rbind(
    results_parentsample,
    data.frame(Age = i, Coef = round(coef_val, 3), SE = round(se_val, 3), N = nobs)
  )
}

print(results_parentsample)

###3rd Column###
# Initialize results table
results_restricted <- data.frame(Age = integer(), Coef = numeric(), SE = numeric(), N = integer(), stringsAsFactors = FALSE)

for (i in 6:12) {
  outcome_var <- paste0("ll", i)
  formula_str <- paste(outcome_var, "~ treat + factor(year_3rd) + factor(bula_3rd) + factor(cityno)")
  
  # Subset data beforehand to keep everything consistent
  subset_data <- data[filter_cond_restricted, ]
  
  # Fit model on subset
  model <- lm(as.formula(formula_str), data = subset_data)
  
  # Compute clustered SEs using cityno from subset
  clustered_se <- coeftest(model, vcov = vcovCR(model, cluster = subset_data$cityno, type = "CR2"))
  
  # Extract and store
  coef_val <- clustered_se["treat", "Estimate"]
  se_val   <- clustered_se["treat", "Std. Error"]
  nobs     <- nobs(model)
  
  results_restricted <- rbind(
    results_restricted,
    data.frame(Age = i, Coef = round(coef_val, 3), SE = round(se_val, 3), N = nobs)
  )
}

print(results_restricted)




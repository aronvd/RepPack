# Load necessary libraries
library(dplyr)
library(ggplot2)
library(foreign)
library(tidyr)
library(dplyr)
library(broom)
library(patchwork)

setwd(getwd())

# Load data
data <- read_dta(file.path(DATA_IN, "MSZ_main-data.dta"))

# Préambule : Génération des variables nécessaires
sample <- data %>% filter(bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1)
yrs <- data %>% filter(year_3rd >= 2006 & year_3rd <= 2010)
out <- c("kommheard", "kommgotten", "kommused", "sportsclub", "sport_hrs", "oweight")

# Figure 1 : Développement des variables de résultat dans les états de traitement et de contrôle
library(ggplot2)

data %>%
  filter(year_3rd >= 2006 & year_3rd <= 2010) %>%
  group_by(tbula_3rd, year_3rd) %>%
  summarise(across(all_of(out), mean, na.rm = TRUE), treat = mean(treat, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(out), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = year_3rd, y = value, color = factor(tbula_3rd))) +
  geom_point() +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "", y = "Percent", color = "State") +
  theme_minimal() +
  ggsave("results/DiD_alt1.pdf")

# Figure 2 : Hétérogénéité des effet
#########
# Figure 2: Effect Heterogeneity
# Figure 2: Effect Heterogeneity
library(dplyr)
library(ggplot2)
library(coefplot)



results <- list()
for (x in out) {
  for (group in hte) {
    formula <- as.formula(paste(x, "~ treat *", group, "+ factor(year_3rd) *", group, "+ factor(bula_3rd) *", group, "+ factor(cityno)"))
    model <- lm(formula, data = sample)
    results[[paste(x, group, sep = "_")]] <- tidy(model)
  }
}

plot_list <- list()
for (x in out) {
  plot_data <- do.call(rbind, lapply(hte, function(group) {
    result <- results[[paste(x, group, sep = "_")]]
    result <- result %>% filter(term == paste("treat:", group, sep = ""))
    result$group <- group
    return(result)
  }))
  
  p <- ggplot(plot_data, aes(x = group, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "", y = "Effect Size") +
    theme_minimal()
  
  plot_list[[x]] <- p
}


for (x in out) {
  ggsave(filename = paste0("hte_", x, ".pdf"), plot = plot_list[[x]])
}


combined_plot_fig2 <- plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + 
  plot_list[[4]] + plot_list[[5]] + plot_list[[6]] +
  plot_layout(ncol = 2) # Adjust the layout as needed

# Display the combined plot
print(combined_plot)

ggsave(filename = "combined_plot_fig2.pdf", plot = combined_plot_fig2, width = 10, height = 8)


# Figure 3: Heterogeneity across Cohorts


filtered_data <- data %>%
  filter(bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1 & year_3rd >= 2006 & year_3rd <= 2010)

outcomes <- c("kommheard", "kommgotten", "kommused", "sportsclub", "sport_hrs", "oweight")
controls <- c("female", "siblings", "born_germany", "parent_nongermany", "newspaper", "art_at_home", "academictrack", "sportsclub_4_7", "music_4_7")
further <- c("sport1hrs", "sport2hrs", "sport3hrs", "sport_alt2", "health1", "obese", "eversmoked", "currentsmoking", "everalc", "alclast7")
hte <- c("female", "urban", "newspaper", "art_at_home", "academictrack", "sportsclub_4_7")
fe3 <- c("year_3rd", "bula_3rd", "cityno")
fe1 <- c("year_1st", "bula_1st", "cityno")
fe_now <- c("year_3rd", "bula", "cityno")
age <- c(5, 12)


##run reg 
results <- list()
for (outcome in outcomes) {
  model <- lm(as.formula(paste(outcome, "~ t_2008 + t_2009 + t_2010 + year_3rd + bula_3rd + cityno")), data = filtered_data)
  results[[outcome]] <- tidy(model)
}

#plotcoeff 

plot_list <- list()
for (outcome in outcomes) {
  p <- ggplot(results[[outcome]], aes(x = term, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(limits = c(-0.2, 0.4)) +
    scale_x_discrete(labels = c("1st cohort", "2nd cohort", "3rd cohort")) +
    theme_minimal() +
    labs(title = outcome)
  plot_list[[outcome]] <- p
}

#save 

for (outcome in outcomes) {
  ggsave(filename = paste0("event_", outcome, ".pdf"), plot = plot_list[[outcome]])
}

combined_plot_fig3 <- plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + 
  plot_list[[4]] + plot_list[[5]] + plot_list[[6]] +
  plot_layout(ncol = 2) # Adjust the layout as needed

# Display the combined plot


ggsave(filename = "combined_plot_fig3.pdf", plot = combined_plot_fig3, width = 10, height = 8)

########




# Figure 4 : Adhésion au club de sport par âge

sample <- data %>% filter(bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1)
yrs <- sample %>% filter(year_3rd >= 2006 & year_3rd <= 2010)

data_filtered <- yrs %>% select(starts_with("LL_sport"), kommheard, kommgotten, sportsclub, treat, bula_3rd, bula, year_3rd, cityno)
data_filtered <- data_filtered %>% mutate(id = row_number())


# Reshape data to long format and extract numeric part of LL_sport column names as age
data_long <- data_filtered %>% pivot_longer(
  cols = starts_with("LL_sport"),
  names_to = "age",
  values_to = "LL_sport",
  names_transform = list(age = ~ as.numeric(gsub("LL_sport", "", .)))
)


# Generate tyear and tbula columns

data_long <- data_long %>% mutate(
  tyear = ifelse(year_3rd >= 2008 & year_3rd <= 2010, 1, 0),
  tbula = ifelse(bula_3rd == 13, 1, 0),
  cnt = 1
)

# Filter based on age
data_long <- data_long %>% filter(age >= 5 & age <= 12)

# Check the filtered data
print(head(data_long))

# Collapse data by tbula, tyear, and age
data_collapsed <- data_long %>% group_by(tbula, tyear, age) %>% summarise(
  LL_sport = mean(LL_sport, na.rm = TRUE) * 100,
  kommheard = mean(kommheard, na.rm = TRUE),
  kommgotten = mean(kommgotten, na.rm = TRUE),
  treat = mean(treat, na.rm = TRUE),
  sportsclub = mean(sportsclub, na.rm = TRUE),
  cnt = sum(cnt)
)


# Plot Figure 4 with faceting

ggplot(data_collapsed, aes(x = age, y = LL_sport, color = factor(tbula))) +
  geom_point() +
  geom_line() +
  facet_wrap(~ tyear, scales = "free_y") +
  labs(x = "Age", y = "Percent", color = "State") +
  theme_minimal() +
  ggsave("results/DiD_fig4.pdf")


#Table 5 : Suggestive envidence mechanism

sample_data <- data %>% filter(bula_3rd == 13 & bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1 & year_3rd >= 2006 & year_3rd <= 2010)

##5.1 New sport dscipline

ggplot(sample_data, aes(x = factor(v_579))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "black") +
  labs(y = "Percent", x = "Tried new sport discipline(s)") +
  theme_minimal() +
  ggsave("results/bar_5.1_en.pdf")

##5.2 Could redeem for desired discipline

# Plot
ggplot(sample_data, aes(x = factor(favsport))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "black") +
  labs(y = "Percent", x = "Could redeem the voucher for desired discipline") +
  theme_minimal() +
  ggsave("results/bar_5.2_en.pdf")


##5.3 Could not afford without voucher

ggplot(sample_data, aes(x = factor(v_582))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "black") +
  labs(y = "Percent", x = "Could not afford membership w/o voucher") +
  theme_minimal() +
  ggsave("results/bar_5.3_en.pdf")

##5.4 Parents happy to save money

# Plot
ggplot(sample_data, aes(x = factor(v_583))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "black") +
  labs(y = "Percent", x = "Parents happy to save money b/c of voucher") +
  theme_minimal() +
  ggsave("results/bar_5.4_en.pdf")


###Transport do not work yet

##5.5 Transport as supply side barrier

transport_data <- data %>% filter(v_566 != 1)

# Plot
ggplot(transport_data, aes(x = factor(variable), y = value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "black") +
  scale_x_discrete(labels = c("Foot", "Bike", "Public transport", "Driven by parents")) +
  labs(y = "Share", x = "Mode of Transportation") +
  theme_minimal() +
  ggsave("results/transportation.pdf")

##5.6 Rural or urban mode of transport 

ggplot(transport_data, aes(x = factor(urban), y = value, fill = factor(variable))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c("Rural", "Urban")) +
  scale_fill_manual(values = c("black", "gray", "darkgray", "lightgray"), labels = c("Foot", "Bike", "Public transport", "Driven by parents")) +
  labs(y = "Share", x = "Mode of Transportation") +
  theme_minimal() +
  ggsave("results/transportation2.pdf")

#6 : Figure 6: Supply-Side Restrictions? Number of Sports Clubs per ZIP Code

# Filter data
clubs_data <- data %>% filter(tbula_3rd == 1 & bula == 13 & !is.na(einwohner) & bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1 & year_3rd >= 2006 & year_3rd <= 2010)

# Plot


##6.1 Number of Sports Clubs per ZIP Code
ggplot(clubs_data, aes(x = vereine_cat)) +
  geom_histogram(binwidth = 1, fill = "black", aes(y = ..density..)) +
  scale_x_continuous(breaks = 0:7, labels = c("0", "1-5", "6-10", "11-15", "16-20", "21-30", "31-40", ">40")) +
  labs(y = "Proportion", x = "Sports clubs per ZIP code") +
  theme_minimal() +
  ggsave("results/clubs.pdf")
##6.2 Sports Club Disciplines per ZIP Code

# Plot
ggplot(clubs_data, aes(x = sparten_cat)) +
  geom_histogram(binwidth = 1, fill = "black", aes(y = ..density..)) +
  scale_x_continuous(breaks = 0:7, labels = c("0", "1-10", "11-20", "21-30", "31-40", "41-50", "51-60", ">60")) +
  labs(y = "Proportion", x = "Sports club disciplines per ZIP code") +
  theme_minimal() +
  ggsave("results/divisions.pdf")

#7 Figure 7: Further Outcomes

# Load necessary libraries

# Filter data

sample_data <- data %>% filter(bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1 & year_3rd >= 2006 & year_3rd <= 2010)

# Placebo Outcomes
placebo_results <- list()
controls <- c("female", "siblings", "born_germany", "parent_nongermany", "newspaper", "art_at_home", "academictrack", "sportsclub_4_7", "music_4_7")

for (control in controls) {
  sample_data <- sample_data %>% mutate(treat_temp = treat)
  model <- lm(as.formula(paste(control, "~ treat_temp + year_3rd + bula_3rd + cityno")), data = sample_data)
  placebo_results[[control]] <- tidy(model, conf.int = TRUE)
  sample_data <- sample_data %>% mutate(treat = treat_temp)
}

placebo_df <- bind_rows(placebo_results, .id = "control")



##7.1 a) Placebo Outcomes

# Plot

ggplot(placebo_df, aes(x = control, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Control Variables", y = "Estimate") +
  theme_minimal() +
  ggsave("results/placebo.pdf")

### DOES NOT WORK YET EITHER 7.2 b) Additional Outcomes

sample_data <- data %>% filter(bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1 & year_3rd >= 2006 & year_3rd <= 2010)

# Additional Outcomes
additional_results <- list()
further <- c("Does_sport", "At_least_2_hrs_sports_week", "At_least_3_hrs_sports_week", 
    "Sport_is_important", "Very_good_health", "Overweight_BMI_25", 
    "Ever_smoked_cigarettes", "Current_smoker", "Ever_consumed_alcohol", 
    "Consumed_alcohol_in_last_7_days")


for (outcome in further) {
  sample_data <- sample_data %>% mutate(treat_temp = treat)
  model <- lm(as.formula(paste(outcome, "~ treat_temp + year_3rd + bula_3rd + cityno")), data = sample_data)
  additional_results[[outcome]] <- tidy(model, conf.int = TRUE)
  sample_data <- sample_data %>% mutate(treat = treat_temp)
}

additional_df <- bind_rows(additional_results, .id = "outcome")

##7.1 b) Additional Outcomes

# Plot
ggplot(additional_df, aes(x = outcome, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Outcome Variables", y = "Estimate") +
  theme_minimal() +
  ggsave("results/further.pdf")



#APPENDIX #not there yet either. But will end up managing.


library(dplyr)
library(ggplot2)
library(broom)

# Figure A2: YOLO Sampling and Sample Size
n1 <- nrow(data)
n2 <- nrow(data %>% filter(target == 1))
cat(n2 / n1 * 100, "\n")

n4 <- nrow(data %>% filter(target == 1 & bula_3rd %in% c(4, 13, 16) & year_3rd >= 2006 & year_3rd <= 2011))
cat(n4 / n2 * 100, "\n")

n5 <- nrow(data %>% filter(target == 1 & bula_3rd %in% c(4, 13, 16) & year_3rd >= 2006 & year_3rd <= 2011 & nonmiss == 1))
cat(n5 / n4 * 100, "\n")

n6 <- nrow(data %>% filter(target == 1 & bula_3rd %in% c(4, 13, 16) & year_3rd >= 2006 & year_3rd <= 2010 & nonmiss == 1))
cat(n6 / n5 * 100, "\n")

# A.4: Duration of survey in our sample

sample_data <- data %>% filter(bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1 & year_3rd >= 2006 & year_3rd <= 2010)

# A.4: Duration of survey in our sample
ggplot(data %>% filter(sample == TRUE & yrs == TRUE), aes(x = duration)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Duration", y = "Frequency") +
  ggsave("results/hist_dur.pdf")


# Figure B1: Sports Disciplines for which Vouchers Were Redeemed
vouchers <- data %>% count(v_560_2, sort = TRUE)
print(vouchers)

# Figure B2: Development of Outcome Variables—Synthetic Control Group
# --> see 03_synthetic_control

# Figure B3: Outcome Difference—Treatment vs. Control States
years <- 2006:2010
for (y in years) {
  data <- data %>% mutate(!!paste0("t", y) := year_3rd == y & tbula_3rd == 1)
}

outcome_vars <- c("sportsclub", "oweight", "sport_hrs")
for (x in outcome_vars) {
  data <- data %>% mutate(!!x := !!sym(x) * 100)
  model <- lm(as.formula(paste(x, "~ t2006 + t2007 + t2008 + t2009 + t2010 + year_3rd")), data = data %>% filter(sample & yrs))
  results <- tidy(model, conf.int = TRUE)
  
  ggplot(results, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Year", y = "Estimate") +
    theme_minimal() +
    ggsave(paste0("results/diff_", x, ".pdf"))
  
  data <- data %>% mutate(!!x := !!sym(x) / 100)
}

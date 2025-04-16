# Load necessary libraries
library(dplyr)
library(ggplot2)
library(foreign)
library(tidyr)

setwd(getwd())

# Setting the specification
sample <- "bula_3rd %in% c(4, 13, 16) & target == 1 & nonmiss == 1"
yrs <- "year_3rd >= 2006 & year_3rd <= 2010"
vce <- "cityno"

# Outcomes
out <- c("kommheard", "kommgotten", "kommused", "sportsclub", "sport_hrs", "oweight")

# Further
controls <- c("female", "siblings", "born_germany", "parent_nongermany", "newspaper", "art_at_home", "academictrack", "sportsclub_4_7", "music_4_7")
further <- c("sport1hrs", "sport2hrs", "sport3hrs", "sport_alt2", "health1", "obese", "eversmoked", "currentsmoking", "everalc", "alclast7")
hte <- c("female", "urban", "newspaper", "art_at_home", "academictrack", "sportsclub_4_7")
fe3 <- c("year_3rd", "bula_3rd", "cityno")
fe1 <- c("year_1st", "bula_1st", "cityno")
fe_now <- c("year_3rd", "bula", "cityno")
age <- "age >= 5 & age <= 12"

# Load data
data <- read_dta(file.path(DATA_IN, "MSZ_main-data.dta"))

# Graphs: Main

# Figure 1: Development of Outcome Variables in Treatment and Control States across Cohorts
data <- data %>%
  mutate(cnt = 1) %>%
  group_by(tbula_3rd, year_3rd) %>%
  summarize(across(all_of(out), mean), treat = mean(treat), cnt = sum(cnt))

years <- c("2006/07", "2007/08", "2008/09", "2009/10", "2010/11", "2011/12")

for (x in out) {
  data[[x]] <- data[[x]] * 100
  ggplot(data, aes(x = year_3rd, y = !!sym(x), color = factor(tbula))) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c("black", "grey")) +
    labs(title = paste("DiD_", x), x = "", y = "Percent") +
    theme_minimal() +
    ggsave(paste0("$MY_TAB/DiD_", x, ".pdf"))
  data[[x]] <- data[[x]] / 100
}

# Figure 2: Effect Heterogeneity
for (x in out) {
  for (group in hte) {
    data <- data %>%
      mutate(tXgroup = treat * !!sym(group))
    model <- lm(as.formula(paste(x, "~ treat + tXgroup + year_3rd *", group, "+ bula_3rd *", group, "+ cityno *", group)), data = data)
    summary(model)
    data <- data %>%
      select(-tXgroup)
  }
}

# Figure 3: Heterogeneity across Cohorts
for (x in out) {
  model <- lm(as.formula(paste(x, "~ t_2008 + t_2009 + t_2010 + year_3rd + bula_3rd + cityno")), data = data)
  summary(model)
}

# Figure 4: Sports Club Membership by Age
data <- data %>%
  filter(eval(parse(text = sample))) %>%
  filter(eval(parse(text = yrs))) %>%
  select(starts_with("LL_sport"), kommheard, kommgotten, sportsclub, treat, bula_3rd, bula, year_3rd, cityno) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = starts_with("LL_sport"), names_to = "age", values_to = "LL_sport") %>%
  mutate(tyear = year_3rd >= 2008 & year_3rd <= 2010,
         tbula = bula_3rd == 13,
         cnt = 1) %>%
  filter(eval(parse(text = age))) %>%
  group_by(tbula, tyear, age) %>%
  summarize(across(c(LL_sport, kommheard, kommgotten, treat, sportsclub), mean), cnt = sum(cnt))

data$LL_sport <- data$LL_sport * 100
ggplot(data, aes(x = age, y = LL_sport, color = factor(tbula))) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("black", "grey")) +
  labs(title = "DiD_alt1", x = "Age", y = "Percent") +
  theme_minimal() +
  ggsave("$MY_TAB/DiD_alt1.pdf")

ggplot(data, aes(x = age, y = LL_sport, color = factor(tbula))) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("black", "grey")) +
  labs(title = "DiD_alt2", x = "Age", y = "Percent") +
  theme_minimal() +
  ggsave("$MY_TAB/DiD_alt2.pdf")


# Figure 5: Suggestive Evidence on Mechanisms

# a) Tried new sport discipline(s)
ggplot(data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))) %>% filter(bula_3rd == 13), aes(x = factor(v_579))) +
  geom_bar() +
  labs(title = "Tried new sport discipline(s)", y = "Percent") +
  theme_minimal() +
  ggsave("$MY_TAB/bar_579_en.pdf")

# b) Could redeem the voucher for desired discipline
ggplot(data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))) %>% filter(bula_3rd == 13), aes(x = factor(favsport))) +
  geom_bar() +
  labs(title = "Could redeem the voucher for desired discipline", y = "Percent") +
  theme_minimal() +
  ggsave("$MY_TAB/bar_561_en.pdf")

# c) Could not afford membership w/o voucher
ggplot(data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))) %>% filter(bula_3rd == 13), aes(x = factor(v_582))) +
  geom_bar() +
  labs(title = "Could not afford membership w/o voucher", y = "Percent") +
  theme_minimal() +
  ggsave("$MY_TAB/bar_582_en.pdf")

# d) Parents happy to save money b/c of voucher
ggplot(data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))) %>% filter(bula_3rd == 13), aes(x = factor(v_583))) +
  geom_bar() +
  labs(title = "Parents happy to save money b/c of voucher", y = "Percent") +
  theme_minimal() +
  ggsave("$MY_TAB/bar_583_en.pdf")

# e) Transport as a supply-side barrier
transport_data <- data %>% filter(v_566 != 1) %>% summarize(across(v_562:v_565, mean))
transport_data_long <- transport_data %>% pivot_longer(cols = everything(), names_to = "transport_mode", values_to = "mean_value")
ggplot(transport_data_long, aes(x = transport_mode, y = mean_value)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Foot", "Bike", "Public transport", "Driven by parents")) +
  labs(title = "Transport as a supply-side barrier", y = "Share") +
  theme_minimal() +
  ggsave("$MY_TAB/transportation.pdf")

# f) Mode of Transportation (Urban vs. Rural)
transport_data_urban <- data %>% filter(v_566 != 1) %>% group_by(urban) %>% summarize(across(v_562:v_565, mean))
transport_data_urban_long <- transport_data_urban %>% pivot_longer(cols = -urban, names_to = "transport_mode", values_to = "mean_value")
ggplot(transport_data_urban_long, aes(x = transport_mode, y = mean_value, fill = factor(urban))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c("Foot", "Bike", "Public transport", "Driven by parents")) +
  scale_fill_manual(values = c("grey", "black"), labels = c("Rural", "Urban")) +
  labs(title = "Mode of Transportation (Urban vs. Rural)", y = "Share") +
  theme_minimal() +
  ggsave("$MY_TAB/transportation2.pdf")

# Figure 6: Supply-Side Restrictions? Number of Sports Clubs per ZIP Code
ggplot(data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))) %>% filter(tbula_3rd == 1 & bula == 13 & !is.na(einwohner)), aes(x = factor(vereine_cat))) +
  geom_histogram(binwidth = 1) +
  scale_x_discrete(labels = c("0", "1-5", "6-10", "11-15", "16-20", "21-30", "31-40", ">40")) +
  labs(title = "Sports clubs per ZIP code", y = "Proportion") +
  theme_minimal() +
  ggsave("$MY_TAB/clubs.pdf")

ggplot(data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))) %>% filter(tbula_3rd == 1 & bula == 13 & !is.na(einwohner)), aes(x = factor(sparten_cat))) +
  geom_histogram(binwidth = 1) +
  scale_x_discrete(labels = c("0", "1-10", "11-20", "21-30", "31-40", "41-50", "51-60", ">60")) +
  labs(title = "Sports club disciplines per ZIP code", y = "Proportion") +
  theme_minimal() +
  ggsave("$MY_TAB/divisions.pdf")

# Figure 7: Further Outcomes
# a) Placebo Outcomes
for (x in controls) {
  data <- data %>% mutate(treat_x = treat)
  model <- lm(as.formula(paste(x, "~ treat_x +", paste(fe3, collapse = " + "), "+ cityno")), data = data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))))
  summary(model)
  data <- data %>% select(-treat_x)
}

# b) Additional outcomes
for (x in further) {
  data <- data %>% mutate(treat_x = treat)
  model <- lm(as.formula(paste(x, "~ treat_x +", paste(fe3, collapse = " + "), "+ cityno")), data = data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))))
  summary(model)
  data <- data %>% select(-treat_x)
}

# Graphs: Appendix

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
ggplot(data %>% filter(eval(parse(text = sample))) %>% filter(eval(parse(text = yrs))), aes(x = duration)) +
  geom_histogram() +
  labs(title = "Duration of survey in our sample", y = "Frequency") +
  theme_minimal() +
  ggsave("$MY_TAB/hist_dur.pdf")

# Figure B1: Sports Disciplines for which Vouchers Were Redeemed
table(data$v_560_2)

# Figure B2: Development of Outcome Variables—Synthetic Control Group
# --> see 03_synthetic_control

# Figure B3: Outcome Difference—Treatment vs. Control States


###############################################################################
# DATA ANALYSIS PART 1: Description of Data
###############################################################################
# In that first part of the data analysis, the aim is to visualize the data and
# to gain new information. Descriptive statistics are used. First, information
# about demographics are gained. Then, two variables are described: BDI and TAF.


###############################################################################
# Opening and cleaning the data
###############################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2)

# Open the csv. file and save the path as "path", and the data as "data"
path <- "C:/Users/Camille Rérat/Documents/Camille/BASEL/Master/Masterarbeit/Analysis Thesis/Data/processed/data_1st_part.csv"
data <- read.csv2(path)

# Delete columns X and X.1 - X.5, as they do not containt any values
data[c("X", "X.1", "X.2", "X.3", "X.4", "X.5")] <- NULL

# Delete rows 32-67 as they don't contain any values
data <- data[-c(32:67), ]

# Clean the name of each variable (no capital letter, no point, etc.)
data <- janitor :: clean_names (data)

###############################################################################
# Demographics
###############################################################################
# Problem: R doesn't read the "ä" of "Männlich" correctly.Because there are only 
# men and women in the dataset (no participant answered "Others" in the 
# questionnaire about demographics), the problem is solved that way:
data$Geschlecht <- ifelse(data$geschlecht == "Weiblich", "Weiblich", "Männlich")

# Number of people in both groups and percentage
table(data$group) # number of people in both groups

counts <- table(data$group)   #percentage
prop.table(counts) * 100      

# Percentage of men/women in both groups
library(dplyr)
data %>%
  group_by(group, geschlecht) %>%          
  summarise(count = n(), .groups = "drop") %>%  
  group_by(group) %>%                  
  mutate(percent = count / sum(count) * 100)

# Mean age in the total sample (first rename the column id_pre which contains the age)
names(data)[names(data) == "id_pre"] <- "age"   # Rename the column

mean(data$age, na.rm = TRUE)  # Mean age in the whole sample

# Mean age by group
aggregate(age ~ group, data = data, mean, na.rm = TRUE)

# Range of the age variable in the whole sample
range(data$age, na.rm = TRUE)


# Create a table with demographics
library(dplyr)
library(tidyr)

# Total sample size
total_n <- nrow(data)

# Age mean and sd
age_row <- data %>%
  group_by(group) %>%
  summarise(
    mean = mean(age, na.rm = TRUE),
    sd   = sd(age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(value = paste0(round(mean, 2), " (", round(sd, 2), ")")) %>%
  select(group, value) %>%
  pivot_wider(names_from = group, values_from = value)

# Total column
age_row$Total <- paste0(
  round(mean(data$age, na.rm = TRUE), 2),
  " (",
  round(sd(data$age, na.rm = TRUE), 2),
  ")"
)

age_row <- age_row %>%
  mutate(Variable = "Age, M (SD)") %>%
  select(Variable, everything())

# Gender header row
gender_header <- tibble(
  Variable = "Gender, n (%)",
  "CBM-I Training" = "",       # without the "", R can't read "CBM-I Training" correctly
  Control = "",
  Total = ""
)

# Gender rows: Male and Female
gender_rows <- data %>%
  count(group, geschlecht) %>%
  group_by(group) %>%
  mutate(
    percent = round(n / sum(n) * 100, 1),
    value = paste0(n, " (", percent, "%)")
  ) %>%
  ungroup() %>%
  select(group, geschlecht, value) %>%
  pivot_wider(names_from = group, values_from = value)

# Total column (counts + 100%)
gender_rows <- gender_rows %>%
  mutate(
    Total = paste0(
      rowSums(
        sapply(select(., -geschlecht), 
               function(x) as.numeric(sub(" .*", "", x)))
      ),
      " (100%)"
    ),
    Variable = paste0("  ", geschlecht)  # indentation
  ) %>%
  select(Variable, 'CBM-I Training ', Control, Total)

# Trim column names for all tables (To solve problems with "CBM-I Training" that wasn't read correctly)
age_row <- age_row %>% rename_with(~trimws(.))
gender_rows <- gender_rows %>% rename_with(~trimws(.))
gender_header <- gender_header %>% rename_with(~trimws(.))

# Table (APA-Style)
table_apa <- bind_rows(
  age_row,
  gender_header,
  gender_rows
)

# I want an APA version that can be directly opened in Word
install.packages("officier")
install.packages("flextable")
library(officer)
library(flextable)

# Convert tibble to flextable
ft <- flextable(table_apa)

# Align, bold headers, adjust width
ft <- ft %>%
  bold(part = "header") %>%
  autofit()

# Export to Word
read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "APA_table.docx")
###############################################################################
# Description of the BDI variable in this sample
###############################################################################
# The intervention took place between mid and post, therefore the values pre
# are not of big interest here

# First rename the bdi_tot_pre in bdi_pre
names(data)[names(data) == "bdi_tot_pre"] <- "bdi_pre"

# 1) Mean BDI-values at mid in two groups
aggregate(bdi_mid ~ Group, data = data, mean, na.rm = TRUE)

# 2) Standard deviation of BDI-values at mid in two groups
aggregate(bdi_mid ~ group, data = data, sd, na.rm = TRUE)

# 3) Histogram to visualize the data
ggplot(data, aes(x = bdi_mid)) +
  geom_histogram(bins = 15, fill = "slategray2", color = "black") + 
  facet_wrap(~ group) +
  labs(
    title = NULL,
    x = "Depression Score (BDI)",
    y = "Count"
  ) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# 4) Graph: difference between two groups before training (Plot)
plot_1 <- ggplot(data, aes(x = group, y = bdi_mid, fill = group)) +
  geom_boxplot(outlier.color = "gray40", alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  labs(
    title = NULL,
    x = "Group",
    y = "Depression Value (BDI)"
  ) +
  theme_classic() +
theme(legend.position = "none")

# 5) Mean in 2 groups at post
aggregate(bdi_post ~ group, data = data, mean, na.rm = TRUE)

# 6) Standard deviation in 2 groups at post
aggregate(bdi_post ~ group, data = data, sd, na.rm = TRUE)

# 7) Histogram
ggplot(data, aes(x = bdi_post)) +
  geom_histogram(bins = 15, fill = "slategray2", color = "black") + 
  facet_wrap(~ group) +
  labs(
    title = "Histogram: BDI-values before the training",
    x = "Depression Score (BDI)",
    y = "Count"
  ) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# 8) Graph: difference between two groups after training (Plot)
plot_2 <- ggplot(data, aes(x = group, y = bdi_post, fill = group)) +
  geom_boxplot(outlier.color = "gray40", alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  labs(
    title = NULL,
    x = "Group",
    y = "Depression Value (BDI)"
  ) +
  theme_classic() +
  theme(legend.position = "none")


# I want to put plot_1 and plot_2 next to each other
library(ggplot2)
library(patchwork)

plot_1 + plot_2

# Test for normality: are BDI-values normally distributed in both groups?
# Use of a Shapiro Test, because the sample is small
by(data$bdi_mid, data$group, shapiro.test)


###############################################################################
# Description of the TAF variable in this sample
###############################################################################

# 1) Mean TAF values before the training 
mean(data$taf_tot_mid, na.rm = TRUE)

# 2) SD before the training
sd(data$taf_tot_mid, na.rm = TRUE)

# 3) Histogram to see the repartition of the TAF values before the training
ggplot(data, aes(x = taf_tot_mid)) +
  geom_histogram(bins = 15, fill = "slategray2", color = "black") + 
  labs(
    title = NULL,
    x = "TAF scores (TAFs)",
    y = "Count"
  ) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# 4) Mean and SD of the three subscales
mean(data$taf_moral_mid, na.rm = TRUE)
mean(data$taf_likelihood_self_mid, na.rm = TRUE)
mean(data$taf_likelihood_other_mid, na.rm = TRUE)

sd(data$taf_moral_mid, na.rm = TRUE)
sd(data$taf_likelihood_self_mid, na.rm = TRUE)
sd(data$taf_likelihood_other_mid, na.rm = TRUE)

# 5) Mean and SD of TAF Likelihood (Self AND others together)
#First create a column with the total TAF-Likelihood at mid
data$taf_likelihood_mid <- data$taf_likelihood_self_mid + data$taf_likelihood_other_mid

mean(data$taf_likelihood_mid, na.rm = TRUE)
sd(data$taf_likelihood_mid, na.rm = TRUE)

# 6) Boxplot of two subscales TAF-Moral and TAF-Likelihood at mid
long_data <- pivot_longer(
  data,
  cols = c(taf_moral_mid, taf_likelihood_mid),
  names_to = "variable",
  values_to = "value"
)

plot_3 <- ggplot(long_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.color = "gray40", alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_x_discrete(labels = c(
    "taf_likelihood_mid" = "TAF Likelihood",
    "taf_moral_mid" = "TAF Moral"
  )) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    x = "TAF Subscales",
    y = "Score"
  ) +
  theme_classic()+
theme(legend.position = "none")

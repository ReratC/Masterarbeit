###############################################################################
# DATA ANALYSIS PART 1: Description of Data
###############################################################################
# In the first part of the data analysis, the aim is to explore and describe 
# the data using descriptive statistics and visual inspection. Firstly, 
# information about demographics is obtained. Then, the two main study 
# variables (BDI and TAF) are described.

###############################################################################
# Opening and cleaning the data
###############################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2)

# Open the csv. file and save the path as "path", and the data as "data". Load
# the dataset used for the analysis
path <- "C:/Users/Camille Rérat/Documents/Camille/BASEL/Master/Masterarbeit/Analysis Thesis/Data/processed/data_1st_part.csv"
data <- read.csv2(path)

# Delete empty columns X and X.1 - X.5 created during data export (contain no data)
data[c("X", "X.1", "X.2", "X.3", "X.4", "X.5")] <- NULL

# Delete empty rows 32-67 created during data export (contain no data)
data <- data[-c(32:67), ]

# Clean the name of each variable (lowercase, no special characters)
data <- janitor :: clean_names (data)

# Rename the two groups as "Control group" and "Experimental group"
data$group <- factor(
  data$group,
  levels = c("CBM-I Training ", "Control"),
  labels = c("Experimental group", "Control group")
)

###############################################################################
# Demographics
###############################################################################
# Note: R does not display correctly the "ä" in "Männlich". As the dataset only 
# contains the categories "Weiblich" and "Männlich" (no participant selected 
# "Other") the issue can be addressed that way:
data$geschlecht <- ifelse(data$geschlecht == "Weiblich", "Weiblich", "Männlich")

# Number of people per group and corresponding percentage
table(data$group) # number of people in both groups
counts <- table(data$group)   #percentage
prop.table(counts) * 100      

# Gender distribution (counts and percentages calculated within each group)
library(dplyr)
data %>%
  group_by(group, geschlecht) %>%          
  summarise(count = n(), .groups = "drop") %>%  
  group_by(group) %>%                  
  mutate(percent = count / sum(count) * 100)

# Rename age variable (originally labelled "id_pre")
names(data)[names(data) == "id_pre"] <- "age"   

# Mean and SD of age in the total sample 
mean(data$age, na.rm = TRUE)  
sd(data$age, na.rm = TRUE)

# Mean age by group
aggregate(age ~ group, data = data, mean, na.rm = TRUE)

# Range of the age variable in the total sample
range(data$age, na.rm = TRUE)

############################# TABLE WITH DEMOGRAPHICS ##########################
# Create APA-style demographic table
# Load important packages 
library(dplyr)
library(tidyr)

# Total sample size used in the analysis 
total_n <- nrow(data)

# Age mean and sd by group (displayed in the table as: Mean (SD))
# 1. Compute mean and SD of age for each group
age_row <- data %>%
  group_by(group) %>%
  summarise(
    mean = mean(age, na.rm = TRUE),
    sd   = sd(age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
# 2. Age values are formated as character strings
  mutate(value = paste0(round(mean, 2), " (", round(sd, 2), ")")) %>%
# 3. Reshape to wide format in order to have each group as a column
  select(group, value) %>%
  pivot_wider(names_from = group, values_from = value)

# Add total sample column (Total Mean and SD)
age_row$Total <- paste0(
  round(mean(data$age, na.rm = TRUE), 2),
  " (",
  round(sd(data$age, na.rm = TRUE), 2),
  ")"
)
# Label should follow APA conventions
age_row <- age_row %>%
  mutate(Variable = "Age, M (SD)") %>%
  select(Variable, everything())

# Gender distribution by group
# Gender header row
gender_header <- tibble(
  Variable = "Gender, n (%)",
  "Experimental group" = "",       
  "Control group" = "",
  Total = ""
)

# Compute gender counts and percentages (within group)
gender_rows <- data %>%
  count(group, geschlecht) %>%
  group_by(group) %>%
  mutate(
    percent = round(n / sum(n) * 100, 1),
    value = paste0(n, " (", percent, "%)")
  ) %>%
  ungroup() %>%
# Reshape to wide format
  select(group, geschlecht, value) %>%
  pivot_wider(names_from = group, values_from = value)

# Add total column (counts + 100%)
gender_rows <- gender_rows %>%
  mutate(
    Total = paste0(
      rowSums(
        sapply(select(., -geschlecht), 
               function(x) as.numeric(sub(" .*", "", x)))
      ),
      " (100%)"
    ),
    # Indent category labels (Male, Female) under Gender header
    Variable = paste0("  ", geschlecht)  
  ) %>%
  select(Variable, 'Experimental group', 'Control group', Total)

# Table (APA-Style): Combine all rows 
table_apa <- bind_rows(
  age_row,
  gender_header,
  gender_rows
)

# Create a version of the table which can be directly opened in Word
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
# Description of the BDI variable
###############################################################################
# The intervention took place between mid and post assessments.
# Therefore the focus is on mid and post BDI scores.

# Rename the BDI pre-intervention for clarity
names(data)[names(data) == "bdi_tot_pre"] <- "bdi_pre"

# Mean BDI scores at mid assessment by group
aggregate(bdi_mid ~ group, data = data, mean, na.rm = TRUE)

# Standard deviation of BDI scores at mid assessment by group
aggregate(bdi_mid ~ group, data = data, sd, na.rm = TRUE)

# Mean BDI scores at post assessment by group
aggregate(bdi_post ~ group, data = data, mean, na.rm = TRUE)

# Standard deviation of BDI scores at post assessment by group 
aggregate(bdi_post ~ group, data = data, sd, na.rm = TRUE)

# Distribution of the BDI scores within each group before CBM-I/ control group
# trainig (Shapiro-Wilk test)
by(data$bdi_mid, data$group, shapiro.test)

########################### GRAPHS BDI ########################################
# Load packages
library(ggplot2)
library(patchwork) # to combine ggplots

# Histogram of BDI scores before the CBM-I/control group training (= at mid assessment)
# Goal: visualise the distribution of depression scores in both groups
ggplot(data, aes(x = bdi_mid)) +
  geom_histogram(
    binwidth = 1,
    fill = "slategray3",
    color = "gray30",
    alpha = 0.85
  ) +
  facet_wrap(~ group) +
  scale_x_continuous(
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Depression score (BDI)",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

# Boxplot: BDI scores before training (= at mid assessment)
# Goal: Compare depression scores between groups
plot_1 <- ggplot(data, aes(x = group, y = bdi_mid, fill = group)) +
  geom_boxplot(outlier.color = "gray40", alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_brewer(
    palette = "Blues",
  ) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) + # Highest BDI score is 27 in the sample
  labs(
    x = "Group",
    y = "Depression scores (BDI)"
  ) +
  theme_classic() +
  theme(legend.position = "none")

# Boxplot: BDI scores after training
# Goal: Compare depression scores between groups
plot_2 <- ggplot(data, aes(x = group, y = bdi_post, fill = group)) +
  geom_boxplot(outlier.color = "gray40", alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_brewer(
    palette = "Blues",
  ) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(
    x = "Group",
    y = "Depression scores (BDI)"
  ) +
  theme_classic() +
  theme(legend.position = "none")

# Combine plot 1 and plot 2
plot_1 + plot_2

###############################################################################
# Description of the TAF variable
###############################################################################
# Note: the TAF variable will be used for a correlation. Only the TAF scores
# before the CBM-I / control group training (= at mid assessment) is of interest in this 
# thesis. Moreover, the TAF scores will always be calculated for the total sample.

# Mean TAF scores at mid assessment 
mean(data$taf_tot_mid, na.rm = TRUE)

# SD TAF scores at mid assessment
sd(data$taf_tot_mid, na.rm = TRUE)

# Range of the TAF variable at mid assessment
range(data$taf_tot_mid, na.rm = TRUE)

# Distribution of the BDI scores
# Use of a Shapiro Test to test for normality, because the sample is small
shapiro.test(data$taf_tot_mid)

# Mean and SD of the three TAF subscales at mid assessment
mean(data$taf_moral_mid, na.rm = TRUE)
mean(data$taf_likelihood_self_mid, na.rm = TRUE)
mean(data$taf_likelihood_other_mid, na.rm = TRUE)

sd(data$taf_moral_mid, na.rm = TRUE)
sd(data$taf_likelihood_self_mid, na.rm = TRUE)
sd(data$taf_likelihood_other_mid, na.rm = TRUE)

# Mean and SD of TAF Likelihood (= TAF Likelihood Self AND TAF Likelihood others
# as one score)
#Firstly Create a column with the total TAF-Likelihood at mid
data$taf_likelihood_mid <- data$taf_likelihood_self_mid + data$taf_likelihood_other_mid
# Then measure the Mean and SD
mean(data$taf_likelihood_mid, na.rm = TRUE)
sd(data$taf_likelihood_mid, na.rm = TRUE)

############################## GRAPHS TAF######################################
# Histogram of total TAF scores
# Goal: visualise the distribution of TAF scores before the CBM-I/control group
# training (= at mid assessment)
ggplot(data, aes(x = taf_tot_mid)) +
  geom_histogram(
    binwidth = 1,
    fill = "slategray3",
    color = "gray30",
    alpha = 0.85
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "TAF scores (TAFS)",
    y = "Count"
  ) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Boxplot of the two TAF subscales (TAF-Moral and TAF-Likelihood)
# Goal: Compare the two subscales at mid assessment
# 1. Transform the data from Wide to long
long_data <- pivot_longer(
  data,
  cols = c(taf_moral_mid, taf_likelihood_mid),
  names_to = "variable",
  values_to = "value"
)

plot_3 <- ggplot(long_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.color = "gray40", alpha = 1.5) +
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


###############################################################################
# DATA ANALYSIS PART 2: Testing the hypotheses
###############################################################################
# In that part, the 3 hypotheses will be tested
# H1: Evolution of BDI scores during the CBM-I training
# H2: Difference between groups in BDI reduction
# H3: Association between BDI scores and TAF scores

###############################################################################
#                        HYPOTHESIS 1
################################################################################
# H1: Participants in the experimental group will show a significant reduction 
# in depressive symptoms (BDI) after the CBM-I training.

# Prepare sample for the analysis
table(data$group)
exp_data <- subset(data, group == "Experimental group")
colSums(is.na(exp_data[, c("bdi_mid", "bdi_post")])) # no missing data 

# Normality of the difference scores in the experimental group: it is normally 
# distributed
exp_data$D <- exp_data$bdi_post - exp_data$bdi_mid # Compute the difference scores
sum(complete.cases(exp_data$bdi_mid, exp_data$bdi_post)) # 21 participants with no missing data

# Test for normality 
hist(exp_data$D, breaks = 10)  # Histogram shows a normal distribution
qqnorm(exp_data$D); qqline(exp_data$D)  # Seems normally distributed
shapiro.test(exp_data$D)  # p-value of 0.79, data normally distributed

# Small sample size and normal distribution of the difference scores: paired 
# t-test can be implemented. Because of the exploratory nature of the analysis, 
# the t-test will be two-tailed
t_res <- t.test(
  exp_data$bdi_post,
  exp_data$bdi_mid,
  paired = TRUE,
  alternative = "two.sided"
)

t_res
# Note: mean difference = post - mid

# Measure the effect size (Cohen's d)
install.packages("effectsize")
library(effectsize)

cohens_d(
  exp_data$bdi_post,
  exp_data$bdi_mid,
  paired = TRUE
)

###############################################################################
#                        HYPOTHESIS 2
################################################################################
# H2: Participants in the experimental group will show a greater reduction in 
# depressive symptoms than participants in the control group after the training.

# Sample information: different group sizes, small sample size. Therefore, a 
# LME is used instead of a mixed ANOVA
table(data$group)  # 31 participants in total
sum(complete.cases(data$bdi_mid, data$bdi_post))  # no missing data

# Prepare and reshape data for the LME
install.packages("tidyr")
library(tidyr)
library(dplyr)

# Add an ID column
data$subject <- 1:nrow(data)

# Long format for LME
data_long <- data %>%
  pivot_longer(cols = c(bdi_mid, bdi_post),
               names_to = "Time",
               values_to = "BDI") %>%
  mutate(
    Time = factor(Time, levels = c("bdi_mid", "bdi_post"), labels = c("Mid", "Post")),
    Group = factor(group),
    Subject = factor(subject)
  )

# Run the LME
# Install and load packages
install.packages("lme4")
install.packages("lmerTest")

library(lme4)
library(lmerTest)

model <- lmer(BDI ~ Time * Group + (1 | Subject), data = data_long)

summary(model)  
anova(model)    

# Check LME assumptions: normal distribution of the residuals and homoscedasticity
# 1. Normality of residuals
qqnorm(resid(model))
qqline(resid(model)) # Residuals approximately normally distributed 
# 2. Homoscedasticity
plot(fitted(model), resid(model))  # Homoscedasticity is acceptable

# Create APA table with the Output which can be opened in Word
# 1. Fixed effects data frame
fixed_effects <- data.frame(
  `Fixed Effect` = c("Intercept", "Time (Post)", "Group (Control)", "Time × Group (Control)"),
  B = c(9.571, -1.524, -1.271, 5.124),
  SE = c(1.657, 1.548, 2.918, 2.726),
  df = c(44.016, 29.000, 44.016, 29.000),
  t = c(5.776, -0.984, -0.436, 1.880),
  p = c("< .001", ".33", ".67", ".07")
)

# 2. Round numeric columns to 2 decimals (except p, already formatted)
fixed_effects$B <- round(fixed_effects$B, 2)
fixed_effects$SE <- round(fixed_effects$SE, 2)
fixed_effects$t <- round(fixed_effects$t, 2)
fixed_effects$df <- round(fixed_effects$df, 2)

# 3. Rename and reorder columns for APA: Fixed Effect | B | SE | df | t | p | Significance
colnames(fixed_effects) <- c("FixedEffect", "B", "SE", "df", "t", "p")
fixed_effects <- fixed_effects[, c("FixedEffect", "B", "SE", "df", "t", "p")]

# APA-style table as text
library(knitr)
kable(fixed_effects, format = "pandoc", digits = 2, caption = "Fixed effects from LME predicting BDI-II")


# Effect size: Cohen's d between groups
library(dplyr)

# Calculate mid-post change per participant
change_data <- data_long %>%
  pivot_wider(names_from = Time, values_from = BDI) %>%  # make wide format again for change
  mutate(Change = Mid - Post)  # mid minus post, positive = improvement

# Check the result
head(change_data)

# Separate change scores by group
change_exp <- change_data$Change[change_data$Group == "Experimental group"]  
change_ctrl <- change_data$Change[change_data$Group == "Control group"]

# Pooled SD
sd_pooled <- sqrt( ((length(change_exp)-1)*var(change_exp) + (length(change_ctrl)-1)*var(change_ctrl)) /
                     (length(change_exp) + length(change_ctrl) - 2) )

# Cohen's d
cohens_d <- (mean(change_exp) - mean(change_ctrl)) / sd_pooled

cohens_d

###############################################################################
#                        HYPOTHESIS 3
################################################################################
# H3: Higher BDI scores will be associated with higher TAF scores.

# Test for  normality of both variables at mid 
hist(data$bdi_mid,
     breaks = "Sturges",
     main = "Histogram of BDI scores",
     xlab = "BDI score")                  # not normally distributed

hist(data$taf_tot_mid,
     breaks = "Sturges",
     main = "Histogram of TAF scores",
     xlab = "TAF score")                  # not normally distributed

# Check for extreme outliers
boxplot(data$bdi_mid, main = "BDI")
boxplot(data$taf_tot_mid, main = "TAF")

Q1 <- quantile(data$bdi_mid, 0.25, na.rm = TRUE)
Q3 <- quantile(data$bdi_mid, 0.75, na.rm = TRUE)
IQRv <- IQR(data$bdi_mid, na.rm = TRUE)

lower_extreme <- Q1 - 3 * IQRv
upper_extreme <- Q3 + 3 * IQRv

data$bdi_mid[data$bdi_mid < lower_extreme | data$bdi_mid > upper_extreme]
# No extreme outliers for the BDI variable

Q1b <- quantile(data$taf_tot_mid, 0.25, na.rm = TRUE)
Q3b <- quantile(data$taf_tot_mid, 0.75, na.rm = TRUE)
IQRv <- IQR(data$taf_tot_mid, na.rm = TRUE)

lower_extreme <- Q1b - 3 * IQRv
upper_extreme <- Q3b + 3 * IQRv

data$taf_tot_mid[data$taf_tot_mid < lower_extreme | data$taf_tot_mid > upper_extreme]
# No extreme outliers for the TAF variable

# Visualize the data
plot(data$bdi_mid, data$taf_tot_mid,
     xlab = "BDI score",
     ylab = "TAF score",
     main = "Scatterplot of BDI and TAF")

# Both variables are not normally distributed and the sample is small. Therefore,
# a Spearman correlation is implemented
cor.test(data$bdi_mid, data$taf_tot_mid,
         method = "spearman",
         use = "complete.obs")

# Scatterplot 
library(ggplot2)

# 1. Compute Spearman rho
rho <- cor(data$bdi_mid, data$taf_tot_mid, method = "spearman", use = "complete.obs")
# 2. Create the plot
ggplot(data, aes(x = bdi_mid, y = taf_tot_mid)) +
  geom_point(color = "black", size = 2) +                   
  geom_smooth(method = "loess", color = "slategray3", se = FALSE, linewidth = 1) +
  annotate("text", 
           x = max(data$bdi_mid)*0.7, 
           y = max(data$taf_tot_mid)*0.9,
           label = paste0("Spearman ρ = ", round(rho, 2)),
           size = 4, hjust = 0, fontface = "plain") +
  labs(title = NULL,                                           
       x = "BDI score",
       y = "TAF score") +
  theme_minimal(base_size = 12) +                             
  theme(
    axis.title = element_text(face = "plain"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_blank(),                       
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black") 
  )

# There is a small, non-significant positive correlation between both variables.
# As both could be influence by OCD, a partial correlation controlling
# for OCD (measured with DOCS) will be implemented. 
# As the variables are not normally distributed, the partial correlation will be
# measured using the residual from the regression (non-parametric friendly approach)

# Regress BDI on DOCS
resid_bdi <- resid(lm(bdi_mid ~ docs_tot_mid, data = data))

# Regress TAF on DOCS
resid_taf <- resid(lm(taf_tot_mid ~ docs_tot_mid, data = data))

# Spearman correlation of residuals
cor.test(resid_bdi, resid_taf, method = "spearman")
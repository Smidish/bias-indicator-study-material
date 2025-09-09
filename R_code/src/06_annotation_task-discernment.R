####  00. Setup  ####

library(dplyr)
library(car)
library(tidyverse)
library(caret)
library(lme4) #lmer()
library(lmerTest)
library("report") # genereate reports from R objects
library(ggResidpanel) #visualize residual plots
library(mltools)
library(psycho) 
library(ggplot2)
#install.packages("ggbeeswarm")
library(ggbeeswarm)
library(RColorBrewer)
library(patchwork)
#install.packages("viridis")
library(viridis)
#install.packages("effsize")

#### 01.Load data ####

rm(list = ls())
setwd(".../R-Code")

df_d <-read.csv("./data/processed/survey_detection_long")
mat_d <-read.csv("./data/raw/mat_detection.csv")

gold1 <- read.csv("./data/temp/gold_1.csv")
gold2 <- read.csv("./data/temp/gold_2.csv")
gold3 <- read.csv("./data/temp/gold_3.csv")
gold4 <- read.csv("./data/temp/gold_4.csv")
gold5 <- read.csv("./data/temp/gold_5.csv")
gold6 <- read.csv("./data/temp/gold_6.csv")


#### 02.Quick comparison between participants ####
#Look at the word-level annotations of the gold standard and two participants 
#view(gold3[, c(1, 2, grep("cd1a9208", colnames(gold3)), grep("7888fc", colnames(gold3)))])


#### 03. Calculate dprime & join tables ####

# Remove NAs 
df_d <- df_d[complete.cases(df_d$Annotations), ]

#Calculate mcc and dprime based on the confusion matrix values, filter NA rows
#df_d$mcc <- mcc(TP = df_d$TP, FP = df_d$FP, TN = df_d$TN, FN = df_d$FN)
dprime <- dprime(n_hit = df_d$TP, n_fa = df_d$FP, n_miss =  df_d$FN, n_cr =  df_d$TN)
df_d <- cbind(df_d, dprime)

# Join with material values
library(dplyr)
df_d <- left_join(df_d, mat_d, by = "s_id")
df_d <- rename(df_d, matPol = polDummy)

# Fix BiasScore values
df_d$biasScore <- gsub(",", ".", df_d$biasScore)
#view(df_d)

#### 04. Correlations ####

cor.test(df_d$bias,as.numeric(df_d$biasScore), method ="pearson")
cor(df_d$bias, as.numeric(df_d$biasScore))
cor(df_d$bias, df_d$dprime)
cor(df_d$bias, df_d$Annotations)
cor(df_d$bias, df_d$TP)


#### 05. Exploratory F1 Plots ####

##### 5.1 Per Treatment Group #####

#F1
ggplot(df_d, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(co), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Indicator Group") +
  #ggtitle("Perceived Bias vs Detected Bias (F1) by Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  theme_bw(base_size = 20)

#d'
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(co), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d' Prime", color = "Indicator Group") +
  #ggtitle("Perceived Bias vs Detected Bias (d') by Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  theme_bw(base_size = 20)


##### 5.2 Per Congruence #####
# 1 represents congruence, and 0 represents non-congruence
df_d$heuristic <- as.factor(df_d$heuristic)
df_d$congruence <- as.factor(df_d$congruence)

viridis_colors <- viridis(n = 5, option = "D")[c(1, 4)]

#F1
ggplot(df_d, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(co), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = congruence, color = congruence), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Political Congruency") +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Congruency") +
  scale_color_manual(
    values = viridis_colors,
    labels = c("Non-congruent", "Congruent")  # Custom labels for 0 and 1
  ) +
  theme_bw()

#d'prime
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  geom_smooth(aes(group = congruence, color = congruence), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d'prime", color = "Political Congruency") +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Congruency") +
  scale_color_manual(
    values = viridis_colors,
    labels = c("Non-congruent", "Congruent")  # Custom labels for 0 and 1
  ) +
  theme_bw()

##### 5.3 Per Pol Category: 1 = liberal, 2= moderate, 3=conservative #####
large_viridis_colors <- viridis(n = 10, option = "D")
viridis_colors <- c(large_viridis_colors[1], large_viridis_colors[5], large_viridis_colors[8])  # Select a slightly darker yellow

#F1
ggplot(df_d, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.1, size = 2) +
  geom_smooth(aes(group = pol_dummy, color = pol_dummy), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Political Orientation") +
  scale_color_manual(
    values = viridis_colors,
    labels = c("liberal", "moderate", "conservative")  # Custom labels for each level
  ) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Political Orientation") +
  theme_bw()

#d'prime
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  geom_smooth(aes(group = pol_dummy, color = pol_dummy), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d'prime", color = "Political Orientation") +
  scale_color_manual(
    values = viridis_colors,
    labels = c("liberal", "moderate", "conservative")  # Custom labels for each level
  ) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Political Orientation") +
  theme_bw()


#### 06.  Exploratory D Prime Plots ####

##### 6.1 One regression line per treatment ######
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(color = heuristic), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Heuristic") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Indicator Group")

ggplot(df_d, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(color = heuristic), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Heuristic") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Indicator Group")

##### 6.2 One regression line per pol category (pol_dummy: 1 = liberal, 2 = moderate, 3 = conservative) #####
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  geom_smooth(aes(group = pol_dummy, color = pol_dummy), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Congruence") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d') by Heuristic")


#### 6.3 One regression line per congruence #####
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  geom_smooth(aes(group = congruence, color = congruence), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Congruence") +
  ggtitle("Perceived Bias vs Biased Words (%) by Heuristic")

##### 6.4 Filter per Statements #####

#F1
df_d_subset <- filter(df_d, s_id == 1)
ggplot(df_d_subset, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Group (Statement 1)")

df_d_subset <- filter(df_d, s_id == 2)
ggplot(df_d_subset, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Group (Statement 2)")

df_d_subset <- filter(df_d, s_id == 3)
ggplot(df_d_subset, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Group (Statement 3)")

df_d_subset <- filter(df_d, s_id == 4)
ggplot(df_d_subset, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Group (Statement 4)")

df_d_subset <- filter(df_d, s_id == 5)
ggplot(df_d_subset, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Group (Statement 5)")

df_d_subset <- filter(df_d, s_id == 6)
ggplot(df_d_subset, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (F1) by Group (Statement 6)")


#d'prime
df_d_subset <- filter(df_d, s_id == 1)
ggplot(df_d_subset, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d'prime", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Group (Statement 1)")

df_d_subset <- filter(df_d, s_id == 2)
ggplot(df_d_subset, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d'prime", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Group (Statement 2)")

df_d_subset <- filter(df_d, s_id == 3)
ggplot(df_d_subset, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Group (Statement 3)")

df_d_subset <- filter(df_d, s_id == 4)
ggplot(df_d_subset, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d'prime", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by  Group (Statement 4)")

df_d_subset <- filter(df_d, s_id == 5)
ggplot(df_d_subset, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d'prime", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Group (Statement 5)")

df_d_subset <- filter(df_d, s_id == 6)
ggplot(df_d_subset, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "d'prime", color = "Indicator Group") +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  ggtitle("Perceived Bias vs Detected Bias (d'prime) by Group (Statement 6)")


#### 07. Treatment Summary Plots ####
#important
View(df_d)
tests_gauge_cont_df <- df_d %>% filter(heuristic %in% c("BIAS_GAUGE", "CONTROL"))
tests_pol_cont_df <- df_d %>% filter(heuristic %in% c("POLITICAL", "CONTROL"))
tests_hi_cont_df <- df_d %>% filter(heuristic %in% c("BIAS_HIGH", "CONTROL"))
tests_trust_cont_df <- df_d %>% filter(heuristic %in% c("TRUST", "CONTROL"))
tests_bar_cont_df <- df_d %>% filter(heuristic %in% c("BIAS_BAR", "CONTROL"))
tests_sentiment_cont_df <- df_d %>% filter(heuristic %in% c("SENTIMENT", "CONTROL"))
View(tests_hi_cont_df)

# =========================
# T Tests (F1)
# =========================
#For F1 Scores Highlights
t.test(tests_hi_cont_df$F1~tests_hi_cont_df$heuristic)
wilcox.test(tests_hi_cont_df$F1~tests_hi_cont_df$heuristic)
ks.test(tests_hi_cont_df$F1~tests_hi_cont_df$heuristic)

t.test(tests_pol_cont_df$F1~tests_pol_cont_df$heuristic)
wilcox.test(tests_pol_cont_df$F1~tests_pol_cont_df$heuristic)
ks.test(tests_pol_cont_df$F1~tests_pol_cont_df$heuristic)

t.test(tests_gauge_cont_df$F1~tests_gauge_cont_df$heuristic)
wilcox.test(tests_gauge_cont_df$F1~tests_gauge_cont_df$heuristic)
ks.test(tests_gauge_cont_df$F1~tests_gauge_cont_df$heuristic)

t.test(tests_trust_cont_df$F1~tests_trust_cont_df$heuristic)
wilcox.test(tests_trust_cont_df$F1~tests_trust_cont_df$heuristic)
ks.test(tests_trust_cont_df$F1~tests_trust_cont_df$heuristic)

t.test(tests_bar_cont_df$F1~tests_bar_cont_df$heuristic)
wilcox.test(tests_bar_cont_df$F1~tests_bar_cont_df$heuristic)
ks.test(tests_bar_cont_df$F1~tests_bar_cont_df$heuristic)

t.test(tests_sentiment_cont_df$F1~tests_sentiment_cont_df$heuristic)
wilcox.test(tests_sentiment_cont_df$F1~tests_sentiment_cont_df$heuristic)
ks.test(tests_sentiment_cont_df$F1~tests_sentiment_cont_df$heuristic)

p_values <- c(
  0.0002387, # BIAS_HIGH vs CONTROL F1
  1.493e-05, # BIAS_GAUGE vs CONTROL F1
  0.008342,  # TRUST vs CONTROL F1
  0.9271,    # BIAS_BAR vs CONTROL F1
  0.4086,   # POLITICAL vs CONTROL F1
  .0395     #SENTIMENT vs CONTROL F1
)

# Bonferroni correction
p_bonf <- p.adjust(p_values, method = "bonferroni")
# Benjamini-Hochberg (FDR) correction
p_bh <- p.adjust(p_values, method = "BH")
data.frame(original = p_values, bonferroni = p_bonf, BH = p_bh)


library(effsize)

#Cohen's d 
cohen.d(tests_gauge_cont_df$F1 ~ tests_gauge_cont_df$heuristic)
cohen.d(tests_hi_cont_df$F1 ~ tests_hi_cont_df$heuristic)
cohen.d(tests_pol_cont_df$F1 ~ tests_pol_cont_df$heuristic)
cohen.d(tests_trust_cont_df$F1 ~ tests_trust_cont_df$heuristic)
cohen.d(tests_bar_cont_df$F1 ~ tests_bar_cont_df$heuristic)
cohen.d(tests_sentiment_cont_df$F1 ~ tests_sentiment_cont_df$heuristic)

# =========================
# ANOVA: F1
# =========================

# Combine all F1 dprime data into a single data frame
all_f1 <- rbind(
  data.frame(heuristic = tests_hi_cont_df$heuristic, F1 = tests_hi_cont_df$F1),
  data.frame(heuristic = tests_gauge_cont_df$heuristic, F1 = tests_gauge_cont_df$F1),
  data.frame(heuristic = tests_pol_cont_df$heuristic, F1 = tests_pol_cont_df$F1),
  data.frame(heuristic = tests_trust_cont_df$heuristic, F1 = tests_trust_cont_df$F1),
  data.frame(heuristic = tests_bar_cont_df$heuristic, F1 = tests_bar_cont_df$F1),
  data.frame(heuristic = tests_sentiment_cont_df$heuristic, F1 = tests_sentiment_cont_df$F1)
)

# Fit one-way ANOVA model
anova_model_f1 <- aov(F1 ~ heuristic, data = all_f1)
summary(anova_model_f1)

# =========================
# Effect Sizes
# =========================
library(effectsize)

# Eta-squared (proportion of variance explained)
eta_squared(anova_model_f1, partial = FALSE)   # η²

# Partial eta-squared (same as η² for one-way designs)
eta_squared(anova_model_f1, partial = TRUE)    # partial η²

# Cohen's f
cohens_f(anova_model_f1)

# =========================
# Post-hoc comparisons
# =========================
# Tukey HSD
TukeyHSD(anova_model_f1)

subset_data_gauge <- subset(all_f1, heuristic %in% c("BIAS_GAUGE", "CONTROL"))
cohens_d(F1 ~ heuristic, data = subset_data_gauge)
subset_data_high <- subset(all_f1, heuristic %in% c("BIAS_HIGH", "CONTROL"))
cohens_d(F1 ~ heuristic, data = subset_data_high)
subset_data_trust <- subset(all_f1, heuristic %in% c("TRUST", "CONTROL"))
cohens_d(F1 ~ heuristic, data = subset_data_trust)

# beeswarm instead of boxplot
ggplot(df_d, aes(x=heuristic, y=F1, color=heuristic)) +
  geom_beeswarm() +
  guides(colour = guide_legend(title = "Indicator")) +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  theme_bw() #legend.position = "none"

#add random noise to make to shape visible
ggplot(df_d, aes(x=heuristic, y=F1, color=heuristic)) +
  geom_quasirandom() +
  xlab("Indicator Group") +
  ylab("F1 Score") +
  guides(colour = guide_legend(title = "Indicator Group")) + #F1_allGroups_RandomNoisePointBoxplot
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  theme_bw()

#scatterplot by Group
#ggplot(df_d, aes(x=heuristic, y=F1, color=heuristic)) +
#  geom_point() +
#  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
#  theme_bw()

# =========================
#T Tests for D'prime Scores
# =========================
t.test(tests_hi_cont_df$dprime~tests_hi_cont_df$heuristic)
wilcox.test(tests_hi_cont_df$dprime~tests_hi_cont_df$heuristic)
ks.test(tests_hi_cont_df$dprime~tests_hi_cont_df$heuristic)

t.test(tests_gauge_cont_df$dprime~tests_gauge_cont_df$heuristic)
wilcox.test(tests_gauge_cont_df$dprime~tests_gauge_cont_df$heuristic)
ks.test(tests_gauge_cont_df$dprime~tests_gauge_cont_df$heuristic)

t.test(tests_pol_cont_df$dprime~tests_pol_cont_df$heuristic)
wilcox.test(tests_pol_cont_df$dprime~tests_pol_cont_df$heuristic)
ks.test(tests_pol_cont_df$dprime~tests_pol_cont_df$heuristic)

t.test(tests_trust_cont_df$dprime~tests_trust_cont_df$heuristic)
wilcox.test(tests_trust_cont_df$dprime~tests_trust_cont_df$heuristic)
ks.test(tests_trust_cont_df$dprime~tests_trust_cont_df$heuristic)

t.test(tests_bar_cont_df$dprime~tests_bar_cont_df$heuristic)
wilcox.test(tests_bar_cont_df$dprime~tests_bar_cont_df$heuristic)
ks.test(tests_bar_cont_df$dprime~tests_bar_cont_df$heuristic)

t.test(tests_sentiment_cont_df$dprime~tests_sentiment_cont_df$heuristic)
wilcox.test(tests_sentiment_cont_df$dprime~tests_sentiment_cont_df$heuristic)
ks.test(tests_sentiment_cont_df$dprime~tests_sentiment_cont_df$heuristic)

p_values_d <- c(
  1.865e-05, # BIAS_HIGH vs CONTROL dprime
  0.0008226, # BIAS_GAUGE vs CONTROL dprime
  0.00447,  # TRUST vs CONTROL dprime
  0.8281,    # BIAS_BAR vs CONTROL dprime
  0.5544,     # POLITICAL vs CONTROL dprime
  0.1012     # SENTIMENT vs CONTROL dprime
)

# Bonferroni correction
p_bonf_d <- p.adjust(p_values_d, method = "bonferroni")

# Benjamini-Hochberg (FDR) correction
p_bh_d <- p.adjust(p_values_d, method = "BH")

# Check results
data.frame(original = p_values_d, bonferroni = p_bonf_d, BH = p_bh_d)

#Cohen's d 
cohen.d(tests_gauge_cont_df$dprime ~ tests_gauge_cont_df$heuristic)
cohen.d(tests_hi_cont_df$dprime ~ tests_hi_cont_df$heuristic)
cohen.d(tests_pol_cont_df$dprime ~ tests_pol_cont_df$heuristic)
cohen.d(tests_trust_cont_df$dprime ~ tests_trust_cont_df$heuristic)
cohen.d(tests_bar_cont_df$dprime ~ tests_bar_cont_df$heuristic)
cohen.d(tests_sentiment_cont_df$dprime ~ tests_sentiment_cont_df$heuristic)

# =========================
# ANOVA for D'prime Scores
# =========================
all_dprime <- rbind(
  data.frame(heuristic = tests_hi_cont_df$heuristic, dprime = tests_hi_cont_df$dprime),
  data.frame(heuristic = tests_gauge_cont_df$heuristic, dprime = tests_gauge_cont_df$dprime),
  data.frame(heuristic = tests_pol_cont_df$heuristic, dprime = tests_pol_cont_df$dprime),
  data.frame(heuristic = tests_trust_cont_df$heuristic, dprime = tests_trust_cont_df$dprime),
  data.frame(heuristic = tests_bar_cont_df$heuristic, dprime = tests_bar_cont_df$dprime),
  data.frame(heuristic = tests_sentiment_cont_df$heuristic, dprime = tests_sentiment_cont_df$dprime)
)
anova_model <- aov(dprime ~ heuristic, data = all_dprime)
summary(anova_model)

library(effectsize)

# eta-squared
eta_squared(anova_model, partial = FALSE)   # η² (proportion of variance explained)
# partial eta-squared
eta_squared(anova_model, partial = TRUE)    # partial η²
cohens_f(anova_model) #cohens f
# POST HOC: Tukey HSD post-hoc
TukeyHSD(anova_model)

subset_data_gauge_d <- subset(all_dprime, heuristic %in% c("BIAS_GAUGE", "CONTROL"))
cohens_d(dprime ~ heuristic, data = subset_data_gauge_d)
subset_data_high_d <- subset(all_dprime, heuristic %in% c("BIAS_HIGH", "CONTROL"))
cohens_d(dprime ~ heuristic, data = subset_data_high_d)
subset_data_trust_d <- subset(all_dprime, heuristic %in% c("TRUST", "CONTROL"))
cohens_d(dprime ~ heuristic, data = subset_data_trust_d)


ggplot(df_d, aes(x=heuristic, y=dprime, color=heuristic)) +
  geom_beeswarm() +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  theme_bw()

ggplot(df_d, aes(x=heuristic, y=dprime, color=heuristic)) +
  geom_quasirandom() +
  xlab("Indicator Group") +
  ylab("d' Prime Score") +
  guides(colour = guide_legend(title = "Indicator Group")) +
  scale_color_viridis(discrete=TRUE, limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  theme_bw()

##### 7.1 F1 score per heuristic #####

ggplot(df_d, aes(x=heuristic, y=F1, fill=heuristic)) +
  geom_boxplot(notch=TRUE) +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="white") +
  scale_fill_viridis_d(
    option = "D", limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  scale_x_discrete(
    limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  labs(
    #title = "Average Bias Detection (F1) per Indicator Group",
    x = "Treatment Group",
    y = "F1 Score",
    fill = "Indicator Group"
  ) +
  theme_minimal(base_size = 20)   # << increases all font sizes


##### 7.2 D Prime plot per heuristic #####
#important
ggplot(df_d, aes(x=heuristic, y=dprime, fill=heuristic)) +
  geom_boxplot(notch=TRUE) +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="white") +
  scale_fill_viridis_d(
    option = "D", limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  scale_x_discrete(
    limits = c("BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST", "CONTROL"), labels = c("Bias Bar", "Bias Gauge", "Bias Highlights", "Political", "Emotional", "Trust", "None (Control)")) +
  labs(
   # title="Average Bias Detection (d') per Indicator Group", 
    x="Treatment Group", 
    y="d'prime",
    fill="Indicator Group"  # Update the legend title
  )+
  theme_minimal(base_size = 20) 



ggplot(df_d, aes(x=heuristic, y=dprime, fill=heuristic)) +
  geom_boxplot(notch=TRUE) + 
  #geom_point(position=position_jitter(width=0.2, height=0), color="black", size=1) +
  #stat_summary(fun=mean, geom="point", shape=18, size=3, color="darkgrey") +
  scale_fill_viridis_d(
    option = "D", 
    labels = c("Bar", "Gauge", "Highlights", "Control", "Political", "Sentiment", "Trust")
  ) +  
  labs(
    title="Average Bias Detection (d') per Indicator Group", 
    x="Treatment Group", 
    y="d'prime",
    fill="Indicator Group"
  )


#### 08. Report average bias perception, f1 and d-prime  ####

##### 8.1 Statement Perception ####
# Aggregate the data by s_id
df_summary <- aggregate(df_d[, c("bias", "sent", "trust", "share")], 
                        by = list(df_d$s_id, df_d$key), 
                        FUN = function(x) c(mean = round(mean(x),2), sd = round(sd(x),2)))

print(df_summary)

#important
key_order <- c("cmea", "rmea", "lmea", "chia", "rhia", "lhia")
df_d$key <- factor(df_d$key, levels = key_order)
df_d$biasScore <- as.character(df_d$biasScore)

ggplot(df_d, aes(x = biasScore, y = bias, group = biasScore, fill = as.factor(biasScore))) +
  geom_boxplot() +
  scale_fill_viridis_d(
    name = "Statement Number",
    option = "D", 
    labels = c("Statement 1 (cmea)", "Statement 2 (rmea)", "Statement 3 (lmea)", "Statement 4 (chia)", "Statement 5 (rhia)", "Statement 6 (lhia)")
  ) +
labs(
  x = "Biased Words in Statement (%)",
  y = "Perceived Bias",
  title = "Boxplot of Perceived Bias by Biased Words in Statement (Expert Standard)"
) +
  facet_wrap(~key, nrow = 1, scales = "free_x") +
  theme_bw() +
  theme(legend.title = element_blank())



##### 8.2 Average d-prime scores #####
#per participant
print(aggregate(df_d$dprime, by = list(df_d$participantId), FUN = mean))
print(aggregate(df_d$dprime, by = list(df_d$key), FUN = function(x) c(mean = mean(x), sd = sd(x))))
print(aggregate(df_d$dprime, by = list(df_d$heuristic, df_d$s_id), FUN = function(x) c(mean = mean(x), sd = sd(x))))

# Aggregate the dprime data by heuristic and s_id
df_summary <- aggregate(df_d$dprime, by = list(df_d$heuristic, df_d$s_id), FUN = function(x) c(mean = mean(x), sd = sd(x)))
View(df_summary)
df_wide <- pivot_wider(df_summary, names_from = "Group.2", values_from = "x", names_prefix = "dprime_")
View(df_wide)
#average of d' prime, important, export table with f1 scores
print(aggregate(df_d$dprime, by = list(df_d$heuristic), FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))))


##### 8.3 Average f1 scores #####
print(aggregate(df_d$F1, by = list(df_d$heuristic), FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))))
print(aggregate(df_d$precision, by = list(df_d$key), FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))))
print(aggregate(df_d$recall, by = list(df_d$key), FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))))


#### 09. Annotations ####

##### 9.1 Report Annotations over Bias ####

# Colored edition with TP, FP, Sum #
df_anno <- df_d %>% 
  pivot_longer(c(Annotations, TP, FP), names_to = "anno_cat", values_to = "anno_val") %>%
  mutate(anno_cat = factor(anno_cat, levels = c("Total", "TP", "FP")))

df_anno <- df_d %>%
  pivot_longer(c(Annotations, TP, FP), names_to = "anno_cat", values_to = "anno_val")

ggplot(df_anno, aes(factor(bias), anno_val, fill = anno_cat)) +
  geom_boxplot(position = position_dodge(width = 0.85)) +
  scale_fill_manual(
    values = c("Annotations" = viridis::viridis(4)[2], 
               "TP" = viridis::viridis(4)[3], 
               "FP" = viridis::viridis(4)[4]),
    labels = c("Annotations" = "Total", 
               "TP" = "True Positives", 
               "FP" = "False Positives")
  ) +
  labs(
    title="True and False Positives VS Amount of Perceived Bias", 
    x = "Perceived Amount of Bias",
    y = "Number of Annotations",
    fill = "Category"  # Set the legend title
  ) +
  scale_x_discrete(
    labels = c("Strongly\ndisagree", "Disagree", "Somewhat\ndisagree", 
               "Somewhat\nagree", "Agree", "Strongly\nagree")
  ) +
  theme_bw()


##### 9.2 Annotations per statement ####
#important
df_anno_sum <- df_d %>% 
  group_by(s_id, key) %>% 
  summarise(
    avg_bias = round(mean(bias),3),
    sd_bias = round(sd(bias),3),
    avg_annotations = round(mean(Annotations),3),
    sd_annotations = round(sd(Annotations),3),
    avg_TP = round(mean(TP),3),
    sd_TP = round(sd(TP),3)
  )

view(df_anno_sum)


#### 10.0 Create Contrast ####

df_d$treatment.f = as.factor(df_d$heuristic)
contrasts(df_d$treatment.f) <- contr.treatment(7, base = 4) #make "CONTROL" the fourth category, the baseline

df_d$pol.m = as.factor(df_d$matPol)
contrasts(df_d$pol.m) <- contr.treatment(3, base = 2) 

df_d$pol.p = as.factor(df_d$pol_dummy)
contrasts(df_d$pol.p) <- contr.treatment(3, base = 2) 

df_d$congruence = as.factor(df_d$congruence)
contrasts(df_d$congruence) <- contr.treatment(2, base = 1) #base = not congruent 

df_d$gender <- dplyr::recode(df_d$gender, "Female" = 0, "Male" = 1, "I prefer not to say" = 2, "Other" = 2)
df_d$gender.g = as.factor(df_d$gender)
contrasts(df_d$gender.g) <- contr.treatment(3, base = 1) #base = female, 1 = male, 2 = other 

df_d$biasScore = as.numeric(df_d$biasScore)


#### 11.0 Main Model Detection: D-Prime ####
library(lme4)
library(effectsize)
install.packages("emmeans")
library(emmeans)
library(lmerTest)

##### 11.1 D-PRIME LMM, REML ####
#important
#Main Model
lmm_d <- lmer(dprime ~ bias + pol.p + congruence + pol.p * congruence+ treatment.f + gender.g + age + edu_score + biasScore + sentMag + musk1 + musk_opinion + (1|participantId), data = df_d)
#sink("lmm_d.txt")

#LRT approach
## ---------------------------
## 1. Likelihood Ratio Tests (LRTs) -> more commenly used, more conservative
## ---------------------------

# Compare full model with reduced models (drop 1 term at a time)
# test effect of bias indicator:
m_reduced_treatment <- update(lmm_d, . ~ . - treatment.f)
anova(m_reduced_treatment, lmm_d, test = "LRT")  # Likelihood ratio test

drop1(lmm_d, test = "Chisq")  # chi-sq tests via LRT for each fixed effect
confint(lmm_d) #compute 95% CIs
performance::r2(lmm_d) #Marginal R² (variance explained by fixed effects) and conditional R² (fixed + random)
summary(lmm_d)
standardize_parameters(lmm_d, method = "refit")

print(lmm_d, correlation=TRUE) #for all the correlations and p's
conf_intervals_ml_d <- confint(lmm_d, level = 0.95)
View(conf_intervals_ml_d)
#sink()

#identifying which fixed or random effects have the most impact on the model fit
drop1(lmm_d, test = "Chisq")

##### 11.2 Likelihood ratio ####
#Use ML to compare models and REML for reporting, as ML may underestimate the variance of random effects -> aecurity check for the maximum likelihood

lmm_d_null_ML <- lmer(dprime ~ (1|participantId), data = df_d, REML = FALSE)
lmm_d_main_ML <- lmer(dprime ~ bias + pol.p + congruence + treatment.f +gender.g + age + edu_score + (1|participantId), data = df_d, REML = FALSE)
lmm_d_int_ML <- lmer(dprime ~ bias + pol.p*congruence + pol.p + congruence + treatment.f + gender.g + age + edu_score + (1|participantId), data = df_d, REML = FALSE)

anova(lmm_d_main_ML, lmm_d_int_ML)

##### 11.3 Assumptions ####
library(performance)
library(lme4)
library(see)
performance::check_model(lmm_d_main_ML, panel = TRUE, check = "all")
# model fits the data well (posterior predictive check). No severe collinearity or influential observation issues. Normality of residuals and random effects is reasonable.
# minor deviations in homogeneity of variance and normality of residuals at the tails, though these are unlikely to significantly affect model performance.

ggResidpanel::resid_panel(lmm_d_main_ML, smoother = TRUE, qqbands = TRUE, type = "pearson")
#The residuals are mostly normally distributed, as shown by the Q-Q plot and histogram. Independence of residuals is not violated, according to the index plot.
# Potential Issues: The residual plot shows a slight trend, indicating potential non-linearity or missing interactions. Mild deviations from normality are observed in the tails, but they are not severe enough to warrant immediate concern.

#model_check <- performance::check_model(lmm_d_main, panel = FALSE) 
#plot(model_check, panel = FALSE, filename = c("fitted_res.png", "res_vs_pred.png", "leverage.png", "cooks_dist.png", "scale_loc.png"))


#### 12.0 Main Model Detection: F1 ####

##### 12.1 F1 LMM, REML ####
#Main Model F1
lmm_F1 <- lmer(F1 ~ bias  + pol.p + pol.p*congruence + congruence + treatment.f + gender.g + age + edu_score + musk1 + musk_opinion + biasScore + sentMag + (1|participantId), data = df_d)
#sink("lmmF1.txt")

## ---------------------------
## 1. Likelihood Ratio Tests (LRTs) -> more commenly used, more conservative
## ---------------------------

# Compare full model with reduced models (drop treatment indicator)
m_reduced_treatment_f <- update(lmm_F1, . ~ . - treatment.f)
anova(m_reduced_treatment_f, lmm_F1, test = "LRT") 
drop1(lmm_F1, test = "Chisq")  # chi-sq tests via LRT for each fixed effect
confint(lmm_F1) #compute 95% CIs
performance::r2(lmm_F1) #Marginal R² (variance explained by fixed effects) and conditional R² (fixed + random)

summary(lmm_F1) #p-values is Pr(>|t|), using Satterthwaite
#print(lmm_F1, correlation=TRUE)
standardize_parameters(lmm_F1, method = "refit")
#sink()

#identifying which fixed or random effects have the most impact on the model fit
drop1(lmm_F1, test = "Chisq")


##### 12.2 Likelihood ratio test ####
#Use ML to compare models and REML for reporting, as ML may underestimate the variance of random effects

lmm_F1_null_ML <- lmer(F1 ~ (1|participantId), data = df_d, REML = FALSE)
lmm_F1_main_ML <- lmer(F1 ~ bias + pol.p + congruence + treatment.f + gender + age + edu_score + (1|participantId), data = df_d, REML = FALSE)
lmm_F1_int_ML <- lmer(F1 ~ bias + pol.p*congruence + pol.p + congruence + treatment.f + gender + age + edu_score + (1|participantId), data = df_d, REML = FALSE)

anova(lmm_F1_main_ML, lmm_F1_int_ML)
drop1(lmm_F1_main_ML, test = "Chisq")


##### 12.3 Assumptions ####
performance::check_model(lmm_F1_main_ML) 
#model fits the data well, as shown by the posterior predictive check.
#Linearity, homoscedasticity, and independence of residuals are satisfied. No significant collinearity among predictors. Residuals and random effects are approximately normal.
#Minor deviations from normality in the residuals' tails, though they are not severe enough to impact conclusions.

ggResidpanel::resid_panel(lmm_F1_main_ML, smoother = TRUE, qqbands = TRUE, type = "pearson")
#residuals are approximately normal (Q-Q plot and histogram). Independence of residuals is satisfied (Index plot). The model fits the data reasonably well, as seen in the Residual Plot.
#Weaknesses: Slight non-linearity in the Residual Plot suggests that additional transformations or interaction terms might improve the model. Mild deviations from normality in the tails (Q-Q Plot and Histogram) are not severe but could be addressed if needed for highly sensitive analyses.

view(df_d)
view(dprime)

####  00. Setup  ####

library(tidyverse) # pivot longer
install.packages('dplyr')
library(dplyr) #data manipulation, summarize
library(ggplot2)
library(caret) #F1 Scores & Confusion matrix
library(psych) #kmo
library(ltm)
install.packages("nlme",lib=.libPaths()[2])
library(lme4) #lmer()
library(car)# regression diagnostics
library(lmerTest) #p values for lmer()
library(ordinal) #clmm()
library("report") # genereate reports from R objects
library(ggResidpanel) #visualize residual plots
library(mltools)
library(psycho) #pyschometrics
library(viridis)
install.packages("tidyverse")


#### 01.Load data ####
rm(list = ls())
setwd(".../R-Code")
mat_p <-read.csv("./data/raw/mat_perception.csv")
df_p <-read.csv("./data/processed/survey_perception_long")

### 02. Join material with survey results###
df_p <- left_join(df_p, mat_p, by = "s_id")
df_p <- rename(df_p, matPol = polDummy)

#Fix comma in biasScore
df_p$biasScore <- gsub(",", ".", df_p$biasScore)

#### important
#### 02. Item Correlation ####

# Select the relevant columns
df_corr <- df_p[, c("bias", "sent", "share", "trust")]

# Compute the correlation matrix with significance values
library(psych)
corr <- corr.test(df_corr,  method ="spearman")

# Print the correlation matrix with significance values
print(corr$r)
print(corr$p)

corr_list <- list()

# Split the data frame by s_id and apply the cor() function to each subset
corr_list <- by(df_p, df_p$s_id, function(subset) {
  # Select the relevant columns
  subset_corr <- subset[, c("bias", "sent", "share", "trust")]
  # Compute the correlation matrix and store it in the list
  s_id <- as.character(unique(subset$s_id))
  if (!(s_id %in% names(corr_list))) {
    corr_list[[s_id]] <- cor(subset_corr,  method ="spearman")
  }
})

# Print correlation + significance all statements
print(corr_list)



#### 03. Validity Check: Statement Perception ####

key_order <- c("clot", "llot", "rlot", "cmet", "lmet", "rmet", "chit","lhit", "rhit")
df_p$key <- factor(df_p$key, levels = key_order)
custom_colors <- c("grey", "grey", "grey","yellow", "yellow", "yellow", "orange", "orange", "orange")

# Create a new variable to group biasScore into three categories (first 3, middle 3, last 3)
# Convert biasScore to a factor and group into first, middle, and last 3 categories

df_p <- df_p %>%
  mutate(biasScore = as.factor(biasScore),  # Convert biasScore to a factor
         biasGroup = case_when(
           biasScore %in% head(sort(unique(biasScore)), 3) ~ "Low Bias",
           biasScore %in% tail(sort(unique(biasScore)), 3) ~ "High Bias",
           TRUE ~ "Medium Bias"
         ),
         biasGroup = factor(biasGroup, levels = c("Low Bias", "Medium Bias", "High Bias")))

custom_colors <- c("Low Bias" = viridis(6)[2], 
                   "Medium Bias" = viridis(6)[4],
                   "High Bias" = viridis(6)[6])   


# Plot with custom y-axis labels, x-axis percentage formatting, and grouped colors
ggplot(df_p, aes(x = biasScore, y = bias, group = biasScore, fill = biasGroup)) +
  geom_boxplot(show.legend = TRUE) +
  xlab("Bias Score (% of biased words in statement)") +
  ylab("Perceived Bias") +
  scale_y_continuous(breaks = c(-2, 0, 2), labels = c("Low Bias (-2)", "Medium Bias (0)", "High Bias (2)")) +
  # Multiply x-axis labels by 100 and add "%" sign
  scale_x_discrete(labels = function(x) paste0(as.numeric(x) * 100, "%")) +
  scale_fill_manual(values = custom_colors) +
  guides(fill = guide_legend(title = "Statement Bias", reverse = FALSE)) +
  facet_wrap(~key, nrow = 1, scales = "free_x") +   
  theme_bw(base_size = 20)


#### 04. Average Bias Percepetion ####

##### 4.1. Aggregate bias by s_id across all treatments #####
  
# Aggregate the data by s_id
item_summary <- aggregate(df_p[, c("bias", "sent", "trust", "share")], 
                        by = list(df_p$s_id, df_p$key), 
                        FUN = function(x) c(mean = round(mean(x),2), sd = round(sd(x),2)))
View(item_summary)



##### 4.2. Aggregate bias by s_id for each treatment #####
# Separate mean and sd calculations in aggregate
bias <- aggregate(df_p[["bias"]],
                  by = list(s_id = df_p$s_id, key = df_p$key, Treatment = df_p$heuristic),
                  FUN = function(x) c(MEAN = round(mean(x), 2), SD = round(sd(x), 2)))

# Convert the list columns in 'bias' to separate MEAN and SD columns
bias <- data.frame(bias[, 1:3], MEAN = bias$x[, "MEAN"], SD = bias$x[, "SD"])
colnames(bias) <- c("s_id", "key", "Treatment", "MEAN", "SD")
library(tidyr)
bias_wide <- pivot_wider(bias, names_from = Treatment, values_from = c(MEAN, SD))


#### 05. Create Contrasts ####

df_p$treatment.f = as.factor(df_p$heuristic)
contrasts(df_p$treatment.f) <- contr.treatment(7, base = 4) #make "CONTROL" the fourth category the baseline !!!

df_p$pol.m = as.factor(df_p$matPol)
contrasts(df_p$pol.m) <- contr.treatment(3, base = 2) 

df_p$pol.p = as.factor(df_p$pol_dummy)
contrasts(df_p$pol.p) <- contr.treatment(3, base = 2) 

df_p$congruence = as.factor(df_p$congruence)
contrasts(df_p$congruence) <- contr.treatment(2, base = 1) #base = not congruent !!!

df_p$biasScore = as.numeric(df_p$biasScore)
df_p$gender <- dplyr::recode(df_p$gender, "Female" = 0, "Male" = 1, "I prefer not to say" = 2, "Other" = 2) !!!
df_p$gender.g = as.factor(df_p$gender)

#### important
#### 06. Main Model Bias Perception ####
install.packages("devtools")
library("devtools"); install_github("lme4/lme4",dependencies=TRUE)
install.packages("lme4", type="source")
library("lme4")
library(lmerTest)
#ML as the gorups are similarly sized
lmm_bias <- lmer(bias ~ biasScore + sentMag + treatment.f + edu_score + pol.p + congruence + pol.p * congruence + gender.g + age + sentMag + musk1 + musk_opinion + (1|participantId), data = df_p)
#NEW LRT approach
## ---------------------------
## 1. Likelihood Ratio Tests (LRTs) -> more commonly used, more conservative
## ---------------------------
# Compare full model with reduced models (drop 1 term at a time)
# rest effect of bias:
m_reduced_treatment_bias <- update(lmm_bias, . ~ . - treatment.f)
anova(m_reduced_treatment_bias, lmm_bias, test = "LRT")  # Likelihood ratio test
# dropping each fixed effect:
drop1(lmm_bias, test = "Chisq")  # chi-sq tests via LRT for each fixed effect
confint(lmm_bias) #compute 95% CIs
performance::r2(lmm_bias) #Marginal R² (variance explained by fixed effects) and conditional R² (fixed + random)
#The final linear mixed-effects model explained 46% of the variance in d′ (conditional R² = .46), with 42% attributable to the fixed effects alone (marginal R² = .42).
summary(lmm_bias)
print(lmm_bias, correlation=TRUE)
standardize_parameters(lmm_bias, method = "refit")

lmm_bias_extended <- lmer(bias ~ biasScore + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + pol.p * congruence + gender.g + age + sentMag + musk1 + musk_opinion + sent + trust + share + (1|participantId), data = df_p)
m_reduced_treatment_bias_ex <- update(lmm_bias_extended, . ~ . - treatment.f)
anova(m_reduced_treatment_bias_ex, lmm_bias_extended, test = "LRT")  # Likelihood ratio test

standardize_parameters(lmm_bias_extended, method = "refit")

drop1(lmm_bias_extended, test = "Chisq")  # chi-sq tests via LRT for each fixed effect
confint(lmm_bias_extended) #compute 95% CIs
performance::r2(lmm_bias_extended) #Marginal R² (variance explained by fixed effects) and conditional R² (fixed + random)
summary(lmm_bias_extended) #68% and 64% Q_Q
print(lmm_bias_extended, correlation=TRUE)
conf_intervals <- confint(lmm_bias_extended, level = 0.95)
View(conf_intervals)

lmm_bias_ml <- lmer(bias ~ biasScore + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)
summary(lmm_bias_ml)
print(lmm_bias_ml, correlation=TRUE)
conf_intervals_ml <- confint(lmm_bias_ml, level = 0.95)
View(conf_intervals_ml)

#identifying which fixed or random effects have the most impact on the model fit
drop1(lmm_bias)
drop1(lmm_bias_ml)
#drop1(lmm_bias_ml, test = "LRT")

#Assumptions Check
ggResidpanel::resid_panel(lmm_bias, smoother = TRUE, qqbands = TRUE, type = "pearson")
ggResidpanel::resid_panel(lmm_bias_ml, smoother = TRUE, qqbands = TRUE, type = "pearson")


#### 07. Main Model Sharing Intention ####

lmm_share_p <- lmer(share ~ bias + sent + trust + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p)
lmm_share_ml <- lmer(share ~ bias + sent + trust + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)

#report(lmm_share_p)
summary(lmm_share_p)
print(lmm_share_p)
print(lmm_share_p, correlation=TRUE)
drop1(lmm_share_p, test = "LRT")

summary(lmm_share_ml)
print(lmm_share_ml)
print(lmm_share_ml, correlation=TRUE)
drop1(lmm_share_ml, test = "LRT")

ggResidpanel::resid_panel(lmm_share_ml, smoother = TRUE, qqbands = TRUE, type = "pearson")


#### 08. Main Model Trust #####
lmm_trust <- lmer(trust ~ bias + sent + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence  + gender.g + age + (1|participantId), data = df_p)
lmm_trust_ml <- lmer(trust ~ bias + sent + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)

summary(lmm_trust)
print(lmm_trust, correlation=TRUE)
drop1(lmm_trust, test = "LRT")
summary(lmm_trust_ml)
print(lmm_trust_ml, correlation=TRUE)
drop1(lmm_trust_ml, test = "LRT")

ggResidpanel::resid_panel(lmm_trust_ml, smoother = TRUE, qqbands = TRUE, type = "pearson")

#### 09. Main Model Emotionality #####

lmm_sent <- lmer(sent ~ bias + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p)
lmm_sent_ml <- lmer(sent ~ bias + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)

summary(lmm_sent)
print(lmm_sent, correlation=TRUE)
drop1(lmm_sent, test = "LRT")
#ml
summary(lmm_sent_ml)
print(lmm_sent_ml, correlation=TRUE)
drop1(lmm_sent_ml, test = "LRT")

#Assumptions
ggResidpanel::resid_panel(lmm_sent, smoother = TRUE, qqbands = TRUE, type = "pearson")


#### 10. Plot average perceived bias per treatment and bias level #####
# Prepare data
parallel <- data.frame(treatment = df_p$heuristic, 
                            statement = df_p$key, 
                            perceived_bias = df_p$bias)

parallel_summary<- aggregate(perceived_bias ~ treatment + statement, data = parallel, mean)

parallel$treatment <- factor(parallel$treatment, levels = c("CONTROL", "BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST"))

# Gather data for each bias level
summary_low <-  parallel_summary[parallel_summary$statement %in% c("clot", "llot", "rlot"), ]
summary_med <-  parallel_summary[parallel_summary$statement %in% c("cmet", "lmet", "rmet"), ]
summary_hi <-  parallel_summary[parallel_summary$statement %in% c("lhit", "rhit", "chit"), ]


# Set colors
statement_colors <- c("clot" = viridis(6)[1],
                      "cmet" = viridis(6)[1],
                      "chit" = viridis(6)[1],
                      "llot" = viridis(6)[3],
                      "lmet" = viridis(6)[3],
                      "lhit" = viridis(6)[3],
                      "rlot" = viridis(6)[5],
                      "rmet" = viridis(6)[5],
                      "rhit" = viridis(6)[5])

#### 10.1 Plot average perceived bias per treatment: low bias #####
ggplot(summary_low, aes(x = treatment, y = perceived_bias, group = statement, color = statement)) +
  geom_path() +
  geom_point(shape = 23) +
  ylim(-3, 3) +
  scale_color_manual(values = statement_colors, 
                     labels = c('Center', 'Left', 'Right')) + # Set legend labels here
  labs(x = "Indicator Group",
       y = "Average Perceived Bias (Low Bias Statements)",
       color = "Statement") +
  scale_y_continuous(
    breaks = seq(-3, 3, 0.5),
    minor_breaks = seq(-3, 3, 0.1),
    limits = c(-3, 3),
    expand = c(0, 0)
  )


#### 10.2 Plot average perceived bias per treatment: medium bias #####
ggplot(summary_med, aes(x = treatment, y = perceived_bias, group = statement, color = statement)) +
  geom_path() +
  geom_point(shape = 23) +
  scale_color_manual(values = statement_colors, 
                     labels = c('Center', 'Left', 'Right')) +
  ylim(-3, 3) +
  labs(x = "Indicator Group",
       y = "Average Perceived Bias (Medium Bias Statements)",
       color = "Statement")  +
  scale_y_continuous(
    breaks = seq(-3, 3, 0.5),
    minor_breaks = seq(-3, 3, 0.1),
    limits = c(-3, 3),
    expand = c(0, 0)
  )

#### 10.2 Plot average perceived bias per treatment: high bias #####
ggplot(summary_hi, aes(x = treatment, y = perceived_bias, group = statement, color = statement)) +
  geom_path() +
  geom_point(shape = 23) +
  scale_color_manual(values = statement_colors, 
                     labels = c('Center', 'Left', 'Right')) +
  ylim(-3, 3) +
  labs(x = "Indicator Group",
       y = "Average Perceived Bias (High Bias Statements)",
       color = "Statement")  +
  scale_y_continuous(
    breaks = seq(-3, 3, 0.5),
    minor_breaks = seq(-3, 3, 0.1),
    limits = c(-3, 3),
    expand = c(0, 0)
  )

# =========================
# ANOVA: d' (F1)
# =========================
library(dplyr)
tests_gauge_cont_df <- df_p %>% filter(heuristic %in% c("BIAS_GAUGE", "CONTROL"))
tests_pol_cont_df <- df_p %>% filter(heuristic %in% c("POLITICAL", "CONTROL"))
tests_hi_cont_df <- df_p %>% filter(heuristic %in% c("BIAS_HIGH", "CONTROL"))
tests_trust_cont_df <- df_p %>% filter(heuristic %in% c("TRUST", "CONTROL"))
tests_bar_cont_df <- df_p %>% filter(heuristic %in% c("BIAS_BAR", "CONTROL"))
tests_sentiment_cont_df <- df_p %>% filter(heuristic %in% c("SENTIMENT", "CONTROL"))
View(tests_hi_cont_df)


# Combine all perception data into a single data frame
all_perception <- rbind(
  data.frame(heuristic = tests_hi_cont_df$heuristic, bias = tests_hi_cont_df$bias),
  data.frame(heuristic = tests_gauge_cont_df$heuristic, bias = tests_gauge_cont_df$bias),
  data.frame(heuristic = tests_pol_cont_df$heuristic, bias = tests_pol_cont_df$bias),
  data.frame(heuristic = tests_trust_cont_df$heuristic, bias = tests_trust_cont_df$bias),
  data.frame(heuristic = tests_bar_cont_df$heuristic, bias = tests_bar_cont_df$bias),
  data.frame(heuristic = tests_sentiment_cont_df$heuristic, bias = tests_sentiment_cont_df$bias)
)

# Fit one-way ANOVA model
anova_model_perception <- aov(bias ~ heuristic, data = all_perception)
summary(anova_model_perception)

# =========================
# Effect Sizes
# =========================
library(effectsize)

# Eta-squared 
eta_squared(anova_model_perception, partial = FALSE)   # η²

# Partial eta-squared
eta_squared(anova_model_perception, partial = TRUE)    # partial η²

# Cohen's f
cohens_f(anova_model_perception)

# =========================
# Post-hoc comparisons
# =========================
TukeyHSD(anova_model_perception)

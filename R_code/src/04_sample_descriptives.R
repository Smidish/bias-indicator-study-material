####  0.0 Setup  ####

library(tidyverse)
library(dplyr)
library("report") # generate reports from R objects
library(REdaS) #  Bartlett test
library(psych) #kmo
library(irr) #cronbach-alpha
library(ggplot2)

#### 1.0 Load data  ####
rm(list = ls())
setwd(".../R-Code")

# Material, necessary to calculcate congruence
mat <-read.csv("./data/raw/material.csv")
row.names(mat) <- mat[, 1]  # set row names to first column
mat <- mat[, -1] 

df_1 <-read.csv("./data/temp/survey_part1_pol.csv")
df_2 <-read.csv("./data/temp/survey_part2_res.csv")

#### 2.0 Merge tables  ####
df_j <- subset(df_1, select = -c(perception1, perception2, perception3, perception4, perception5, perception6, perception7,
                                 perception8, perception9, detection1, detection2, detection3, detection4, detection5, detection6, annotation1, annotation2, annotation3, annotation4, annotation5, 
                                 annotation6, attentioncheck))

# Create one table out of the results for task 1 and 2 (indication and testing phases)
df <- list(df_j, df_2) %>% reduce(inner_join, by='participantId')


#### 3.0 Duration ####

install.packages("viridis")
library(viridis)
#png(file="duration_hist1.png", width=1200, height=700)
hist(as.numeric(df$duration), breaks=20, main="Survey Completion Time", 
     xlab="Completion Time (Minutes)", 
     ylab="Frequency (Count of Participants)",
     col=viridis(20)
     )


boxplot(df$duration, horizontal=TRUE, main="Survey Completion Time", xlab="Completion Time (Minutes)", 
      col=viridis(3))
summary(as.numeric(df$duration))

mean_duration <- mean(as.numeric(df$duration))
sd_duration <- sd(as.numeric(df$duration))
upper <- mean_duration+sd_duration
lower <- mean_duration-sd_duration


#### 4.0 Demographics ####

##### 4.1 Age #####

view(table(df$age))

sum(df$age<= 24)              # 36 17%
sum(df$age>= 25 & df$age<=34) # 73 34%
sum(df$age>= 35 & df$age<=44) # 52 24%
sum(df$age>= 45 & df$age<=54) # 22 10%
sum(df$age>= 55 & df$age<=64) # 21 10%
sum(df$age>= 65)              # 10 5%

vage <- c("18-25","25-34","35-44","45-54","55-64","64-99")
vamount <- c(sum(df$age<= 24),sum(df$age>= 25 & df$age<=34),sum(df$age>= 35 & df$age<=44),sum(df$age>= 45 & df$age<=54),sum(df$age>= 55 & df$age<=64),sum(df$age>= 65))
df_age_view <- data.frame(vage, vamount)
#view(df_age_view)


# Calculate percentages
df_age_view <- df_age_view %>%
  mutate(percentage = vamount / sum(vamount) * 100)

ggplot(data = df_age_view, aes(x = "", y = vamount, fill = vage)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_polar(theta = "y", start = 0, direction = -1, clip = "off") + 
  geom_text(aes(x = 1.72, 
                label = paste0(vage, " (", round(percentage, 1), "%)")),  # Combine vage and percentage
            position = position_stack(vjust = 0.5)) + 
  theme_void() + 
  guides(fill = guide_legend(title = "Age Groups")) + 
  scale_fill_viridis_d()


##### 4.2 Gender #####

table(df$gender)
#Female   Male   Other  NA 
#108      101    2      3
#51%     47%.   2%

##### 4.3 English Profiency #####

view(table(df$englishLevel))
#Advanced    Intermediate   
#209         5              

##### 4.4A Education ######

view(table(df$educationLevel))

#before recording

#Doctoral degree                  10x
#Graduate work                    32
#Bachelor’s degree                72
#Associate degree                 18
#Some college                     43x
#High school graduate             32x
#Some high school                 02x
#Vocational / technical school    03x
#I prefer not to say              02x


#Recoding 
df$edu_score <-  dplyr::recode(df$educationLevel,"Vocational or technical school"=0, "Some high school"=0, "High school graduate"=0, "Some college"=1, 
                       "Associate degree"=2, "Bachelor’s degree"=3, "Graduate work"=4, "Doctoral degree" = 5, "I prefer not to say"=6)

table(df$edu_score)

tempvar <- c("Doctoral degree","Graduate work","Bachelor’s degree","Associate degree","Some college","High school graduate","Some high school","Vocational / technical school","Prefer not to say")
tempfreq <- c(10,32,72,28,43,32,2,3,2)
df_edu_view <- data.frame(tempvar,tempfreq)
#view(df_edu_view)
library(scales)
breaks = levels(df_edu_view$tempvar)
labels         = as.expression(breaks)
labels[[best]] = bquote(underline(bold(.(labels[[best]]))))

# Set the factor levels in the desired order
df_edu_view$tempvar <- factor(df_edu_view$tempvar, 
                              levels = c("Prefer not to say", "Vocational / technical school", "Some high school", "High school graduate", 
                                         "Some college", "Associate degree", "Bachelor’s degree", 
                                         "Graduate work", "Doctoral degree"))

ggplot(data = df_edu_view, aes(x = tempvar, y = tempfreq, fill = tempvar)) + 
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = tempfreq), vjust = -0.3, size = 3.5) + 
  scale_fill_viridis_d() +  # Apply the viridis color scheme
  labs(title = "Educational Levels Frequency", x = "", y = "Frequency (Number of Participants)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

tempvarre <- c("Never attended college","Some college","Two-year degree","Four-year degree","Graduate","Ph.D.","Prefer not to say")
tempfreqre <- c(37,43,18,72,32,10,2)
df_edu_viewre <- data.frame(tempvarre,tempfreqre)
#view(df_edu_viewre)

#0 Never attended college          37 17%
#1 Some college                    43 20% 
#2 Two-year degree                 18 8%
#3 four-year degree                72 34%
#4 graduate                        32 15%
#5 phd                             10 5%
#6 Prefer not to say               02 1%

df_edu_viewre$tempvarre <- factor(df_edu_viewre$tempvarre, 
                                  levels = c("Prefer not to say", "Never attended college", "Some college", "Two-year degree", 
                                             "Four-year degree", "Graduate", "Ph.D."))

ggplot(data = df_edu_viewre, aes(x = tempvarre, y = tempfreqre, fill = tempvarre)) + 
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = tempfreqre), vjust = -0.3, size = 3.5) + 
  scale_fill_viridis_d() +  
  labs(title = "Educational Levels Frequency", x = "", y = "Frequency (Number of Participants)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")


##### 4.4B US Census #####

#US Census data
# 37% have never attended college
#15% some college
#10% associate
#23% bachelors
#14% masters + doctorate

tempvarre <- c("Never attended college", "Some college", "Two-year degree", "Four-year degree", "Graduate + Ph.D.", "Prefer not to say")
tempfreqre <- c(37, 43, 18, 72, 42, 2)

df_edu_viewre <- data.frame(tempvarre, tempfreqre)
df_edu_viewre$tempvarre <- factor(df_edu_viewre$tempvarre, 
                                  levels = c("Prefer not to say", "Never attended college", "Some college", 
                                             "Two-year degree", "Four-year degree", "Graduate + Ph.D."))

# US Census percentages
us_census_data <- data.frame(
  tempvarre = c("Prefer not to say", "Never attended college", "Some college", "Two-year degree", "Four-year degree", "Graduate + Ph.D."),
  census_percentage = c(NA, 37, 15, 10, 23, 14)  # Masters and Ph.D. combined for simplicity
)

# Merge
df_combined <- merge(df_edu_viewre, us_census_data, by = "tempvarre", all.x = TRUE)

lilac <- viridis(2)[1]

df_combined <- df_combined %>%
  mutate(percentage = tempfreqre / sum(tempfreqre) * 100)

ggplot(data = df_combined, aes(x = tempvarre, y = tempfreqre, fill = tempvarre)) + 
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = paste0(tempfreqre, " (", round(percentage, 1), "%)")), 
            vjust = -0.3, size = 3.5) + 
  # Overlay the US Census data as points
  geom_point(aes(y = census_percentage * 2.14),  # Scale percentages to match the y-axis
             color = lilac, size = 3) + 
  # Add percentage labels for US Census data
  geom_text(aes(y = census_percentage * 2.14, 
                label = paste0(census_percentage, "%")), 
            color = lilac, vjust = -1.2, size = 3.5) + 
  scale_fill_viridis_d() + 
  labs(title = "Educational Levels Frequency vs US Census Data (lilac dot)", 
       x = "", y = "Frequency (Number of Participants)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")


##### 4.5 News Consumption #####

# Frequency:

view(table(df$newsConsumption))
#Never or very rarely         14
#Several times per month      29
#Several times per week       50
#Once per day                 38
#Several times per day        83

df_newscon$temp_news <- factor(df_newscon$temp_news, 
                               levels = c("Never or very rarely", "Several times per month", 
                                          "Several times per week", "Once per day", "Several times per day"))

# Total count for percentage calculation
total_count <- sum(df_newscon$temp_count)

ggplot(data = df_newscon, aes(x = temp_news, y = temp_count, fill = temp_news)) + 
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = paste0(temp_count, " (", round(temp_count / total_count * 100, 1), "%)")), 
            vjust = -0.5, size = 3.5) + 
  guides(fill = guide_legend(title = "News Consumption")) + 
  labs(title = "News Consumption Frequency", x = "", y = "Count (Number of Participants") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_viridis_d()


#temp_news <- c("Never or very rarely", "Several times per month" ,"Several times per week", "Once per day","Several times per day")
#temp_count <- c(14,29,50,38,83)
#df_newscon <- data.frame(temp_news, temp_count)

#ggplot(data = df_newscon, aes(x = "", y = temp_count, fill = temp_news)) + 
#  geom_bar(stat = "identity", color = "black") +
#  scale_fill_discrete(labels = temp_news)  +
#  guides(fill=guide_legend(title="News Consumption")) +
#  coord_polar(theta = "y", start = 0, direction = -1, clip = "off") +
#  geom_text(aes(x = 1.2, label = percent(temp_count/300)), position = position_stack(vjust = 0.5)) +
#  theme_void() +
#  scale_x_discrete(breaks = breaks, labels = temp_news, guide  = guide_axis(check.overlap = T))

df$frequency <- dplyr::recode(df$newsConsumption,"Several times per day"=5,"Once per day"=4,"Several times per week"=3,"Several times per month"=2,"Never or very rarely"=1)
summary(df$frequency)
sd(df$frequency)

##### 4.6 Political affiliation #####

df$pol_dummy[df$politicalSpectrum < -3] <- 1     # Lean left
df$pol_dummy[df$politicalSpectrum <= 3 & df$politicalSpectrum >= -3] <- 2    # Center
df$pol_dummy[df$politicalSpectrum > 3] <- 3     # Lean right

#L     C    R 
#118   67   29 

table(df$pol_dummy)


##### 4.7 Frequency, Outlet and own Political Leaning #####

# Plotting avg affiliation and avg leaning
ggplot(data = df, mapping = aes(x = politicalSpectrum, y = mean_outlet, color = politicalSpectrum)) + 
  geom_smooth(method = "lm", se = TRUE, fill = "gray90", color = "gray75") + 
  geom_count(show.legend = FALSE) + 
  scale_size(range = c(1.5, 8)) + 
  scale_color_viridis_c() + 
  scale_x_continuous(
    breaks = c(-10, -5, 0, 5, 10),  # Define breaks
    labels = c("Left (-10)", "Lean Left (-5)", "Center (0)", "Lean Right (5)", "Right (10)")
  ) + 
  scale_y_continuous(
    breaks = c(1, 2, 3, 4, 5),  # Define breaks
    labels = c("Left", "Lean Left", "Center", "Lean Right", "Right")  
  ) + 
  labs(y = "Average Political Affiliation of Outlets Consumed by Participant", 
       x = "Political Affiliation of Participant") + 
  theme_minimal()


# Mean outlet bias per political group
df %>%
  group_by(pol_dummy) %>%
  summarise(avg_outlet = mean(mean_outlet, na.rm = TRUE), 
            sd_outlet= sd(mean_outlet, na.rm = TRUE))


##### 4.8 Predict Outlet Leaning #####

lm_outlet <- lm(mean_outlet ~ politicalSpectrum, data = df)
lm_freq <- lm(frequency ~ age, data = df)


####
####
####
####
####
#### 5.0 Treatment Group ####
table(df$heuristic)

# BIAS_BAR    33
# BIAS_GAUGE  33
# BIAS_HIGH   31
# CONTROL     33
# POLITICAL   29
# SENTIMENT   33
# TRUST       32

# Recode treatment groups
df$vID <- dplyr::recode(df$heuristic,  "CONTROL"= 0, "BIAS_HIGH"= 1, "BIAS_BAR"= 2,  "BIAS_GAUGE" = 3,  
                 "POLITICAL" = 4, "SENTIMENT" = 5, "TRUST" = 6)
df$control_dummy <- ifelse(df$heuristic != "CONTROL", 0, 1)



#### 6.0 Statement Congruence ####

# Match participant political leaning with outlet for each statements, political leaning is in both cases scored 1-3
for (i in 1:9) {
  #Generate column names and index
  colname <- paste0("congr_P", i)
  #as.numeric converts comparison to 1 or 1
  df[[colname]] <- as.numeric(mat[paste0("P", i), "polDummy"] == df$pol_dummy)
}
for (i in 1:6) {
  colname <- paste0("congr_D", i)
  df[[colname]] <- as.numeric(mat[paste0("D", i), "polDummy"] == df$pol_dummy)
}


#### 7.0 Topic Opinion ####

#Generally, Elon Musk's Twitter acquisition is... 
#musk1: Irrelevant-Relevant
#musk2: Unacceptable-Acceptable
#musk3: Bad-Good
#musk4: Wrong-Right 

# 7.1 Factor analysis prerequisties

cor(as.data.frame(cbind(df$musk1,df$musk2,df$musk3,df$musk4)))

musk_all <- as.data.frame(cbind(df$musk1,df$musk2,df$musk3,df$musk4))
musk_opinion <- as.data.frame(cbind(df$musk2,df$musk3,df$musk4))

KMO(musk_all) #MSA =  0.75, with musk1 only  0.27
KMO(musk_opinion) #MSA =  0.77, all higher 0.7

alpha(musk_all, check.keys=TRUE) # 0.716 -> acceptable
alpha(musk_opinion, check.keys=TRUE) # 0.945 -> excellent

# Relevance musk1 likely loads on different construct 

#Bartlett-test of sphericity
bart_spher(musk_opinion)
# p-value < 2.22e-16, reject the null hypothesis ->  evidence to suggest that musk2 - 4 are correlated

pca <- principal(na.omit(musk_opinion), nfactors = ncol(musk_opinion), rotate = "varimax")

# 7.2 Conduct PCA

# Extract eigenvalues
eigenvalues <- pca$values
# Identify eigenvalues greater than 1
components_to_keep <- eigenvalues >= 1
# Number of components to retain
n_components <- sum(components_to_keep)
cat("Eigenvalues:", eigenvalues, "\n")
cat("Components to keep:", components_to_keep, "\n")
cat("Number of components to retain:", n_components, "\n")
# Suggests just retaining one factor
pca <- principal(na.omit(musk_opinion), nfactors = 1, rotate = "varimax")

# 7.3 Simplified approach: Rowwise means (musk2 - musk4)

df$musk_opinion <- rowMeans(musk_opinion)
cor.test(df$musk1,df$musk_opinion, method ="pearson")


# 7.4 Political affiliation and topic opinion

# Does politicical affiliatipn predict topic opinion?
lm_musk <- lm(musk_opinion  ~ pol_dummy + gender, data = df)
summary(lm_musk)
# Yes, but gender as well!


#### 8.0 Combined Trust Measure ####
#Truthfulness, Credibility and Believability are usually averaged in literature to derive trust 

##### 8.1 Create separate data frames ####

trust_P1 <- as.data.frame(cbind(df$truth_perception1, df$cred_perception1, df$bel_perception1))
trust_P2 <- as.data.frame(cbind(df$truth_perception2, df$cred_perception2, df$bel_perception2))
trust_P3 <- as.data.frame(cbind(df$truth_perception3, df$cred_perception3, df$bel_perception3))
trust_P4 <- as.data.frame(cbind(df$truth_perception4, df$cred_perception4, df$bel_perception4))
trust_P5 <- as.data.frame(cbind(df$truth_perception5, df$cred_perception5, df$bel_perception5))
trust_P6 <- as.data.frame(cbind(df$truth_perception6, df$cred_perception6, df$bel_perception6))
trust_P7 <- as.data.frame(cbind(df$truth_perception7, df$cred_perception7, df$bel_perception7))
trust_P8 <- as.data.frame(cbind(df$truth_perception8, df$cred_perception8, df$bel_perception8))
trust_P9 <- as.data.frame(cbind(df$truth_perception9, df$cred_perception9, df$bel_perception9))

trust_D1 <- as.data.frame(cbind(df$truth_detection1, df$cred_detection1, df$bel_detection1))
trust_D2 <- as.data.frame(cbind(df$truth_detection2, df$cred_detection2, df$bel_detection2))
trust_D3 <- as.data.frame(cbind(df$truth_detection3, df$cred_detection3, df$bel_detection3))
trust_D4 <- as.data.frame(cbind(df$truth_detection4, df$cred_detection4, df$bel_detection4))
trust_D5 <- as.data.frame(cbind(df$truth_detection5, df$cred_detection5, df$bel_detection5))
trust_D6 <- as.data.frame(cbind(df$truth_detection6, df$cred_detection6, df$bel_detection6))


##### 8.2 Prerequisites: Cronbachs alpha & Bartlett #####
install.packages("psych")

library(psych)
#Lowest:
alpha(trust_P1) #alpha: 0.943
#Highest
alpha(trust_D6) #alpha: 0.976

# Bartlett test of sphericity 
# Just one example, but all null hypotheses were rejected
cortest.bartlett(trust_P1)

##### 8.3 PCA #####

# PCA with all trust items, just one here as example, but all 9 + 6
pca1 <- principal(trust_P1, nfactors = 1, rotate = "varimax")
pca1$uniquenesses

##### 8.4 Trust: Rowwise means  #####

#Decision: Average the trust items according to Moravec Kim
#Append Trust Variable to dataset
df$trust_perception1 <- rowMeans(trust_P1)
df$trust_perception2 <- rowMeans(trust_P2)
df$trust_perception3 <- rowMeans(trust_P3)
df$trust_perception4 <- rowMeans(trust_P4)
df$trust_perception5 <- rowMeans(trust_P5)
df$trust_perception6 <- rowMeans(trust_P6)
df$trust_perception7 <- rowMeans(trust_P7)
df$trust_perception8 <- rowMeans(trust_P8)
df$trust_perception9 <- rowMeans(trust_P9)

df$trust_detection1 <- rowMeans(trust_D1)
df$trust_detection2 <- rowMeans(trust_D2)
df$trust_detection3 <- rowMeans(trust_D3)
df$trust_detection4 <- rowMeans(trust_D4)
df$trust_detection5 <- rowMeans(trust_D5)
df$trust_detection6 <- rowMeans(trust_D6)


#Clean up, remove all the trust related raw variables
rm(trust_D1, trust_D2,trust_D3,trust_D4,trust_D5,trust_D6,trust_P1,trust_P2,trust_P3,trust_P4,trust_P5,trust_P6,trust_P7,trust_P8,trust_P9)
df <- df[, !(names(df) %in% c("bel_perception1", "bel_perception2", "bel_perception3", "bel_perception4", "bel_perception5", "bel_perception6", "bel_perception7", "bel_perception8", "bel_perception9", 
                              "cred_perception1", "cred_perception2", "cred_perception3", "cred_perception4", "cred_perception5", "cred_perception6", "cred_perception7", "cred_perception8", "cred_perception9",
                              "truth_perception1", "truth_perception2", "truth_perception3", "truth_perception4", "truth_perception5", "truth_perception6", "truth_perception7", "truth_perception8", "truth_perception9", 
                              "bel_detection1", "bel_detection2", "bel_detection3", "bel_detection4", "bel_detection5", "bel_detection6", 
                              "cred_detection1", "cred_detection2", "cred_detection3", "cred_detection4", "cred_detection5", "cred_detection6", 
                              "truth_detection1", "truth_detection2", "truth_detection3", "truth_detection4", "truth_detection5", "truth_detection6"))]


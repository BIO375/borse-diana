#### Lab 6 More Practice with t-tests, and friends ####

# Clean up the working environment
rm(list = ls())
# Verify Working Directory
getwd()

### Install and load packages ####

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

#### Problem 13-20 ####

library(readr)
Craig_Foote <- read_csv("~/Bio 375/Analyses/borse-diana/datasets/demos/Craig_Foote.csv")

# This problem has two independent samples, so I will start by analyzing assumptions
# of a two-sample t-test.

# Calculate summary statistics
summ_Craig_Foote <- Craig_Foote %>%
  group_by(Type) %>%
summarise(mean_Color = mean(Color),
          median_Color = median(Color),
          IQR_Color = IQR(Color),
          sd_Color = sd(Color),
          var_Color = var(Color),
          se_Color = sd(Color)/sqrt(n()),
          n_Color = n()) 

view(summ_C)

# Checking Normality of distributions

ggplot(Craig_Foote) +
  geom_histogram(aes(Color), binwidth = .1)+
  facet_wrap(~Type)

# Testing whether the variances are similar enough

ratio <- (max(summ_Craig_Foote$sd_Color))/(min(summ_Craig_Foote$sd_Color))

view(ratio)

# Trying a transformation

Craig_Foote <- Craig_Foote %>%
  mutate(logColor = log(Color))

summ_Craig_Foote <- Craig_Foote %>%
  group_by(Type) %>%
  summarise(mean_logColor = mean(logColor),
            median_logColor = median(logColor),
            IQR_logColor = IQR(logColor),
            sd_logColor = sd(logColor),
            var_logColor = var(logColor),
            se_logColor = sd(logColor)/sqrt(n()),
            n_logColor = n()) 

#Checking Normality

ggplot(Craig_Foote) +
  geom_histogram(aes(logColor), binwidth = .05)+
  facet_wrap(~Type)

ggplot(Craig_Foote) +
  geom_boxplot(aes(x = Type, y = logColor))

# Check if the ratio of sdmax to sdmin is acceptable

ratio <- (max(summ_Craig_Foote$sd_logColor))/(min(summ_Craig_Foote$sd_logColor))

view(ratio)

# Perform t-test (Mann Whitney U / Wilcox)

wilcox.test(Color ~ Type, data = Craig_Foote, var.equal = TRUE, alternative = "less", mu = 0, conf.level = 0.95)

t.test(Color ~ Type, data = Craig_Foote, var.equal = TRUE, alternative = "less", mu = 0, conf.level = 0.95)

#### Problem 13-25 ####

library(readr)
chap13q25Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

# This data has one sample, but the data is in terms of difference. Therefore, I 
# will be examining assumptions of a paired t-test.

# Calculate summary statistics
summ_chap13q25Clearcuts <- chap13q25Clearcuts %>%
  summarise(mean_biomassChange = mean(biomassChange),
            median_biomassChange = median(biomassChange),
            IQR_biomassChange = IQR(biomassChange),
            sd_biomassChange = sd(biomassChange),
            var_biomassChange = var(biomassChange),
            se_biomassChange = sd(biomassChange)/sqrt(n()),
            n_biomassChange = n()) 

view(summ_chap13q25Clearcuts)

# Test assumption of normality

ggplot(chap13q25Clearcuts) +
  geom_histogram(aes(biomassChange), binwidth = .65)

ggplot(chap13q25Clearcuts) +
  geom_boxplot(aes(x = "", y = biomassChange))

# Both of these show that the differences are not normally distributed

# Try a transformation

chap13q25Clearcuts <- chap13q25Clearcuts %>%
  mutate(logbiomassChange = log(biomassChange))

# Cannot take log of a negative number... oopse
# squared differences and then took the log of the squared differences

chap13q25Clearcuts <- chap13q25Clearcuts %>%
  mutate(squaredbiomassChange = (biomassChange^2))

chap13q25Clearcuts <- chap13q25Clearcuts %>%
  mutate(logsquaredbiomassChange = log(squaredbiomassChange))

# Test for normality

summ_chap13q25Clearcuts <- chap13q25Clearcuts %>%
  summarise(mean_logsquaredbiomassChange = mean(logsquaredbiomassChange),
            median_logsquaredbiomassChange = median(logsquaredbiomassChange),
            IQR_logsquaredbiomassChange = IQR(logsquaredbiomassChange),
            sd_logsquaredbiomassChange = sd(logsquaredbiomassChange),
            var_logsquaredbiomassChange = var(logsquaredbiomassChange),
            se_logsquaredbiomassChange = sd(logsquaredbiomassChange)/sqrt(n()),
            n_logsquaredbiomassChange = n()) 

ggplot(chap13q25Clearcuts) +
  geom_histogram(aes(logsquaredbiomassChange), binwidth = .5)

ggplot(chap13q25Clearcuts) +
  geom_boxplot(aes(x = "", y = logsquaredbiomassChange))

# This is more normal, but still not entirely normal.

# Sign test.

SignTest(chap13q25Clearcuts$logsquaredbiomassChange, alternative = "two.sided", mu = 0, conf.level = 0.95)

# We found that change in biomass between rainforests before and after next to 
# clearcuts next to them was significantly different
# (one sample sign test: s = 25, df = 34, p < .01)
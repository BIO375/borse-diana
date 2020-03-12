#### Lab 5 Confidence Intervals, t-tests, and friends ####

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

#### Confidence Intervals ####

# Data needed:
summarise(mean_variable = mean(variable),
          median_variable = median(variable),
          IQR_variable = IQR(variable),
          sd_variable = sd(variable),
          var_variable = var(variable),
          n_variable = n())

alpha <- 0.05
mean <- summary$mean
se <- summary$se
df <- summary$n -1

# mean + c(-1, 1)*qt(1-alpha, df)*se

#### Question 1 #####

library(readr)
Obliquity <- read_csv("datasets/demos/Obliquity.csv")
View(Obliquity)

summary_Obliquity <- Obliquity %>%
  summarise(mean_Obliquity = mean(Obliquity),
          median_Obliquity = median(Obliquity),
          IQR_Obliquity = IQR(Obliquity),
          sd_Obliquity = sd(Obliquity),
          var_Obliquity = var(Obliquity),
          n_Obliquity = n())

alpha <- 0.05
mean <- summary$mean
se <- summary$se
df <- summary$n -1

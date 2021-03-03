#### Area differences ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

library(readr)
PPatch_Area_All_years <- read_csv("~/Hoscho Thesis/Data Tables/Phragmites Patch Area All years.csv")
View(PPatch_Area_All_years)

# plots
ggplot(PPatch_Area_All_years, aes(x = Area))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(PPatch_Area_All_years)+
  geom_qq(aes(sample = Area))

# Calculate summary statistics
Summ_PPatch_Area_All_years <- PPatch_Area_All_years %>%
  summarise(mean_Area = mean(Area),
            median_Area = median(Area),
            IQR_Area = IQR(Area),
            sd_Area = sd(Area),
            var_Area = var(Area),
            se_Area = sd(Area)/sqrt(n()),
            n_Area = n()) 

#Performing the t-test?

t.test(PPatch_Area_All_years$Area, alternative = "greater", mu = 0, conf.level = 0.95)

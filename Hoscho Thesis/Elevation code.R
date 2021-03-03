#### Elevation Stats ####

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
TyphaPatches_elevation <- read_csv("~/Hoscho Thesis/Data Tables/Fall_2020_TyphaPatches+elevation.dbf.csv")
View(TyphaPatches_elevation)

#Calculate Summary statistics
TPatches_elevation <- TyphaPatches_elevation

# visualization
ggplot(data = TPatches_elevation) + 
  geom_point(mapping = aes(x = Patch_ID, y = SUM))

# Calculate summary statistics
Summ_TPatches_elevation <- TPatches_elevation %>%
  summarise(mean_MEAN = mean(MEAN),
            median_MEAN = median(MEAN),
            IQR_MEAN = IQR(MEAN),
            sd_MEAN = sd(MEAN),
            var_MEAN = var(MEAN),
            se_MEAN = sd(MEAN)/sqrt(n()),
            n_MEAN = n()) 

view(Summ_TPatches_elevation)

# Check for normality

ggplot(TPatches_elevation, aes(x = MEAN))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(TPatches_elevation) +
  geom_histogram(aes(MEAN), binwidth = .1)
ggplot(TPatches_elevation)+
  geom_qq(aes(sample = MEAN))

# Safe to say it is not a normal distribution
# Try a transformation?

TPatches_elevation <- TPatches_elevation %>%
  mutate(logMEAN = log(MEAN))

# check for normality again
ggplot(TPatches_elevation, aes(x = logMEAN))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(TPatches_elevation) +
  geom_histogram(aes(logMEAN), binwidth = .0001)
ggplot(TPatches_elevation)+
  geom_qq(aes(sample = logMEAN))

# did not help at all.

#### Phrag Patches elevation ####

library(readr)
PPatches_elevation <- read_csv("~/Hoscho Thesis/Data Tables/Fall_2020_PhragPatches+elevation.dbf.csv")

# Calculate summary statistics
Summ_PPatches_elevation <- PPatches_elevation %>%
  summarise(mean_MEAN = mean(MEAN),
            median_MEAN = median(MEAN),
            IQR_MEAN = IQR(MEAN),
            sd_MEAN = sd(MEAN),
            var_MEAN = var(MEAN),
            se_MEAN = sd(MEAN)/sqrt(n()),
            n_MEAN = n()) 

view(Summ_PPatches_elevation)

# Check for normality

ggplot(PPatches_elevation, aes(x = MEAN))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(PPatches_elevation) +
  geom_histogram(aes(MEAN), binwidth = .1)
ggplot(PPatches_elevation)+
  geom_qq(aes(sample = MEAN))

# Not super normal.
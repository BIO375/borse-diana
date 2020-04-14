#### Lab 7: Introduction to 1-way ANOVA ####

# Clean up working environment
rm(list = ls())
# Verify working directory
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

#### Question 1 ####

# Load data
Jaffe <- read_csv("datasets/demos/Jaffe.csv")

# Do the necessary transformation for ANOVA
Jaffe <- Jaffe %>%
  mutate(Depth = fct_recode(Depth, Surface = "Surface",
                               Middepth = "Middepth",
                              Bottom = "Bottom"))
# Look at it

head(Jaffe)
summary(Jaffe)

# Plots to check if assumption of normality is met for Aldrin concentration
ggplot(Jaffe, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

# Plots to check if the assumption of normality is met from HCB concentration
ggplot(Jaffe, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB)) +
          facet_wrap(~Depth)

# Normailty is not met for Aldrin concentration

# Must transform:
Jaffe <- Jaffe %>%
  mutate(ln_Aldrin = log(Aldrin))

# Try checking for normality again
ggplot(Jaffe, aes(x = Depth, y = ln_Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(ln_Aldrin), binwidth = .5)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = ln_Aldrin)) +
  facet_wrap(~Depth)

# Testing for homogeneous variance of Aldrin
summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())
ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

# Testing for homogeneous variance of HCB
summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

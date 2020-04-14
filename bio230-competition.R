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

competition <- read_csv("datasets/demos/Intraspecific-competition.csv") %>%
  group_by(Treatment)

#### Leaf Mass ####

# Calculate summary statistics
summary <- competition %>%
  group_by(Treatment) %>%
  summarise(mean_leafMass_g = mean(leafMass_g),
            median_leafMass_g = median(leafMass_g),
            IQR_leafMass_g = IQR(leafMass_g),
            sd_leafMass_g = sd(leafMass_g),
            var_leafMass_g = var(leafMass_g),
            se_leafMass_g = sd(leafMass_g)/sqrt(n()),
            n_leafMass_g = n()) 

# Testing for normality 
ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = leafMass_g))+
  stat_summary(aes(group = Treatment, x = Treatment, y = leafMass_g), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(leafMass_g), binwidth = .1)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = leafMass_g)) +
  facet_wrap(~Treatment)

# Try a transformation to get rid of the outlier in Treatment group 3

competition <- competition %>%
  mutate(ln_leafMass_g = log(leafMass_g))

# Check if that helped... idk

ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = ln_leafMass_g))+
  stat_summary(aes(group = Treatment, x = Treatment, y = ln_leafMass_g), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(ln_leafMass_g), binwidth = .5)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = ln_leafMass_g)) +
  facet_wrap(~Treatment)


#### Plant Mass ####

# Calculate summary statistics
summary2 <- competition %>%
  group_by(Treatment) %>%
  summarise(mean_plantmass = mean(plantmass),
            median_plantmass = median(plantmass),
            IQR_plantmass = IQR(plantmass),
            sd_plantmass = sd(plantmass),
            var_plantmass = var(plantmass),
            se_plantmass = sd(plantmass)/sqrt(n()),
            n_plantmass = n()) 

# Testing for normality
ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = plantmass))+
  stat_summary(aes(group = Treatment, x = Treatment, y = plantmass), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(plantmass), binwidth = .25)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = plantmass)) +
  facet_wrap(~Treatment)

# Check for homogeneous variance

summ_plantmass <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_plantmass = mean(plantmass),
            sd_plantmass = sd(plantmass),
            n_plantmass = n())

ratio <-(max(summ_plantmass$sd_plantmass))/(min(summ_plantmass$sd_plantmass))

# DATA IS ACCEPTABLE!!!! YAY!

# Let's do an ANOVA
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

model01 <- lm(plantmass~Treatment, data = competition)

autoplot(model01)

anova(model01)

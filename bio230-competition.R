#### Intraspecific Competition Data Analysis ####

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

# Perform unplanned test - Tukey HSD

tukey <- glht(model01, linfct = mcp(Treatment = "Tukey"))
summary(tukey)

#### Internode Length ####

# Calculate summary statistics

summary3 <- competition %>%
  group_by(Treatment) %>%
  summarise(mean_intLength_mm = mean(intLength_mm),
            median_intLength_mm = median(intLength_mm),
            IQR_intLength_mm = IQR(intLength_mm),
            sd_intLength_mm = sd(intLength_mm),
            var_intLength_mm = var(intLength_mm),
            se_intLength_mm = sd(intLength_mm)/sqrt(n()),
            n_intLength_mm = n())

# Testing for normality
ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = intLength_mm))+
  stat_summary(aes(group = Treatment, x = Treatment, y = intLength_mm), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(intLength_mm), binwidth = 2.5)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = intLength_mm)) +
  facet_wrap(~Treatment)

# Check for homogeneous variance

summ_intLength_mm <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_intLength_mm = mean(intLength_mm),
            sd_intLength_mm = sd(intLength_mm),
            n_intLength_mm = n())

ratio <-(max(summ_intLength_mm$sd_intLength_mm))/(min(summ_intLength_mm$sd_intLength_mm))

# Variance is not homogeneous.
# Try a transformation 

competition <- competition %>%
  mutate(ln_intLength_mm = log(intLength_mm))

# Check it
summ_ln_intLength_mm <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_ln_intLength_mm = mean(ln_intLength_mm),
            sd_ln_intLength_mm = sd(ln_intLength_mm),
            n_ln_intLength_mm = n())

ratio <-(max(summ_ln_intLength_mm$sd_ln_intLength_mm))/(min(summ_ln_intLength_mm$sd_ln_intLength_mm))

# It fixed the variance... let's see what it looks like

ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = ln_intLength_mm))+
  stat_summary(aes(group = Treatment, x = Treatment, y = ln_intLength_mm), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(ln_intLength_mm), binwidth = .5)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = ln_intLength_mm)) +
  facet_wrap(~Treatment)

# May have made it a bit less normal, but not unacceptable
# Perform the ANOVA

model02 <- lm(plantmass~Treatment, data = competition)

autoplot(model02)

anova(model02)

# Perform unplanned test - Tukey HSD

tukey <- glht(model02, linfct = mcp(Treatment = "Tukey"))
summary(tukey)

#### Flowers ####

summary4 <- competition %>%
  group_by(Treatment) %>%
  summarise(mean_nFlowers = mean(nFlowers),
            median_nFlowers = median(nFlowers),
            IQR_nFlowers = IQR(nFlowers),
            sd_nFlowers = sd(nFlowers),
            var_nFlowers = var(nFlowers),
            se_nFlowers = sd(nFlowers)/sqrt(n()),
            n_nFlowers = n())

# Testing for normality
ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = nFlowers))+
  stat_summary(aes(group = Treatment, x = Treatment, y = nFlowers), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(nFlowers), binwidth = 5)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = nFlowers)) +
  facet_wrap(~Treatment)

# Check for homogeneous variance

summ_nFlowers <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_nFlowers = mean(nFlowers),
            sd_nFlowers = sd(nFlowers),
            n_nFlowers = n())

ratio <-(max(summ_nFlowers$sd_nFlowers))/(min(summ_nFlowers$sd_nFlowers))

# Normality and variance are acceptable for an ANOVA

model03 <- lm(nFlowers~Treatment, data = competition)

autoplot(model03)

anova(model03)

#### Plant Height ####

summary5 <- competition %>%
  group_by(Treatment) %>%
  summarise(mean_plantHeight_cm = mean(plantHeight_cm),
            median_plantHeight_cm = median(plantHeight_cm),
            IQR_plantHeight_cm = IQR(plantHeight_cm),
            sd_plantHeight_cm = sd(plantHeight_cm),
            var_plantHeight_cm = var(plantHeight_cm),
            se_plantHeight_cm = sd(plantHeight_cm)/sqrt(n()),
            n_plantHeight_cm = n())

# Testing for normality
ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = plantHeight_cm))+
  stat_summary(aes(group = Treatment, x = Treatment, y = plantHeight_cm), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(plantHeight_cm), binwidth = 2.5)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = plantHeight_cm)) +
  facet_wrap(~Treatment)

# Check for homogeneous variance

summ_plantHeight_cm <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_plantHeight_cm = mean(plantHeight_cm),
            sd_plantHeight_cm = sd(plantHeight_cm),
            n_plantHeight_cm = n())

ratio <-(max(summ_plantHeight_cm$sd_plantHeight_cm))/(min(summ_plantHeight_cm$sd_plantHeight_cm))

# Variance is not acceptable, try a transformation

competition <- competition %>%
  mutate(ln_plantHeight_cm = log(plantHeight_cm))

# Check again

summ_ln_plantHeight_cm <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_ln_plantHeight_cm = mean(ln_plantHeight_cm),
            sd_ln_plantHeight_cm = sd(ln_plantHeight_cm),
            n_ln_plantHeight_cm = n())

ratio <-(max(summ_ln_plantHeight_cm$sd_ln_plantHeight_cm))/(min(summ_ln_plantHeight_cm$sd_ln_plantHeight_cm))

# That made it worse

# Perform welch's test
oneway.test(plantHeight_cm ~ Treatment, data = competition)

# Try a tukey test
model04 <- lm(plantHeight_cm~Treatment, data = competition)

tukey <- glht(model04, linfct = mcp(Treatment = "Tukey"))
summary(tukey)

#### Branches ####


summary6 <- competition %>%
  group_by(Treatment) %>%
  summarise(mean_nBranches = mean(nBranches),
            median_nBranches = median(nBranches),
            IQR_nBranches = IQR(nBranches),
            sd_nBranches = sd(nBranches),
            var_nBranches = var(nBranches),
            se_nBranches = sd(nBranches)/sqrt(n()),
            n_nBranches = n())

# Testing for normality
ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = nBranches))+
  stat_summary(aes(group = Treatment, x = Treatment, y = nBranches), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(nBranches), binwidth = .5)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = nBranches)) +
  facet_wrap(~Treatment)

# Check for homogeneous variance

summ_nBranches <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_nBranches = mean(nBranches),
            sd_nBranches = sd(nBranches),
            n_nBranches = n())

ratio <-(max(summ_nBranches$sd_nBranches))/(min(summ_nBranches$sd_nBranches))

# Not normal and not homogeneous variance

#### Chlorophyll Concentration ####

summary7 <- competition %>%
  group_by(Treatment) %>%
  summarise(mean_ChlorophyllConc = mean(ChlorophyllConc),
            median_ChlorophyllConc = median(ChlorophyllConc),
            IQR_ChlorophyllConc = IQR(ChlorophyllConc),
            sd_ChlorophyllConc = sd(ChlorophyllConc),
            var_ChlorophyllConc = var(ChlorophyllConc),
            se_ChlorophyllConc = sd(ChlorophyllConc)/sqrt(n()),
            n_ChlorophyllConc = n())

# Testing for normality
ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = ChlorophyllConc))+
  stat_summary(aes(group = Treatment, x = Treatment, y = ChlorophyllConc), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(competition) +
  geom_histogram(aes(ChlorophyllConc), binwidth = 50)+
  facet_wrap(~Treatment)
ggplot(competition)+
  geom_qq(aes(sample = ChlorophyllConc)) +
  facet_wrap(~Treatment)

# Check for homogeneous variance

summ_ChlorophyllConc <- competition %>%
  group_by(Treatment) %>% 
  summarise(mean_ChlorophyllConc = mean(ChlorophyllConc),
            sd_ChlorophyllConc = sd(ChlorophyllConc),
            n_ChlorophyllConc = n())

ratio <-(max(summ_ChlorophyllConc$sd_ChlorophyllConc))/(min(summ_ChlorophyllConc$sd_ChlorophyllConc))

# Do an ANOVA

model06 <- lm(ChlorophyllConc~Treatment, data = competition)

autoplot(model06)

anova(model06)

# Not significant

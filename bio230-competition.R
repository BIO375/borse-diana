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

ggplot(competition) +
  geom_boxplot(aes(group = Treatment, x = Treatment, y = leafMass_g))+
  stat_summary(aes(group = Treatment, x = Treatment, y = leafMass_g), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)



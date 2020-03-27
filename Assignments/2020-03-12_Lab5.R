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
          se_obliquity = sd(Obliquity)/sqrt(n()),
          n_Obliquity = n())

view(summary_Obliquity)

alpha <- 0.05
mean <- 23.49878
se <- 	0.008771201
df <- 4
null_mean <- 0

t_sample <- (mean - null_mean)/(se)

view(t_sample)

t.test(Obliquity$Obliquity, alternative = "two.sided", mu = 0, conf.level = 0.95)

#### Question 2 ####

heart <- read_csv("datasets/demos/HeartAttack_short.csv", col_types = cols(group = col_character()))

view(heart)

summ_heart <- heart %>%
  group_by(group) %>%
  summarise(mean_cholest = mean(cholest),
            median_cholest = median(cholest),
            IQR_cholest = IQR(cholest),
            sd_cholest = sd(cholest),
            var_cholest = var(cholest),
            se_cholest = sd(cholest)/sqrt(n()),
            n_cholest = n()) 

view(summ_heart)

ratio <- (max(summ_heart$sd_cholest))/(min(summ_heart$sd_cholest))

view(ratio)

#Test whether assumptions are violated
#Boxplot

ggplot(heart) +
  geom_histogram(aes(cholest), binwidth = 10)

ggplot(heart) +
  geom_boxplot(aes(cholest))

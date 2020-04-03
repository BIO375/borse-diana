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
#Craig_Foote <- read_csv("datasets/demos/Craig_Foote.csv")
### DIANA: I can't find the data file from above, so I read in the one from the abd folder
# and then renamed the variables with your names 

Craig_Foote <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

Craig_Foote <- rename(Craig_Foote, Type = species, Color = skinColor)

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

view(summ_Craig_Foote)

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
### Diana, you are going to scream but it was literally just that you had the W in wilcox.test capitalized.  
# Because there are repeats, you also have to throw the argument exact = FALSE in there.

# wilcox.test(Color ~ Type, data = Craig_Foote, var.equal = TRUE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Color ~ Type, data = Craig_Foote, var.equal = TRUE, alternative = "less", mu = 0, exact = FALSE, conf.level = 0.95)
t.test(Color ~ Type, data = Craig_Foote, var.equal = TRUE, alternative = "less", mu = 0, conf.level = 0.95)



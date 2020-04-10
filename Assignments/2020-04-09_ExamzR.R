#### Exam II ####

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

#### Question 17 ####

# Read in the data
baker <- read_csv("datasets/demos/baker.csv")

# Calculate difference
baker <- baker %>%
  mutate(diff = Before-After)

# Calculate summary statistics
summary <- baker %>%
  summarise(mean = mean(diff),
            sd = sd(diff),
            n = n(),
            se = sd(diff)/sqrt(n()))

alpha <- 0.05
mean <- summary$mean
se <- summary$se
df <- summary$n -1

# Check if the data violates assumption of normality

# Histogram
ggplot(baker) +
  geom_histogram(aes(diff), binwidth = .5)

# Boxplot
ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))+
  stat_summary(aes(x = "", y = diff), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Q-Q plot
ggplot(baker)+
  geom_qq(aes(sample = diff))

# It does. Super not normal

# Transformation time! Differences have negative values, so we can't just take 
# Log(diff). 

# mutate so that it is diff^2
baker <- baker %>%
  mutate(diffsqrd = diff^2)

#Then take the log
baker <- baker %>%
  mutate(log_diffsqrd = log(diffsqrd))

# Check for normality 
summary <- baker %>%
  summarise(mean = mean(log_diffsqrd),
            sd = sd(log_diffsqrd),
            n = n(),
            se = sd(log_diffsqrd)/sqrt(n()))

# Histogram
ggplot(baker) +
  geom_histogram(aes(log_diffsqrd), binwidth = .5)

# Boxplot
ggplot(baker) +
  geom_boxplot(aes(x = "", y = log_diffsqrd))+
  stat_summary(aes(x = "", y = log_diffsqrd), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Q-Q plot
ggplot(baker)+
  geom_qq(aes(sample = log_diffsqrd))

# That made it acceptably normal, so I can perform a paired t-test.
t.test(baker$After, baker$Before, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)

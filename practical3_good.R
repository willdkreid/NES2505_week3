# Practical 3 - multiple explanatory variables
# This practical covers block designs, interaction terms and how
# to fit curves

# Begin by loading the relevant libraries used in this practical
library(bio2020)
library(tidyr)

# 1. Block designs ----
# 1.1 Theory ----
# Block designs structure the randomisation, so that there is less risk of all
# the treatments being biased, purely by chance, e.g. near the cold door of a
# glasshouse or Petri dishes in an incubator, or waterlogged part of a field.

# 1.2 Example block design pea growth ----
# Import and check the data
# It consists of 5 columns, one for Block, and 4 different fertilisers 
pea_dat<-read.csv("Data/Peas.csv")
summary(pea_dat)
str(pea_dat)
head(pea_dat)

# 1.2.1 Reshape the data from 'wide' to 'long' format
# Long format is expected for most analyses and plotting
pea_dat_long <- pivot_longer(
  data = pea_dat,           # The original dataset
  cols = NitroGrow:Control, # The 4 columns of yields to restructure
  names_to = "Treatment",   # The 4 cols will go into a new col called Treatment
  values_to = "Yield"       # Cell entries from fertilisers in new col Yield
  )

# Overall average yield across all treatments
mean(Yield ~ NULL, data = pea_dat_long) # mean(~Yield, data=pea_dat_long) OK

# Boxplots show the median, interquartile range (50% of data) etc.
gf_boxplot(Yield ~ Treatment, data = pea_dat_long) %>%
  gf_labs(x = "Experimental treatment", y = "Yield of peas per plot (g)") %>% 
  gf_refine(theme_classic())


# 1.3 Linear models of pea data ----
# 1.3.1 Recode Block and Treatment ----

# The Block variable is categorical, but as it was coded numerically, R has
# assumed it is a number. Use as.factor() to recode
# The summary() function displays mean of Block etc. which is nonsense!
# Treatment is shown as a character, which is not ideal
summary(pea_dat_long) 
pea_dat_long$Block <- as.factor(pea_dat_long$Block)
pea_dat_long$Treatment <- as.factor(pea_dat_long$Treatment)
# Now the summary() function displays Block and Treatment categories
summary(pea_dat_long)

# 1.3.2 Linear models with and without Block ----
# First a linear model without Block. Treatment is not significant
pea_dat_lm1 <- lm(Yield ~ Treatment, data=pea_dat_long)
anova(pea_dat_lm1) # Non-sig p=0.062
# Second, a linear model with Block. Both Treatment and Block significant
# Some of the unexplained variation has gone into Block. This has increased
# the F-ratio for Treatment, which is now significant.
pea_dat_lm2 <- lm(Yield ~ Treatment + Block, data=pea_dat_long)
anova(pea_dat_lm2) # Sig for both Treatment and Block

# 1.3.3 Which fertiliser treatment is best? ----
# The linear model merely shows that Treatment is significant. It does not show
# which pairs of fertilisers differ from each other. The Tukey test does a 
# series of pairwise comparisons. If the mean does NOT cross zero, based on
# the 95 confidence intervals, then we assume the pair is different.
TukeyHSD(pea_dat_lm2, which="Treatment")
plot(TukeyHSD(pea_dat_lm2, which="Treatment"))
# Results show that:
# NitroGrow significantly bigger than both Control and PowerGro

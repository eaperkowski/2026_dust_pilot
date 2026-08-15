#####################################################################
# Libraries
#####################################################################
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(performance)
library(multcomp)

#####################################################################
# Load uncleaned dataset
#####################################################################
df_cleaned <- read.csv("../data_sheets/li600_cleaned.csv") %>%
  separate(id, into = c("dust_trt", "dust_rep", "ovr_rep"), remove = F)
head(df_cleaned)  

#####################################################################
# Stomatal conductance model
#####################################################################
gsw_model <- lmer(sqrt(gsw) ~ dust_trt + (1 | Date),
                  data = subset(df_cleaned, dust_status == "post_dust"))

# Check model assumptions
plot(gsw_model)
qqnorm(residuals(gsw_model))
qqline(residuals(gsw_model))
hist(residuals(gsw_model))
densityPlot(residuals(gsw_model))
shapiro.test(residuals(gsw_model))
outlierTest(gsw_model)

# Model output
summary(gsw_model)
Anova(gsw_model)
performance(gsw_model)

#####################################################################
# Tleaf model
#####################################################################
tleaf_model <- lmer(Tleaf ~ dust_trt * dust_status + (1 | Date),
                  data = df_cleaned)

# Check model assumptions
plot(tleaf_model)
qqnorm(residuals(tleaf_model))
qqline(residuals(tleaf_model))
hist(residuals(tleaf_model))
densityPlot(residuals(tleaf_model))
shapiro.test(residuals(tleaf_model))
outlierTest(tleaf_model)

# Model output
summary(tleaf_model)
Anova(tleaf_model)
performance(tleaf_model)

# Post-hoc comparisons
cld(emmeans(tleaf_model, pairwise ~ dust_trt*dust_status))

#####################################################################
# PhiPSII model
#####################################################################
phips2_model <- lmer(PhiPS2 ~ dust_trt + (1 | Date),
                    data = subset(df_cleaned, dust_status == "post_dust"))

# Check model assumptions
plot(phips2_model)
qqnorm(residuals(phips2_model))
qqline(residuals(phips2_model))
hist(residuals(phips2_model))
densityPlot(residuals(phips2_model))
shapiro.test(residuals(phips2_model))
outlierTest(phips2_model)

# Model output
summary(phips2_model)
Anova(phips2_model)
performance(phips2_model)

# Post-hoc comparisons
emmeans(phips2_model, pairwise ~ dust_trt)

#####################################################################
# ETR model
#####################################################################
etr_model <- lmer(ETR ~ dust_trt + (1 | Date),
                  data = subset(df_cleaned, dust_status == "post_dust"))

# Check model assumptions
plot(etr_model)
qqnorm(residuals(etr_model))
qqline(residuals(etr_model))
hist(residuals(etr_model))
densityPlot(residuals(etr_model))
shapiro.test(residuals(etr_model))
outlierTest(etr_model)

# Model output
summary(etr_model)
Anova(etr_model)
performance(etr_model)

# Post-hoc comparisons
emmeans(etr_model, pairwise ~ dust_trt)

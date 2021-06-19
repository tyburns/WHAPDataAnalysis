# Effect of action in previous 3 years

library(tidyverse)
library(car)
library(boot)
library(emmeans)
library(rsample)
library(MASS)
library(ggforce)
library(knitr)
library(kableExtra)
library(lme4)
library(effects)


# Use qdt_mass_g_m2 from WHAP_Main_Script_2019.Rmd

qdt_mass_g_m2_2019 <- read.csv("/Users/emilioalaca/Google Drive/Documents/PROJECTS/RefugesIMKaylene/WaterbirdHabitat2017/WHAPDataAnalysis_git/WHAPDataAnalysis/qdt_mass_g_m2_2019.txt") %>%
  dplyr::select(-X) %>%
  dplyr::filter(LIT == "KRN")

# There is data on act only for KRN and most of the flooding data is for KRN.
# Thus, only KRN data are used for these comparisons.

act <- read.csv("/Users/emilioalaca/Google Drive/Documents/PROJECTS/RefugesIMKaylene/WaterbirdHabitat2017/WHAPDataAnalysis_git/WHAPDataAnalysis/WHAP2019_PP_20201002_MUs_KRN_Actions_v2.csv",
                na.strings=c(""," ","NA")) %>%
  mutate(subunit_ID = paste(LIT, Unit_Name, Subunit_Name, sep = "_"),
         act = factor(Action_yn)) %>%
  dplyr::filter(LIT == "KRN") %>%
  dplyr::select(LIT,
                Unit_Name,
                Subunit_Name,
                subunit_ID,
                act,
                Action)

# effects of flooding in KRN

# y.flooded.n are number of years flooded out of the last n
# y20167, y20178, y20189 are flooding in the last 3 seasons up to 2019

water2 <- read.csv("/Users/emilioalaca/Google Drive/Documents/PROJECTS/RefugesIMKaylene/WaterbirdHabitat2017/WHAPDataAnalysis_git/WHAPDataAnalysis/Kern_Flooding_Schedule.csv") %>%
  mutate(LIT = "KRN",
         Subunit_Name = SubUnit) %>%
  dplyr::select(LIT,
         Subunit_Name,
         starts_with("y"))

qdt_mass_act_fld <- left_join(qdt_mass_g_m2_2019, act) %>%
  left_join(water2) %>%
  dplyr::select(LIT,
                Unit_Name,
                Subunit_Name,
                subunit_ID,
                act,
                Action,
                y.flooded.15,
                y.flooded.5,
                y20167,
                y20178,
                y20189,
                st_g_m2, # none of the units has watergrass
                st_mass_m2_var)

str(qdt_mass_act_fld)

table(qdt_mass_act_fld$Action)

# Analyze _apparent_ effects of "treatments"

scatterplot(st_g_m2 ~ y.flooded.5 | act, qdt_mass_act_fld, smooth = FALSE)

scatterplot(st_g_m2 ~ y.flooded.15 | act, qdt_mass_act_fld, smooth = FALSE)

boxplot(st_g_m2 ~ y20189, qdt_mass_act_fld) # only 2 were not flooded

plot(st_g_m2 ~ act, data = qdt_mass_act_fld)





plot(st_g_m2 ~ factor(y20189), qdt_mass)

anova(lm(st_g_m2 ~ act + y.flooded.15, qdt_mass))

summary(lm(st_g_m2 ~ y.flooded.15 + act, qdt_mass))


anova(lm(st_g_m2 ~ y.flooded.5 * act, qdt_mass))


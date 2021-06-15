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


act <- read.csv("WHAP2019_PP_20201002_MUs_KRN_Actions_v2.csv",
                na.strings=c(""," ","NA")) %>%
  mutate(subunit_ID = paste(LIT, Unit_Name, Subunit_Name, sep = "_"),
         act = factor(Action_yn))

str(act)


# Use qdt_mass_g_m2 from WHAP_Main_Script.Rmd

qdt_mass <- left_join(qdt_mass_g_m2, act)

str(qdt_mass)

act_st1 <- lm(st_g_m2 ~ act * LIT,
#                weights = 1/st_mass_m2_var,
                data  = qdt_mass)

anova(act_st1)

plot(allEffects(act_st1))

plot(st_g_m2 ~ act, data = qdt_mass)

plot(wg_g_m2 ~ act, data = qdt_mass)


# effects of flooding in KRN

water2 <- read.csv('WHAP_mapping/Kern_Flooding_Schedule.csv') %>%
  mutate(LIT = "KRN",
         Subunit_Name = SubUnit) %>%
  dplyr::select(LIT,
         Subunit_Name,
         starts_with("y"))

str(water2)

qdt_mass <- qdt_mass %>%
  left_join(water2)

qdt_mass %>%
  dplyr::select(LIT,
                subunit_ID,
                wg_g_m2,
                st_g_m2,
                tot_g_m2,
                act,
                starts_with("y")) %>%
  print(n = Inf)


plot(st_g_m2 ~ factor(y20189), qdt_mass)

scatterplot(st_g_m2 ~ y.flooded.5 | act, qdt_mass, smooth = FALSE)

scatterplot(st_g_m2 ~ y.flooded.15 | act, qdt_mass, smooth = FALSE)

anova(lm(st_g_m2 ~ act + y.flooded.15, qdt_mass))

summary(lm(st_g_m2 ~ y.flooded.15 + act, qdt_mass))


anova(lm(st_g_m2 ~ y.flooded.5 * act, qdt_mass))


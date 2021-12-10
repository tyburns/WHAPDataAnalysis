library(tidyverse)
library(car)
library(boot)
library(emmeans)
library(rsample)
library(MASS)
library(ggforce)
library(knitr)
library(kableExtra)
library(measurements)
library(lubridate)
library(compositions)
library(magrittr)

vps0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_VantagePolygons_20210831.csv",
                 stringsAsFactors = FALSE)

cps0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_CirclePlots_20210901.csv",
                 stringsAsFactors = FALSE)

qdt0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_Quadrats_20210923.csv",
                 stringsAsFactors = FALSE)

mus0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_ManagementUnits_20210902.csv",
                 stringsAsFactors = FALSE)


# Fix vernacular names

vps0 %<>%
  mutate(vernacularName = str_replace(vernacularName, " ", "_"))

cps0 %<>%
  mutate(vernacularName = str_replace(vernacularName, " ", "_"))

qdt0 %<>%
  mutate(vernacularName = str_replace(vernacularName, " ", "_"))


# Fix domain of quadratSize

qdtSizeKey <- c("15x15cm" = "15x15",
                "30x30cm" = "30x30",
                "5x5cm" = "5x5")

qdt0$quadratSize <- dplyr::recode(qdt0$quadratSize, !!!qdtSizeKey)

# Fix unit and subunit names

mus0 %<>%
  mutate(subunitName = ifelse(subunitName == "", unitName, subunitName))

vps0 %<>%
  mutate(subunitName = ifelse(subunitName == "", unitName, subunitName))

cps0 %<>%
  mutate(subunitName = ifelse(subunitName %in% c("0", ""), unitName, subunitName))

qdt0 %<>%
  mutate(subunitName = ifelse(subunitName == "", unitName, subunitName))

# In Rebeca's computer some escape characters are added to the beginning of the file
colnames(vps0)[1] <- "GlobalID"
colnames(cps0)[1] <- "GlobalID"

# prepare column names necessary
spp <- setdiff(unique(c(vps0$vernacularName,
                        cps0$vernacularName,
                        qdt0$vernacularName)),
               c("Other_cover"))

strata <- c("low",
            "medium",
            "high")

in_col_nms <- c("pVA_Other_cover_NA",
                paste("pVA",
                      rep(spp, length(strata)),
                      rep(strata, each = length(spp)),
                      sep = "_"))

out_col_nms <- in_col_nms %>%
  str_replace_all(c("pVA" = "p",
                    "low" = "a.Low",
                    "medium" = "b.Med",
                    "high" = "c.High")) %>%
  set_names(nm = in_col_nms)

closure_proportions <- function(df, .in_cols, .out_cols) {
  df %<>% mutate(tot_obs_prop = rowSums(across(starts_with("pVA_"))))
  for (nm in .in_cols) {
    ifelse(nm %in% names(df),
           df[[.out_cols[nm]]] <- df[[nm]] / df$tot_obs_prop,
           df[[.out_cols[nm]]] <- 0)
  }
  return(df)
}


# Vantage polygon information =============================
vps <- vps0 %>%
  dplyr::select(GlobalID,
                LIT,
                unitName,
                subunitName,
                areaVisible_ac,
                vernacularName,
                stratum,
                proportionVisibleArea) %>% # pVA
  pivot_wider(names_from = c(vernacularName, stratum),
              names_prefix = "pVA_",
              values_from = proportionVisibleArea,
              values_fill = 0) %>%
  closure_proportions(.in_cols = in_col_nms,
                      .out_cols = out_col_nms) %>%
  mutate(level = "VP",
         subunit_ID = factor(paste(LIT, unitName, subunitName, sep = "_"))) %>%
  dplyr::select(LIT,
                unitName,
                subunitName,
                subunit_ID,
                level,
                areaVisible_ac,
                starts_with("p_"))

# Circle plot information =============================
cps <- cps0 %>%
  dplyr::select(GlobalID,
                LIT,
                unitName,
                subunitName,
                vernacularName,
                stratum,
                proportionVisibleArea) %>%
  group_by(GlobalID,#         | This section to be removed
           LIT,#              | once the duplicate records
           unitName,#         | are sorted out.
           subunitName,#      | Duplicates may actually be
           vernacularName,#   | incorrectly identified strata
           stratum) %>%#      | or species
  summarize(proportionVisibleArea = mean(proportionVisibleArea),
            .groups = "drop") %>%
  mutate(vernacularName = str_replace(vernacularName, " ", "_")) %>%
  pivot_wider(names_from = c(vernacularName, stratum),
              names_prefix = "pVA_",
              values_from = proportionVisibleArea,
              values_fill = 0) %>%
  closure_proportions(.in_cols = in_col_nms,
                      .out_cols = out_col_nms) %>%
  mutate(level = "CP",
         subunit_ID = factor(paste(LIT, unitName, subunitName, sep = "_")),
         areaVisible_ac = conv_unit(pi * 15^2, "m2", "acre")) %>%
  dplyr::select(LIT,
                unitName,
                subunitName,
                subunit_ID,
                level,
                areaVisible_ac,
                starts_with("p_"))

# Management unit information =============================
mus <- mus0 %>% # management units
  dplyr::select(LIT,
                unitName,
                subunitName,
                su.area_ac = acreage) %>%
  mutate(subunit_ID = paste(LIT, unitName, subunitName, sep = "_"))

# Join area estimates from VPs and CPs, then get total area from MUs
vpcp <- bind_rows(vps, cps) %>%
  full_join(mus) %>%
  na.omit()

InVPCP <- vpcp %>%
  dplyr::select(-contains("Other")) %>%
  pivot_longer(cols = starts_with("p_"),
               names_to = "spp_strat") %>%
  group_by(LIT, spp_strat) %>%
  summarise(.groups = "drop") %>%
  mutate(spp_strat = str_sub(spp_strat, 3, str_length(spp_strat)))

InVPCP %>%
  xtabs(formula = ~~spp_strat + LIT,
        data = .)

InQDT <- qdt0 %>%
  dplyr::select(GlobalID,
                LIT,
                vernacularName,
                stratum) %>%
  unique() %>%
  mutate(stratum = dplyr::recode(stratum,
                                 `low` = "a.Low",
                                 `medium` = "b.Med",
                                 `high` = "c.High"),
         spp_strat = paste(spp, stratum, sep = "_")) %>%
  dplyr::select(LIT,
                spp_strat)

InQDT %>%
  xtabs(formula = ~spp_strat + LIT,
        data = .)

(lackingQDT <- setdiff(with(InVPCP, paste(LIT, spp_strat)),
                       with(InQDT, paste(LIT, spp_strat))))

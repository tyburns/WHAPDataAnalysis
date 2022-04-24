# WHAP DATA PREPARATION PRIOR TO USING WHAP2_sim_stats.Rmd

# INPUT: WHAP_yyyy_VantagePolygons_yyyymmdd.csv
#        WHAP_yyyy_CirclePlots_yyyymmdd.csv
#        WHAP_yyyy_Quadrats_yyyymmdd.csv
#        WHAP_yyyy_ManagementUnits_yyyymmdd.csv
# i.e., files from Eric after validation with WHAP2_DataChecks.Rmd
#
# OUTPUT: vpcpyyyy
#         qdt_yyyy
#
# to be used in WHAP2_sim_stats.Rmd
#
#
#
#               |======================================|
#               |WHAP_yyyy_VantagePolygons_yyyymmdd.csv|
# survey-123 -> |WHAP_yyyy_CirclePlots_yyyymmdd.csv    |
#               |WHAP_yyyy_Quadrats_yyyymmdd.csv       |
#               |WHAP_yyyy_ManagementUnits_yyyymmdd.csv|
#               |======================================|
#                                     \
#                                      \
#                                      V
#                            WHAP2_DataChecks.Rmd
#                                       \
#                                       V
#                                |=================|            |==========|
#                                |                 |            | vpcpyyyy |
# ->  ->  ->  ->   ->   ->   ->  |WHAP2_DataPrep.R | -> ->  ->  | qdt_yyyy |
#                           _->  |  (This file)    |            |==========|
#                         |      |=================|                |
#                       /                                           |
#          d2m_models_list.rds                                      V
#                 ^                                       WHAP2_sim_stats.Rmd
#               /
#     d2m_modeling.R
#           /
#         /
#   d2m_data.rds
# d2m_data_yyyy.csv
#
#
#
#

# Setup ======

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


spp_prefix <- c(
  `Swamp Timothy` = "st", # may not be necessary. check later.
  Watergrass = "wg",
  Smartweed = "sw"
)

# Read d2m models ================

# d2m_models_list comes from d2m_modeling.R and it contains the user input
# determining what species are valid, i.e., have models to use in this script
# d2m models should be named and retrieved programmatically using a list.
# d2m data files should follow a name convention to be found.
# d2m model list elements are named with species vernacularName:
# "Swamp_Timothy", "Watergrass", "Smartweed", etc.

d2m_models_list <- read_rds("d2m_models_list.rds")

# This list must have elements named "Watergrass", "Swamp_Timothy",
# "Smartweed", etc.

# Data are assumed to have passed WHAP_DataChecks.Rmd
# Species, taxon_ID's and subunits are already validated.


# Read data files =====

# User input: path to folder with file locations
# Only one year at a time is allowed in this code

vps0 <- read.csv(
  "WHAP_2021_ProcessedData_20210902/WHAP_2021_VantagePolygons_20210831.csv",
  stringsAsFactors = FALSE
) %>%
  mutate(
    eventDate = as.Date(eventDate,
      tryFormats = c("%m/%d/%Y")
    ),
    taxonID = as.character(taxonID)
  )


cps0 <- read.csv(
  "WHAP_2021_ProcessedData_20210902/WHAP_2021_CirclePlots_20210901.csv",
  stringsAsFactors = FALSE
) %>%
  mutate(
    eventDate = as.Date(eventDate,
      tryFormats = c("%m/%d/%Y")
    ),
    taxonID = as.character(taxonID)
  )


qdt0 <- read.csv(
  "WHAP_2021_ProcessedData_20210902/WHAP_2021_Quadrats_20210923.csv",
  stringsAsFactors = FALSE
) %>%
  mutate(
    eventDate = as.Date(eventDate,
      tryFormats = c("%m/%d/%Y")
    ),
    taxonID = as.character(taxonID)
  )


mus0 <- read.csv(
  "WHAP_2021_ProcessedData_20210902/WHAP_2021_ManagementUnits_20210902.csv",
  stringsAsFactors = FALSE
)

# Fix vernacular names =====

vps0 %<>%
  mutate(
    vernacularName =
      str_replace(
        vernacularName,
        " ",
        "_"
      )
  )

cps0 %<>%
  mutate(
    vernacularName =
      str_replace(
        vernacularName,
        " ",
        "_"
      )
  )

qdt0 %<>%
  mutate(
    vernacularName =
      str_replace(
        vernacularName,
        " ",
        "_"
      )
  )


# Fix unit and subunit names ======

# Give unit name to subunits without subunit name or 0.

mus0 <- mus0 %>%
  mutate(
    subunitName =
      ifelse(
        subunitName == "",
        unitName,
        subunitName
      )
  )

vps0 <- vps0 %>%
  mutate(
    subunitName =
      ifelse(
        subunitName == "",
        unitName,
        subunitName
      )
  )

cps0 <- cps0 %>%
  mutate(
    subunitName =
      ifelse(
        subunitName %in% c("0", ""),
        unitName,
        subunitName
      )
  )

qdt0 <- qdt0 %>%
  mutate(
    subunitName =
      ifelse(
        subunitName == "",
        unitName,
        subunitName
      )
  )


# Get list of valid species and eliminate others from data =======

spp_valid <- names(d2m_models_list)

vps0 <- vps0 %>%
  dplyr(filter(
    vernacularName %in% spp_valid
  ))

cps0 <- cps0 %>%
  dplyr(filter(
    vernacularName %in% spp_valid
  ))

qdt0 <- qdt0 %>%
  dplyr(filter(
    vernacularName %in% spp_valid
  ))

# Get year of data ======

data_year <- year(vps0$eventDate[1])


# Prepare column names necessary =====

spp <- setdiff(
  unique(
    c(
      vps0$vernacularName,
      cps0$vernacularName,
      qdt0$vernacularName
    )
  ),
  c("Other_cover")
)

strata <- c(
  "low",
  "medium",
  "high"
)

in_col_nms <- c(
  "pVA_Other_cover_NA",
  paste("pVA", # for "proportion of Visible Area"
    rep(spp, length(strata)),
    rep(strata, each = length(spp)),
    sep = "_"
  )
)

out_col_nms <- in_col_nms %>%
  str_replace_all(
    c(
      "pVA" = "p",
      "low" = "a.Low",
      "medium" = "b.Med",
      "high" = "c.High"
    )
  ) %>%
  set_names(nm = in_col_nms)

# Function to bring estimated proportions of cover to 1.00 ======

# This function is necessary because field estimates do not have to add up to
# 1.00 to allow for fast and more accurate relative estimates in the field.
# The function takes a vector with all possible names for proportion cols
# that can results from the pivot_wider below, calculates the total for each
# VP or CP and corrects all results to be 0 if the col in not in the data or
# sum up to 1.00 if it is there. This is used later to create a matrix that has
# rows for VPs or CPs and columns for all combinations of vernacularName and
# stratum. Thus, this function has two functions: make sure proportions add up
# to 1.00 and complete the data with columns with 0's for those combinations
# of vernacularName and stratum that do not appear in the data.

closure_proportions <- function(df, .in_cols, .out_cols) {
  df <- df %>%
    mutate(
      tot_obs_prop = rowSums(
        across(starts_with("pVA_"))
      )
    )
  for (nm in .in_cols) {
    ifelse(nm %in% names(df),
      df[[.out_cols[nm]]] <- df[[nm]] / df$tot_obs_prop,
      df[[.out_cols[nm]]] <- 0
    )
  }
  return(df)
}


# Create vps form vps0. Vantage polygons. =======================

vps <- vps0 %>%
  dplyr::select(
    GlobalID,
    LIT,
    unitName,
    subunitName,
    areaVisible_ac,
    vernacularName,
    stratum,
    proportionVisibleArea
  ) %>% # pVA
  pivot_wider(
    names_from = c(
      vernacularName,
      stratum
    ),
    names_prefix = "pVA_",
    values_from = proportionVisibleArea,
    values_fill = 0
  ) %>%
  closure_proportions(
    .in_cols = in_col_nms,
    .out_cols = out_col_nms
  ) %>%
  mutate(
    level = "VP",
    subunit_ID = factor(paste(LIT,
      unitName,
      subunitName,
      sep = "_"
    ))
  ) %>%
  dplyr::select(
    LIT,
    unitName,
    subunitName,
    subunit_ID,
    level,
    areaVisible_ac,
    starts_with("p_")
  )



# Create cps from cps0. Circle plots. ======================

cps <- cps0 %>%
  dplyr::select(
    GlobalID,
    LIT,
    unitName,
    subunitName,
    vernacularName,
    stratum,
    proportionVisibleArea
  ) %>%
  group_by(
    GlobalID, #         | This section could be removed
    LIT, #              | once the duplicate records
    unitName, #         | are eliminated.
    subunitName, #      | Duplicates may actually be
    vernacularName, #   | incorrectly identified strata
    stratum #           | or species
  ) %>%
  summarize(
    proportionVisibleArea = mean(proportionVisibleArea),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = c(
      vernacularName,
      stratum
    ),
    names_prefix = "pVA_",
    values_from = proportionVisibleArea,
    values_fill = 0
  ) %>%
  closure_proportions(
    .in_cols = in_col_nms,
    .out_cols = out_col_nms
  ) %>%
  mutate(
    level = "CP",
    subunit_ID = factor(paste(LIT,
      unitName,
      subunitName,
      sep = "_"
    )),
    areaVisible_ac = conv_unit(
      pi * 15^2,
      "m2",
      "acre"
    )
  ) %>%
  dplyr::select(
    LIT,
    unitName,
    subunitName,
    subunit_ID,
    level,
    areaVisible_ac,
    starts_with("p_")
  )


# Create mus from mus0. Management units. =========================

mus <- mus0 %>% # management units
  dplyr::select(
    LIT,
    unitName,
    subunitName,
    su.area_ac = acreage
  ) %>%
  mutate(
    subunit_ID = paste(
      LIT,
      unitName,
      subunitName,
      sep = "_"
    )
  )

# Join vps, cps and mus =====================

vpcp <- bind_rows(
  vps,
  cps
) %>%
  full_join(mus) %>%
  na.omit()

# Set output folder path & save vpcpyyyy.rds ===========

path_out <- "path relative to wherever this will run"

out_vpcp_name <- paste0(
  path_out,
  "vpcp",
  data_year
)

write_rds(vpcp, out_vpcp_name)


# Process quadrat information ====================

# Predict mass for each sh using d2m_model_list and sh dimensions.
# Average sh mass per quadrat, multiply by sh density in

qdt <- qdt0 %>%
  pivot_wider(
    names_from = measurementType,
    values_from = measurementValue,
    values_fill = NA
  ) %>%
  dplyr::select(
    GlobalID,
    LIT,
    unitName,
    subunitName,
    stratum,
    quadratSize,
    managementAction,
    nSeedHeads,
    sh_length_mm,
    f2t_length_mm,
    sh_width_mm
  )


# ST quadrat info -> table with LIT, stratum and mass of seed per m2

qdt_st <- qdt0 %>%
  dplyr::filter(vernacularName == "Swamp_Timothy" & stratum != "") %>%
  group_by(
    GlobalID,
    LIT,
    unitName,
    subunitName,
    stratum,
    quadratSize
  ) %>%
  summarise(
    avg_length_mm = mean(measurementValue, na.rm = TRUE),
    nSeedHeads = mean(nSeedHeads, na.rm = TRUE) # all values are repeated anyway
  ) %>%
  mutate(
    qdt_m2 = ifelse(quadratSize == "15x15cm", 0.15^2,
      ifelse(quadratSize == "5x5cm", 0.05^2, 0.10)
    ),
    n_seed_head_m2 = nSeedHeads / qdt_m2,
    no_sh = n_seed_head_m2
  ) %>%
  dplyr::select(
    LIT,
    unitName,
    subunitName,
    stratum,
    avg_length_mm,
    n_seed_head_m2,
    no_sh
  ) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(
    mass_g_m2 = unname(exp(predict(ST_len2mass, newdata = .)) / 1000),
    mass_g_m2 = ifelse(n_seed_head_m2 == 0, 0, mass_g_m2)
  ) %>% # making sure it handles quadrats where no_sh == 0
  dplyr::filter(mass_g_m2 < 2000) %>%
  mutate(
    stratum = dplyr::recode(stratum,
      `low` = "a.Low",
      `medium` = "b.Med",
      `high` = "c.High"
    ),
    stratum = factor(as.character(stratum)),
    subunit_ID = paste(LIT, unitName, subunitName, sep = "_"),
    LIT_Strat = factor(paste(LIT, stratum, sep = "_"))
  ) %>%
  arrange(LIT, unitName, subunitName, stratum)

str(qdt_st)




# WG quadrat info -> table with LIT, stratum and mass of seed per m2

wgc <- unname(coef(WG_len2mass)) # update this line for new model list

qdt_wg <- qdt0 %>%
  dplyr::filter(vernacularName == "Watergrass" &
    stratum != "") %>%
  pivot_wider(
    names_from = measurementType,
    values_from = measurementValue
  ) %>%
  arrange(GlobalID_seed) %>%
  dplyr::filter(sh_length_mm < 250) %>% # remove seed heads beyond model scope
  mutate(
    sh_emerged_wg = f2t_length_mm / sh_length_mm,
    sh_mass_mg = exp(wgc[1] +
      wgc[2] * sh_length_mm / 10 + # divide by 10 because
      wgc[3] * sh_emerged_wg + # model data was in cm
      wgc[4] * f2t_length_mm / 10) - 20
  ) %>%
  mutate(
    qdt_m2 = ifelse(quadratSize == "15x15cm", 0.15^2,
      ifelse(quadratSize == "5x5cm", 0.05^2, 0.10)
    ),
    n_seed_head_m2 = nSeedHeads / qdt_m2
  ) %>%
  group_by(
    GlobalID,
    LIT,
    unitName,
    subunitName,
    stratum,
    quadratSize
  ) %>%
  summarize(
    sh_mass_mg = mean(sh_mass_mg, na.rm = TRUE),
    n_seed_head_m2 = mean(n_seed_head_m2, na.rm = TRUE)
  ) %>%
  mutate(
    mass_g_m2 = n_seed_head_m2 * sh_mass_mg / 1000,
    sh_mass_mg = ifelse(is.nan(sh_mass_mg), NA, sh_mass_mg)
  ) %>%
  dplyr:::select(
    LIT,
    unitName,
    subunitName,
    stratum,
    n_seed_head_m2,
    sh_mass_mg,
    mass_g_m2
  ) %>%
  na.omit() %>%
  mutate(
    stratum = dplyr::recode(stratum,
      `low` = "a.Low",
      `medium` = "b.Med",
      `high` = "c.High"
    ),
    stratum = factor(as.character(stratum)),
    subunit_ID = paste(LIT, unitName, subunitName, sep = "_"),
    LIT_Strat = factor(paste(LIT, stratum, sep = "_"))
  ) %>%
  arrange(LIT, unitName, subunitName, stratum) %>%
  ungroup()

str(qdt_wg)



# Define function to invert the Box-Cox transformation
invBoxCox <- function(x, lambda) {
  if (lambda == 0) exp(x) else (lambda * x + 1)^(1 / lambda)
}


# SW quadrat info -> table with LIT, stratum and mass of seed per m2
# Why is this only passing MDC???

qdt_sw <- qdt0 %>%
  dplyr::filter(vernacularName == "Smartweed" & stratum != "") %>%
  pivot_wider(
    names_from = measurementType,
    values_from = measurementValue
  ) %>%
  arrange(GlobalID_seed) %>%
  # mutate(vol_mm3 = pi * (sh_width_mm/2)^2 * sh_length_mm) %>%
  group_by(
    GlobalID,
    LIT,
    unitName,
    subunitName,
    stratum,
    quadratSize
  ) %>%
  summarise(
    length_mm = mean(sh_length_mm, na.rm = TRUE),
    nSeedHeads = mean(nSeedHeads, na.rm = TRUE)
  ) %>%
  mutate(qdt_m2 = ifelse(quadratSize == "15x15cm", 0.15^2,
    ifelse(quadratSize == "5x5cm", 0.05^2, 0.10)
  )) %>%
  ungroup() %>%
  dplyr::select(
    LIT,
    unitName,
    subunitName,
    stratum,
    qdt_m2,
    nSeedHeads,
    length_mm
  ) %>%
  na.omit() %>%
  mutate(
    sh_mass_mg = unname(invBoxCox(predict(SW_dim2mass, newdata = .),
      lambda = sw_lamb2
    )),
    mass_g_m2 = nSeedHeads * sh_mass_mg / (1000 * qdt_m2),
    stratum = dplyr::recode(stratum,
      `low` = "a.Low",
      `medium` = "b.Med",
      `high` = "c.High"
    ),
    stratum = factor(as.character(stratum)),
    subunit_ID = paste(LIT, unitName, subunitName, sep = "_"),
    LIT_Strat = factor(paste(LIT, stratum, sep = "_"))
  ) %>%
  arrange(LIT, unitName, subunitName, stratum)

str(qdt_sw)



qdt_2021 <- bind_rows(
  (qdt_st %>%
    dplyr::select(
      LIT,
      subunit_ID,
      stratum,
      LIT_Strat,
      mass_g_m2
    ) %>%
    mutate(species = "Swamp_Timothy")),
  (qdt_wg %>%
    dplyr::select(
      LIT,
      subunit_ID,
      stratum,
      LIT_Strat,
      mass_g_m2
    ) %>%
    mutate(species = "Watergrass")),
  qdt_sw %>%
    dplyr::select(
      LIT,
      subunit_ID,
      stratum,
      LIT_Strat,
      mass_g_m2
    ) %>%
    mutate(species = "Smartweed")
) %>%
  mutate(species = factor(species))

write_rds(qdt_2021, "qdt_2021.rds")

# Data checks WHAP 2021

# Packages
library(tidyverse)
library(magrittr)

# Set of correct vernacularName ==============================================
vernac_names <- sort(c("Other cover",
                       "Watergrass",
                       "Swamp Timothy",
                       "Smartweed"))

# Set of correct LIT codes ===================================================
LIT_codes <- sort(c("KRN",
                    "PIX",
                    "MDC",
                    "SAC",
                    "SLW",
                    "CLS"))


# Read files =================================================================
vps0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_VantagePolygons_20210831.csv")
cps0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_CirclePlots_20210901.csv")
qdt0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_Quadrats_20210902.csv")
mus0 <- read.csv("WHAP_2021_ProcessedData_20210902/WHAP_2021_ManagementUnits_20210902.csv")


# Check LIT codes =====================================================
setdiff(unique(vps0$LIT), LIT_codes)
setdiff(unique(cps0$LIT), LIT_codes)
setdiff(unique(qdt0$LIT), LIT_codes) # LMC should be PIX
setdiff(unique(mus0$LIT), LIT_codes)


# Fix LIT codes
qdt0$LIT <- dplyr::recode(qdt0$LIT, "LMC" = "PIX")


# Check variable names =====================================================
# Column names that should be in there but are not:
names(mus0)
names(vps0)
names(cps0)
names(qdt0)


# Check species names ========================================================
# Species vernacular names that should not be there
unique(vps0$vernacularName)
unique(cps0$vernacularName)
unique(qdt0$vernacularName)

# For future we need to have a list of permitted vernacularName values.

# Check consistency of strata between vps + cps and qdts =====================
# The following combinations of species and strata lack quadrat information.

bind_rows((vps0 %>% dplyr::select(vernacularName, stratum)),
          (cps0 %>% dplyr::select(vernacularName, stratum))) %>%
  group_by(vernacularName, stratum) %>%
  summarize(n_propArea = n()) %>%
  full_join(qdt0 %>% dplyr::select(vernacularName, stratum) %>%
              group_by(vernacularName, stratum) %>%
              summarize(n_qdt = n())) %>%
  arrange(vernacularName, stratum) %>%
  dplyr::filter(is.na(n_qdt) & n_propArea > 0 & vernacularName != "Other cover")


# Check uniqueness of proportion of area estimations ==========================

# VPS
print(vp_dups <- vps0 %>%
        group_by(GlobalID,
                 vernacularName,
                 stratum) %>%
        count() %>%
        dplyr::filter(n > 1),
      n = Inf)

# No duplicate records in VPs

# CPS
print(cp_dups <- cps0 %>%
        group_by(GlobalID,
                 LIT,
                 unitName,
                 subunitName,
                 vernacularName,
                 stratum) %>%
        count() %>%
        dplyr::filter(n > 1),
      n = Inf)

# Several duplicate records in CPs

id_cols <- c("GlobalID",
             "LIT",
             "unitName",
             "subunitName",
             "vernacularName",
             "stratum")

dups_from_top <- duplicated(cps0[, id_cols])

dups_from_bottom <- duplicated(cps0[, id_cols], fromLast = TRUE)

cps0[dups_from_top | dups_from_bottom, ] %>%
  dplyr::select(recordedBy,
                LIT,
                unitName,
                subunitName,
                vernacularName,
                stratum,
                proportionVisibleArea) %>%
  arrange(subunitName,
          vernacularName,
          stratum)

# Check consistency in set of subunits across files ==========================

# MUS
mus0 %<>%
  mutate(subunit_ID = paste(LIT,
                            unitName,
                            subunitName,
                            sep = "_"))

# VPS
vps0 %<>%
  mutate(subunit_ID = paste(LIT,
                            unitName,
                            subunitName,
                            sep = "_"),
         spp_LIT_Strat = paste(LIT,
                               vernacularName,
                               stratum,
                               sep = "_"))

# CPS
cps0 %<>%
  mutate(subunit_ID = paste(LIT,
                            unitName,
                            subunitName,
                            sep = "_"),
         spp_LIT_Strat = paste(LIT,
                               vernacularName,
                               stratum,
                               sep = "_"))

# QDTS
qdt0 %<>%
  mutate(subunit_ID = paste(LIT,
                            unitName,
                            subunitName,
                            sep = "_"),
         spp_LIT_Strat = paste(LIT,
                               vernacularName,
                               stratum,
                               sep = "_"))

setdiff(unique(vps0$subunit_ID), unique(mus0$subunit_ID))
setdiff(unique(cps0$subunit_ID), unique(vps0$subunit_ID)) # KRN_5A_5A-6 not in VPS
setdiff(unique(vps0$subunit_ID), unique(cps0$subunit_ID)) # but it's in CPS
setdiff(unique(cps0$subunit_ID), unique(mus0$subunit_ID))
setdiff(unique(qdt0$subunit_ID), unique(mus0$subunit_ID))

mus0[, c("LIT", "unitName", "subunitName")] %>%
  unique() %>%
  arrange(LIT,
          unitName,
          subunitName) %>%
  as_tibble() %>%
  print(n = Inf)

subunits_vps <- vps0[, c("LIT", "unitName", "subunitName")] %>%
  unique() %>%
  arrange(LIT,
          unitName,
          subunitName) %>%
  as_tibble() %>%
  mutate(su_length = str_length(subunitName)) %>%
  print(n = Inf)

# VPS has subunit names that are spaces in MDC. They should be empty strings.
# They are OK in cps.

subunits_cps <- cps0[, c("LIT", "unitName", "subunitName")] %>%
  unique() %>%
  arrange(LIT,
          unitName,
          subunitName) %>%
  as_tibble() %>%
  print(n = Inf)


# Except for cps0, all other files have errors in the unitName and subunitName
# for KRN 4A-3, KRN 4A-5 and KRN 4A-6
# cps0 has some 0's as subunit names. Should be blank.

subunits_qdt <- qdt0[, c("LIT", "unitName", "subunitName")] %>%
    unique() %>%
    arrange(LIT,
            unitName,
            subunitName) %>%
    as_tibble() %>%
  print(n = Inf)

# Unit-subunit names are not consistent across files or with last year.

# Units and subunits that remain the same over time should be named in a 
# backward compatible manner.
# These are the units that were named in 2020:

(subunits_2020 <- read.csv("../../WHAP2020-21/ScriptInput2020/mu2020c.csv") %>%
    dplyr::select(LIT,
                  Unit_Name,
                  Subunit_Name) %>%
    arrange(LIT,
            Unit_Name,
            Subunit_Name))


# Coordinates =================================================================

# VPS
# Check that each point appears only once in globalid
vps0 %>%
  group_by(GlobalID, decimalLatitude, decimalLongitude) %>%
  count() %>%
  dim() %>%
  `[`(1)

vps0 %>%
  group_by(GlobalID) %>%
  count() %>%
  dim() %>%
  `[`(1)

vps0 %>% # Why are there fewer unique coords than ID's?
  group_by(decimalLatitude, decimalLongitude) %>%
  count() %>%
  dim() %>%
  `[`(1)

# CPS
# Check that each point appears only once in GlobalID
cps0 %>%
  group_by(GlobalID, decimalLatitude, decimalLongitude) %>%
  count() %>%
  dim() %>%
  `[`(1)

cps0 %>%
  group_by(GlobalID) %>%
  count() %>%
  dim() %>%
  `[`(1)

cps0 %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  count() %>%
  dim() %>%
  `[`(1)


## Check domain of quadrat sizes===========================================

with(qdt0, table(quadratSize))

# Quadrat size domain has been changed to include "cm" at the end.
# Suggest that units be placed in the variable name, not in the values.

## Why are there data for PIX-5 and KRN-Shrew Slough? =========================

# RB and TB report not having measured that unit.
# Looks like one vp from unit PIX-4 was reported as unit PIX-5

vps0 %>% group_by(LIT, unitName, subunitName) %>% count() %>% print(n = Inf)

cps0 %>% group_by(LIT, unitName, subunitName) %>% count() %>% print(n = Inf)

qdt0 %>% group_by(LIT, unitName, subunitName) %>% count() %>% print(n = Inf)


## Uniqueness of seed head identity ===========================================

qdt0 %>% # several seed head id's are repeated. Why?
  dplyr::filter(vernacularName == "Swamp_Timothy" & stratum != "") %>%
  group_by(GlobalID, GlobalID_seed, measurementType) %>%
  count() %>%
  arrange(GlobalID, GlobalID_seed, measurementType) %>%
  print(n = Inf)

qdt0 %>% # several seed head id's are repeated with the same meas. type. Why?
  dplyr::filter(vernacularName == "Watergrass" & stratum != "") %>%
  group_by(GlobalID, GlobalID_seed, measurementType) %>%
  count() %>%
  arrange(GlobalID, GlobalID_seed, measurementType) %>%
  print(n = Inf)

qdt0 %>% # several seed head id's are repeated with the same meas. type. Why?
  dplyr::filter(vernacularName == "Watergrass" & stratum != "") %>%
  group_by(GlobalID_seed, measurementType) %>%
  count() %>%
  arrange(GlobalID_seed, measurementType) %>%
  print(n = Inf)

# Seed head ID's seem to be incorrect.
# GlobalID_seed should not have any duplicates in Swamp Timothy.
# GlobalID_seed should have exactly 2 duplicates in Watergrass, one for each
# measurement type.
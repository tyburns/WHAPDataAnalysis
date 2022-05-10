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
# 8 May 2022: SCHEMATIC NEEDS TO BE UPDATED and moved to main Rmd
#
#               |--------------------------------------|
#               |WHAP_yyyy_VantagePolygons_yyyymmdd.csv|
# survey-123 -> |WHAP_yyyy_CirclePlots_yyyymmdd.csv    |
#               |WHAP_yyyy_Quadrats_yyyymmdd.csv       |
#               |WHAP_yyyy_ManagementUnits_yyyymmdd.csv|
#               |--------------------------------------|
#                                      V
#                                      V
#                                      V
#                            WHAP2_DataChecks.Rmd
#                                       V
#                                       V
#                                       V
#                                       V
#                               Validated field files
#                                       V
#                                       V
#                                      \/                       |----------|
#                                |-----------------|            | vpcpyyyy |
#                                |WHAP2_DataPrep.R |  -> ->  -> | qdt_yyyy |
#                           _ -> |  (This file)    |            |----------|
#                         ^      |-----------------|                 V
#                       ^                                            V
#          d2m_models_list.rds                                      \/
#                /\                                       WHAP2_sim_stats.Rmd
#               ^                                                  /\
#     d2m_modeling.R                                             ^  ^
#          /\                                                  ^     ^
#         ^                                                  ^        ^
#   d2m_data.rds                                           ^           ^
# d2m_data_yyyy.csv                                     ^       SimPropAreaFun
#                                            SimMassBySuLITFun
#                                                   /\
#                                                   ^
#                                         SimMassPerAreaFun
#
#

# Setup ======

require(tidyverse, quietly = TRUE)
require(car, quietly = TRUE)
require(measurements, quietly = TRUE)
require(lubridate, quietly = TRUE)
require(mgcv, quietly = TRUE)

# setwd("WHAP2_SimBasedEstimates")


# Define function to invert the Box-Cox transformation =======
invBoxCox <- function(x, lambda) {
  if (lambda == 0) exp(x) else (lambda * x + 1)^(1 / lambda)
}


# Read d2m models ================

# d2m_models_list comes from d2m_modeling.R and it contains the user input
# determining what species are valid, i.e., have models to use in this script
# d2m data files should follow a name convention to be found.
# d2m models has a column with species vernacularName:
# "Swamp_Timothy", "Watergrass", "Smartweed", etc.

d2m_models_list <- read_rds("./d2mFiles/d2m_models_list.rds")

# This nested tibble must have elements named "Watergrass", "Swamp_Timothy",
# "Smartweed", etc.

# Data are assumed to have passed WHAP_DataChecks.Rmd
# Species, taxon_ID's and subunits are already validated.


# Determine what years need data preparation =====
# Years without all inputs and with all proc files are processed
input_files <- list.files(path = "./inputFiles/") %>%
  grep("qdt_|vpcp|\\d{4}.rds", ., value = TRUE)

proc_file_regx <- "WHAP_\\d{4}_(VantagePolygons_|CirclePlots_|Quadrats_|ManagementUnits_)\\d{8}.csv"

pff_path <- "./ProcessedFieldData/"

proc_files <- list.files(path = pff_path) %>%
  grep(proc_file_regx, ., value = TRUE)

ftype_regx <- "qdt|vpcp|VantagePolygons|CirclePlots|Quadrats|ManagementUnits"

file_tbl <- c(
  input_files,
  proc_files
) %>%
  as_tibble() %>%
  mutate(
    ftype = str_extract(
      value,
      ftype_regx
    ),
    year = str_extract(
      value,
      "(\\d{4})"
    )
  ) %>%
  group_by(year) %>%
  summarize(
    has_all_inputs = all(
      c("qdt", "vpcp") %in%
        unique(ftype)
    ),
    has_proc_files = all(
      c(
        "VantagePolygons",
        "CirclePlots",
        "Quadrats",
        "ManagementUnits"
      ) %in%
        unique(ftype)
    ),
    incomplete_proc_files = !has_all_inputs & !has_proc_files
  )

# Alert user when needed processed files are incomplete

if (any(file_tbl$incomplete_proc_files)) {
  cat("Years with incomplete processed field files needed:")
  print(file_tbl$year[file_tbl$incomplete_proc_files])
}

# Determine which years have incomplete inputFiles and complete
# ProcessedFieldData. Run DataPrep on those.

years2prep <- file_tbl %>%
  dplyr::filter(
    has_all_inputs == FALSE &
      has_proc_files == TRUE
  ) %>%
  pull(year) %>%
  as.numeric()

# Loop preparation of file sets over years2prep =======

for (the_year in years2prep) {

  # Read data files =====

  # Only one year at a time is allowed in this code

  file_names <- paste0( # necessary??
    pff_path,
    list.files(path = pff_path)
  )

  vps_path <- paste0(
    pff_path,
    "WHAP_",
    the_year,
    "_VantagePolygons_",
    the_year,
    "\\d{4}.csv"
  ) %>%
    grep(file_names,
      value = TRUE
    )

  cps_path <- paste0(
    pff_path,
    "WHAP_",
    the_year,
    "_CirclePlots_",
    the_year,
    "\\d{4}.csv"
  ) %>%
    grep(file_names,
      value = TRUE
    )

  qdt_path <- paste0(
    pff_path,
    "WHAP_",
    the_year,
    "_Quadrats_",
    the_year,
    "\\d{4}.csv"
  ) %>%
    grep(file_names,
      value = TRUE
    )

  mus_path <- paste0(
    pff_path,
    "WHAP_",
    the_year,
    "_ManagementUnits_",
    the_year,
    "\\d{4}.csv"
  ) %>%
    grep(file_names,
      value = TRUE
    )

  vps0 <- read_csv(
    vps_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      eventDate = as.Date(eventDate,
        tryFormats = c("%m/%d/%Y")
      ),
      taxonID = as.character(taxonID),
      LIT = factor(LIT)
    )


  cps0 <- read_csv(
    cps_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      eventDate = as.Date(eventDate,
        tryFormats = c("%m/%d/%Y")
      ),
      taxonID = as.character(taxonID),
      LIT = factor(LIT)
    )


  qdt0 <- read_csv(
    qdt_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      eventDate = as.Date(eventDate,
        tryFormats = c("%m/%d/%Y")
      ),
      taxonID = as.character(taxonID),
      LIT = factor(LIT)
    )


  mus0 <- read_csv(
    mus_path,
    show_col_types = FALSE
  ) %>%
    mutate(LIT = factor(LIT))


  # Fix vernacular names =====

  vps0 <- vps0 %>%
    mutate(
      vernacularName =
        str_replace(
          vernacularName,
          " ",
          "_"
        )
    )

  cps0 <- cps0 %>%
    mutate(
      vernacularName =
        str_replace(
          vernacularName,
          " ",
          "_"
        )
    )
  qdt0 <- qdt0 %>%
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
          (subunitName == "" | is.na(subunitName)),
          unitName,
          subunitName
        )
    )

  vps0 <- vps0 %>%
    mutate(
      subunitName =
        ifelse(
          (subunitName == "" | is.na(subunitName)),
          unitName,
          subunitName
        )
    )

  cps0 <- cps0 %>%
    mutate(
      subunitName =
        ifelse(
          subunitName %in% c("0", "", NA),
          unitName,
          subunitName
        )
    )

  qdt0 <- qdt0 %>%
    mutate(
      subunitName =
        ifelse(
          (subunitName == "" | is.na(subunitName)),
          unitName,
          subunitName
        )
    )


  # Get list of valid species and eliminate others from data =======

  spp_valid <- d2m_models_list %>%
    pull(vernacularName) %>%
    c("Other_cover")

  vps0 <- vps0 %>%
    dplyr::filter(
      vernacularName %in% spp_valid
    )

  cps0 <- cps0 %>%
    dplyr::filter(
      vernacularName %in% spp_valid
    )

  qdt0 <- qdt0 %>%
    dplyr::filter(
      vernacularName %in% spp_valid
    )


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


  # Create vps from vps0. Vantage polygons. =======================

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
  # inputFiles refer to the fact that the files are the input for the simulations

  path_out <- "./inputFiles/"

  out_vpcp_name <- paste0(
    path_out,
    "vpcp",
    the_year,
    ".rds"
  )

  write_rds(vpcp, out_vpcp_name)


  # Process quadrat information ====================

  # Predict mass for each sh using d2m_model_list and sh dimensions.
  # Average sh mass per quadrat, multiply by sh density

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
      vernacularName,
      sh_length_mm,
      f2t_length_mm,
      sh_width_mm
    ) %>%
    mutate(
      emerged = f2t_length_mm / sh_length_mm,
      LIT_group = factor(
        ifelse(
          LIT == "MDC",
          "MDC",
          "not_MDC"
        )
      )
    ) %>%
    group_by(vernacularName) %>%
    nest() %>%
    left_join({
      d2m_models_list %>%
        dplyr::select(-data)
    }) %>%
    dplyr::mutate(
      sh_mass_mg = pmap(
        .l = list(
          ..1 = models,
          ..2 = data,
          ..3 = lambdas,
          ..4 = max_sh_length_mm
        ),
        .f = ~ ifelse(
          ..2$sh_length_mm > ..4,
          NA,
          invBoxCox(
            predict(
              object = ..1,
              newdata = ..2
            ),
            lambda = ..3
          )
        )
      )
    ) %>%
    unnest(cols = c(data, sh_mass_mg)) %>%
    mutate(
      qdt_m2 = ifelse(
        quadratSize == "15x15cm",
        0.15^2,
        ifelse(quadratSize == "5x5cm",
          0.05^2,
          0.10
        )
      ),
      n_seed_head_m2 = nSeedHeads / qdt_m2
    ) %>%
    group_by(
      GlobalID,
      LIT,
      unitName,
      subunitName,
      vernacularName,
      stratum,
      quadratSize,
      managementAction
    ) %>%
    summarize(
      sh_mass_mg = mean(sh_mass_mg, na.rm = TRUE),
      n_seed_head_m2 = mean(n_seed_head_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mass_g_m2 = n_seed_head_m2 * sh_mass_mg / 1000,
      sh_mass_mg = ifelse(is.nan(sh_mass_mg), NA, sh_mass_mg)
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
    dplyr:::select(
      LIT,
      subunit_ID,
      stratum,
      LIT_Strat,
      vernacularName,
      n_seed_head_m2,
      sh_mass_mg,
      mass_g_m2,
      managementAction
    ) %>%
    ungroup()


  # Save qdt in inputFiles folder as qdt_yyyy.rds

  qdt_file_path <- paste0("./inputFiles/qdt_", the_year, ".rds")

  write_rds(qdt, qdt_file_path)
}

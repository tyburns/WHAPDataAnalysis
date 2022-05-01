# MODELS TO PREDICT SEED HEAD MASS FROM DIMENSIONS

# Setup =============

require(sets, quietly = TRUE) # load first to avoid %>% conflicts
require(car, quietly = TRUE)
require(mgcv, quietly = TRUE)
require(tidyverse, quietly = TRUE)


## Define function to create model formulas and fit models ======

model_sh_mass <- function(.xdata) {
  p_NA <- .xdata %>%
    dplyr::select(-c(
      LIT,
      year,
      mass_mg,
      LIT_group
    )) %>%
    map(.f = ~ sum(is.na(.x)) / length(.x))

  predictors <- names(which(p_NA < 0.1))

  n_LITg_gt_1 <- length(unique(.xdata$LIT_group)) > 1
  
  rhs <- set_power(predictors) %>%
    lapply(as.character) %>%
    `[`(-1) %>%
    map_chr(~ paste(.x, collapse = ", ")) %>%
    map_chr(~ paste("s(", .x, ")", sep = "")) %>%
    paste(collapse = " + ") %>%
    str_replace(
      ., "s\\(sh_length_mm\\)",
      "s\\(sh_length_mm, k = 4\\)"
    ) %>%
    {
      if (n_LITg_gt_1) {
        str_replace(
          ., "s\\(sh_length_mm, k = 4\\)",
          "s\\(sh_length_mm, by = LIT_group, k = 4\\)"
        )
      } else {
        .
      }
    }
  
  frmla1 <- paste("mass_mg ~ ",
                  rhs,
                  sep = ""
  ) %>%
    as.formula()

  lambda <- powerTransform(
    gam(
      formula = frmla1,
      data = .xdata
    )
  ) %>%
    coef(round = TRUE) %>%
    unname()

  frmla2 <- paste("bcPower(mass_mg, lambda) ~ ",
    rhs,
    sep = ""
  ) %>%
    as.formula()

  model <- gam(
    formula = frmla2,
    data = .xdata,
    select = TRUE
  )

  return(
    list(
      lambda = lambda,
      model = model
    )
  )
}


# Read seed head dimensions and mass data ================
# These data are a user input; a file that has to be provided by the user.
# d2m_data.rds already exists and contains all data collected by May 2022.
# Additional data, if any are available, should be provided in a file named
# d2m_data_yyyy.csv with one row per seed head and the following columns:
#
# LIT
# year
# vernacularName
# f2t_length_mm
# sh_length_mm
# sh_width_mm

# vernacularName has to be the same as in field data, but with _ instead of spaces.
# All added d2m files post 2022 should remain in the folder for future use.

# Set paths and get file names ======

folder <- "./d2mFiles/"

csv_files <- list.files(path = folder) %>%
  grep("d2m_data_\\d{4}.csv", ., value = TRUE) %>%
  str_remove(".csv")

d2m_models_exist <- list.files(path = folder) %>%
  grepl("d2m_models_list.rds", ., ) %>%
  any()


# Determine if it is necessary to recreate d2m models ==========
# This section should be moved to the controlling script

files_processed <- read_rds(
  paste0(
    folder,
    "files_already_processed.rds"
  )
)

need2run_d2m <- !(d2m_models_exist &
  setequal(csv_files, files_processed))

if (need2run_d2m) {

  # Read original and additional data files ==============

  d2m_data <- read_rds(
    paste0(
      folder,
      "d2m_data.rds"
    )
  )

  for (fname in csv_files) {
    d2m_data <- bind_rows(
      d2m_data,
      read_csv(
        paste0(
          folder,
          fname,
          ".csv"
        ),
        show_col_types = FALSE
      )
    )
  }

  d2m_data <- d2m_data %>%
    mutate(
      emerged = f2t_length_mm / sh_length_mm,
      LIT_group = factor(
        ifelse(
          LIT == "MDC",
          "MDC",
          "not_MDC"
        )
      )
    )


  # Fit models by vernacularName ==================

  # Pseudocode for modeling
  # Group data by species and nest.
  # Map over data column in nested tibble:
  # Identify columns that have sh dimensions data for each species.
  # Create a gam formula with those predictors including interactions.
  # Fit the gam model
  # Save nested tibble with models


  ## Create column with models ======

  d2m_models_list <- d2m_data %>%
    group_by(vernacularName) %>%
    nest() %>%
    mutate(
      models = map(
        data,
        ~ model_sh_mass(.xdata = .x)
      )
    ) %>%
    mutate(
      lambdas = map(models, ~ .x[[1]]),
      models = map(models, ~ .x[[2]]),
      max_sh_length_mm = map(data, ~ max(.x$sh_length_mm, na.rm = TRUE))
    )

  write_rds(
    d2m_models_list,
    paste0(
      folder,
      "d2m_models_list.rds"
    )
  )


  write_rds(
    csv_files,
    paste0(
      folder,
      "files_already_processed.rds"
    )
  )
} else {
  print("d2m models already exists. To force recalculation, remove d2m_models_list.rds from the d2mFiles folder.")
}

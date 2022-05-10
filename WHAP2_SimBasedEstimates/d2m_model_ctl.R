#  CONTROL THE CREATION OR RECREATION OF D2M MODELS
# Read seed head dimensions and mass data ================
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
# 
# The files should be in the folder "./d2mFiles/"

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

files_processed <- read_rds(
  paste0(
    folder,
    "files_already_processed.rds"
  )
)

need2run_d2m <- !(d2m_models_exist &
                    setequal(csv_files, files_processed))

# Recreate models or issue message that models are already present ======

if (need2run_d2m) {
  
  ## Read original and additional data files ==============
  
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
  
  
  ## Fit models by vernacularName ==================
  
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
        ~ create_d2m_model(.xdata = .x)
      )
    ) %>%
    mutate(
      lambdas = map(models, ~ .x[[1]]),
      models = map(models, ~ .x[[2]]),
      max_sh_length_mm = map(data, ~ max(.x$sh_length_mm, na.rm = TRUE))
    )
  
  ## Save models object with a column for each vernacularName =====
  write_rds(
    d2m_models_list,
    paste0(
      folder,
      "d2m_models_list.rds"
    )
  )
  
  ## Save a list of the d2m files already processed ======
  
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


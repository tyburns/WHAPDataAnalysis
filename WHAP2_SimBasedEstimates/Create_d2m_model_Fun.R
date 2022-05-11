# MODELS TO PREDICT SEED HEAD MASS FROM DIMENSIONS
# 
# This function's argument is a data frame with the following columns and
# structure.
# 
# > str(d2m_data)
# 'data.frame':	1729 obs. of  8 variables:
# $ LIT           : Factor w/ 6 levels "CLS","KRN","MDC",..: 1 1 1  1 1 ...
# $ year          : num  2019 2019 2019 2019 2019 ...
# $ vernacularName: chr  "Swamp_Timothy" "Swamp_Timothy" "Swamp_Timothy" ...
# $ f2t_length_mm : num  NA NA NA NA NA NA NA NA NA NA ...
# $ sh_length_mm  : num  14.78 10.93 15.41 15.93 9.96 ...
# $ sh_width_mm   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ mass_mg       : num  21.2 19 28.2 53.7 18.1 ...
# $ emerged       : num  NA NA NA NA NA NA NA NA NA NA ...
# 
# > d2m_data
# LIT year vernacularName f2t_length_mm sh_length_mm sh_width_mm    mass_mg   emerged
# 1   CLS 2019  Swamp_Timothy         NA    14.784011       NA    21.198502        NA
# 2   CLS 2019  Swamp_Timothy         NA    10.932480       NA    18.974359        NA
# 3   CLS 2019  Swamp_Timothy         NA    15.410755       NA    28.221154        NA
# 4   CLS 2019  Swamp_Timothy         NA    15.933893       NA    53.733333        NA
# 5   CLS 2019  Swamp_Timothy         NA     9.960213       NA    18.089888        NA
# 6   CLS 2019  Swamp_Timothy         NA    16.227950       NA    61.287129        NA
# 


# Setup =============
# 
# require(sets, quietly = TRUE) # load first to avoid %>% conflicts
# require(car, quietly = TRUE)
# require(mgcv, quietly = TRUE)
# require(tidyverse, quietly = TRUE)


## Define function to create model formulas and fit models ======

create_d2m_model <- function(.xdata) {
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
  
  rhs <- sets::set_power(predictors) %>%
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

## FUNCTION TO SIMULATE MASS PER UNIT AREA
##
## Arguments ======
##
# This function takes two arguments: the .qdt_path to a qdt_yyyy.rds file containing
# the parsed information about mass per quadrat in yyyy, and the number of
# simulations desired, .nsim.
# This file has to have the following structure:

# tibble [526 × 6] (S3: tbl_df/tbl/data.frame)
# $ LIT       : chr [1:526] "KRN" "KRN" "KRN" "KRN" ...
# $ subunit_ID: chr [1:526] "KRN_4A_4A-1" "KRN_4A_4A-1" "KRN_4A_4A-1" ...
# $ stratum   : Factor w/ 3 levels "a.Low","b.Med",..: 1 1 1 2 3 3 1 2 2 2 ...
# $ LIT_Strat : Factor w/ 12 levels "KRN_a.Low","KRN_b.Med",..: 1 1 1 2 3 3 ...
# $ mass_g_m2 : num [1:526] 68.9 23.5 76.7 56 506.1 ...
# $ species   : Factor w/ 3 levels "Swamp_Timothy",..: 1 1 1 1 1 1 1 1 1 1 ...
#

## Value =====
# The object returned is a list of tibbles with as many elements as there are
# refuges in qdt_yyyy.rds Each tibble has nsim rows with 9 columns. Each row is a random simulation of proportion of area in each of the nine species-stratum
# combinations, as shown below for n = 4000.


# List of 4
# $ KRN: tibble [4,000 × 9] (S3: tbl_df/tbl/data.frame)
# ..$ Smartweed_a.Low     : num [1:4000] 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Smartweed_b.Med     : num [1:4000] 35.08 55.36 2.2 5 6.97 ...
# ..$ Smartweed_c.High    : num [1:4000] 91.9 29.3 42.3 53.1 162.3 ...
# ..$ Swamp_Timothy_a.Low : num [1:4000] 23.6 38.6 28.4 20.8 19.7 ...
# ..$ Swamp_Timothy_b.Med : num [1:4000] 114 96 96.7 98.6 109.2 ...
# ..$ Swamp_Timothy_c.High: num [1:4000] 335 372 345 362 347 ...
# ..$ Watergrass_a.Low    : num [1:4000] 8.71 10.38 4.66 7.46 4.57 ...
# ..$ Watergrass_b.Med    : num [1:4000] 17.85 7.03 21.73 17 3.15 ...
# ..$ Watergrass_c.High   : num [1:4000] 7 1.81 19.82 15.04 8.29 ...
# $ MDC: tibble [4,000 × 9] (S3: tbl_df/tbl/data.frame)
# ..$ Smartweed_a.Low     : num [1:4000] 7.08 10.55 11.71 12.25 10.44 ...
# ..$ Smartweed_b.Med     : num [1:4000] 74.1 63 58.9 61 66.1 ...
# ..$ Smartweed_c.High    : num [1:4000] 279 248 382 249 355 ...
# ..$ Swamp_Timothy_a.Low : num [1:4000] 11.7 15.6 16.2 17.7 21.2 ...
# ..$ Swamp_Timothy_b.Med : num [1:4000] 68.2 33.9 96.6 51.3 43.2 ...
# ..$ Swamp_Timothy_c.High: num [1:4000] 285 254 361 369 289 ...
# ..$ Watergrass_a.Low    : num [1:4000] 3.06 4.14 3.83 3.42 4.57 ...
# ..$ Watergrass_b.Med    : num [1:4000] 24.9 14.7 21.9 17.1 34.3 ...
# ..$ Watergrass_c.High   : num [1:4000] 4.12 21.57 15.24 6.32 7.46 ...
# $ PIX: tibble [4,000 × 9] (S3: tbl_df/tbl/data.frame)
# ..$ Smartweed_a.Low     : num [1:4000] 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Smartweed_b.Med     : num [1:4000] 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Smartweed_c.High    : num [1:4000] 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Swamp_Timothy_a.Low : num [1:4000] 25.3 27.9 28.9 22.7 27.5 ...
# ..$ Swamp_Timothy_b.Med : num [1:4000] 108 197 121 221 114 ...
# ..$ Swamp_Timothy_c.High: num [1:4000] 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Watergrass_a.Low    : num [1:4000] 10.6 24.1 12.1 18.3 10.8 ...
# ..$ Watergrass_b.Med    : num [1:4000] 56.6 30.3 28.2 38.5 34.1 ...
# ..$ Watergrass_c.High   : num [1:4000] 86.4 87.6 107.9 81.4 90.4 ...
# $ SLW: tibble [4,000 × 9] (S3: tbl_df/tbl/data.frame)
# ..$ Smartweed_a.Low     : num [1:4000] 6.68 5.18 3.77 2.89 5.05 ...
# ..$ Smartweed_b.Med     : num [1:4000] 42.7 26.4 66.6 32.4 27.6 ...
# ..$ Smartweed_c.High    : num [1:4000] 186.7 68.7 130.5 70.6 103.3 ...
# ..$ Swamp_Timothy_a.Low : num [1:4000] 11.6 12.38 13.98 12.56 8.19 ...
# ..$ Swamp_Timothy_b.Med : num [1:4000] 58.5 33 57.4 83.9 88.7 ...
# ..$ Swamp_Timothy_c.High: num [1:4000] 513 116 362 313 358 ...
# ..$ Watergrass_a.Low    : num [1:4000] 5.46 3.65 4.27 6.12 8.54 ...
# ..$ Watergrass_b.Med    : num [1:4000] 19.38 13.35 7.09 8.57 18.37 ...
# ..$ Watergrass_c.High   : num [1:4000] 235 195 477 275 309 ...


# Simulations Outline =====

# Mass per unit area for each species in each stratum and refuge are
# simulated using a model fitted to the .qdt_path file. Estimated model
# parameters are the estimates of interest (seed head mass index per unit area
# for each LIT:vernacularName:stratum combination). It is assumed that the estimated
# parameters have a multivariate normal distribution and simulations are obtained directly from mvrnorm() using the estimated parameters and their estimated
# vcov() matrix.

## Pseudocode =====

# 1. Get qdt_yyyy file
# 2. Model mass_g_m2 as an lm() of LIT:vernacularName:stratum
# 3. Get estimates and vcov
# 4. Simulate 4000 realizations of estimates and back-transform.

## Function definition =====

sim_mass_area <- function(.qdt_path, .nsim = 1000) {

  ## Define function to invert the Box-Cox transformation =====

  invBoxCox <- function(x, lambda) {
    if (lambda == 0) exp(x) else (lambda * x + 1)^(1 / lambda)
  }

  ## Set up packages needed =====

  # require(MASS, quietly = TRUE)
  # require(car, quietly = TRUE)
  # require(tidyverse, quietly = TRUE)

  ## Read and wrangle data =====

  qdt <- read_rds(.qdt_path)

  # Model mass per area by LIT:vernacularName:stratum =====

  # Get lambda for st
  mass_lambda <- powerTransform(
    lm(mass_g_m2 ~
    -1 + LIT:vernacularName:stratum,
    data = qdt
    )
  ) %>%
    coef(round = TRUE) %>%
    unname()

  # Use linear model
  mass_m <- lm(
    bcPower(mass_g_m2, lambda = mass_lambda) ~
    -1 + LIT:vernacularName:stratum,
    na.action = na.exclude,
    data = qdt
  )


  ## Get covariance matrix of estimates =====

  {
    vcov_mass <- vcov(mass_m)

    colnames(vcov_mass) <-
      rownames(vcov_mass) <-
      gsub(
        "LIT|vernacularName|stratum",
        "",
        colnames(vcov_mass)
      )

    vcov_mass[is.na(vcov_mass)] <- 0.00001
  }

  # Covariance is a diagonal.
  sum(round(vcov_mass, 4)) == sum(diag(round(vcov_mass, 4)))

  ## Make vector of means for LIT:vernacularName:stratum (Lss) =====

  {
    mass_Lss <- coef(mass_m)

    names(mass_Lss) <- gsub(
      "LIT|vernacularName|stratum",
      "",
      names(mass_Lss)
    )

    ## Some combinations may not be present, so there may be NA's that
    ## are transformed into practical 0's
    mass_Lss[is.na(mass_Lss)] <- -5 # in BoxCox scale ~ 0 in mass/area
  }

  # Simulations have to be done in the Box-Cox transformed dimension
  # and then results need to be back-transformed

  ## Simulate from coef distribution and back-transform =====

  sim_massLss <- invBoxCox(
    mvrnorm(
      n = .nsim,
      mu = mass_Lss,
      Sigma = vcov_mass
    ),
    lambda = mass_lambda
  )


  sim_massLss_lst <- unique(qdt$LIT) %>%
    as.character() %>%
    as.list() %>%
    set_names(unique(qdt$LIT)) %>%
    map(.f = ~ (sim_massLss %>%
      as_tibble() %>%
      dplyr::select(contains(.x)))) %>%
    map(~ rename_with(.x, .f = function(z) gsub("^[A-Z]{3}:", "", z))) %>%
    map(~ rename_with(.x, .f = function(z) gsub(":", "_", z))) %>%
    # This part has to set column order as in SimPropAreaFun.R
    map(~ dplyr::select(
      .x,
      sort(
        names(.x)
      )
    ))
  
  
  return(sim_massLss_lst)
}

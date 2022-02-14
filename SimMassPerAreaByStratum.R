# Setup =====
library(compositions)
library(tidyverse)
library(magrittr)
library(Matrix)
library(psych)
library(corrplot)
library(tidymodels)
library(calculus)
library(MASS)
library(tictoc)
library(styler)
library(car)

# Simulations Outline =====

# Mass per unit area in for each species in each stratum and refuge are
# simulated using the qdt_spp files. Simulations are later joined with area
# simulations to create simulated mass per unit area for each subunit.

## Pseudocode =====

# 1. Get qdt_yyyy file
# 2. Model mass_g_m2 as a function of LIT:species:stratum
# 3. Get estimates and vcov
# 4.Simulate 4000 realizations of estimates and back-transform as necessary.
# Then:
#  Combine with quadrat simulations to get simulations of mass/area

# 

# Data for 2021 =====

qdt_2021 <- read_rds("WHAP2021-22/Output2021/qdt_2021.rds")

# Model mass per area by LIT:species:stratum
mass_m2021 <- lm(mass_g_m2 ~ -1 + LIT:species:stratum,
  data = qdt_2021
)

summary(mass_m2021)

# Heterogeneity of variance and lack of normality
opar <- par(mfrow = c(2,2))
plot(mass_m2021)
par(opar)

# Define function to invert the Box-Cox transformation
invBoxCox <- function(x, lambda) {
  if (lambda == 0) exp(x) else (lambda * x + 1)^(1 / lambda)
}

# Get lambda for st
mass_lambda_2021 <- powerTransform(
  lm(mass_g_m2 ~
  -1 + LIT:species:stratum,
  data = qdt_2021
  )
) %>%
  coef(round = TRUE) %>%
  unname()

# Use linear model
mass_m2021b <- lm(
  bcPower(mass_g_m2, lambda = mass_lambda_2021) ~
  -1 + LIT:species:stratum,
  na.action = na.exclude,
  data = qdt_2021
)

opar <- par(mfrow = c(2,2))
plot(mass_m2021b)
par(opar)

### Get covariance matrix of estimates =====

{
  vcov_mass_2021 <- vcov(mass_m2021b)
  
  colnames(vcov_mass_2021) <-
    rownames(vcov_mass_2021) <-
    gsub(
      "LIT|species|stratum",
      "",
      colnames(vcov_mass_2021)
    )
  
  vcov_mass_2021[is.na(vcov_mass_2021)] <- 0.001
}

# Covariance is a diagonal.
sum(round(vcov_mass_2021, 4)) == sum(diag(round(vcov_mass_2021, 4)))

## Make vector of means for LIT:species:stratum (Lss) =====

{
  mass_Lss_2021 <- coef(mass_m2021b)
  
  names(mass_Lss_2021) <- gsub(
    "LIT|species|stratum",
    "",
    names(mass_Lss_2021)
  )
  
  mass_Lss_2021[is.na(mass_Lss_2021)] <- -5 # in BoxCox scale ~ 0 i mass/area
}

# Simulations have to be done in the transformed dimension
# and then results need to be back-transformed


## Simulate from coef distribution and back-transform =====

sim_massLss_2021 <- invBoxCox(
  mvrnorm(
    n = 4000,
    mu = mass_Lss_2021,
    Sigma = vcov_mass_2021
  ),
  lambda = mass_lambda_2021
)

sim_massLss_lst_2021 <- unique(qdt_2021$LIT) %>%
  as.list() %>%
  set_names(unique(qdt_2021$LIT)) %>%
  map(.f = ~sim_massLss_2021 %>%
    as_tibble() %>%
    dplyr::select(contains(.x))) %>%
  map(~rename_with(.x, .f = function(z) gsub("^[A-Z]{2,4}:", "", z))) %>%
  map(~rename_with(.x, .f = function(z) gsub(":", "_", z)))

str(sim_massLss_lst_2021)

# Summary of simulations of mass per area by LIT spp strat

list_sp_strat_summary <- map(sim_massLss_lst_2021, ~round(colMeans(.x), 0)) %>%
  bind_rows() %>%
  mutate(LIT = names(sim_massLss_lst_2021))



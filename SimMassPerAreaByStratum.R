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
mass_lambda <- powerTransform(
  lm(mass_g_m2 ~
  -1 + LIT:species:stratum,
  data = qdt_2021
  )
) %>%
  coef(round = TRUE) %>%
  unname()

# Use linear model
mass_m2021b <- lm(
  bcPower(mass_g_m2, lambda = mass_lambda) ~
  -1 + LIT:species:stratum,
  na.action = na.exclude,
  data = qdt_2021
)

opar <- par(mfrow = c(2,2))
plot(mass_m2021b)
par(opar)

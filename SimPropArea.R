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

# CALCULATIONS ASSUME THAT THERE ARE NO OVERLAP OF SPECIES INTHE ESTIMATES.
# WHEN SPECIES OVERLAP IN THE FIELD, OBSERVERS HAVE TO "MENTALLY" SEPARATE
# SPECIES INTO DIFFERENT AREAS AND CORRECT DENSITY ACCORDINGLY.
# THE OVERLAPPING SPECIES ISSUE IS OUTSTANDING.

# Areal compositions will be simulated for each year separately.
# Each subunit_ID will receive multiple simulations of composition.
# Each composition is a vector of 10 numbers: 3 strata * 3 species + other.
# Each set of estimates or simulation has one row per subunit_ID and one
# column per component.
#
## Pseudocode =====
# 1. Get vpcp file for current year.
# 2. Change zeroes to abs(rnorm(mean = 0, sd = 0.0001)).
# 3. Create acomp() object c_Area with all components.
# 4. Regress ilr(c_Area) against subunit_ID without intercept as ilr_model.
# 5. Get vcov1 from ilr_model and transform into cor.
#   Remember that dim(vcov) = n.subunits * (n.components - 1)
# 6. Calculate fpcf for each subunit and multiply by vcov1 diagonal.
# 7. Calculate corrected vcov where variances are multiplied by fpcf.
# 8. Create mvnorm simulations of estimates using corrected vcov.
# 9. Backstransform with ilrInv.
# Then:
#  Combine with quadrat simulations to get simulations of mass/area


## Data for 2021 =====

vpcp2021 <- read_rds("WHAP2021-22/Output2021/vpcp2021.rds") %>%
  as_tibble() %>%
  dplyr::select(
    LIT,
    subunit_ID,
    level,
    areaVisible_ac,
    su.area_ac,
    starts_with("p_")
  ) %>%
  group_by(LIT, subunit_ID) %>%
  mutate(
    t_area_vis = sum(areaVisible_ac),
    fpcf = 1 - t_area_vis / su.area_ac,
    fpcf = ifelse(fpcf <= 0.00987654321, 0.00987654321, fpcf)
  ) %>%
  ungroup()

acomp_2021 <- vpcp2021 %>%
  dplyr::select(starts_with("p_")) %>%
  as.matrix() %>%
  `+`(replicate(10, abs(rnorm(dim(vpcp2021)[1],
    mean = 0,
    sd = 0.0001
  )))) %>%
  acomp()

vpcp2021$a_comp <- acomp_2021

### Regress acomp() on subunit_ID

ilr_m2021 <- lm(ilr(a_comp) ~ -1 + subunit_ID,
  data = vpcp2021
)

ilr_m2021_smry <- tidy(ilr_m2021) %>%
  mutate(
    subunit_ID = gsub("subunit_ID", "", term),
    subunit_ID_Y = paste(subunit_ID,
      response,
      sep = "_"
    )
  ) %>%
  dplyr::select(-c(statistic, p.value, term))


### Get covariance matrix of estimates and apply fpcf =====

# Make vector of finite population correction factors.

fpcf_2021 <- vpcp2021 %>%
  dplyr::select(subunit_ID, fpcf) %>%
  unique() %>%
  deframe()

# Covariance matrix refers to coefficients for all subunits for each of the 10-1
# components as a single vector.
# All covariances between different units are zero. Only covariances between
# components within units are nonzero.

vcov_ilr <- vcovAcomp(ilr_m2021)
vcov_names <- gsub("subunit_ID", "", attr(vcov_ilr, "dimnames")[[3]])
attr(vcov_ilr, "dimnames") <- list(NULL, NULL, vcov_names, vcov_names)

vcov_list <- list()
for (i in vcov_names) {
  vcov_list[[i]] <- fpcf_2021[i] * vcov_ilr[, , i, i]
}

### Make df of mean vectors

mean_df <- ilr_m2021_smry %>%
  dplyr::select(
    response,
    estimate,
    subunit_ID
  ) %>%
  pivot_wider(
    names_from = subunit_ID,
    values_from = estimate
  ) %>%
  column_to_rownames(var = "response")

### Simulate from distribution of estimated compositions
tic()
if (names(mean_df) == names(vcov_list)) {
  sim_list <- map2(
    .x = mean_df,
    .y = vcov_list,
    .f = ~ ilrInv(
      mvrnorm(
        n = 4000,
        mu = .x,
        Sigma = .y
      )
    )
  )
}
toc()

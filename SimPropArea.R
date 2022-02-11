# Setup =====
library(tidyverse)
library(car)
library(magrittr)
library(Matrix)
library(psych)
library(corrplot)
library(tidymodels)
library(calculus)
library(MASS)
library(tictoc)
library(styler)
library(lme4)
library(DirichletReg)
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
# 3. Use Dirichlet regression to model proportions in each subunit.
# 4. Get means and vcov in log(alpha) scale into parallel lists
# 5. Apply fpcf to vcov and Simulate assuming mv normality
# 6. Backstransform with exp().
# Then (in SimMassPerAreaBySubunit.R):
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
    wt = areaVisible_ac / t_area_vis,
    fpcf = 1 - t_area_vis / su.area_ac,
    fpcf = ifelse(fpcf <= 0.01, 0.01, fpcf)
  ) %>%
  ungroup()


## Function to go from log(alpha) to proportions =====

log_alpha2p <- function(x) {
  a_i <- exp(x)
  a_0 <- if (is.vector(a_i)) sum(a_i) else rowSums(a_i)
  return(a_i / a_0)
}


## Make vector of finite population correction factors =====

fpcf_2021 <- vpcp2021 %>%
  dplyr::select(subunit_ID, fpcf) %>%
  unique() %>%
  deframe()

## Dirichlet Regression of p_area on subunit_ID =====

dr_data <- vpcp2021 %>%
  dplyr::select(starts_with("p_")) %>%
  DR_data()

# Use common model option of DirichReg
dr_m2021 <- DirichReg(
  dr_data ~ -1 + subunit_ID,
    weights = areaVisible_ac,
  data = vpcp2021
)

# Diagnostic of residuals

{
  opar <- par(mfrow = c(3, 3))
  for (i in 2:10) {
    plot(
      residuals(dr_m2021,
        type = "standardized"
      )[, i] ~
      fitted(dr_m2021)[, i],
      col = factor(vpcp2021$LIT),
      xlab = attr(residuals(dr_m2021), "dim.names")[i]
    )
  }
  par(opar)
}

{
  opar <- par(mfrow = c(3, 3))
  for (i in 2:10) {
    qqp(
      residuals(dr_m2021,
        type = "standardized"
      )[, i],
      xlab = attr(residuals(dr_m2021), "dim.names")[i]
    )
  }
  par(opar)
}

## Get list of covariance matrices of estimated log(alphas) =====

su_names <- vpcp2021 %>%
  pluck("subunit_ID") %>%
  unique() %>%
  sort()

{ # Group to run all together
  vcov_dr_2021 <- vcov(dr_m2021)

  vcov_dr_list_2021 <- list()

  for (su_i in su_names) {
    vcov_dr_list_2021[[su_i]] <-
      vcov_dr_2021[
        (vcov_dr_2021 %>%
          rownames() %>%
          str_split_fixed(., ":", 2) %>%
          `[`(, 2) %>%
          gsub("subunit_ID", "", x = .) == su_i),

        (vcov_dr_2021 %>%
          colnames() %>%
          str_split_fixed(., ":", 2) %>%
          `[`(, 2) %>%
          gsub("subunit_ID", "", x = .) == su_i)
      ]

    colnames(vcov_dr_list_2021[[su_i]]) <- vcov_dr_list_2021[[su_i]] %>%
      colnames() %>%
      str_split_fixed(":", 2) %>%
      `[`(, 1) %>%
      as.character()

    rownames(vcov_dr_list_2021[[su_i]]) <- vcov_dr_list_2021[[su_i]] %>%
      rownames() %>%
      str_split_fixed(":", 2) %>%
      `[`(, 1) %>%
      as.character()
  }
}

## Apply fpcf to vcov's =====

for (i in su_names) {
  vcov_dr_list_2021[[i]] <- fpcf_2021[i] * vcov_dr_list_2021[[i]]
}

# Covariance matrix refers to coefficients for all subunits for each of the 10
# components as a single vector.
# All covariances between different units are zero. Only covariances between
# components within units are nonzero.

## Make df of mean vectors in log(alpha) scale =====

mean_df_dr_2021 <- dr_m2021 %>%
  coef() %>%
  bind_cols() %>%
  mutate(
    subunit_ID = gsub(
      "subunit_ID",
      "",
      names(coef(dr_m2021)[[1]])
    )
  ) %>%
  pivot_longer(-subunit_ID) %>%
  pivot_wider(names_from = subunit_ID, values_from = value) %>%
  map(~unname(.x)) %>%
  as_tibble() %>%
  column_to_rownames(var = "name")


## Simulate from distribution of estimated log(alphas) =====

{
  tic()
  if (setequal(
    names(mean_df_dr_2021),
    names(vcov_dr_list_2021)
  ) & setequal(
    rownames(mean_df_dr_2021),
    rownames(vcov_dr_list_2021[[1]])
  )) { # ensure match of subunits and spp_strat
    sim_parea_2021_dr <- map2(
      .x = mean_df_dr_2021,
      .y = vcov_dr_list_2021,
      .f = ~ log_alpha2p(
        mvrnorm(
          n = 4000,
          mu = .x,
          Sigma = .y
        )
      )
    ) %>%
      map(~ as_tibble(.x)) %>%
      map(~ dplyr::select(.x, .f = -p_Other_cover_NA)) %>%
      # rm Other to match mass sims
      map(~ rename_with(.x, .f = function(z) gsub("^p_", "", z)))
  } else {
    print("** ERROR: Subunits or spp_strat do not match!!**")
  }
  toc()
}

str(sim_parea_2021_dr)


## Compare Dirichlet estimates and direct averages of p_area =====

# average proportions
# these are the same as produced by main script
(avg_prop_main <- vpcp2021 %>%
  group_by(subunit_ID) %>%
  summarise(
    across(
      .cols = starts_with("p_"),
      ~ weighted.mean(.x, w = areaVisible_ac)
    )
  )
)

range(as.matrix(avg_prop_main[, 3:11]))

# estimated means based on DirichReg

coef_su <- dr_m2021 %>%
  coef() %>%
  `[[`(1) %>%
  names() %>%
  gsub("subunit_ID",
    "",
    x = .
  )

(est_mn_dr <- dr_m2021 %>%
    coef() %>%
    as_tibble() %>%
    log_alpha2p() %>%
    mutate(subunit_ID = coef_su) %>%
  as_tibble()
)

range(as.matrix(est_mn_dr[, 2:10]))

## Summary of simulated p_areas =====

(p_area_summary_dr <- map(
  .x = sim_parea_2021_dr,
  .f = ~ colMeans(.x)
) %>%
  bind_rows() %>%
  mutate(subunit_ID = names(sim_parea_2021_dr)))

range(as.matrix(p_area_summary_dr[, 1:9]))

mean(abs(as.matrix(avg_prop_main[, 3:11]) - as.matrix(p_area_summary_dr[, 1:9])))
max(abs(as.matrix(avg_prop_main[, 3:11]) - as.matrix(p_area_summary_dr[, 1:9])))

hist(abs(as.matrix(avg_prop_main[, 3:11]) -
           as.matrix(p_area_summary_dr[, 1:9])),
     breaks = 30)







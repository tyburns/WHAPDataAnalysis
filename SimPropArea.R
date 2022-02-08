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
library(lme4)
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

# Check if this approach weights p_area based on areas of VPs and CPs.
# It should use the weights to match previous approach.


# Data for 2021 =====

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
    fpcf = ifelse(fpcf <= 0.00987654321, 0.00987654321, fpcf)
  ) %>%
  ungroup()

acomp_2021 <- vpcp2021 %>%
  dplyr::select(starts_with("p_")) %>%
    as.matrix() %>%
  # `+`(replicate(10, abs(rnorm(dim(vpcp2021)[1],
  #   mean = 0.00,
  #   sd = 0.0001
  # )))) %>%
  acomp()

vpcp2021$a_comp <- acomp_2021

### Regress acomp() on subunit_ID

ilr_m2021 <- lm(
  ilr(a_comp) ~ -1 + subunit_ID,
#  weights = areaVisible_ac,
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

# residuals were checked and are acceptable

rsdl <- as.data.frame.rmult(residuals(ilr_m2021)) %>% as.matrix()

# pairs.panels(rsdl, breaks = 20)

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

vcov_ilr_2021 <- vcovAcomp(ilr_m2021)
vcov_names <- gsub("subunit_ID", "", attr(vcov_ilr_2021, "dimnames")[[3]])
attr(vcov_ilr_2021, "dimnames") <- list(NULL, NULL, vcov_names, vcov_names)

vcov_list_2021 <- list()
for (i in vcov_names) {
  vcov_list_2021[[i]] <- fpcf_2021[i] * vcov_ilr_2021[, , i, i]
}

### Make df of mean vectors =====

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

### Simulate from distribution of estimated ilr compositions =====

{tic()
if (setequal(
  names(mean_df),
  names(vcov_list_2021)
)) { # make sure subunits match
  sim_parea_2021_ilr <- map2(
    .x = mean_df,
    .y = vcov_list_2021,
    .f = ~ ilrInv(
      mvrnorm(
        n = 4000,
        mu = .x,
        Sigma = .y
      ),
      orig = acomp_2021
    )
  ) %>% # to recover names of variables
    map(.f = ~ as.data.frame.acomp(.x)) %>% 
    map(.f = ~ dplyr::select(., -p_Other_cover_NA))# rm Other to match mass sims
    map(.f = ~ rename_with(.x, .f = function(z) gsub("^p_", "", z)))
}
toc()
}

str(sim_parea_2021_ilr)

p_area_summary_ilr <- map(sim_parea_2021_ilr, ~round(colMeans(.x), 2)) %>%
  bind_rows() %>%
  mutate(subunit_ID = names(sim_parea_2021_ilr))


## Checking source of discrepancy between ilr and direct analysis of p_area

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

# estimated means based on lm of ilr

(est_mn_ilr <- ilr_m2021 %>%
  coef() %>%
  ilrInv(orig = acomp_2021) %>%
  as_tibble(rownames = "subunit_ID") %>%
  mutate(subunit_ID = gsub("subunit_ID", "", subunit_ID))
)

# data -> ilr -> average -> ilrInv

(irlInv_avg_ilr <- acomp_2021 %>%
  ilr() %>%
  as_tibble() %>%
  mutate(subunit_ID = vpcp2021$subunit_ID,
         areaVisible_ac = vpcp2021$areaVisible_ac) %>%
  group_by(subunit_ID) %>%
  summarise(
    across(
      .cols = V1:V9,
      ~ weighted.mean(.x, w = areaVisible_ac)
    )
  ) %>%
    dplyr::select(V1:V9) %>%
    ilrInv(orig = acomp_2021)
)


## Dirichlet Regression =====
## ilr approach produces means quite different from means of untransformed
## data. Try Dirichlet distribution.

### Regress p_area on subunit_ID =====

dr_data <- vpcp2021 %>%
  dplyr::select(starts_with("p_")) %>%
  DR_data()

# Common model option of DirichReg
dr_m2021 <- DirichReg(
  dr_data ~ -1 + subunit_ID,
    weights = areaVisible_ac,
  data = vpcp2021
)

## Get covariance matrix of estimated log(alphas) =====

# first try commented out:
# vcov_dr_m2021 <- vcov(dr_m2021) %>%
#   as_tibble(rownames = "var_suID") %>%
#   separate(var_suID, c("variable", "subunit_ID"), sep = ":") %>%
#   mutate(subunit_ID = gsub("subunit_ID", "", subunit_ID)) %>%
#   arrange(subunit_ID, variable) %>%
#   rename_with(~ gsub("subunit_ID", "", .x), starts_with("p_")) %>%
#   rename_with(
#     ~ paste(
#       str_split_fixed(.x, ":", 2)[, 2],
#       ":",
#       str_split_fixed(.x, ":", 2)[, 1],
#       sep = ""
#     ),
#     starts_with("p_")
#   ) %>%
#   dplyr::select(order(colnames(.)))

# Make list of covariance matrices

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

# summary(dr_m2021)

### Make df of mean vectors in log scale =====

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


### Function to go from log(alpha) to proportions

log_alpha2p <- function(x) {
  a_i <- exp(x)
  a_0 <- ifelse(is.vector(a_i),
    sum(a_i),
    rowSums(a_i)
  )
  return(a_i / a_0)
}


### Simulate from distribution of estimated log(alphas) =====

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
      map(~ dplyr::select(-p_Other_cover_NA)) %>% # rm Other to match mass sims
      map(~ rename_with(.x, .f = function(z) gsub("^p_", "", z)))
  } else {
    print("** ERROR: Subunits or spp_strat do not match!!**")
  }
  toc()
}

str(sim_parea_2021_dr)

p_area_summary_dr <- map(sim_parea_2021_dr, ~round(colMeans(.x), 2)) %>%
  bind_rows() %>%
  mutate(subunit_ID = names(sim_parea_2021_dr))













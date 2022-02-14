# FUNCTION TO SIMULATE PROPORTIONS OF AREA COVERED
# 
## Arguments ======
## 
# This function takes two arguments: the .vpcp_path to a vpcpyyyy.rds file containing
# the parsed information about VP's and CP's for year yyyy, and the number of
# simulations desired, .nsim.
# This file has to have the following structure:
# 
# tibble [443 × 17] (S3: tbl_df/tbl/data.frame)
# $ LIT                   : chr [1:443] "KRN" "KRN" "KRN" "KRN" ...
# $ unitName              : chr [1:443] "4B" "4B" "4B" "4B" ...
# $ subunitName           : chr [1:443] "4B-3" "4B-3" "4B-3" "4B-4" ...
# $ subunit_ID            : chr [1:443] "KRN_4B_4B-3" "KRN_4B_4B-3" "KRN_4B_4B-3" "KRN_4B_4B-4" ...
# $ level                 : chr [1:443] "VP" "VP" "VP" "VP" ...
# $ areaVisible_ac        : num [1:443] 0.435 0.656 3.416 1.088 2.165 ...
# $ p_Other_cover_NA      : num [1:443] 0.05 0.196 0.238 0.189 0.35 ...
# $ p_Swamp_Timothy_a.Low : num [1:443] 0.15 0.049 0.0952 0.0943 0.3 ...
# $ p_Watergrass_a.Low    : num [1:443] 0 0.0098 0 0.00943 0 ...
# $ p_Smartweed_a.Low     : num [1:443] 0 0.0098 0 0 0 ...
# $ p_Swamp_Timothy_b.Med : num [1:443] 0.2 0.049 0.571 0.377 0.35 ...
# $ p_Watergrass_b.Med    : num [1:443] 0 0 0 0 0 0 0 0 0 0.3 ...
# $ p_Smartweed_b.Med     : num [1:443] 0 0 0 0 0 0 0 0 0 0 ...
# $ p_Swamp_Timothy_c.High: num [1:443] 0.6 0.6863 0.0952 0.1887 0 ...
# $ p_Watergrass_c.High   : num [1:443] 0 0 0 0.142 0 ...
# $ p_Smartweed_c.High    : num [1:443] 0 0 0 0 0 0 0 0 0 0 ...
# $ su.area_ac            : num [1:443] 46.9 46.9 46.9 21.3 48.5 ...
# - attr(*, "na.action")= 'omit' Named int [1:22] 444 445 446 447 448 449 450 451 452 453 ...
# ..- attr(*, "names")= chr [1:22] "444" "445" "446" "447" ...

## Value =====
## 
# The object returned is a list of tibbles with as many elements as there are
# subunits in vpcp. Each tibble has nsim rows with 9 columns. Each row is a random simulation of proportion of area in each of the nine species-stratum
# combinations, as shown below for n = 4000.

# List of 68
# $ KRN_4A_4A-1                          : tibble [4,000 × 9] (S3: tbl_df/tbl/data.frame)
# ..$ Smartweed_a.Low     : num [1:4000] 0.0471 0.0413 0.0275 0.0403 0.017 ...
# ..$ Smartweed_b.Med     : num [1:4000] 0.0674 0.0808 0.0275 0.0631 0.0344 ...
# ..$ Smartweed_c.High    : num [1:4000] 0.0382 0.0366 0.0313 0.0504 0.0384 ...
# ..$ Swamp_Timothy_a.Low : num [1:4000] 0.1214 0.0894 0.0771 0.0755 0.0915 ...
# ..$ Swamp_Timothy_b.Med : num [1:4000] 0.372 0.376 0.439 0.425 0.477 ...
# ..$ Swamp_Timothy_c.High: num [1:4000] 0.0642 0.06 0.0833 0.0725 0.0525 ...
# ..$ Watergrass_a.Low    : num [1:4000] 0.051 0.0341 0.0333 0.0521 0.0292 ...
# ..$ Watergrass_b.Med    : num [1:4000] 0.0842 0.0494 0.0548 0.0378 0.018 ...
# ..$ Watergrass_c.High   : num [1:4000] 0.0247 0.0589 0.0346 0.0437 0.0239 ...
# $ KRN_4A_4A-2                          : tibble [4,000 × 9] (S3: tbl_df/tbl/data.frame)
# ..$ Smartweed_a.Low     : num [1:4000] 0.0349 0.0408 0.0447 0.0242 0.0245 ...
# ..$ Smartweed_b.Med     : num [1:4000] 0.0318 0.0365 0.0388 0.0238 0.0348 ...
# ..$ Smartweed_c.High    : num [1:4000] 0.0264 0.0304 0.0345 0.0262 0.0263 ...
# ..$ Swamp_Timothy_a.Low : num [1:4000] 0.099 0.1145 0.0931 0.1252 0.0945 ...
# ..$ Swamp_Timothy_b.Med : num [1:4000] 0.345 0.423 0.416 0.341 0.41 ...
# ..$ Swamp_Timothy_c.High: num [1:4000] 0.1107 0.0761 0.071 0.052 0.0854 ...
# ..$ Watergrass_a.Low    : num [1:4000] 0.0376 0.0308 0.0349 0.0398 0.0303 ...
# ..$ Watergrass_b.Med    : num [1:4000] 0.0395 0.0441 0.0294 0.0345 0.0382 ...
# ..$ Watergrass_c.High   : num [1:4000] 0.0355 0.0238 0.0311 0.0257 0.0574 ...
# 
## Details =====
##
# CALCULATIONS ASSUME THAT THERE ARE NO OVERLAP OF SPECIES INTHE ESTIMATES.
# WHEN SPECIES OVERLAP IN THE FIELD, OBSERVERS HAVE TO "MENTALLY" SEPARATE
# SPECIES INTO DIFFERENT AREAS AND CORRECT DENSITY ACCORDINGLY.

# Areal compositions will be simulated for each year separately.
# Each subunit_ID will receive multiple simulations of composition.
# Each composition is a vector of 10 numbers: 3 strata * 3 species + other.
# Each set of estimates or simulation has one row per subunit_ID and one
# column per component.
# 
# # Proportions of area covered by each species:stratum combination are 
# simulated using a model fitted to the .vpcp_path file. Estimated model 
# parameters are the logs of Dirichlet's alphas in the common parameterization. 
# These are back-transformed into the estimates of interest (proportion of area 
# covered by each species:stratum in each subunit). It is assumed that the 
# estimated parameters have a multivariate normal distribution and simulations 
# are obtained directly from mvrnorm() using the estimated parameters and their 
# estimated vcov() matrix.
#
## Pseudocode =====
# 1. Get vpcp file for current year.
# 2. Change zeroes to abs(rnorm(mean = 0, sd = 0.0001)).
# 3. Use Dirichlet regression to model proportions in each subunit.
# 4. Get means and vcov in log(alpha) scale into parallel lists
# 5. Apply fpcf to vcov and Simulate assuming mv normality
# 6. Backstransform with exp().

## Function definition =====

sim_prop_area <- function(.vpcp_path, .nsim = 1000) {
  
  ## Set up packages needed
  
  require(DirichletReg, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  require(MASS, quietly = TRUE)
  
  
  ## Function to go from log(alpha) to proportions =====
  
  log_alpha2p <- function(x) {
    a_i <- exp(x)
    a_0 <- if (is.vector(a_i)) sum(a_i) else rowSums(a_i)
    return(a_i / a_0)
  }
  
  
  ## Read and wrangle data
  vpcp <- read_rds(.vpcp_path) %>%
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
      nobs = n(),
      wt = areaVisible_ac * nobs / t_area_vis,
      fpcf = 1 - t_area_vis / su.area_ac,
      fpcf = ifelse(fpcf <= 0.01, 0.01, fpcf)
    ) %>%
    ungroup()
  
  
  ## Make vector of finite population correction factors =====
  
  fpcf <- vpcp %>%
    dplyr::select(subunit_ID, fpcf) %>%
    unique() %>%
    deframe()
  
  ## Dirichlet Regression of p_area on subunit_ID =====
  
  dr_data <- vpcp %>%
    dplyr::select(starts_with("p_")) %>%
    DR_data()
  
  # Use common model option of DirichReg
  dr_m <- DirichReg(
    dr_data ~ -1 + subunit_ID,
    weights = areaVisible_ac,
    data = vpcp
  )
  
  ## Create list of vcov's for each subunit =====
  ## 
  vcov_dr <- vcov(dr_m)
  
  vcov_dr_list <- list()
  
  for (su_i in su_names) {
    vcov_dr_list[[su_i]] <-
      vcov_dr[
        (vcov_dr %>%
           rownames() %>%
           str_split_fixed(., ":", 2) %>%
           `[`(, 2) %>%
           gsub("subunit_ID", "", x = .) == su_i),
        
        (vcov_dr %>%
           colnames() %>%
           str_split_fixed(., ":", 2) %>%
           `[`(, 2) %>%
           gsub("subunit_ID", "", x = .) == su_i)
      ]
    
    colnames(vcov_dr_list[[su_i]]) <- vcov_dr_list[[su_i]] %>%
      colnames() %>%
      str_split_fixed(":", 2) %>%
      `[`(, 1) %>%
      as.character()
    
    rownames(vcov_dr_list[[su_i]]) <- vcov_dr_list[[su_i]] %>%
      rownames() %>%
      str_split_fixed(":", 2) %>%
      `[`(, 1) %>%
      as.character()
  }
  
  ## Apply fpcf to vcov's =====
  
  for (i in su_names) {
    vcov_dr_list[[i]] <- fpcf[i] * vcov_dr_list[[i]]
  }
  
  # Covariance matrix refers to coefficients for all subunits for each of the 10
  # components as a single vector.
  # All covariances between different units are zero. Only covariances between
  # components within units are nonzero.
  
  ## Make df of mean vectors in log(alpha) scale =====
  
  mean_df_dr <- dr_m %>%
    coef() %>%
    bind_cols() %>%
    mutate(
      subunit_ID = gsub(
        "subunit_ID",
        "",
        names(coef(dr_m)[[1]])
      )
    ) %>%
    pivot_longer(-subunit_ID) %>%
    pivot_wider(names_from = subunit_ID, values_from = value) %>%
    map(~unname(.x)) %>%
    as_tibble() %>%
    column_to_rownames(var = "name")
  
  
  ## Simulate from distribution of estimated log(alphas) =====
  
  {
    if (setequal(
      names(mean_df_dr),
      names(vcov_dr_list)
    ) & setequal(
      rownames(mean_df_dr),
      rownames(vcov_dr_list[[1]])
    )) { # ensure match of subunits and spp_strat
      sim_parea_dr <- map2(
        .x = mean_df_dr,
        .y = vcov_dr_list,
        .f = ~ log_alpha2p(
          mvrnorm(
            n = .nsim,
            mu = .x,
            Sigma = .y
          )
        )
      ) %>%
        map(~ as_tibble(.x)) %>%
        map(~ dplyr::select(.x, .f = -p_Other_cover_NA)) %>%
        # rm p_Other_cover_NA to match mass sims
        map(~ rename_with(.x, .f = function(z) gsub("^p_", "", z)))
    } else {
      print("** ERROR: Subunits or spp_strat do not match!!**")
    }
  }
  
  sim_parea_dr <- sim_parea_dr %>%
    map(~ dplyr::select(
      .x,
      any_of(
        c(
          "Smartweed_a.Low",
          "Smartweed_b.Med",
          "Smartweed_c.High",
          "Swamp_Timothy_a.Low",
          "Swamp_Timothy_b.Med",
          "Swamp_Timothy_c.High",
          "Watergrass_a.Low",
          "Watergrass_b.Med",
          "Watergrass_c.High"
        )
      )
    ))
  
  return(sim_parea_dr)
  
}
### FUNCTION TO CALCULATE TOTAL MASS AND MASS/AREA PER REFUGE AND SUBUNIT
### BASED ON SIMULATIONS OBTAINED WITH
###
### SimPropAreaFun.R
### SimMassPerAreaFun.R
##
## Arguments ======
##
# This function takes three arguments:
#   .qdt_path gets to the file needed by sim_mass_area() in SimMassPerAreaFun.R
#   .vpcp_path gets to the file needed by sim_prop_area() in SimPropAreaFun.R
#   .nsim is the number of simulations to do in both cases.
#
#
## Value =====
##
# The function produces a list with several objects resulting from analyzing
# the data in the input files. The list preserves all simulations of all
# variables and also includes obvious summaries. Because all simulations are
# passed to the output, sampling distributions can be calculated for any
# function of proportion of areas or mass/area. For example, any two subunits
# can be tested to see if their mass/area or total mass differ.
#
#
## Details =====
##
# TBD

## Define function to sum over strata within each species ======

sum_strt_fun <- function(.df, .spp) {
  mass_col_names <- .spp %>%
    paste0("_g_m2")

  for (spp in .spp) {
    assign(
      paste0(spp, "_g_m2"),
      (dplyr::select(.df, starts_with(spp)) %>%
        rowSums())
    )
  }

  df_out <- mget(mass_col_names) %>%
    as_tibble() %>%
    mutate(tot_g_m2 = rowSums(.)) %>%
    round(digits = 1)

  return(df_out)
}

### Define function to get summary per subunit by spp or total =====

mass_by_su_smmry <- function(.mass_sims = mass_m2_sims, .spp) {
  mass_col <- paste0(.spp, "_g_m2")

  mass_m2_by_su <- .mass_sims %>%
    dplyr::select(
      subunit_ID,
      mass_g_m2
    ) %>%
    unnest(cols = c(mass_g_m2)) %>%
    group_by(subunit_ID) %>%
    summarise(
      g_m2 = mean(get(mass_col)),
      se_g_m2 = sd(get(mass_col)),
      CI90_lwr = quantile(get(mass_col), 0.05),
      CI90_upr = quantile(get(mass_col), 0.95),
      RightCI90_lwr = quantile(get(mass_col), 0.10),
      left_moe_p = (g_m2 - RightCI90_lwr) / g_m2,
      smpl_obj = ifelse(left_moe_p < 0.25,
        "Pass",
        "NO"
      )
    )
  return(mass_m2_by_su)
}



## sim_mass_su_lit Function definition =====

sim_mass_su_lit <- function(.vpcp_path, .qdt_path, .nsim = 1000) {

  sim_parea_dr <- sim_prop_area(
    .vpcp_path = .vpcp_path,
    .nsim = .nsim
  )

  sim_massLss_lst <- sim_mass_area(
    .qdt_path = .qdt_path,
    .nsim = .nsim
  )

  # Get subunit areas to calculate refuge stats =====
  #
  su_areas <- read_rds(.vpcp_path) %>%
    dplyr::select(
      LIT,
      subunit_ID,
      su.area_ac
    ) %>%
    unique() %>%
    mutate(su_area_ha = conv_unit(
      su.area_ac,
      "acre",
      "hectare"
    ))


  # Calculate simulated mass per area =====
  # Simulated proportion of areas are multiplied by simulated mass/area
  # in the corresponding LIT:spp:stratum and then, amounts are summed
  # over strata to obtain mass per area by species and total of all species.

  spp_names <- names(sim_parea_dr[[1]]) %>%
    gsub(
      "_a.Low|_b.Med|_c.High",
      "",
      x = .
    ) %>%
    unique()


  mass_m2_sims <- map2(
    .x = sim_parea_dr,
    .y = names(sim_parea_dr),
    .f = ~ (.x * sim_massLss_lst[[substr(.y, 1, 3)]]) %>%
      mutate(subunit_ID = .y)
  ) %>%
    bind_rows() %>%
    group_by(subunit_ID) %>%
    nest() %>%
    mutate(
      mass_g_m2 =
        map(
          .x = data,
          .f = ~ sum_strt_fun(.df = .x, .spp = spp_names)
        )
    ) %>% # Add subunit areas to calculate refuge stats
    full_join(su_areas,
      by = c("subunit_ID")
    ) %>%
    # Within each LIT, subunits are arranged in decreasing order of total mass
    # contributed, the total mass is accumulated and CI and moe's calculated
    group_by(LIT) %>%
    mutate(area_wt = su_area_ha / sum(su_area_ha)) %>%
    bind_cols(map_dfr(.$mass_g_m2, ~ colMeans(.x))) %>%
    mutate(tot_ton = su_area_ha * tot_g_m2 / 100) %>%
    arrange(LIT, desc(tot_ton)) %>%
    mutate(
      cum_tot_ton = accumulate(tot_ton, `+`),
      cum_tot_ton_sims = map2(
        .x = mass_g_m2,
        .y = su_area_ha,
        ~ .x * .y / 100
      ) %>%
        accumulate(`+`)
    ) %>%
    mutate(
      ctt_RightCI90_lwr =
        map_dbl(
          .x = cum_tot_ton_sims,
          .f = ~ quantile(.x$tot_g_m2, 0.10)
        ),
      ctt_CI90_lwr =
        map_dbl(
          .x = cum_tot_ton_sims,
          .f = ~ quantile(.x$tot_g_m2, 0.05)
        ),
      ctt_CI90_upr =
        map_dbl(
          .x = cum_tot_ton_sims,
          .f = ~ quantile(.x$tot_g_m2, 0.95)
        ),
      left_moe_p = (cum_tot_ton - ctt_RightCI90_lwr) / (cum_tot_ton)
    )

  ## Cumulative mass of all species by LIT =====

  cum_mass_LIT <- mass_m2_sims %>%
    dplyr::select(
      LIT,
      subunit_ID,
      ctt_RightCI90_lwr,
      ctt_CI90_lwr,
      cum_tot_ton,
      ctt_CI90_upr,
      left_moe_p,
      su_area_ha
    ) %>%
    mutate(
      cum_area_ha = accumulate(su_area_ha, `+`),
      smpl_obj = factor(
        ifelse(left_moe_p < 0.25,
          "Pass",
          "NO"
        ),
        levels = c("Pass", "NO")
      )
    ) %>%
    dplyr::select(-su_area_ha)


  ## Plot cumulative mass vs cumulative area and RightCI's =====

  cum_tot_plot <- ggplot(
    data = cum_mass_LIT,
    aes(
      y = cum_tot_ton,
      x = cum_area_ha,
      groups = LIT,
      color = LIT
    )
  ) +
    geom_line(size = 1.0) +
    geom_errorbar(
      aes(
        ymin = ctt_RightCI90_lwr,
        ymax = cum_tot_ton,
        linetype = smpl_obj
      )
    ) +
    ylab("Cumulative seed head mass index (1000 kg or ton)") +
    xlab("Cumulative area (ha)") +
    geom_text(x = 350, y = 40, label = "upper 90% CI", color = "black")



  ### Mass of all species by subunit =====

  tot_mass_su <- mass_m2_sims %>%
    dplyr::select(
      LIT,
      subunit_ID,
      mass_g_m2
    ) %>%
    unnest(cols = c(mass_g_m2)) %>%
    group_by(subunit_ID) %>%
    summarise(
      all_sp_g_m2 = mean(tot_g_m2),
      se_g_m2 = sd(tot_g_m2),
      CI90_lwr = quantile(tot_g_m2, 0.05),
      CI90_upr = quantile(tot_g_m2, 0.95),
      Low90Bound = quantile(tot_g_m2, 0.10)
    ) %>%
    mutate(
      left_moe_p = (all_sp_g_m2 - Low90Bound) / (all_sp_g_m2),
      smpl_obj = ifelse(left_moe_p < 0.25,
        "Pass",
        "NO"
      )
    )


  ## Mass per subunit by species stats =======

  mass_by_su_spp_stats <- map(
    .x = spp_names,
    .f = ~ mass_by_su_smmry(.mass_sims = mass_m2_sims, .spp = .x)
  ) %>%
    set_names(nm = spp_names)

  # Mass per area by refuge =====

  ### Mass of each and all species by refuge =====
  # Values should be equal to mass/area in the last line of each LIT in the
  # cumulative table.

  lit_mass <- mass_m2_sims %>%
    mutate(wt_mass = map2(
      .x = mass_g_m2,
      .y = area_wt,
      ~ .x * .y
    )) %>%
    dplyr::select(
      LIT,
      wt_mass
    ) %>%
    nest(data = c(wt_mass)) %>%
    mutate(summed = map(data, ~ reduce(.$wt_mass, `+`))) %>%
    mutate(stats = map(
      .x = summed,
      ~ summarise(
        .x,
        across(
          .cols = everything(),
          .fns = list(
            mn = ~ round(mean(.x), 1),
            se = ~ round(sd(.x), 2),
            CI90_lwr = ~ round(quantile(.x, 0.05), 2),
            CI90_upr = ~ round(quantile(.x, 0.95), 2),
            RightCI90_lwr = ~ round(quantile(.x, 0.10), 2)
          )
        )
      )
    ))


  lit_stats <- lit_mass %>%
    dplyr::select(
      LIT,
      stats
    ) %>%
    unnest(cols = stats)

  ## Description of each output list element =====

  mass_m2_sims_def <- "Tibble with one row per subunit, with a data column where each element is a set of 4000 simulation of g/m2 in each species:stratum combinations. Column mass_g_m2 is analogous to data but summed by species and all species together. Colum area_wt is the proportion of total refuge area (including only subunits measured) represented by each subunit.  Column cum_tot_ton_sims is a list of data frames, each of which contains the simulation of cumulative (over subunits in decreasing order of total mass) mass of each and all spp together. Columns starting with ctt are percentile confidence limits based on cum_tot_ton_sims. The rest of the columns, other than area of each subunit, are summaries based on the column mass_g_m2, including average mass per area for each species and all species together, as well as the estimated total tons of seed head mass index in each subunit and acummulated over subunits in decreasing order of total mass contributed within each refuge.  The column of tibbles mass_g_m2 can be used a the start for any estimation or hypothesis testing. left_moe_p is the width of the left margin of error (from estimated mean to 10%-ile of simulated values) as a proportion of estimated mean."

  cum_mass_LIT_def <- "This is a subset of mass_m2_sims_def that can be used to make a table and that was used to produce the plot cum_tot_plot_def."

  cum_tot_plot_def <- "Plot of cumulative seed head mass index over subunits ordered by decreasing mass contributed to the total of each refuge vs. cumulative area. Each point has a downward vertical bar representing the left margin of error that leaves 10% of the simulation below. Dashed error bars indicate subunits and points that meet the desirable sampling objective of moe < 0.25 * mean. Solid error bars indicate that the moe is larger than desired."

  tot_mass_su_def <- "Tibble with estimated means, se and CI's for g/m2 of all species together by subunit. left_moe_p is as in mass_m2_sims_def, and smpl_obj states if sampling objective is met based on this left_moe_p."

  mass_by_su_spp_stats_def <- "A list with an element for each species (vernacularName). Each list element is named with the species name and has the same information as tot_mass_su_def but only for the species."

  lit_stats_def <- "Means, se's and CI's of mass/m2 averaged at the refuge level. Each table with nsim simulations of g_m2 for each species in each subunit was multiplied by the proportion of total subunit area represented by the subunit and added over subunits to obtain nsim simulations of average g_m2 of each species at the refuge level (refuge means subunits measured). Standard errors and confidence intervals were calculated from those simulations."

  ## Prepare output list and return =====
  outlist <- list(
    mass_m2_sims = ungroup(mass_m2_sims),
    mass_m2_sims_def = mass_m2_sims_def,
    cum_mass_LIT = cum_mass_LIT,
    cum_mass_LIT_def = cum_mass_LIT_def,
    cum_tot_plot = cum_tot_plot,
    cum_tot_plot_def = cum_tot_plot_def,
    tot_mass_su = tot_mass_su,
    tot_mass_su_def = tot_mass_su_def,
    mass_by_su_spp_stats = mass_by_su_spp_stats,
    mass_by_su_spp_stats_def = mass_by_su_spp_stats_def,
    lit_stats = lit_stats,
    lit_stats_def = lit_stats_def
  )

  return(outlist)
}

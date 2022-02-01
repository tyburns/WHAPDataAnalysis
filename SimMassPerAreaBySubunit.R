# Join simulations of pArea and mass.m2 by LIT spp stratum =====
# to calculate simulated mass/area, se and CI for each subunit

source("SimPropArea.R")
source("SimMassPerAreaByStratum.R")


## Order columns within each data frame in both lists =====
# to be able to use matrix multiplication for calculations

sim_parea_2021 <- sim_parea_2021 %>%
  map(~ dplyr::select(
    .x,
    Smartweed_a.Low,
    Smartweed_b.Med,
    Smartweed_c.High,
    Swamp_Timothy_a.Low,
    Swamp_Timothy_b.Med,
    Swamp_Timothy_c.High,
    Watergrass_a.Low,
    Watergrass_b.Med,
    Watergrass_c.High
  ))

sim_massLss_lst_2021 <- sim_massLss_lst_2021 %>%
  map(~ dplyr::select(
    .x,
    Smartweed_a.Low,
    Smartweed_b.Med,
    Smartweed_c.High,
    Swamp_Timothy_a.Low,
    Swamp_Timothy_b.Med,
    Swamp_Timothy_c.High,
    Watergrass_a.Low,
    Watergrass_b.Med,
    Watergrass_c.High
  ))


# Calculate simulated mass per area =====

mass_m2_2021_sims <- map2(
  .x = sim_parea_2021,
  .y = names(sim_parea_2021),
  ~ (.x * sim_massLss_lst_2021[[substr(.y, 1, 3)]]) %>%
    mutate(subunit_ID = .y)
) %>%
  bind_rows() %>%
  group_by(subunit_ID) %>%
  nest() %>%
  mutate(
    mass_g_m2 =
      map(
        .x = data,
        ~ mutate(
          .x,
          sw_g_m2 = Smartweed_a.Low + Smartweed_b.Med + Smartweed_c.High,
          st_g_m2 = Swamp_Timothy_a.Low + Swamp_Timothy_b.Med + Swamp_Timothy_c.High,
          wg_g_m2 = Watergrass_a.Low + Watergrass_b.Med + Watergrass_c.High,
          tot_g_m2 = sw_g_m2 + st_g_m2 + wg_g_m2
        ) %>%
          dplyr::select(
            sw_g_m2,
            st_g_m2,
            wg_g_m2,
            tot_g_m2
          ) %>%
          round(1)
      )
  ) %>%
  dplyr::select(-data)


# Add subunit areas to be able to calculate refuge stats

str(vpcp2021)

su_areas_2021 <- vpcp2021 %>%
  dplyr::select(
    LIT,
    subunit_ID,
    su.area_ac
  ) %>%
  unique()

mass_m2_2021_sims <- full_join(mass_m2_2021_sims,
                               su_areas_2021,
                               by = c("subunit_ID")) %>%
  group_by(LIT) %>%
  mutate(area_wt = su.area_ac / sum(su.area_ac))


## Mass per area by subunit =====

### Mass of all species by subunit =====

tot_mass_su_2021 <- mass_m2_2021_sims %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    all_sp_g_m2 = mean(tot_g_m2),
    se_g_m2 = sd(tot_g_m2),
    CI90_lwr = quantile(tot_g_m2, 0.05),
    CI90_upr = quantile(tot_g_m2, 0.95),
    Low80Bound = quantile(tot_g_m2, 0.20) # approx. equating sampling with posterior
  )


### Smartweed mass per subunit =====

sw_mass_m2_2021 <- mass_m2_2021_sims %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    g_m2 = mean(sw_g_m2),
    se_g_m2 = sd(sw_g_m2),
    CI90_lwr = quantile(sw_g_m2, 0.05),
    CI90_upr = quantile(sw_g_m2, 0.95),
    Low80Bound = quantile(sw_g_m2, 0.20) # approx. equating sampling with posterior
  )


### Swamp Timothy mass per subunit =====

st_mass_m2_2021 <- mass_m2_2021_sims %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    g_m2 = mean(st_g_m2),
    se_g_m2 = sd(st_g_m2),
    CI90_lwr = quantile(st_g_m2, 0.05),
    CI90_upr = quantile(st_g_m2, 0.95),
    Low80Bound = quantile(st_g_m2, 0.20) # approx. equating sampling with posterior
  )


### Watergrass mass per subunit =====

wg_mass_m2_2021 <- mass_m2_2021_sims %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    g_m2 = mean(wg_g_m2),
    se_g_m2 = sd(wg_g_m2),
    CI90_lwr = quantile(wg_g_m2, 0.05),
    CI90_upr = quantile(wg_g_m2, 0.95),
    Low80Bound = quantile(wg_g_m2, 0.20) # approx. equating sampling with posterior
  )


# Mass per area by refuge =====

### Mass of all species by refuge =====

lit_mass <- mass_m2_2021_sims %>%
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
    ~summarise(
      .x,
      across(
        .cols = everything(),
        .fns = list(
          mn = ~round(mean(.x), 1),
          se = ~round(sd(.x)),
          CI90_lwr = ~round(quantile(.x, 0.05)),
          CI90_upr = ~round(quantile(.x, 0.95)),
          Low80 = ~round(quantile(.x, 0.20))
        )
      )
    )
  ))


lit_stats <- lit_mass %>%
  dplyr::select(LIT,
                stats) %>%
  unnest(cols = stats)

t(lit_stats)


### Smartweed mass per refuge =====



### Swamp Timothy mass per refuge =====



### Watergrass mass per refuge =====














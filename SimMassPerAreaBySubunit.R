# Join simulations of pArea and mass.m2 by LIT spp stratum =====
# to calculate simulated mass/area, se and CI for each subunit

library(measurements)

source("SimPropArea.R")
source("SimMassPerAreaByStratum.R")


## Order columns within each data frame in both lists =====
# to be able to use direct multiplication for calculations

sim_parea_2021_dr <- sim_parea_2021_dr %>%
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

# Get subunit areas to calculate refuge stats
# 
su_areas_2021 <- vpcp2021 %>%
  dplyr::select(
    LIT,
    subunit_ID,
    su.area_ac
  ) %>%
  unique() %>%
  mutate(su_area_ha = conv_unit(su.area_ac, "acre", "hectare"))


# Calculate simulated mass per area =====

mass_m2_2021_sims <- map2(
  .x = sim_parea_2021_dr,
  .y = names(sim_parea_2021_dr),
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
  ) %>% # Add subunit areas to calculate refuge stats
  full_join(su_areas_2021,
    by = c("subunit_ID")
  ) %>%
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
      moe_p = (ctt_CI90_upr - ctt_CI90_lwr) / (2 * cum_tot_ton)
    )

## Cumulative mass by LIT =====
# Within each LIT, subunits are arranged in decreasing order of total mass
# contributed, the total mass is accumulated and CI and moe's calculated

mass_LIT <- mass_m2_2021_sims %>%
  dplyr::select(LIT,
                subunit_ID,
                ctt_RightCI90_lwr,
                ctt_CI90_lwr,
                cum_tot_ton,
                ctt_CI90_upr,
                moe_p) %>%
  full_join(su_areas_2021) %>%
  mutate(cum_area_ha = accumulate(su_area_ha, `+`))

print(mass_LIT, n = Inf)

## Display cumulative mass vs cumulative area and CI's

ggplot(
  data = mass_LIT,
  aes(
    y = cum_tot_ton,
    x = cum_area_ha,
    groups = LIT,
    color = LIT
  )
) +
  geom_line(size = 1.0) +
  geom_errorbar(aes(
    ymin = ctt_CI90_lwr,
    ymax = ctt_CI90_upr
  )) +
  ylab("Cumulative seed head mass index (1000 kg or ton)") +
  xlab("Cumulative area (ha)")

## Display cumulative mass vs cumulative area and RightCI's

ggplot(
  data = mass_LIT,
  aes(
    y = cum_tot_ton,
    x = cum_area_ha,
    groups = LIT,
    color = LIT
  )
) +
  geom_line(size = 1.0) +
  geom_errorbar(aes(
    ymin = ctt_RightCI90_lwr,
    ymax = cum_tot_ton
  )) +
  ylab("Cumulative seed head mass index (1000 kg or ton)") +
  xlab("Cumulative area (ha)") +
  geom_text(x = 20, y = 350, label = "upper 90% CI", color = "black")


## Mass per area by subunit =====

### Mass of all species by subunit =====

tot_mass_su_2021 <- mass_m2_2021_sims %>%
  dplyr::select(LIT,
                subunit_ID,
                mass_g_m2) %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    all_sp_g_m2 = mean(tot_g_m2),
    se_g_m2 = sd(tot_g_m2),
    CI80_lwr = quantile(tot_g_m2, 0.10),
    CI80_upr = quantile(tot_g_m2, 0.90),
    Low80Bound = quantile(tot_g_m2, 0.20) # approx. equating sampling with posterior
  ) %>%
  mutate(moe_p = (CI80_upr - CI80_lwr)/(2 * all_sp_g_m2))

# Compare with results from main script

qdt_mass_2021 <- read_rds("WHAP2021-22/Output2021/qdt_mass_g_m2_2021.rds")

tot_mass_su_2021 <- tot_mass_su_2021 %>%
  full_join(qdt_mass_2021 %>%
              dplyr::select(LIT,
                            subunit_ID,
                            tot_g_m2,
                            CI80lwr,
                            CI80upr))

ggplot(data = tot_mass_su_2021,
       aes(y = tot_g_m2,
           x = all_sp_g_m2,
           color = LIT)) +
  geom_point(size = 2) +
  geom_errorbar(aes(
    ymin = CI80lwr,
    ymax = CI80upr
  )) +
  geom_errorbarh(aes(
    xmin = CI80_lwr,
    xmax = CI80_upr
  )) +
  geom_abline() +
  ylab("Seed head mass index original main script (g/m2)") +
    xlab("Seed head mass index using simulations (g/m2)") +
    geom_text(x = 20, y = 350, label = "80% CI's", color = "black")



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
          se = ~round(sd(.x), 2),
          CI90_lwr = ~round(quantile(.x, 0.05), 1),
          CI90_upr = ~round(quantile(.x, 0.95), 1),
          Low80 = ~round(quantile(.x, 0.20), 1)
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














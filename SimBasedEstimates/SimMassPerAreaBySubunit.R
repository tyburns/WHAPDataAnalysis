# Join simulations of pArea and mass.m2 by LIT spp stratum =====
# to calculate simulated mass/area, se and CI for each subunit

library(measurements)


## Run scripts to get simulations of area proportions and mass per area =====
source("SimPropArea.R")
source("SimMassPerAreaByStratum.R")


## Order columns within each data frame in both lists =====
# to be able to use direct multiplication for calculations

sim_parea_2021_dr <- sim_parea_2021_dr %>%
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

sim_massLss_lst_2021 <- sim_massLss_lst_2021 %>%
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

# Get subunit areas to calculate refuge stats =====
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
          sw_g_m2 = (dplyr::select(., starts_with("Smart")) %>% rowSums()),
          st_g_m2 = (dplyr::select(., starts_with("Swamp")) %>% rowSums()),
          wg_g_m2 = (dplyr::select(., starts_with("Water")) %>% rowSums()),
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
      left_moe_p = (cum_tot_ton - ctt_RightCI90_lwr) / (cum_tot_ton)
    )

## Cumulative mass by LIT =====
# Within each LIT, subunits are arranged in decreasing order of total mass
# contributed, the total mass is accumulated and CI and moe's calculated

cum_mass_LIT <- mass_m2_2021_sims %>%
  dplyr::select(LIT,
                subunit_ID,
                ctt_RightCI90_lwr,
                ctt_CI90_lwr,
                cum_tot_ton,
                ctt_CI90_upr,
                left_moe_p) %>%
  full_join(su_areas_2021) %>%
  mutate(cum_area_ha = accumulate(su_area_ha, `+`),
         smpl_obj = ifelse(left_moe_p < 0.25,
                           "Pass",
                           "NO")) %>%
  dplyr::select(-su_area_ha)
                

print(cum_mass_LIT, n = Inf)

## Display cumulative mass vs cumulative area and CI's =====

ggplot(
  data = cum_mass_LIT,
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
    ymax = ctt_CI90_upr,
    linetype = smpl_obj
  )) +
  ylab("Cumulative seed head mass index (1000 kg or ton)") +
  xlab("Cumulative area (ha)")

## Display cumulative mass vs cumulative area and RightCI's =====

ggplot(
  data = cum_mass_LIT,
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
    ymax = cum_tot_ton,
    linetype = smpl_obj
  )) +
  ylab("Cumulative seed head mass index (1000 kg or ton)") +
  xlab("Cumulative area (ha)") +
  geom_text(x = 20, y = 350, label = "upper 90% CI", color = "black")


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
    CI90_lwr = quantile(tot_g_m2, 0.05),
    CI90_upr = quantile(tot_g_m2, 0.95),
    Low90Bound = quantile(tot_g_m2, 0.10)
  ) %>%
  mutate(left_moe_p = (all_sp_g_m2 - Low90Bound)/(all_sp_g_m2),
         smpl_obj = ifelse(left_moe_p < 0.25,
                           "Pass",
                           "NO"))

print(tot_mass_su_2021, n = Inf)

# Compare with results from main script =====
## Mass of all species by subunit to compare with main script

tot_mass_su_2021b <- mass_m2_2021_sims %>%
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
  )

qdt_mass_2021 <- read_rds("../WHAP2021-22/Output2021/qdt_mass_g_m2_2021.rds")

tot_mass_su_2021b <- tot_mass_su_2021b %>%
  full_join(qdt_mass_2021 %>%
              dplyr::select(LIT,
                            subunit_ID,
                            tot_g_m2,
                            CI80lwr,
                            CI80upr))

ggplot(data = tot_mass_su_2021b,
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
    geom_text(x = 20, y = 350, label = "80% CI's \n 2021", color = "black")


### Smartweed mass per subunit =====

sw_mass_m2_2021 <- mass_m2_2021_sims %>%
  dplyr::select(subunit_ID,
                mass_g_m2) %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    g_m2 = mean(sw_g_m2),
    se_g_m2 = sd(sw_g_m2),
    CI90_lwr = quantile(sw_g_m2, 0.05),
    CI90_upr = quantile(sw_g_m2, 0.95),
    RightCI90_lwr = quantile(sw_g_m2, 0.10),
    left_moe_p = (g_m2 - RightCI90_lwr) / g_m2,
    smpl_obj = ifelse(left_moe_p < 0.25,
                      "Pass",
                      "NO"
  ))

print(sw_mass_m2_2021, n = Inf)


### Swamp Timothy mass per subunit =====

st_mass_m2_2021 <- mass_m2_2021_sims %>%
  dplyr::select(subunit_ID,
                mass_g_m2) %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    g_m2 = mean(st_g_m2),
    se_g_m2 = sd(st_g_m2),
    CI90_lwr = quantile(st_g_m2, 0.05),
    CI90_upr = quantile(st_g_m2, 0.95),
    RightCI90_lwr = quantile(st_g_m2, 0.10),
    left_moe_p = (g_m2 - RightCI90_lwr) / g_m2,
    smpl_obj = ifelse(left_moe_p < 0.25,
                      "Pass",
                      "NO"
    )
  )

print(st_mass_m2_2021, n = Inf)


### Watergrass mass per subunit =====

wg_mass_m2_2021 <- mass_m2_2021_sims %>%
  dplyr::select(subunit_ID,
                mass_g_m2) %>%
  unnest(cols = c(mass_g_m2)) %>%
  group_by(subunit_ID) %>%
  summarise(
    g_m2 = mean(wg_g_m2),
    se_g_m2 = sd(wg_g_m2),
    CI90_lwr = quantile(wg_g_m2, 0.05),
    CI90_upr = quantile(wg_g_m2, 0.95),
    RightCI90_lwr = quantile(wg_g_m2, 0.10),
    left_moe_p = (g_m2 - RightCI90_lwr) / g_m2,
    smpl_obj = ifelse(left_moe_p < 0.25,
                      "Pass",
                      "NO"
    )
  )

print(wg_mass_m2_2021, n = Inf)


# Mass per area by refuge =====

### Mass of all species by refuge =====
# Values should be equal to mass/area in the last line of each LIT in the
# cumulative table.

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
          CI90_lwr = ~round(quantile(.x, 0.05), 2),
          CI90_upr = ~round(quantile(.x, 0.95), 2),
          RightCI90_lwr = ~round(quantile(.x, 0.10), 2)
        )
      )
    )
  ))


lit_stats <- lit_mass %>%
  dplyr::select(LIT,
                stats) %>%
  unnest(cols = stats)

t(lit_stats)














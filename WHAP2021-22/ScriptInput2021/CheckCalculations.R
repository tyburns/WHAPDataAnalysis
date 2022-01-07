library(tidyverse)
# run WHAP_Main_Script_2021-22.Rmd first (no need to run bootstraps)
# plot yield components to diagnose
# This script was made in response to comments that yields of wg in MDC 
# were lower than expected based on field experience.
ggplot(data = qdt_wg,
       aes(x = mass_g_m2,
           group = LIT,
           color  = stratum)) +
  geom_histogram() +
  facet_wrap(~LIT) # extreme oultier in SLW

slw_outl_qdt <- qdt_wg %>%
  dplyr::filter(mass_g_m2 > 1000) %>%
  pluck("GlobalID")

qdt_wg_check <- qdt0 %>%
  dplyr::filter(vernacularName == "Watergrass" & stratum != "") %>%
  pivot_wider(names_from = measurementType,
              values_from = measurementValue) %>%
  arrange(GlobalID_seed) %>%
  dplyr::select(GlobalID,
                LIT,
                quadratSize,
                stratum,
                nSeedHeads,
                f2t_length_mm,
                sh_length_mm) %>%
  dplyr::filter(sh_length_mm < 250) %>% # remove seed heads beyond model scope
  mutate(qdt_m2 = ifelse(quadratSize == "15x15", 0.15^2,
                         ifelse(quadratSize == "5x5", 0.05^2, 0.10)),
         n_seed_head_m2 = nSeedHeads / qdt_m2,
         sh_emerged_wg = f2t_length_mm / sh_length_mm,
         sh_mass_mg = exp(wgc[1] +
                            wgc[2] * sh_length_mm / 10 + # divide by 10 because
                            wgc[3] * sh_emerged_wg + # model data was in cm
                            wgc[4] * f2t_length_mm / 10) - 20) %>%
  group_by(GlobalID,
           LIT,
           stratum) %>%
  summarise(n_seed_head_m2 = mean(n_seed_head_m2, na.rm = TRUE),
            sh_mass_mg = mean(sh_mass_mg, na.rm = TRUE),
            f2t_length_mm = mean(f2t_length_mm, na.rm = TRUE),
            sh_length_mm = mean(sh_length_mm, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(mass_g_m2 = n_seed_head_m2 * sh_mass_mg / 1000) %>%
  dplyr::select(GlobalID,
                LIT,
                stratum,
                sh_length_mm,
                f2t_length_mm,
                n_seed_head_m2,
                sh_mass_mg,
                mass_g_m2)


str(qdt_wg_check)

# All components of mass_g_m2 prior to weighting by proportion of area
# seem to be correct; at least they are very similar in all refuges.
# 
# We should compare the proportion of areas among refuges and between years
# for MDC. I think the proportions of area covered by wg were much lower in 2021
# 


ggplot(data = qdt_wg_check,
       aes(x = sh_length_mm,
           y = n_seed_head_m2,
           group = LIT,
           color  = stratum)) +
  geom_point() +
  facet_wrap(~LIT) # extreme oultier in SLW (removed)

ggplot(data = qdt_wg_check,
       aes(x = sh_length_mm,
           y = mass_g_m2,
           group = LIT,
           color  = stratum)) +
  geom_point() +
  ylim(0, 200) +
  facet_wrap(~LIT) # extreme oultiers in SLW not shown

ggplot(data = qdt_wg_check,
       aes(x = sh_length_mm,
           group = LIT,
           color = LIT)) +
  geom_density(aes(fill = LIT), alpha = 0.4)

ggplot(data = qdt_wg_check,
       aes(x = sh_mass_mg,
           group = LIT,
           color = LIT)) +
  geom_density(aes(fill = LIT), alpha = 0.4)


ggplot(data = qdt_wg_check,
       aes(x = n_seed_head_m2,
           group = LIT,
           color = LIT)) +
  geom_density(aes(fill = LIT), alpha = 0.4)

ggplot(data = qdt_wg_check,
       aes(x = mass_g_m2,
           group = LIT,
           color = LIT)) +
  geom_density(aes(fill = LIT), alpha = 0.4)



wg_qdt_means


yield_comp_2021 <- full_join(su_mass2021, p_Area) %>%
  mutate(wg_g_m2_check = wg_g_m2_a.Low * p_Watergrass_a.Low +
           wg_g_m2_b.Med * p_Watergrass_b.Med +
           wg_g_m2_c.High * p_Watergrass_c.High)

plot(wg_g_m2 ~ wg_g_m2_check, yield_comp_2021)
abline(0, 1)

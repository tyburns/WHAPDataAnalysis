# Precision of subunit estimates

## setup

library(tidyverse)
library(magrittr)
library(car)

# Define invBoxCox transform =====
invBoxCox <- function(x, lambda) {
  if (lambda == 0) exp(x) else (lambda * x + 1) ^ (1 / lambda)}

invBoxCox <- Vectorize(invBoxCox)


# ALL SPECIES =====

## Read files =====

su_mass2019 <- read_rds("WHAP2019-20/Output2019/qdt_mass_g_m2_2019.rds") %>%
  mutate(year = 2019) %>%
  rename(unitName = Unit_Name,
         subunitName = Subunit_Name)

su_mass2020 <- read_rds("WHAP2020-21/Output2020/qdt_mass_g_m2_2020.rds") %>%
  mutate(year = 2020) %>%
  rename(unitName = Unit_Name,
         subunitName = Subunit_Name)

su_mass2021 <- read_rds("WHAP2021-22/Output2021/qdt_mass_g_m2_2021.rds") %>%
  mutate(year = 2021)

su_mass <- bind_rows(su_mass2019, su_mass2020, su_mass2021)

## Calculate precision =====

su_mass %<>%
  mutate(st_moe_prop = 0.5 * (CI80upr_st - CI80lwr_st) / st_g_m2,
         wg_moe_prop = 0.5 * (CI80upr_wg - CI80lwr_wg) / wg_g_m2,
         sw_moe_prop = 0.5 * (CI80upr_sw - CI80lwr_sw) / sw_g_m2)

## Calculate sample sizes =====

### Area sampled in VP and CP =====



### Number of quadrats ====

## Visualize =====
### Swamp Timothy ====
st_p_graph <- su_mass %>%
  dplyr::filter(st_g_m2 > 0) %>%
  dplyr::select("st_moe_prop") %>%
  na.omit() %>%
  arrange(st_moe_prop) %>%
  mutate(prop_ci = 1:n()/n())

plot(prop_ci ~ st_moe_prop,
     type = "l",
     log = "x",
     main = "Swamp Timothy",
     xlab = "X: CI half-width as proportion of mean",
     ylab = "Proportion of CI half-widths smaller than X",
     data = st_p_graph)
abline(v = c(0.2, 0.5),
       col = c("green", "red"))

### Watergrass ====
wg_p_graph <- su_mass %>%
  dplyr::filter(wg_g_m2 > 0) %>%
  dplyr::select("wg_moe_prop") %>%
  na.omit() %>%
  arrange(wg_moe_prop) %>%
  mutate(prop_ci = 1:n()/n())

plot(prop_ci ~ wg_moe_prop,
     type = "l",
     log = "x",
     main = "Watergrass",
     xlab = "X: CI half-width as proportion of mean",
     ylab = "Proportion of CI half-widths smaller than X",
     data = wg_p_graph)
abline(v = c(0.2, 0.5),
       col = c("green", "red"))

### Smartweed ====
sw_p_graph <- su_mass %>%
  dplyr::filter(sw_g_m2 > 0) %>%
  dplyr::select("sw_moe_prop") %>%
  na.omit() %>%
  arrange(sw_moe_prop) %>%
  mutate(prop_ci = 1:n()/n())

plot(prop_ci ~ sw_moe_prop,
     type = "l",
     log = "x",
     main = "Smartweed",
     xlab = "X: CI half-width as proportion of mean",
     ylab = "Proportion of CI half-widths smaller than X",
     data = sw_p_graph)
abline(v = c(0.2, 0.5),
       col = c("green", "red"))


# SWAMP TIMOTHY ====
## Quadrat source of variation ====
# Note that I use 90% confidence
# Note that year is set by whatever analysis is current in the environment
st.boot <- bootstraps(qdt_st,
                      times = 3000,
                      apparent = TRUE,
                      strata = LIT_Strat) %>%
  rename(qdt_st = splits) %>%
  mutate(qdt_st = map(qdt_st, ~analysis(.x)),
         
         st_lambda = map_dbl(qdt_st,
                         ~unname(
                           coef(
                             powerTransform(
                               lm(mass_g_m2 ~ LIT_Strat,
                                  na.action = na.exclude,
                                  data = .x)
                             ), round = TRUE
                           )
                         )
         ),
         st_lm3 = map2(.x = st_lambda,
                       .y = qdt_st,
                       ~lm(bcPower(mass_g_m2, lambda = .x) ~ LIT_Strat,
                       na.action = na.exclude,
                       data = .y)),
         emns = map(st_lm3, ~as_tibble(emmeans(.x, "LIT_Strat", level = 0.90)))
  )


st_boot_CI <- st.boot %>%
  dplyr::select(id,
                st_lambda,
                emns) %>%
  unnest(cols = c(emns)) %>%
  dplyr::filter(id != "Apparent") %>%
  dplyr::select(LIT_Strat,
                st_lambda,
                emmean) %>%
  mutate(emmean = map2_dbl(.x = emmean,
                           .y = st_lambda,
                           ~invBoxCox(.x, .y))) %>%
  group_by(LIT_Strat) %>%
  summarise(boot_mn = mean(emmean, na.rm = TRUE),
            boot_lwr = quantile(emmean, 0.05),
            boot_upr = quantile(emmean, 0.95),
            boot_moe = (boot_upr - boot_lwr) / 2)


qn_st <- qdt_st %>%
  group_by(LIT, stratum) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(LIT_Strat = paste(LIT, stratum, sep = "_")) %>%
  dplyr::select(LIT_Strat,
                n)


st_lm3_CI <- st.boot %>%
  dplyr::select(id,
                st_lambda,
                emns) %>%
  unnest(cols = c(emns)) %>%
  dplyr::filter(id == "Apparent") %>%
  mutate(lm3_mn = invBoxCox(emmean, st_lambda),
         lm3_lwr = invBoxCox(lower.CL, st_lambda),
         lm3_upr = invBoxCox(upper.CL, st_lambda),
         lm3_moe = (lm3_upr - lm3_lwr) / 2) %>%
  dplyr::select(LIT_Strat,
                lm3_mn,
                lm3_lwr,
                lm3_upr,
                lm3_moe)


st_CIs <- full_join(st_boot_CI, st_lm3_CI) %>%
  full_join(qn_st) %>%
  dplyr::select(LIT_Strat,
                boot_mn,
                lm3_mn,
                boot_moe,
                lm3_moe,
                n,
                boot_lwr,
                lm3_lwr,
                boot_upr,
                lm3_upr) %>%
  mutate(avg_moe = (boot_moe + lm3_moe) / 2,
         moe_prop = avg_moe / lm3_mn)


ggplot(data = st_CIs,
       aes(y = moe_prop,
           x = n)) +
  geom_point() + 
  geom_smooth()


# Area source of variation

moe_area <- 2 * (v_Area_c %>%
  dplyr::select(starts_with("p_")) %>%
  sqrt() %>%
  as.matrix())


  '/'(., as.matrix(p_Area %>%
        dplyr::select(starts_with("p_"))) * 2)





























# Compare mass between years within subunits

# Yearly files are put together inside this module.
# In the future, the complete file would be directly downloaded from server.

library(knitr)
library(tidyverse)


# Read and join files
gm2_2019 <-read.csv("qdt_mass_g_m2_2019.txt") %>%
    dplyr::select(-X) %>%
    mutate(year = 2019)

gm2_2020 <-read.csv("qdt_mass_g_m2_2020.txt") %>%
    dplyr::select(-X) %>%
    mutate(year = 2020)

whap_su_years <- bind_rows(gm2_2019, gm2_2020) %>%
    mutate(year = factor(year)) %>%
    dplyr::select(LIT,
                  year,
                  subunit_ID,
                  wg_g_m2,
                  st_g_m2,
                  tot_g_m2,
                  wg_mass_m2_var,
                  st_mass_m2_var,
                  tot_mass_m2_var) %>%
    pivot_wider(id_cols = c(LIT,
                            subunit_ID),
                names_from = year,
                values_from = c(wg_g_m2,
                                st_g_m2,
                                tot_g_m2,
                                wg_mass_m2_var,
                                st_mass_m2_var,
                                tot_mass_m2_var)) %>%
    mutate(
        wg_diff = wg_g_m2_2020 - wg_g_m2_2019,
        wg_diff_se = sqrt(wg_mass_m2_var_2019 + wg_mass_m2_var_2020),
        wg_p_value = round(1 - pt(q = abs(wg_diff) / wg_diff_se, df = 30), 5),
        st_diff = st_g_m2_2020 - st_g_m2_2019,
        st_diff_se = sqrt(st_mass_m2_var_2019 + st_mass_m2_var_2020),
        st_p_value = round(1 - pt(q = abs(st_diff) / st_diff_se, df = 30), 5),
        tot_diff = tot_g_m2_2020 - tot_g_m2_2019,
        tot_diff_se = sqrt(tot_mass_m2_var_2019 + tot_mass_m2_var_2020),
        tot_p_value = round(1 - pt(q = abs(tot_diff) / tot_diff_se, df = 30), 5)
        ) %>%
    dplyr::select(LIT,
                  subunit_ID,
                  wg_diff,
                  wg_diff_se,
                  wg_p_value,
                  st_diff,
                  st_diff_se,
                  st_p_value,
                  tot_diff,
                  tot_diff_se,
                  tot_p_value) %>%
    dplyr::filter(rowSums(is.na(.)) != ncol(.) - 2)

kable(whap_su_years,
      digits = c(0, 2, 5, 0, 2, 5, 0, 2, 5),
      caption = "Differences mass 2020 - mass 2019 for all subunits measured in both years. Comparisons are made as single t-tests with an assumed df = 30.")


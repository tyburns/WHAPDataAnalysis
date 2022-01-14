# ESTIMATION OF VARIANCE OF PROPORION OF AREAS =====
# AS A FUNCITON OF AREA OF SAMPLED POLYGON

# Calculate deleted averages for each subunit and observation. Deleted
# averages are those that do not include the observation whose deviation
# from the average is to be calculated.
# 
# Calculate deleted deviations from the average for each observation.
# Study distribution of deleted deviations as a function of absolute and
# relative area of each observation.


# Get all data for all years =====

vpcp2019 <- read_rds("WHAP2019-20/Output2019/vpcp2019.rds") %>%
  mutate(year = 2019,
         p_Smartweed_a.Low = 0,
         p_Smartweed_b.Med = 0,
         p_Smartweed_c.High = 0) %>%
  rename(unitName = Unit_Name,
         subunitName = Subunit_Name,
         p_Other_cover_NA = p_noFood,
         areaVisible_ac = AreaVisible) %>%
  rename_with(~gsub("_wg", "_Watergrass", .x, fixed = TRUE)) %>%
  rename_with(~gsub("_st", "_Swamp_Timothy", .x, fixed = TRUE))
  
vpcp2020 <- read_rds("WHAP2020-21/Output2020/vpcp2020.rds") %>%
  mutate(year = 2020,
         p_Smartweed_a.Low = 0,
         p_Smartweed_b.Med = 0,
         p_Smartweed_c.High = 0) %>%
  rename(unitName = Unit_Name,
         subunitName = Subunit_Name,
         p_Other_cover_NA = p_noFood,
         areaVisible_ac = AreaVisible) %>%
  rename_with(~gsub("_wg", "_Watergrass", .x, fixed = TRUE)) %>%
  rename_with(~gsub("_st", "_Swamp_Timothy", .x, fixed = TRUE))


vpcp2021 <- read_rds("WHAP2021-22/Output2021/vpcp2021.rds") %>%
  mutate(year = 2021)

str(vpcp2019)
str(vpcp2020)
str(vpcp2021)

vpcp_19_21 <- bind_rows(vpcp2019,
                        vpcp2020,
                        vpcp2021) %>%
  arrange(year, LIT, subunit_ID, level)

str(vpcp_19_21)

# Calculate deleted averages =====

p_Area_19_21 <- vpcp_19_21 %>%
  group_by(year, LIT, subunit_ID) %>%
  summarise(across(starts_with("p_"),
                   ~weighted.mean(.x, areaVisible_ac)),
            .groups = "drop") %>%
  arrange(year, LIT, subunit_ID) %>%
  rename_with(~gsub("^p_", "avg_p_", .x))

tot_area_vis <- vpcp_19_21 %>%
  group_by(year, LIT, subunit_ID) %>%
  summarize(t_area_vis = sum(areaVisible_ac), .groups = "drop")


vpcp_19_21 %<>%
  full_join(p_Area_19_21) %>%
  full_join(tot_area_vis)

Xbar <- as.matrix(vpcp_19_21 %>% dplyr::select(starts_with("avg_p_")))

sumW <- as.numeric(vpcp_19_21 %>% pluck("t_area_vis"))

wi <- as.numeric(vpcp_19_21 %>% pluck("areaVisible_ac"))

propi <- as.matrix(vpcp_19_21 %>% dplyr::select(starts_with("p_")))

# Check order of columns
colnames(Xbar)
colnames(propi)


dXbar <- as.data.frame((Xbar*sumW - wi * propi)/(sumW - wi))
names(dXbar) <- paste("d",names(dXbar), sep = "")

vpcp_19_21 %<>%
  bind_cols(dXbar)

vpcp_19_21  <- vpcp_19_21[, order(colnames(vpcp_19_21))]

str(vpcp_19_21)

# Calculate deleted residuals or deviations =====

resdls <- (vpcp_19_21 %>% dplyr::select(starts_with("p_"))) -
  (vpcp_19_21 %>% dplyr::select(starts_with("davg_p")))

names(resdls) <- paste("d", names(resdls), sep = "")

# subunit year groups with only one observations yield NaN for deleted averages
# so they have to be excluded. There was only one subunit SLW BeachLake 3 with
# only one estimation of proportion of areas.
vpcp_19_21 %<>%
  bind_cols(resdls) %>%
  na.omit()

# Calculate sample variance per group =====

vpcp_19_21 %<>%
  group_by(LIT, year, subunit_ID) %>%
  mutate(across(starts_with("dp_"), var, .names = "v_{.col}")) %>%
  ungroup()

# Visualize results =====

plot(density(vpcp_19_21$dp_Swamp_Timothy_a.Low))
hist(vpcp_19_21$dp_Swamp_Timothy_a.Low, breaks = 50)
hist(vpcp_19_21$dp_Swamp_Timothy_b.Med, breaks = 50)

ggplot(data = vpcp_19_21,
       aes(x = dp_Swamp_Timothy_a.Low,
           groups = level,
           color = level)) +
  geom_density(aes(fill = level),
               alpha = 0.5)

ggplot(data = vpcp_19_21,
       aes(x = dp_Swamp_Timothy_b.Med,
           groups = level,
           color = level)) +
  geom_density(aes(fill = level),
               alpha = 0.5)

ggplot(data = vpcp_19_21,
       aes(x = dp_Swamp_Timothy_c.High,
           groups = level,
           color = level)) +
  geom_density(aes(fill = level),
               alpha = 0.5)

ggplot(data = vpcp_19_21,
       aes(x = dp_Other_cover_NA,
           groups = level,
           color = level)) +
  geom_density(aes(fill = level),
               alpha = 0.5)


ggplot(data = vpcp_19_21,
       aes(x = dp_Watergrass_a.Low,
           groups = level,
           color = level)) +
  geom_density(aes(fill = level),
               alpha = 0.5)

ggplot(data = vpcp_19_21,
       aes(x = dp_Watergrass_b.Med,
           groups = level,
           color = level)) +
  geom_density(aes(fill = level),
               alpha = 0.5)

ggplot(data = vpcp_19_21,
       aes(x = dp_Watergrass_c.High,
           groups = level,
           color = level)) +
  geom_density(aes(fill = level),
               alpha = 0.5)

# There is a consistent tendency for CP estimates to have more frequent
# negative residuals than VP estimates, but all have the mode of 0.

# Relationship between group averages and variances =====

plot(v_dp_Watergrass_a.Low ~ davg_p_Watergrass_a.Low, vpcp_19_21)
plot(v_dp_Watergrass_b.Med ~ avg_p_Watergrass_b.Med, vpcp_19_21)

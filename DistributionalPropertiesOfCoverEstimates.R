# ESTIMATION OF VARIANCE OF PROPORION OF AREAS =====
# AS A FUNCITON OF AREA OF SAMPLED POLYGON

# Calculate deleted averages for each subunit and observation. Deleted
# averages are those that do not include the observation whose deviation
# from the average is to be calculated.
# 
# Calculate deleted deviations from the average for each observation.
# Study distribution of deleted deviations as a function of absolute and
# relative area of each observation.

library(compositions)
library(tidyverse)
library(magrittr)
library(Matrix)
library(psych)
library(corrplot)
library(tidymodels)

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

vpcp_19_21 <- vpcp_19_21[, order(colnames(vpcp_19_21))]

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

# Composition Approach =====

## Transform data into a compositions class =====

# Select columns and deal with zeroes
c_Area <- vpcp_19_21 %>%
  dplyr::select(starts_with("p_")) %>%
  as.matrix() %>%
`+`(replicate(10, abs(rnorm(dim(vpcp_19_21)[1],
                    mean = 0,
                    sd = 0.0001)))) %>%
  acomp()

class(c_Area)

## Estimate compositions for each subunit year =====

vpcp_19_21 %<>%
  mutate(su_yr = paste(subunit_ID, year, sep = "_"),
         wt = areaVisible_ac/t_area_vis,
         fpcf = 1 - t_area_vis / su.area_ac)

hist(vpcp_19_21$fpcf) # some have sampled areas larger than subunit area??!!

# Check area visible for errors

vpcp_19_21 %>%
  dplyr::filter(fpcf <= 0) %>%
  dplyr::select(year,
                subunit_ID,
                areaVisible_ac,
                t_area_vis,
                su.area_ac,
                fpcf) %>%
  print(n = Inf)

# In 2019-2021 there were 9 units where observed areas larger than unit.
# An fpcf of 0.00987654321 is imputed to those.

vpcp_19_21 %<>%
  mutate(fpcf = ifelse(fpcf <= 0.00987654321, 0.00987654321, fpcf))

p_Area_m0 <- lm(ilr(c_Area) ~ -1 + su_yr,
                data = vpcp_19_21)

p_Area_m1 <- lm(ilr(c_Area) ~ -1 + su_yr,
                weights = wt,
                data = vpcp_19_21)

p_Area_m2 <- lm(ilr(c_Area) ~ -1 + su_yr, # This is probably the correct one
                weights = areaVisible_ac,
                data = vpcp_19_21)


## Check distribution of residuals =====

rsdl.m0 <- p_Area_m0 %>%
  residuals(type = "pearson") %>%
  as_tibble()

opar <- par(mfrow = c(3,3))
for (i in names(rsdl.m0)) {
  plot(pluck(rsdl.m0, i) ~ vpcp_19_21$areaVisible_ac)}
par(opar)

rsdl.m2 <- p_Area_m2 %>%
  weighted.residuals() %>%
  as_tibble()

opar <- par(mfrow = c(3,3))
for (i in names(rsdl.m2)) {
  plot(pluck(rsdl.m2, i) ~ vpcp_19_21$areaVisible_ac)}
par(opar)

area_g <- cut(vpcp_19_21$areaVisible_ac, 5)

opar <- par(mfrow = c(3,3))
for (i in names(rsdl.m0)) {
  boxplot(pluck(rsdl.m0, i) ~ area_g)}
par(opar)

opar <- par(mfrow = c(3,3))
for (i in names(rsdl.m2)) {
  boxplot(pluck(rsdl.m2, i) ~ area_g)}
par(opar)



pairs.panels(rsdl.m0,
             lm = TRUE,
             rug = FALSE,
             method = "pearson")

pairs(residuals(p_Area_m2), col = as.factor(vpcp_19_21$level))

pairs.panels(residuals(p_Area_m2),
             lm = TRUE,
             rug = FALSE,
             method = "pearson")

## Explore variance of estimates =====

smpl_sz <- vpcp_19_21 %>%
  group_by(subunit_ID, year, su_yr) %>%
  summarise(n = n(),
            t_area_vis = mean(t_area_vis),
            su.area_ac = mean(su.area_ac),
            fpcf = mean(fpcf),
            .groups = "drop")

m0_smmry <- tidy(p_Area_m0) %>%
  dplyr::select(-c(statistic, p.value)) %>%
  mutate(model = "m0",
         subunit_ID = substr(term,
                             6,
                             str_length(term) - 5),
         year = substr(term,
                       str_length(term) - 3,
                       str_length(term)))

m1_smmry <- tidy(p_Area_m1) %>%
  dplyr::select(-c(statistic, p.value)) %>%
  mutate(model = "m1",
         subunit_ID = substr(term,
                             6,
                             str_length(term) - 5),
         year = substr(term,
                       str_length(term) - 3,
                       str_length(term)))

m2_smmry <- tidy(p_Area_m2) %>%
  dplyr::select(-c(statistic, p.value)) %>%
  mutate(model = "m2",
         subunit_ID = substr(term,
                             6,
                             str_length(term) - 5),
         year = substr(term,
                       str_length(term) - 3,
                       str_length(term)))

all_smmry <- bind_rows(m0_smmry, m1_smmry, m2_smmry) %>%
  mutate(year = as.numeric(year)) %>%
  full_join(smpl_sz) %>%
  mutate(c_se = std.error * sqrt(fpcf))

ggplot(data = all_smmry,
       aes(y = std.error,
           x = t_area_vis,
           group = model,
           color = model)) +
  geom_point() +
  facet_wrap(~response)


ggplot(data = all_smmry,
       aes(y = c_se,
           x = t_area_vis,
           group = model,
           color = model)) +
  geom_point() +
  facet_wrap(~response)

# Simulations Outline =====

# CALCULATIONS ASSUME THAT THERE ARE NO OVERLAP OF SPECIES INTHE ESTIMATES.
# WHEN SPECIES OVERLAP IN THE FIELD, OBSERVERS HAVE TO "MENTALLY" SEPARATE
# SPECIES INTO DIFFERENT AREAS AND CORRECT DENSITY ACCORDINGLY.
# THE OVERLAPPING SPECIES ISSUE IS OUTSTANDING.

# Areal compositions will be simulated for each year separately.
# Each subunit_ID will receive multiple simulations of composition.
# Each composition is a vector of 10 numbers: 3 strata * 3 species + other.
# Each set of estimates or simulation has one row per subunit_ID and one column per component.
# 
## Pseudocode =====
# 1. get vpcp file for current year.
# 2. Change zeroes to abs(rnorm(mean = 0, sd = 0.0001)).
# 3. Create acomp() object c_Area with all components.
# 4. Regress ilr(c_Area) against subunit_ID without intercept as ilr_model.
# 5. Get vcov1 from ilr_model and transform into cor.
#   Remember that dim(vcov) = n.subunits * (n.components - 1)
# 6. Calculate fpcf for each subunit and multiply by vcov1 diagonal.
# 7. Calculate corrected vcov where variances are multiplied by fpcf.
# 8. Create mvnorm simulations of estimates using corrected vcov.
# 9. Backstransform with ilrInv.
# 10. Combine with quadrat bootstrap samples to get simulations of mass/area


## Data for 2021 =====

vpcp2021 <- read_rds("WHAP2021-22/Output2021/vpcp2021.rds") %>% 
  as_tibble() %>%
  dplyr::select(LIT,
                subunit_ID,
                level,
                areaVisible_ac,
                su.area_ac,
                starts_with("p_")) %>%
  group_by(LIT, subunit_ID) %>%
  mutate(t_area_vis = sum(areaVisible_ac),
         fpcf = 1 - t_area_vis / su.area_ac,
         fpcf = ifelse(fpcf <= 0.00987654321, 0.00987654321, fpcf)) %>%
  ungroup()

acomp_2021 <- vpcp2021 %>%
  dplyr::select(starts_with("p_")) %>%
  as.matrix() %>%
  `+`(replicate(10, abs(rnorm(dim(vpcp2021)[1],
                              mean = 0,
                              sd = 0.0001)))) %>%
  acomp()

vpcp2021$a_comp <- acomp_2021

## Regress acomp() on subunit_ID

ilr_m2021 <- lm(ilr(a_comp) ~ -1 + subunit_ID,
                     data = vpcp2021)

ilr_m2021_smry <- tidy(ilr_m2021) %>%
  mutate(subunit_ID = gsub("subunit_ID", "", term),
         subunit_ID_Y = paste(subunit_ID,
                              response,
                              sep = "_")) %>%
  dplyr::select(-c(statistic, p.value, term))


## Get covariance matrix of estimates and apply fpcf

# Covariance matrix refers to coefficients for all subunits for each of the 10-1
# components as a single vector.
# All covariances between different units are zero. Only covariances between components within units are nonzero.

vcov_ilr <- vcovAcomp(ilr_m2021)
vcov_names <- gsub("subunit_ID", "", attr(vcov_ilr, "dimnames")[[3]])



# Simulations can be done one subunit at a time to simplify the vcov matrices.

vcov_ilr <- vcov_ilr[row.order, col.order]



fpcf_2021 <- vpcp2021 %>%
  dplyr::select(subunit_ID, fpcf) %>%
  unique()









## Identify arguments of rnorm.acomp

# Simulate data
# 
myvar <- matrix(c( # because of dimensions, this vcov must be in original units
  0.2, 0.1, 0.0,
  0.1, 0.2, 0.0,
  0.0, 0.0, 0.2),
  byrow = TRUE,
  nrow = 3)

mymean <- c(1, 1, 2)

rdat <- rnorm.acomp(100000,
                    mymean,
                    myvar)

class(rdat)

var(rdat)

mean(rdat)

ilrvar2clr(var(rdat))

variation2clrvar(myvar)

var(cbind(as.numeric(rdat[, 1]), as.numeric(rdat[, 2]), as.numeric(rdat[, 3])))




matrix(rnorm(5 * 2), ncol = 2) %*% 
                    chol(clrvar2ilr(var))


m2_vcov <- Matrix(as.matrix(vcov(ilr_model_2021)),
                  sparse = TRUE)

class(cov2cor(m1_vcov))

str(cov2cor(vcov(p_Area_m1)))
sum(cov2cor(vcov(p_Area_m1)) != 0)

image(cov2cor(vcov(p_Area_m1)))

length(coef(p_Area_m1))


# Number of observations per subunit year

with(vpcp_19_21, table(su_yr)) %>%
  hist(breaks = 20)



















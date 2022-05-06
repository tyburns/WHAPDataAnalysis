# ASSOCIATION OF MANAGEMENT AND SEED HEAD MASS PRODUCTION =======
# 
# As of May 2022 mowing was the only management recorded at the quadrat level.
# Other management actions can probably be addressed by this code if all 
# combinations of management factors are listed as separate "treatments" in the
# managementAction field of Survey 123.
# 
# Management action is available since 2021.
# 
# This script uses the qdt_yyyy.rds file as input.
# 

require(lme4, quietly = TRUE)
require(emmeans, quietly = TRUE)
require(car, quietly = TRUE)


qdt_2021 %>%
  group_by(
    managementAction,
    vernacularName
  ) %>%
  summarise(
    mass_g_m2 = mean(mass_g_m2,
                     na.rm = TRUE)
  )


lambda <- powerTransform(
  lmer(mass_g_m2 ~ managementAction:vernacularName +
         (1 | LIT / subunit_ID),
       data = qdt_2021)
) %>%
  coef(round = TRUE) %>%
  unname()


m1 <- lmer(bcPower(mass_g_m2, lambda) ~ managementAction:vernacularName +
             (1 | LIT / subunit_ID),
           data = qdt_2021)

summary(m1)

anova(m1)

manage_means <- emmeans(m1,
                        ~ managementAction:vernacularName,
                        type = "response")

pairs(manage_means)

plot(m1)






























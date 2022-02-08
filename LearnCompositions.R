## Learn properties of compositions analyses


fake_data <- data.frame(
  comp1 = c(0.001, 0.50, 0.90),
  comp2 = c(0.50, 0.10, 0.10),
  comp2 = c(0.50, 0.40, 0.001)
)

(fake_data <- vpcp2021 %>%
  dplyr::filter(subunit_ID == "KRN_4A_4A-1") %>%
  dplyr::select(p_Other_cover_NA, starts_with("p_Swamp")) %>%
  `+`(replicate(
    10, abs(
      rnorm(
        6,
        mean = 0.00,
        sd = 0.0001
      )
    )
  )))

wt <- vpcp2021 %>%
  dplyr::filter(subunit_ID == "KRN_4A_4A-1") %>%
  pluck("areaVisible_ac")


(fake_acomp <- acomp(fake_data))

mean(fake_acomp)

plot(fake_acomp)

(fake_ilr <- ilr(fake_acomp))

(avg_ilr <- coef(lm(fake_ilr ~ 1)))


(round(ilrInv(avg_ilr, orig = fake_acomp), 2))

(avg_acom <- fake_data %>%
    summarise(
      across(
        .cols = everything(),
        ~ weighted.mean(.x, w = wt)
      )
    )
)

diri_dat <- DR_data(fake_data)

# Using Dirichlet

library(DirichletReg)

diri_dat <- DR_data(fake_data)

diri_m1 <- DirichReg(diri_dat ~ 1,
                     weights = wt)

predict(diri_m1)


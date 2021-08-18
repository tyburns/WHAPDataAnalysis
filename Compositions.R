library(compositions)

# Analysis of cover proportions with ilr

# Vantage points
vp.acomp <- vps %>%
  dplyr::select(starts_with("p_")) %>%
  acomp()

vpcomp.m1 <- lm(ilr(vp.acomp) ~ subunit_ID, data = vps)

summary(comp.m1)

vp.pred <- ilrInv(predict(vpcomp.m1), orig = vp.acomp)

opar <- par(mfrow=c(7, 7),
            mar = c(2, 2, 1, 1),
            oma = c(4, 4, 0, 0))

for(i in 1:7){
  for(j in 1:7){
    plot(log(vp.pred[, i] / vp.pred[, j]),
         log(vp.acomp[, i] / vp.acomp[, j]),
         pch = ifelse(i != j, 1, ""))
    if( i== j) {text(x = 0, y = 0,
                  labels = colnames(vp.acomp)[i], cex = 1.5)
    } else {
      abline(a = 0, b = 1, col = "gray", lwd = 3)
    }
  }
}

par(opar)


# Circle plots
cp.acomp <- cps %>%
  dplyr::select(starts_with("p_")) %>%
  acomp()

cpcomp.m1 <- lm(ilr(cp.acomp) ~ subunit_ID, data = cps)

summary(cpcomp.m1)

cp.pred <- ilrInv(predict(cpcomp.m1), orig = cp.acomp)

opar <- par(mfrow=c(7, 7),
            mar = c(2, 2, 1, 1),
            oma = c(4, 4, 0, 0))

for(i in 1:7){
  for(j in 1:7){
    plot(log(cp.pred[, i] / cp.pred[, j]),
         log(cp.acomp[, i] / cp.acomp[, j]),
         pch = ifelse(i != j, 1, ""))
    if( i== j) {text(x = 0, y = 0,
                     labels = colnames(cp.acomp)[i], cex = 1.5)
    } else {
      abline(a = 0, b = 1, col = "gray", lwd = 3)
    }
  }
}

par(opar)
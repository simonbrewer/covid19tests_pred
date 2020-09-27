## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message = FALSE------------------------------------------------------------------------
set.seed(1234)
library(dplyr)
library(skimr)
library(caret)
library(gbm) ## For importance scores
library(randomForest)
library(ranger)
library(ggpubr)
library(vip)
library(pdp)


load("./covid19.RData")

dat <- dat %>%
  filter(!is.na(pcaseNew_lag))

dat$ltest_rate <- log(dat$test_rate+1e-5)

load("./covid19new.RData")

newdat <- newdat %>%
  filter(!is.na(pcaseNew_lag))

# dat <- dat %>%
#   filter(state %in% c("CT", "MI", "NY", "WA"))

## -------------------------------------------------------------------------------------------
f1 <- ltest_rate ~ lpState_popn + lpPop_o_60 + lpPop_m + lpPop_white + 
  lpPop_black + lpPop_AmIndAlNat + lpPop_asia + lpPop_NaHaPaIs +
  lIncome + lpBachelor + phospitals + pnursing + puniversities +
  pcaseNew_lag + daysSinceC + pdeathNew_lag + daysSinceD + hospRate + wday # + sTest

# f1 <- test_rate ~ pnursing +
#   pcaseNew + daysSinceC + pdeathNew + daysSinceD + hospRate

## -------------------------------------------------------------------------------------------
parGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 4)

dat$baseline <- ((dat$sTest * dat$pState_popn) / dat$Tot_pop) * 1e3

mod <- ranger(f1, data = dat)

pred <- predict(mod, newdat, predict.all = TRUE,
                verbose = TRUE, type = "response")

newdat$pred <- exp(apply(pred$predictions, 1, mean))
# newdat$cilo <- exp(apply(pred$predictions, 1, quantile, 0.025))
# newdat$cihi <- exp(apply(pred$predictions, 1, quantile, 0.975))

newdat$pred = newdat$pred - 1e-5

out <- newdat %>% 
  select(state, date, FIPS, Province_State, sFIPS, pred)

write.csv(out, "COVID19_tests_pred_ranger.csv", row.names = FALSE)

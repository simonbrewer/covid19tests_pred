## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message = FALSE------------------------------------------------------------------------
set.seed(1234)
library(dplyr)
library(lubridate)
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
# dat$daysSinceC[dat$daysSinceC < 0] <- 0
load("./covid19new.RData")

newdat <- newdat %>%
  filter(!is.na(pcaseNew_lag))
# newdat$daysSinceC[newdat$daysSinceC < 0] <- 0



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

## Case weights
ltr_hist <- hist(dat$ltest_rate, plot = FALSE,
                 seq(min(dat$ltest_rate), max(dat$ltest_rate), length.out = 100))
wgt_vec <- 1 / ltr_hist$counts
casewgt <- wgt_vec[cut(dat$ltest_rate, include.lowest = TRUE,
                       breaks = ltr_hist$breaks, labels = FALSE)]
## Build model
# mod <- ranger(f1, data = dat, num.trees = 500,
#               # importance = 'permutation',
#               case.weights = casewgt)
mod <- ranger(f1, data = dat, num.trees = 500,
              mtry = 6, min.node.size = 4,
              # importance = 'permutation',
              case.weights = casewgt)

save(mod, file = "rf_pred.RData")
## Predict
pred <- predict(mod, newdat, predict.all = TRUE,
                verbose = TRUE, type = "response")

newdat$pred <- exp(apply(pred$predictions, 1, mean))
# newdat$cilo <- exp(apply(pred$predictions, 1, quantile, 0.025))
# newdat$cihi <- exp(apply(pred$predictions, 1, quantile, 0.975))

newdat$pred = newdat$pred - 1e-5

out <- newdat %>% 
  select(state, date, FIPS, Province_State, sFIPS, pred)

write.csv(out, "COVID19_tests_pred_ranger.csv", row.names = FALSE)


stop()
fips = 53033
preds.sub <- newdat %>% 
  filter(FIPS == fips)
preds.sub$date <- ymd(preds.sub$date)
ggline(preds.sub, x = "date", y = "pred")

ggline(preds.sub, x = "date", y = "lpState_popn")
ggline(preds.sub, x = "date", y = "lpPop_o_60")
ggline(preds.sub, x = "date", y = "pnursing")
ggline(preds.sub, x = "date", y = "puniversities")
ggline(preds.sub, x = "date", y = "pcaseNew_lag")
ggline(preds.sub, x = "date", y = "daysSinceC")
ggline(preds.sub, x = "date", y = "pdeathNew_lag")
ggline(preds.sub, x = "date", y = "daysSinceD")
ggline(preds.sub, x = "date", y = "wday")

ggscatter(preds.sub, x = "daysSinceC", y = "pred")
ggscatter(preds.sub, x = "daysSinceD", y = "pred")
ggscatter(preds.sub, x = "pcaseNew_lag", y = "pred")
ggscatter(preds.sub, x = "pdeathNew_lag", y = "pred")
ggscatter(preds.sub, x = "wday", y = "pred")

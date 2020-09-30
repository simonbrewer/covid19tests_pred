## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message = FALSE------------------------------------------------------------------------
set.seed(1234)
library(dplyr)
library(skimr)
library(lubridate)
library(caret)
library(gbm) ## For importance scores
library(randomForest)
library(ranger)
library(ggpubr)
library(vip)
library(pdp)
library(DALEX)
library(DALEXtra)

load("./covid19.RData")

dat <- dat %>%
  filter(!is.na(pcaseNew_lag))

dat$ltest_rate <- log(dat$test_rate+1e-5)

load("./covid19new.RData")

newdat <- newdat %>%
  filter(!is.na(pcaseNew_lag))

# dat <- dat %>%
#   filter(state %in% c("CT", "MI", "NY", "WA"))

dat$baseline <- ((dat$sTest * dat$pState_popn) / dat$Tot_pop) * 1e3

## -------------------------------------------------------------------------------------------
f1 <- ltest_rate ~ lpState_popn + lpPop_o_60 + lpPop_m + lpPop_white + 
  lpPop_black + lpPop_AmIndAlNat + lpPop_asia + lpPop_NaHaPaIs +
  lIncome + lpBachelor + phospitals + pnursing + puniversities +
  pcaseNew_lag + daysSinceC + pdeathNew_lag + daysSinceD + hospRate + wday # + sTest

dat2 <- dat %>% select(ltest_rate, lpState_popn, lpPop_o_60, lpPop_m, lpPop_white, 
  lpPop_black, lpPop_AmIndAlNat, lpPop_asia, lpPop_NaHaPaIs,
  lIncome, lpBachelor, phospitals, pnursing, puniversities,
  pcaseNew_lag, daysSinceC, pdeathNew_lag, daysSinceD, hospRate, wday)

# f1 <- test_rate ~ pnursing +
#   pcaseNew + daysSinceC + pdeathNew + daysSinceD + hospRate

## -------------------------------------------------------------------------------------------
parGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 4)

mod <- ranger(f1, data = dat2)

pred <- predict(mod, newdat, predict.all = TRUE,
                verbose = TRUE, type = "response")

newdat$pred <- exp(apply(pred$predictions, 1, mean))
# newdat$cilo <- exp(apply(pred$predictions, 1, quantile, 0.025))
# newdat$cihi <- exp(apply(pred$predictions, 1, quantile, 0.975))

newdat$pred = newdat$pred - 1e-5

# out <- newdat %>% 
#   select(state, date, FIPS, Province_State, sFIPS, pred)
# 
# write.csv(out, "COVID19_tests_pred_ranger.csv", row.names = FALSE)

explain_rf <- explain(mod,
                      data = dat2[ ,-1],
                      y = dat$ltest_rate,
                      label = "RF regression",
                      colorize = FALSE)

vi <- variable_importance(explain_rf)
plot(vi)

ve_p <- variable_profile(explain_rf, variables = "wday", type = "partial")
ve_p$color = "_label_"
plot(ve_p)

ve_p <- variable_profile(explain_rf, variables = "daysSinceC", type = "partial")
ve_p$color = "_label_"
plot(ve_p)

ve_p <- variable_profile(explain_rf, variables = "pnursing", type = "partial")
ve_p$color = "_label_"
plot(ve_p)

ve_p <- variable_profile(explain_rf, variables = "lIncome", type = "partial")
ve_p$color = "_label_"
plot(ve_p)

bd <- variable_attribution(explain_rf, dat2[1,], type = "break_down")
plot(bd)

wy <- newdat %>% 
  filter(state == "WY") %>% 
  select(date, lpState_popn, lpPop_o_60, lpPop_m, lpPop_white, 
         lpPop_black, lpPop_AmIndAlNat, lpPop_asia, lpPop_NaHaPaIs,
         lIncome, lpBachelor, phospitals, pnursing, puniversities,
         pcaseNew_lag, daysSinceC, pdeathNew_lag, daysSinceD, hospRate, wday) %>%
  group_by(date) %>%
  summarise(across(lpState_popn:hospRate, mean, na.rm= TRUE))

ddate <- ymd(wy$date)
wy$wday <- wday(ddate, week_start = 1, label = TRUE)

bd <- variable_attribution(explain_rf, wy[1,], type = "break_down")
plot(bd)
pdf("wy_1.pdf")
plot(bd)
dev.off()

sc <- newdat %>% 
  filter(state == "SC") %>% 
  select(date, lpState_popn, lpPop_o_60, lpPop_m, lpPop_white, 
         lpPop_black, lpPop_AmIndAlNat, lpPop_asia, lpPop_NaHaPaIs,
         lIncome, lpBachelor, phospitals, pnursing, puniversities,
         pcaseNew_lag, daysSinceC, pdeathNew_lag, daysSinceD, hospRate, wday) %>%
  group_by(date) %>%
  summarise(across(lpState_popn:hospRate, mean, na.rm= TRUE))

ddate <- ymd(sc$date)
sc$wday <- wday(ddate, week_start = 1, label = TRUE)

bd <- variable_attribution(explain_rf, sc[1,], type = "break_down")
plot(bd)
pdf("sc_1.pdf")
plot(bd)
dev.off()


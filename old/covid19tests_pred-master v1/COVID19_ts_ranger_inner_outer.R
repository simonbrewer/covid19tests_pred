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

# dat <- dat %>%
#   filter(state %in% c("CT", "MI", "NY", "WA"))

# dat <- dat[sample(nrow(dat), 5000), ]
dat$ltest_rate <- log(dat$test_rate+1e-5)
## -------------------------------------------------------------------------------------------
f1 <- ltest_rate ~ lpState_popn + lpPop_o_60 + lpPop_m + lpPop_white + 
  lpPop_black + lpPop_AmIndAlNat + lpPop_asia + lpPop_NaHaPaIs +
  lIncome + lpBachelor + phospitals + pnursing + puniversities +
  pcaseNew_lag + daysSinceC + pdeathNew_lag + daysSinceD + hospRate + wday # + sTest

# f1 <- test_rate ~ pnursing + wday +
#   pcaseNew_lag + daysSinceC + pdeathNew_lag + daysSinceD + hospRate
# 
## -------------------------------------------------------------------------------------------
## Outer
nfolds <- 4
myfolds <- createFolds(dat$test_rate, k = nfolds)

## -------------------------------------------------------------------------------------------
## Inner set up
ctrl <- trainControl(method = "cv", 
                     number = 3,
                     verboseIter = TRUE)
# ctrl <- trainControl(method = "none", verboseIter = TRUE)

## -------------------------------------------------------------------------------------------
## Parameter grid
parGrid = expand.grid(mtry = 2:6, 
                      splitrule = "variance", 
                      min.node.size = 4:8)

mod_mae <- rep(NA, nfolds)
base_mae <- rep(NA, nfolds)

for (i in 1:nfolds) {
  training <- dat[-myfolds[[i]],]
  testing  <- dat[ myfolds[[i]],]
  
  ltr_hist <- hist(training$ltest_rate, plot = FALSE,
                   seq(min(training$ltest_rate), max(training$ltest_rate), length.out = 100))
  wgt_vec <- 1 / ltr_hist$counts
  casewgt <- wgt_vec[cut(training$ltest_rate, include.lowest = TRUE,
                         breaks = ltr_hist$breaks, labels = FALSE)]
  
  ## ----results='hide', message=FALSE----------------------------------------------------------
  modFit <- train(
    f1,
    data = training,
    method = "ranger",
    ## Center and scale the predictors for the training
    ## set and all future samples.
    preProc = c("center", "scale"),
    ## increase parameter set
    tuneGrid = parGrid,
    ## added:
    num.trees = 500,
    weights = casewgt,
    #importance = 'permutation',
    trControl = ctrl
  )
  modFit
  
  
  ## -------------------------------------------------------------------------------------------
  ggplot(modFit)
  
  ## -------------------------------------------------------------------------------------------
  pred_test <- predict(modFit, newdata = testing)
  # mod_results[mod_id, 2:4] <- postResample(pred = pred_test, obs = testing$test_rate)
  print(postResample(pred = exp(pred_test), obs = testing$test_rate))
  mod_mae[i] <- postResample(pred = exp(pred_test), obs = testing$test_rate)[3]
  
  ## -------------------------------------------------------------------------------------------
  testing$baseline <- testing$pState_popn
  testing$baseline <- ((testing$sTest * testing$pState_popn) / testing$Tot_pop) * 1e3
  print(postResample(pred <- testing$baseline, obs = testing$test_rate))
  base_mae[i] <- postResample(pred <- testing$baseline, obs = testing$test_rate)[3]
  
}


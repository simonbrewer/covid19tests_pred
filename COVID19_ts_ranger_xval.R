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


load("./outputs/covid19.RData")
dat <- dat %>%
  filter(!is.na(pcaseNew_lag))

# dat <- dat %>%
#   filter(state %in% c("CT", "MI", "NY", "WA"))

# dat <- dat[sample(nrow(dat), 5000), ]
dat$ltest_rate <- log(dat$test_rate+1e-1)

# f1 <- test_rate ~ pnursing + wday +
#   pcaseNew_lag + daysSinceC + pdeathNew_lag + daysSinceD + hospRate
# 
## -------------------------------------------------------------------------------------------
inTrain <- createDataPartition(
  y = dat$test_rate,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training <- dat[ inTrain,]
testing  <- dat[-inTrain,]

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


## -------------------------------------------------------------------------------------------
## Training model
## Case weights
ltr_hist <- hist(training$ltest_rate, plot = FALSE,
                 seq(min(training$ltest_rate), max(training$ltest_rate), length.out = 100))
wgt_vec <- 1 / ltr_hist$counts
casewgt <- wgt_vec[cut(training$ltest_rate, include.lowest = TRUE,
                       breaks = ltr_hist$breaks, labels = FALSE)]
## Build model
# mod <- ranger(f1, data = dat, num.trees = 500,
#               # importance = 'permutation',
#               case.weights = casewgt)
mod <- ranger(f1, data = training, num.trees = 500,
              mtry = 6, min.node.size = 4,
              # importance = 'permutation',
              case.weights = casewgt)


## Training set
rf.pred <- predict(mod, training)
plot.df <- data.frame(obs = training$ltest_rate, pred = rf.pred$predictions)
p1 <- plot.df %>%
  filter(obs > log(1e-1)) %>%
  ggscatter(x = "obs", y = "pred", alpha = 0.5, size = 0.75,
            xlab = "log test rate (obs)", ylab = "log test rate (pred)",
            main = "Training set predictions", subtitle = "Random forest") +
  geom_abline(intercept = 0, slope = 1)
pdf("COVID19_rf_obs_pred_train.pdf")
print(p1)
dev.off()

## Test set
rf.pred <- predict(mod, testing)
plot.df <- data.frame(obs = testing$ltest_rate, pred = rf.pred$predictions)


#obs vs. pred plot
ggplot(filter(plot.df,obs > log(1e-1)), aes(x=obs, y=pred)) + 
  geom_point(size = 0.5, shape = 1)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("log test rate (obs)")+
  ylab("log test rate (pred)")+
  theme_bw()

ggsave('./figures/obs_vs_pred.jpg'
       ,width = 7 
       ,height = 5 
       ,units = "cm"
       ,dpi = 300
       ,device = "jpg"
)
dev.off()

# p2 <- plot.df %>%
#   filter(obs > log(1e-1)) %>%
#   ggscatter(x = "obs", y = "pred", alpha = 0.5, size = 0.75,
#             xlab = "log test rate (obs)", ylab = "log test rate (pred)",
#             main = "Test set predictions", subtitle = "Random forest") +
#   geom_abline(intercept = 0, slope = 1)
# pdf("COVID19_rf_obs_pred_test.pdf")
# print(p2)
# dev.off()

## -------------------------------------------------------------------------------------------
## Full model
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






# modFit <- randomForest(f1, dat = training, do.trace = TRUE)
# plot(modFit)
# varImpPlot(modFit)
# partialPlot(modFit, pred.data = training, x.var = "wday")

## -------------------------------------------------------------------------------------------
# ctrl <- trainControl(method = "repeatedcv", 
#                      number = 5,
#                      repeats = 3,
#                      verboseIter = TRUE)
ctrl <- trainControl(method = "cv", 
                     number = 3,
                     verboseIter = TRUE)
# ctrl <- trainControl(method = "none", verboseIter = TRUE)


## -------------------------------------------------------------------------------------------
parGrid = expand.grid(mtry = 2:6, 
                      splitrule = "variance", 
                      min.node.size = 4:8)
# parGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 4)


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
  importance = 'permutation',
  trControl = ctrl
)
modFit


## -------------------------------------------------------------------------------------------
ggplot(modFit)


## -------------------------------------------------------------------------------------------
# plot(varImp(modFit))


## -------------------------------------------------------------------------------------------
pred_test <- predict(modFit, newdata = testing)
# mod_results[mod_id, 2:4] <- postResample(pred = pred_test, obs = testing$test_rate)
print(postResample(pred = exp(pred_test), obs = testing$test_rate))


## -------------------------------------------------------------------------------------------
testing$baseline <- testing$pState_popn
testing$baseline <- ((testing$sTest * testing$pState_popn) / testing$Tot_pop) * 1e3
print(postResample(pred <- testing$baseline, obs = testing$test_rate))


## -------------------------------------------------------------------------------------------
mydf = data.frame(obs = rep(testing$test_rate, 2), 
                  pred = c(testing$baseline, exp(pred_test)),
                  type = rep(c("base", "pred"), each = length(testing$test_rate)))
p1 = ggscatter(mydf, x = "obs", y = "pred", col = "type",
               main = paste0("COVID 19 testing (ranger, raw-scale)")) + 
  geom_abline()
print(p1)

# ggsave("./results/COVID19_ranger_cv.pdf", p1)

stop()

ltr_hist <- hist(dat$ltest_rate, plot = FALSE,
                 seq(min(dat$ltest_rate), max(dat$ltest_rate), length.out = 100))
wgt_vec <- 1 / ltr_hist$counts
casewgt <- wgt_vec[cut(dat$ltest_rate, include.lowest = TRUE,
                       breaks = ltr_hist$breaks, labels = FALSE)]
mod <- ranger(f1, data = dat, num.trees = 500,
              mtry = 6, min.node.size = 4,
              importance = 'permutation',
              case.weights = casewgt)

# vip(mod)
plot(partial(mod, pred.var = "daysSinceC"))

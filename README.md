<h1> Welcome to COVID19test</h1>

In response to the lack of nationwide COVID-19 testing data at the county-level, we estimated the daily number of diagnostic tests administered for each county in the contiguous United States. We integrated state- and county-level data available from various sources to train a random forest model that predicts the number of tests. 

Our [estimates](outputs/COVID19_tests_pred_ranger.csv) are available from this GitHub repository, which his updated monthly with the newest testing numbers. The units of our estimates are the number of tests per 1,000 population within a county.

**Currently, this repository contains estimates for the period of 4/14/2020 - 9/8/2020.**

We hope that our efforts contribute towards understanding and mitigating the COVID-19 pandemic, especially in the absence of a unified strategy for reporting the number of tests.

Codes are available for full transparency. The six scripts below have to be run in order.

1. COVID19_ts_data_wrangle.r (gather county-level testing data and predictors, cleaning)
2. COVID19_conus_data_wrangle.r (gather nationwide predictors, cleaning) 
3. COVID19_preprocess_data.r (more preprocessing)
4. COVID19_ts_ranger_pred.r (build random forest model, predict)
5. COVID19_ts_ranger_inner_outer.r (model evaluation (mean absolute error), compare to basemodel)
6. COVID19_ts_graphs.r (create plots)
7. COVID19_ts_ranger_statecv.r (statewise cross-validation)

**Current predictions (version 2, 4/14/2020 - 9/8/2020)**
![alt text](figures/pred_cumulative.jpg)

As new testing data becomes available, we retrain our model and predict for the entire (now extended) study period:

[Version 1: 4/14/2020 - 8/8/2020](old/covid19tests_pred-master%20v1/COVID19_tests_pred_ranger.csv)
[Version 2 (current): 4/14/2020 - 9/8/2020](outputs/COVID19_tests_pred_ranger.csv)

Therefore, predictions for the previous study period may change. It follows a summary of the latest changes.


**Time series plot: Number of predicted tests nationwide (Version 1, Version 2)**
![alt text](figures/diff_time_series.jpg)


**Map of relative change between Version 1 and Version 2 (4/14/2020 - 8/8/2020)**
![alt text](figures/change.jpg)

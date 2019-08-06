library(muforecast)
data(Canada)

source("./R/DMF.R")
source("./R/Evaluate_DMF.R")


data = as.MFMatrix(Canada,
                   IDcol = "ID",
                   Tcol = "Time",
                   Vcol = "Value")
train = data[Time < as.Date("1999-01-01")]
test = data[Time >= as.Date("1999-01-01")]

pred1 = DMF(train, horizon = 8, lag = 4, regression_method = "glmnet",
            non_negative = FALSE, first_na = "keep", change_cap = NULL,
            glmnet_params = list(family = "gaussian", alpha = 0.5,
                                 tune_method = "CV", select_lambda = "1se"))


pred2 = DMF(train, horizon = 8, lag = 4, regression_method = "random_reg",
            non_negative = FALSE, first_na = "keep", change_cap = NULL,
            random_reg_params = list(colsample = 0.4, n_reg = 500,
                                     interaction = 2))
DMF.evalDMF(pred2, test, evaluate_method = "MAPE")



pred3 = DMF(train, horizon = 8, lag = 4, regression_method = "rf_glmnet",
            non_negative = FALSE, first_na = "keep", change_cap = NULL,
            rf_glmnet_params = list(ntree = 50, maxnodes = 5,
                                    alpha = 0.5, select_lambda = "1se"))

pred4 = DMF(train, horizon = 8, lag = 4, regression_method = "xgboost",
            non_negative = FALSE, first_na = "keep", change_cap = NULL,
            xgboost_params = list(nrounds = 100, colsample_bytree = 1,
                                  booster = "gbtree", gamma = 0.1))
DMF.evalDMF(pred4, test, evaluate_method = "MAPE")



DMF.evalDMF(pred1, test, evaluate_method = "MAPE")
DMF.evalDMF(pred3, test, evaluate_method = "MAPE")
DMF.evalDMF(pred4, test, evaluate_method = "MAPE")








require(MASS)
require(randomForest)
require(regboost)
require(magrittr)
require(caret)

std_fun = function(x){
  mean_x = apply(x, 2, mean)
  std_x = apply(x, 2, sd)
  return( list(mean_x = mean_x, std_x = std_x) )
}

std_transform = function(x, std_info){
  ( (t(x) - std_info$mean_x) / std_info$std_x ) %>% t() %>% return()
}


std_inverse_transform = function(x, std_info){
  ( (t(x) * std_info$std_x) + std_info$mean_x ) %>% t() %>% return()
}

require(hdi)

data("riboflavin")
str(riboflavin)

df = train_test_split(Boston[,1:13]%>%as.matrix(), Boston[,14]%>%as.matrix(), 0.75)

df = read.csv("housing.csv")
df = df[complete.cases(df),]
df_x = model.matrix(~.-1, df[, c(1:8,10)])
df_y = df[, 9]

df = train_test_split(df_x%>%as.matrix(), df_y%>%as.matrix(), 0.8)

#df = train_test_split(Boston[,1:13]%>%as.matrix(), Boston[,14]%>%as.matrix(), 0.75)


df = read.csv("housing.csv")
df = df[complete.cases(df),]
df_x = model.matrix(~.-1, df[, c(1:8,10)])
df_y = df[, 9]
df = train_test_split(df_x%>%as.matrix(), df_y%>%as.matrix(), 0.8)
x = df$x_train; y = df$y_train

std_fit = std_fun(x)
x = std_transform(x, std_fit)
###%>% as.matrix()
df$x_test = std_transform(df$x_test, std_fit) ###%>% as.matrix()

y = as.matrix(y)
std_fit = std_fun(y)
y = std_transform(y, std_fit)
###%>% as.matrix()



data("riboflavin")
str(riboflavin)
df = train_test_split(riboflavin$x %>% as.matrix(), riboflavin$y %>% as.matrix(), 0.8)
x = df$x_train; y = df$y_train

## randomRegression
rr = randomReg.fit(x = x, y = y, subsample = 1,  colsample = 0.6, holdvar = "default",
                   n_reg = 500, lambda = 6, weight_metric = "rmse",
                   intercept = TRUE, interaction = 1)

#pred = std_inverse_transform(predict(rr, newx = df$x_test)$pred, std_fit)
#cat( "Reg rmse for Boston: ", metric_fun(y = df$y_test, y_hat = pred, metric = "rmse"))
cat( "Reg rmse for Boston: ", predict(rr, newx = df$x_test, newy = df$y_test)$rmse)

##glmnet
cv_glm = cv.glmnet(x=x, y=y, alpha = 1)
cat("glmnet:", metric_fun(y = df$y_test, predict(cv_glm, newx = df$x_test), metric = "rmse"))


## randomForest
rf = randomForest(x = x, y = y, ntree = 500)
cat( "Forest rmse for Boston: ", metric_fun(y = df$y_test, y_hat = predict(rf, newdata = df$x_test), metric = "rmse"))

## regboost
rr.control = list(colsample = 0.4, subsample = 1, holdvar = "default", n_reg = 500, lambda = 6,
                  weight_metric = "rmse", interaction = 1)
rf.control = list()

rgb = regboost.train(x = x, y = y, n_rounds = 5, eta = 0.8, rr_start = TRUE,
                     rr.control = rr.control, rf.control = rf.control,
                     watchlist=list(xval = df$x_test, yval = df$y_test))

predict(rgb, newx = df$x_test, newy = df$y_test)$rmse


# 0.5588282

xgb_train <- xgb.DMatrix(data = x, label = y)
xgb_test <- xgb.DMatrix(data = df$x_test, label = df$y_test)

params <- list(booster = "gbtree", eta=0.1, gamma=0, max_depth=1,
               min_child_weight=1, subsample=1, colsample_bytree=0.5)

params <- list(booster = "gblinear", eta=0.1, alpha = 0, lambda = 0, colsample_bytree=1)


xgbcv <- xgb.cv( params = params, data = xgb_train, nrounds = 100, nfold = 5,
                 showsd = T, stratified = T, print_every_n = 10, early_stopping_round = 10, maximize = F)

xgb1 = xgb.train (params = params, data = xgb_train, nrounds = xgbcv$best_iteration,
                  watchlist = list(val=xgb_test,train=xgb_train),
                  print_every_n = 10, early_stopping_round = 10, maximize = F , eval_metric = "rmse")

xgbpred <- predict(xgb1, df$x_test)
cat( "Forest rmse for Boston: ", metric_fun(y = df$y_test, y_hat = xgbpred, metric = "rmse"))




p = 12
x = matrix(rnorm(100*p), 100, p)
beta = c(c(1,2,3), rep(0,p-3))
Truth = x %*% beta
y = x %*% beta + rnorm(100)



rr = randomReg.fit(x = x, y = y, subsample = 1,  colsample = 0.5, holdvar = "default",
                   n_reg = 500, lambda = 0.05, weight_metric = "rmse",
                   intercept = TRUE, interaction = 1)
rr_fit = predict(rr, newx=x)$pred

lm_fit = lm.fit(x,y)$fitted

metric_fun(y = Truth, y_hat = rr_fit, metric="rmse")
metric_fun(y = Truth, y_hat = lm_fit, metric="rmse")






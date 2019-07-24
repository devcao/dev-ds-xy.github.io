

require(MASS)
require(randomForest)

df = train_test_split(Boston[,1:13]%>%as.matrix(), Boston[,14]%>%as.matrix(), 0.75)

df = read.csv("housing.csv")
df = df[complete.cases(df),]
df_x = model.matrix(~.-1, df[, c(1:8,10)])
df_y = df[, 9]

df = train_test_split(df_x%>%as.matrix(), df_y%>%as.matrix(), 0.8)
x = df$x_train; y = df$y_train


## randomRegression
rr = randomReg.fit(x = x, y = y, colsample = 0.7, holdvar = "default",
                   n_reg = 500, lambda = 0.01, weight_metric = "rmse",
                   intercept = TRUE, interaction = 3)

cat( "Reg rmse for Boston: ", predict(rr, newx = df$x_test, newy = df$y_test)$rmse)


## randomRegression
rr = randomReg.fit(x = x, y = y, colsample = 1, holdvar = "default",
                   n_reg = 500, lambda = 0.01, weight_metric = "rmse",
                   intercept = TRUE, interaction = 1)

cat( "Reg rmse for Boston: ", predict(rr, newx = df$x_test, newy = df$y_test)$rmse)


## randomRegression
rr = randomReg.fit(x = x, y = y, colsample = 0.4, holdvar = "default",
                   n_reg = 500, lambda = 0.01, weight_metric = "rmse",
                   intercept = TRUE, interaction = 5)

cat( "Reg rmse for Boston: ", predict(rr, newx = df$x_test, newy = df$y_test)$rmse)




rr = randomReg.fit(x = x, y = y, colsample = 0.4, holdvar = "default",
                   n_reg = 500, lambda = 0.01, weight_metric = "rmse",
                   intercept = TRUE, interaction = 4)

cat( "Reg rmse for Boston: ", predict(rr, newx = df$x_test, newy = df$y_test)$rmse)




## randomForest
rf = randomForest(x = x, y = y, ntree = 500)
cat( "Forest rmse for Boston: ", metric_fun(y = df$y_test, y_hat = predict(rf, newdata = df$x_test), metric = "rmse"))

## regboost
rr.control = list(colsample = 0.7, subsample = 1, holdvar = "default", n_reg = 500, lambda = 0.01,
                  weight_metric = "rmse", interaction = 2)
rf.control = list()

rgb = regboost.train(x = x, y = y, n_rounds = 5, eta = 0.8, rr_start = FALSE,
                     rr.control = rr.control, rf.control = rf.control,
                     watchlist=list(xval = df$x_test, yval = df$y_test))

predict(rgb, newx = df$x_test, newy = df$y_test)$rmse




xgb_train <- xgb.DMatrix(data = x, label = y)
xgb_test <- xgb.DMatrix(data = df$x_test, label = df$y_test)

params <- list(booster = "gbtree", eta=0.1, gamma=0.1, max_depth=8,
               min_child_weight=1, subsample=1, colsample_bytree=1)


xgbcv <- xgb.cv( params = params, data = xgb_train, nrounds = 100, nfold = 5,
                 showsd = T, stratified = T, print_every_n = 10, early_stopping_round = 10, maximize = F)

xgb1 = xgb.train (params = params, data = xgb_train, nrounds = xgbcv$best_iteration,
                  watchlist = list(val=xgb_test,train=xgb_train),
                  print_every_n = 10, early_stopping_round = 10, maximize = F , eval_metric = "rmse")

#predict (xgb1,xgb_train) %>% plot(y)

xgbpred <- predict(xgb1, df$x_test)


cat( "Forest rmse for Boston: ", metric_fun(y = df$y_test, y_hat = xgbpred, metric = "rmse"))









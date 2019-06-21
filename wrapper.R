
#' Fit randomRegression model
#'
#' @param x matrix or data.frame contains all training features.
#' @param y matrix or data.frame specify the training labels.
#' @param mtry int, number of features in use for each linear regression.
#' @param holdvar vector, specify features whihc will always be included in each linear regression, default -1. If holdvar = -1, no features will be hold.
#' @param n_reg int, total number of regressions, default 500.
#' @param lambda double, l2/ridge penalty factor for each regression,
#' @param weight_metric string, must be ("rmse", "mape", "none"), specify different methods for weighted ensembleing. "none" indicates no weights.
#' @param intercept bool, whether fit with intercept or not
#' @param interaction int, if less than 2, then no interaction term included. If > 2, specify the order of interactions. Default 2, all pairwise interactions included.
#' @return An randomRegression object.
#' @examples
#'
randomReg.fit = function(x, y, mtry, holdvar = -1, n_reg = 500,
                         lambda = 0.01, weight_metric = NULL, intercept = TRUE, interaction = 2){


  if (interaction > 1){

    formula = paste0("~0+", paste(paste0(".^", 2:interaction, collapse="+"))) %>% as.formula()
    x = model.matrix(formula, data = x %>% as.data.frame())

  }

  x = as.matrix(x); y = as.matrix(y)


  n = nrow(x); p = ncol(x);
  if (length(y) != n) stop("length of response must be the same as predictors")

  lambda = as.double(lambda); n_reg=as.integer(n_reg);
  if(missing(mtry)){
    mtry = sqrt( p )
  }else if (mtry > p || mtry <1){
    warning("invalid mtry: reset to within valid range")
    mtry <- max(2, min(p, round(mtry)))
  }
  mtry = as.integer(mtry)

  if(n_reg < 1){
    warning("invalid n_reg: reset to default: 500")
    n_reg = 500L
  }


  if (any(is.na(x))) stop("NA not permitted in predictors")
  if (any(is.na(y))) stop("NA not permitted in response")


  ####

  rd_reg = randomRegression_fit(x = x, y = y,
                       mtry = mtry,
                       n_reg = n_reg,
                       holdvar = holdvar,
                       lambda = lambda,
                       weight_metric = weight_metric,
                       intercept = intercept)

  ####
  cl <- match.call()
  cl[[1]] <- as.name("randomRegression")
  out = list(call = cl,
             rd_reg = rd_reg,
             interaction = interaction)

  class(out) = "randomRegression"
  return(out)
}


#' Predict for randomRegression model
#'
#' @param object A randomRegression object
#' @param newx matrix or data.frame contains all testing features.
#' @param newy matrix or data.frame specify the testing labels. Could be NULL.
#' @return A list of predictions and rmse. If newy is NULL, only predicted values returned.
#' @examples
#'
predict.randomRegression = function(object, newx, newy){

  if (!inherits(object, "randomRegression"))
    stop("object not of class randomRegression")

  newx = as.matrix(newx)


  rd_obj = object[["rd_reg"]]

  if(object$interaction > 1) {
    formula = paste0("~0+", paste(paste0(".^", 2:object$interaction, collapse="+"))) %>% as.formula()
    newx = model.matrix(formula, data = newx %>% as.data.frame())
  }

  pred = randomRegression_predict(rd_obj, newx)

  if (!missing(newy)) {
    newy = as.matrix(newy)
    rmse = metric_fun(newy, pred, metric = "rmse")
  }else{
    rmse = NULL
  }

  return( list(pred = pred, rmse = rmse) )

}



#' Cross Validation engine for randomRegression model
#'
#' @param x matrix or data.frame contains all training features.
#' @param y matrix or data.frame specify the training labels.
#' @param mtry int, number of features in use for each linear regression.
#' @param nfolds int, number of folds for cross validation, default 5.
#' @param n_threads int, number of threads for parallel computing # Under developing
#' @param ...
#' @return A vector contains RMSE for each cv folds.
#' @examples
#'
cv4_randomReg = function(x, y, mtry = 5, nfolds = 5, n_threads = -1, ...){
  # TODO:
  # Arguments:
  # Output:
  n = nrow(x)
  p = ncol(x)
  foldid = createFolds(1:n, k = nfolds)
  # TODO: add surpport for parallel computing
  lapply(1:nfolds, function(fold){
    val_x = x[foldid[[fold]], ]
    val_y = y[foldid[[fold]]]
    train_x = x[-foldid[[fold]], ]
    train_y = y[-foldid[[fold]]]

    rr_fit = randomReg.fit(x = train_x, y = train_y, mtry = mtry, ...)
    predict(rr_fit, newx = val_x, newy = val_y)$rmse

  }) %>% unlist %>% return()

}

#' CV Tuning mtry for randomRegression model
#'
#' @param x matrix or data.frame contains all training features.
#' @param y matrix or data.frame specify the training labels.
#' @param mtry_grid vector, specify candidate values of mtry.
#' @param nfolds int, number of folds for cross validation, default 5.
#' @param n_threads int, number of threads for parallel computing # Under developing
#' @param plot_cv bool, if true, cross validation plot will be drawn.
#' @param ...
#' @return A list of cv results and best mtry.
#' @examples
#'
tune4_randomReg = function(x, y, nfolds = 5, mtry_grid, plot_cv = TRUE, n_threads = -1, ...){
  # TODO:
  # Arguments:
  # Output:

  cv_res = lapply(X = mtry_grid, FUN=cv4_randomReg, x = x, y = y, nfolds = nfolds, ...)

  cv_mean = cv_res %>% lapply(mean) %>% unlist()
  cv_sd = cv_res %>% lapply(sd) %>% unlist()
  cvup = cv_mean + cv_sd
  cvlo = cv_mean - cv_sd
  cv_result = data.frame(mtry = mtry_grid, cvm = cv_mean, cvsd = cv_sd, cvup = cvup, cvlo = cvlo)

  cv_min = min(cv_mean)
  cv_1se = cv_min + sd(cv_sd)
  mtry_min = mtry_grid[which.min(cv_mean)]

  if(plot_cv){
    p <- ggplot(cv_result, aes(x=mtry, y=cvm)) +
      geom_point(size = 4, colour = "red")+
      geom_errorbar(aes(ymin=cvlo, ymax=cvup), width=.2, colour = "grey",
                    position=position_dodge(0.05)) +
      geom_vline(xintercept = mtry_min, linetype = "dashed", color = "grey") +
      theme(legend.position = "none")
    print(p)
  }

  return(list(cv_result = cv_result, mtry_min = mtry_min))


}


#' Gradient Boosting using randomForest
#'
#' @param x matrix or data.frame contains all training features.
#' @param y matrix or data.frame specify the training labels.
#' @param test_x matrix or data.frame specify the testing features.
#' @param n_rounds int, number of gradient boosting rounds.
#' @param ...
#' @return A vector of predictions for test features
#' @examples
#'
rfboost = function(x, y, test_x, n_rounds = 5, ntree = 500, ...){

  pred = 0
  for(i in 1:n_rounds){
    rf_fit = randomForest(x = x, y = y, ntree = ntree, ...)
    y = y - rf_fit$predicted
    pred = pred + predict(rf_fit, test_x)
  }

  return(pred)


}


#' Gradient Boosting using randomForest + randomRegression
#'
#' @param x matrix or data.frame contains all training features.
#' @param y matrix or data.frame specify the training labels.
#' @param test_x matrix or data.frame specify the testing features.
#' @param reg_mtry: int, mtry for randomRegression.
#' @param n_rounds int, number of gradient boosting rounds.
#' @param ...
#' @return A vector of predictions for test features
#' @examples
#'
random_boosting = function(x, y, test_x, reg_try, holdvar, n_reg, n_round, ...){
  # TODO:
  # Argument:
  # Output:
  rr_fit = randomRegression(x = x, y = y, mtry = reg_try, n_reg = n_reg, holdvar = holdvar, test_x = test_x)
  res = y - rr_fit$fitted
  pred = rr_fit$pred

  rfb_pred = rfboost(x = x, y = res, test_x, n_rounds = 5, ntree = 500)

  pred = pred + rfb_pred
  return(pred)
}






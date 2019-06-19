require(lars)
require(glmnet)


extract_lars_null <- function(x, y, intercept = FALSE){
  
  X_scaled <- scale(x)

  lars.out <- lars(X_scaled, y, type = "lasso", 
                   intercept = intercept, use.Gram = FALSE, normalize=TRUE)
  lambda.hat <- sort(c(lars.out$lambda,0),decreasing = FALSE)
  
  return(list(y = y,
              X_scaled = X_scaled,
              lars.out = lars.out,
              lambda.hat = lambda.hat,
              intercept = intercept))
}

extract_lars_alter <- function(lars_null, whichCov){
  
  X_scaled = lars_null$X_scaled; y = lars_null$y;
  lambda.hat = lars_null$lambda.hat; lars.out = lars_null$lars.out;
  
  lars.j.out <- lars(X_scaled[, -whichCov], y, type = "lasso", 
                       intercept = lars_null$intercept, use.Gram = FALSE, normalize=TRUE)
  lambda.j.hat <- sort(c(lars.j.out$lambda,0), decreasing = FALSE)
    
  ## remove the unoverlapping part (test)
  
  if(lambda.hat[1] != lambda.j.hat[1]){
    leftmost = c(lambda.hat[1], lambda.j.hat[1])
    whichone = which.max(leftmost)
    if(whichone ==1){
      lambda.j.hat = lambda.j.hat[lambda.j.hat >= lambda.hat[1]]
      lambda.j.hat = c(lambda.hat[1], lambda.j.hat)
    }else{
      lambda.hat = lambda.hat[lambda.hat >= lambda.j.hat[1]]
      lambda.hat = c(lambda.j.hat[1], lambda.hat)
    }
  }
  
  union.lambda <- sort(unique(c(lambda.hat,lambda.j.hat)), decreasing = FALSE)
  beta.j.hat <- matrix(0, length(union.lambda), p)
  beta.hat <- predict(lars.out, s=union.lambda, type="coef", mode="lambda")$coefficients
  beta.j.hat[, -whichCov] <- predict(lars.j.out, s=union.lambda, type="coef", mode="lambda")$coefficients

  #beta.hat <- coef(lars.out, lam = union.lambda)
  #beta_val <- coef(lars.j.out, lam = union.lambda)

  return(list(union.lambda = union.lambda, beta.hat = beta.hat, beta.j.hat = beta.j.hat)) 
  
}


extract_lars(x, y, intercept = FALSE){
  p = ncol(x)
  lars_null = extract_lars_null(x, y, intercept = FALSE)
  out = lapply(1:p, extract_lars_alter, lars_null = lars_null)
  return(out)
}

locoVarImp <- function(path_obj, s = 2, t = 2){
  return( unlist(lapply(path_obj,locoVarImp_Engine, s = s, t = t)) )
}

  
locoVarImp_Engine <- function(obj, s = 2, t = 2){

  M <- length(obj$union.lambda)
  Delta <- obj$beta.j.hat - obj$beta.hat
  Delta_1 <- Delta[-M, ]
  Delta_2 <- Delta[-1, ]
  Lambda <- diff(obj$union.lambda)
  equal_sign_indicator <- Delta_2 * Delta_1 < 0
  Epsilon <- 1/(s+1) * Lambda * abs( (Delta_2 ^ (s+1) - ((-1) ^ equal_sign_indicator) * Delta_1 ^ (s+1)) / (Delta_2 - Delta_1) )

  if(s == t){
    return( (sum(Epsilon, na.rm = TRUE)) ^ (1/t) )
  }else{
    return( sum(rowSums(Epsilon) ^ (t/s), na.rm = TRUE) ^ (1/t) )
  }
  
}



extract_enet <- function(x, y, whichCov, betaNULL = 0, nlambda = 1000,  alpha = 1, family = "gaussian", intercept = FALSE){
  # Extract Path info
  #
  # Args:
  # X,Y: design matrix and response vector
  # which.covariate: if is a vector, indicating which covariate we will be computing; if is a list: then do multiple testing.
  # 
  # normalize: argguments of lars 
  # betaNULL: same size and same data type with which.covariate, specify the null hypothesis H0: beta = betaNULL. 
  # Returns:
  # A list of lambda vector, original path, and the LOCO path
  #
  #
  n <- nrow(x)
  p <- ncol(x)
  
  ################# Here we go ! ! ! #######################################################  
  
  ##### extract the path #######
  
  if (length(whichCov) != length(betaNULL)){
    
    stop("Length of variables being tested must equal the length of their Null hypothesis")
    
  }else{
    
    multiTest <- length(whichCov) > 1
    
  }
  
  # simultanoues testing 
  if(multiTest){
    
    #### ajust Y by using Y - X_j * beta_j_NULL - X_k * beta_k_NULL - ...
    adjust.X <- rowSums( t( t(x) * betaNULL  ) )    
    adjust.Y <- as.vector( y - adjust.X )
    X_scaled = scale(x)
    
    ## TODO
    max_lam = NA
    
    net.out <- glmnet(X_scaled, adjust.Y, nlambda = nlambda, 
                      lambda.min.ratio = 0.001, alpha = alpha, family = family, intercept = intercept)
    lambda.hat <- sort(net.out$lambda, decreasing = FALSE)
    
    
    net.j.out <- glmnet(X_scaled[, -whichCov], adjust.Y, nlambda = nlambda, 
                        lambda.min.ratio = 0.001, alpha = alpha, family = family, intercept = intercept)
    lambda.j.hat <- sort(net.j.out$lambda, decreasing = FALSE)
    
    
    
    
    # indivdual testing   
  }else if(!multiTest){  #indivdual test
    
    adjust.Y <- as.vector( y - betaNULL * x[,whichCov] )
    X_scaled <- scale(x)
    
    ## TODO
    net.out <- glmnet(X_scaled, adjust.Y, nlambda = nlambda, 
                      lambda.min.ratio = 0.001, alpha = alpha, family = family, intercept = intercept)
    lambda.hat <- sort(net.out$lambda, decreasing = FALSE)
    
    
    net.j.out <- glmnet(X_scaled[, -whichCov], adjust.Y, nlambda = nlambda, lambda.min.ratio = 0.001, alpha = alpha, family = family, intercept = intercept)
    lambda.j.hat <- sort(net.j.out$lambda, decreasing = FALSE)
    
    
    
  }else{
    stop("wrong input of multiTest, must be boolean")
  } 
  
  
  minLam <- min(lambda.hat, lambda.j.hat)
  
  lambda.hat = lambda.hat[lambda.hat > minLam]
  
  lambda.j.hat = lambda.hat[lambda.hat > minLam]
  
  union.lambda <- sort(unique(c(lambda.hat,lambda.j.hat)), decreasing = FALSE)
  
  beta.j.hat <- Matrix(0, length(union.lambda), p)  # sparse Matrix
  
  beta.hat <- predict(net.out, s=union.lambda, type="coef")
  beta.hat <- t( beta.hat[-1, ] )  # we don't need intercept
  
  
  beta.j.tmp <- predict(net.j.out, s=union.lambda, type="coef")
  beta.j.hat[, -whichCov] <- t( beta.j.tmp[-1, ] ) # we don't need intercep
  
  
  return(list(union.lambda = union.lambda, beta.hat = beta.hat, beta.j.hat = beta.j.hat)) 
  
}



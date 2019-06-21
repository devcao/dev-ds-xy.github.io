//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <string>
#include <stdexcept>
#include <omp.h>

using namespace Rcpp;
using namespace std;
using namespace arma;


// [[Rcpp::export]]
arma::mat ridge_solver(arma::mat x, 
                       arma::mat y,
                       double lambda){
  
  mat d = lambda * eye(x.n_cols, x.n_cols);
  mat beta = inv_sympd(x.t() * x + d) * x.t() * y;
  return beta;
}



//[[Rcpp::export]]
double metric_fun(vec y, vec y_hat, std::string metric){
  if (metric == "rmse"){
    return sqrt( mean( square((y - y_hat)) ) ); 
  }else if (metric == "mse"){
    return mean( square((y - y_hat)) );
  }else if (metric == "mae"){
    return mean( abs(y - y_hat) );
  }else if (metric == "mape"){
    y.replace(0, 1e-15);
    return  mean( abs( (y - y_hat) / y ) );
  }else if (metric == "mspe"){
    y.replace(0, 1e-15);
    return sqrt( mean( square( (y - y_hat) / y ) ) );
  }else if (metric == "rmsle"){
    return sqrt ( mean( square(log1p(1 + y) - log1p(1 + y_hat)) ) );
  }else if (metric == "xentropy"){
    return mean( y % log(y_hat) + (1-y) % log(1-y_hat) );
  }else if (metric == "none"){
    return 1;
  }else{
    throw std::invalid_argument( "unsupported metric" );
  }
  return -1; 
}

// [[Rcpp::export]]
List randomRegression_fit(arma::mat x, 
                          arma::mat y,
                          int mtry,
                          uvec holdvar,
                          int n_reg = 500,
                          double lambda = 0.01,
                          string weight_metric = "rmse",
                          bool intercept = true){
  //Function sample = Environment("package:base")["sample"];
  
  int n = x.n_rows; int p = x.n_cols;
  if(mtry <= 0){
    mtry = ceil( std::sqrt(p) );
  }
  
  List betaList(n_reg); List xList(n_reg); List yList(n_reg); List fittedList(n_reg); List OOB_pred(n_reg);
  vec err(n_reg); vec fitted_val = zeros(n); 
  vec w(n_reg); vec oob_err(n_reg); 
  mat beta;
  
  for (int i=0; i<n_reg; ++i){
    
    IntegerVector id1 = Rcpp::seq(0,n-1); IntegerVector id2 = Rcpp::seq(0,p-1);
    NumericVector id1_numType=as<NumericVector>(id1); NumericVector id2_numType=as<NumericVector>(id2);
    NumericVector obsId_numType = sample(id1_numType, n, true); vec varId_vec = sample(id2_numType, mtry, false); 
    
    vec oob_index = setdiff(id1_numType, obsId_numType);
    
    vec obsId_vec = obsId_numType;
    
    uvec boot_index = conv_to<uvec>::from(obsId_vec);
    uvec boot_var = conv_to<uvec>::from(varId_vec);
    
    uvec uoob_index = conv_to<uvec>::from(oob_index);
    
    
    if ( !any(holdvar == -1) ){
      boot_var = unique(join_cols(boot_var, holdvar));
    }
      
    mat x_try = x.submat(boot_index, boot_var);
    vec y_try = y.elem(boot_index);
    
    mat x_oob = x.submat(uoob_index, boot_var);
    vec y_oob = y.elem(uoob_index);
    
    
    if (intercept){
      x_try = join_rows(ones(x_try.n_rows), x_try);
      vec beta_full = zeros(p+1);
      
      xList[i] = x_try;
      yList[i] = y_try;
      beta = ridge_solver(x_try, y_try, lambda);
      beta_full.elem(boot_var + 1) = beta.rows(1, beta.n_rows-1);
      beta_full.row(0) = beta.row(0);
      betaList[i] = beta_full; 
      
      x_oob = join_rows(ones(x_oob.n_rows), x_oob);
      
    }else{
      vec beta_full = zeros(p);
      
      xList[i] = x_try;
      yList[i] = y_try;
      beta = ridge_solver(x_try, y_try, lambda);
      
      beta_full.elem(boot_var) = beta;
      betaList[i] = beta_full; 
    }
    
    vec fitted = x_try * beta;
    vec oob_pred = x_oob * beta;
    err(i) = metric_fun(y_try, fitted, weight_metric);
    oob_err(i) = metric_fun(y_oob, oob_pred, weight_metric);
    
  
    //fittedList[i] = fitted;
    
  }
  
  vec err_inv = 1 / oob_err;
  w = err_inv / sum(err_inv);
  
  
  return List::create(_["x"] = xList,
                      _["y"] = yList,
                      _["inSample_err"] = err,
                      _["oob_err"] = oob_err,
                      _["w"] = w,
                      _["beta"] = betaList,
                      _["intercept"] = intercept,
                      _["n_reg"] = n_reg,
                      _["n"] = n,
                      _["p"] = p);
  
}


// [[Rcpp::export]]
vec randomRegression_predict(List randomReg,
                             mat xnew){
  int n_reg = randomReg["n_reg"];
  vec w = randomReg["w"];
  bool intercept = randomReg["intercept"];
  
  List betaList = randomReg["beta"];
  
  
  vec pred = zeros(xnew.n_rows);
  
  if (intercept){
    xnew = join_rows(ones(xnew.n_rows), xnew);
  }
  
  for (int i=0; i<n_reg; ++i){
    mat beta = betaList[i];
    pred += (xnew * beta) * w[i];
  }
  
  
  return pred;
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/

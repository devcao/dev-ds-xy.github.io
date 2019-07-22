n <- 100
p <- 4
p0 <- 2

beta <- c(rep(1,p0),rep(0,p-p0)) 



X <- (matrix(rnorm(n*p),nrow=n))

Y <- (X %*% beta + rnorm(n))





library(lars)



lars.out <- lars(X,Y,type="lasso",intercept=FALSE)

beta.hat <- coef(lars.out)

lambda.hat <- lars.out$lambda



# remove one variable at a time and see what happens to the lasso solution path:



beta.J.hat <- array(0,dim=c(p,p,p))

lambda.J.hat <- matrix(0,p,p)

for(j in 1:p)
  
{
  
  lars.out <- lars(X[,-j],Y,type="lasso")
  
  beta.J.hat[,-j,j] <- coef(lars.out)
  
  lambda.J.hat[-p,j] <- lars.out$lambda
  
}





# Make plots of each leave-one-variable-out solution path with all-variables soluation path overlaid.

# At the same time compute the "distance" between each leave-one-variable-out solution path and the all-variables solution path (still needs to be coded).


par(mfrow=c(2,2),mar=c(0,0,0,0))



for(j in 1:p)
  
{
  
  plot(NA,ylim=range(beta.J.hat,beta.hat),xlim=range(lambda.J.hat,lambda.hat),xaxt="n",yaxt="n")
  
  
  for(k in 1:p)
    
  {
    #j=1;k=1
    lines(beta.J.hat[,k,j]~lambda.J.hat[,j], lwd = 2, lty=3, col = "red")
    
    lines(beta.hat[,k]~c(lambda.hat,0),lwd = 2)
    xx = c(lambda.J.hat[,j], rev(c(lambda.hat,0)))
    yy = c(beta.J.hat[,k,j], rev(beta.hat[,k]))
    polygon(xx, yy, col = "gray", border = NULL)
    
  }
  
}

library(readxl)
library(plyr)
library(dplyr)
library(modeest)
library(truncnorm)
library(invgamma)
library(psych)

noOfData = 1240

# Reading the data into y and X from .Rdata file created by q2.r
q2_result <- read.table("~/Desktop/working/q2_result.Rdata", quote="\"", comment.char="")
Y <- q2_result[,1]
X <- q2_result[,2:16]
X = as.matrix(X)
X = cbind(1,X)

colnames(X)[1] <- "Intercept"
for(i in 2:16){
  str=paste0("x",i)
  colnames(X)[i] <- str
}

# Summary Report
# print(summary(Y))
# print(summary(X))

###################### Q3.a #######################
# Function to get likelihood for given Y, X and beta.
get_ith_NLL <- function(y,X,beta){
  Phi = pnorm(X %*% beta)
  f = sum(y*log(Phi)) + sum((1-y)*log(1-Phi))
  f = -f
  return(f)
}

getNLL <- function(Y,X,beta,noOfData){
  result = 0
  for(i in 1:noOfData){
    result = result + get_ith_NLL(Y[i],X[i,],beta) 
  }
  return(result)
}

# Likelihood function for given Y and rho as sample mean(i.e. MLE)
get_ith_mean_NLL <- function(y,rho){
  Phi = pnorm(rho)
  f = sum(y*log(Phi)) + sum((1-y)*log(1-Phi))
  f = -f
  return(f)
}

get_mean_NLL <- function(Y,rho,noOfData){
  result = 0
  for(i in 1:noOfData){
    result = result + get_ith_mean_NLL(Y[i],rho)
  }
  return(result)
}

###################### Q3.b #######################
# Computing the Sample mean from given dataset.
sample_mean = sum(Y)/noOfData
rho_dash_dash = qnorm(sample_mean, mean=0, sd=1)
rho_dash = rep(rho_dash_dash,noOfData)
# print(rho_dash_dash)

matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

beta_MLE = solve(X[1:16,]) %*% rho_dash[1:16]
if(matequal(X %*% beta_MLE ,rho_dash)){
  # print("Matrix Invertible")
}else{
  # print(X %*% beta_MLE -rho_dash)
  # print("MLE matrix not invertible")
}
# print(beta_MLE)
# print(X %*% beta_MLE)

beta_OLS = solve(t(X) %*% X) %*% t(X) %*% Y
# print(beta_OLS)
# print(mean(X %*% beta_OLS))

# Decide Beta
beta_non_mcmc = beta_OLS
xi_time_beta_non_mcmc = mean(X %*% beta_non_mcmc)

rownames(beta_non_mcmc) <- t(c("beta1","beta2","beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14","beta15","beta16"))

print("Q3.b")
print(beta_non_mcmc)

###################### Q3.c #######################
covariateEffect = rep(0,max(X[,2]-min(X[,2])))
# Covariate effect of increasing age by 1 year.
for(i in min(X[,2]):(max(X[,2])-5)){
  covariateEffect[i-min(X[,2])] = pnorm(xi_time_beta_non_mcmc-mean(X[,2])*beta_non_mcmc[2]+(i+5)*beta_non_mcmc[2], mean=0, sd=1) - pnorm(xi_time_beta_non_mcmc-mean(X[,2])*beta_non_mcmc[2]+i*beta_non_mcmc[2], mean=0, sd=1)
}
cat("\n\n")
print("Covariate Effect of Age difference of 5yr (Q3.c)")
print(covariateEffect[1:(max(X[,2])-5-min(X[,2]))])
print(paste0("Mean of Age Covariate : ",mean(covariateEffect[1:(max(X[,2])-5-min(X[,2]))])))

###################### Q3.d #######################
binaryCovariateEffect = 0
binaryCovariateEffect = pnorm(xi_time_beta_non_mcmc-mean(X[,6])*beta_non_mcmc[6]+beta_non_mcmc[6], mean=0, sd=1) - pnorm(xi_time_beta_non_mcmc-mean(X[,6])*beta_non_mcmc[6], mean=0, sd=1)
cat("\n\n")
print(paste0("Binary covariate of parent (Q3.d) : ",binaryCovariateEffect))

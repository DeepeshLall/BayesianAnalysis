library(readxl)
library(plyr)
library(dplyr)
library(modeest)
library(truncnorm)
library(invgamma)
library(psych)

noOfData = 1240
totalGibbsIteration = 20000
burnin = 5000

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

n = dim(X)[1]
k = dim(X)[2]

beta.not = numeric(k)
B.not = diag(1, nrow = k, ncol = k)
z = matrix(rep(0,n))

draw.beta = function(z)
{
  B1 = solve(t(X) %*% X + solve(B.not))
  beta.bar = B1 %*% (t(X) %*% as.matrix(z) + solve(B.not) %*% beta.not)
  beta_updated = mvtnorm::rmvnorm(1, mean = beta.bar, sigma = B1) 
  
  return(beta_updated)
}

draw.z = function(beta){
  for (i in 1:n)
  {
    if(Y[i]==0){
      z[i] = rtruncnorm(1, a=-Inf, b=0, mean = as.vector(t(X[i,])) %*% beta, sd = 1)
    }else{
      z[i] = rtruncnorm(1, a=0, b=+Inf, mean = as.vector(t(X[i,])) %*% beta, sd = 1)
    }
  }
  return(z)
}

beta.value = matrix(0,nrow = totalGibbsIteration, ncol = k)

beta.prev = mvtnorm::rmvnorm(1, mean = beta.not, sigma = B.not)
z = draw.z(beta.prev)
beta.value[1,] = beta.prev

for(iter in 2:totalGibbsIteration){
  # comment print below to hide progress
  print(paste0(iter," out of ",totalGibbsIteration))
  beta.prev = draw.beta(z)
  z = draw.z(beta.prev)
  beta.value[iter,] = beta.prev
}
print(psych::describe(beta.value[burnin:totalGibbsIteration,]))

write.table(beta.value, file="Desktop/working/q3_2_result.Rdata", row.names = F, col.names = F)
unlink("~/Desktop/working/q3_2_result.Rdata")
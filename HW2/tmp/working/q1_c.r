set.seed(1)
library(readxl)
library(dplyr)
library(truncnorm)
library(invgamma)
library(psych)

burnin = 2
loopLength = 50

table <- read_excel("Desktop/working/adoption.xlsx")
table$winbet = table$winbet - 125
table = table[,3:16]

feature_vector = table[2:14]
y = table$winbet
feature_vector = as.matrix(feature_vector)
feature_vector = cbind(1,feature_vector)
colnames(feature_vector)[1] <- "Intercept"

n = dim(feature_vector)[1]
k = dim(feature_vector)[2]

beta.not = numeric(k)
B.not = diag(100, nrow = k, ncol = k)
beta.intial = mvtnorm::rmvnorm(1, mean = beta.not, sigma = B.not) 
sigma2.not = rinvgamma(1, shape = 0.5, scale = 0.5)
ind_C = which(y==0)

beta.value = matrix(0,nrow = loopLength, ncol = 14)
sigma.value = matrix(0,nrow = loopLength, ncol = 1)

colnames(beta.value) <- c("beta1","beta2","beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14")
colnames(sigma.value) <- c("sigma2")

get.z = function(beta, sigma2){
  for (i in 1:length(ind_C))
  {
    index = ind_C[i]
    y[index] = rtruncnorm(1, a=-Inf, b=0, mean = as.vector(t(feature_vector[index,])) %*% beta, sd = sigma2^(1/2))
  }
  return(y)
}

get.beta = function(sigma2,z_dash)
{
  B1 = solve((1/sigma2) * t(feature_vector) %*% feature_vector + solve(B.not))
  beta.bar = B1 %*% ((1/sigma2) * t(feature_vector) %*% as.matrix(z_dash) + solve(B.not) %*% beta.not)
  beta_updated = mvtnorm::rmvnorm(1, mean = beta.bar, sigma = B1) 
  
  return(beta_updated)
}

get.sigma2 = function(z_dash, beta)
{
  alpha1 = 1 + n
  delta1 = 1 + t(as.matrix(z_dash) - feature_vector%*%t(beta)) %*% (as.matrix(z_dash) - feature_vector%*%t(beta))
  sigma2_updated = rinvgamma(1, shape = alpha1/2 , scale = delta1/2)
  
  return(sigma2_updated)
}

y = get.z(beta.intial,sigma2.not)
beta.prev = get.beta(sigma2 = sigma2.not, z_dash = y)
sigma2.prev = get.sigma2(z_dash = y, beta = beta.prev)
y= get.z(beta = beta.prev,sigma2 = sigma2.prev)

beta.value[1,] = beta.prev
sigma.value[1,] = sigma2.prev

for(iter in 2:loopLength){
  beta.next = get.beta(sigma2 = sigma2.prev, z_dash = y)
  sigma2.next = get.sigma2(z_dash = y, beta = beta.next)
  y = get.z(beta = beta.next,sigma2 = sigma2.next)

  beta.value[iter,] = beta.next
  sigma.value[iter,] = sigma2.next
  
  beta.prev = beta.next
  sigma2.prev = sigma2.next
}

beta.value = beta.value[burnin:loopLength,]
sigma.value = sigma.value[burnin:loopLength,]

stats <- psych::describe(beta.value)
print(stats)
stats2 <- psych::describe(sigma.value)
print(stats2)



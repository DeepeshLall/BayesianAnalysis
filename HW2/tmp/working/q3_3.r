library(readxl)
library(plyr)
library(dplyr)
library(modeest)
library(truncnorm)
library(invgamma)

noOfData = 1240

q3_result <- read.table("~/Desktop/working/q3_2_result.Rdata", quote="\"", comment.char="")
gibbs_sample = q3_result[5000:20000,]
gibbs_sample = as.matrix(gibbs_sample)

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

colnames(gibbs_sample)[1] <- "beta1"
for(i in 2:16){
  str=paste0("beta",i)
  colnames(gibbs_sample)[i] <- str
}

# Use below snippet for ploting
hist(as.numeric(as.data.frame(gibbs_sample)$beta1), freq=FALSE, breaks=40)

# Printing the Table output same as q3_2.r output for q3.d
# cat("Coefficient \t Mean \t S.D. \t Lower \t Upper \n")
# for(iter in 1:16){
#   cat(paste0("beta",iter),"\t", mean(gibbs_sample[,iter]),"\t", sd(gibbs_sample[,iter]), "\t", max(gibbs_sample[,iter]),"\t",min(gibbs_sample[,iter]),"\n")
# }
print(psych::describe(gibbs_sample))

# Covariate Effect of Age for a difference of 5
sum = 0
for(g in 1:15000){
  # Use print(g) to check progress.
  print(paste0(g," out of ",15000))
  for(p in 1:n){
    sum = sum + gibbs_sample[g,2]*dnorm(t(X[p,]) %*% gibbs_sample[g,], mean=0, sd=1)
  }
}

# Covariate Effect of Parent
sum = 0
for(g in 1:15000){
  # Use print(g) to check progress.
  print(paste0(g," out of ",15000))
  for(p in 1:n){
    sum = sum + pnorm(t(X[p,]) %*% gibbs_sample[g,] - X[p,6]*gibbs_sample[g,6] + gibbs_sample[g,6], mean=0, sd=1) - pnorm(t(X[p,]) %*% gibbs_sample[g,] - X[p,6]*gibbs_sample[g,6], mean=0, sd=1)
  }
}
print(paste0("Marginal Effect of Age : ",sum/(n*15000)))
print(paste0("5 yr Covariate Effect of age : ",sum/(n*15000)*5))
print(paste0("Marginal Effect of Parent : ",sum/(n*15000)))

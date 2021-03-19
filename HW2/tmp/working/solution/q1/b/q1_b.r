set.seed(1)
library(readxl)
library(dplyr)

table <- read_excel("Desktop/working/adoption.xlsx")
table$winbet = table$winbet - 125

X = table[,4:16]
Y = table$winbet
X = as.matrix(X)
X = cbind(1,X)
colnames(X)[1] <- "Intercept"
beta = solve(t(X) %*% X) %*% t(X) %*% Y
print(beta)

error = Y - X %*% beta
print(paste0("standard deviation : ",sd(error) ** 2) )
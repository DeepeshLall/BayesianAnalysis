set.seed(1)
library(readxl)
library(dplyr)
library(psych)

table <- read_excel("Desktop/working/adoption.xlsx")
table$winbet = table$winbet - 125

table = table[,2:16]

stats <- psych::describe(table)
print(stats)

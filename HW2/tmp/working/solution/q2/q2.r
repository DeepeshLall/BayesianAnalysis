library(readxl)
library(plyr)
library(dplyr)
library(modeest)

noOfEntries = 1240
marijuana <- read_excel("Desktop/working/marijuana.xlsx")

# Q2.a
print("==================   Q2.a  =========================")
y <- marijuana$q85
y[y=="Yes, legal"] = 1
y[y=="No, illegal"] = 0
y <- as.integer(y)
print(paste0("Count for y_i=1 (count, %): (",sum(y),", ",sum(y)/noOfEntries*100," %)"))
print(paste0("Count for y_i=0 (count, %): (",noOfEntries-sum(y),", ",100-sum(y)/noOfEntries*100," %)"))

cat("\n")
print("==================   Q2.b  =========================")
x2 <- marijuana$age
x2 <- as.integer(x2)
x3 <- marijuana$hh1
x3 <- as.integer(x3)
print(paste0("Mean of x2: ",mean(x2)))
print(paste0("Median of x2: ",median(x2)))
# print(paste0("Mode of x2: ",mfv(x2)))
print(paste0("Std. Dev. of x2: ",sd(x2)))
print(paste0("Max of x2: ",max(x2)))
print(paste0("Min of x2: ",min(x2)))
print(paste0("Mean of x3: ",mean(x3)))
print(paste0("Median of x3: ",median(x3)))
# print(paste0("Mode of x3: ",mfv(x3)))
print(paste0("Std. Dev. of x3: ",sd(x3)))
print(paste0("Max of x3: ",max(x3)))
print(paste0("Min of x3: ",min(x3)))

cat("\n")
print("==================   Q2.c  =========================")
x4 <- marijuana$`past use`
x4[x4=="Yes"] = 1
x4[x4=="No"] = 0
x4 <- as.integer(x4)
print(paste0("Count for x4=1 (count, %): (",sum(x4),", ",sum(x4)/noOfEntries*100," %)"))
print(paste0("Count for x4=0 (count, %): (",noOfEntries-sum(x4),", ",100-sum(x4)/noOfEntries*100," %)"))

cat("\n")
print("==================   Q2.d  =========================")
x5 <- marijuana$sex
x5[x5=="Male"] = 1
x5[x5=="Female"] = 0
x5 <- as.integer(x5)
print(paste0("Count for x5=1 (count, %): (",sum(x5),", ",sum(x5)/noOfEntries*100," %)"))
print(paste0("Count for x5=0 (count, %): (",noOfEntries-sum(x5),", ",100-sum(x5)/noOfEntries*100," %)"))

x6 <- marijuana$parent
x6[x6=="Yes"] = 1
x6[x6=="No"] = 0
x6 <- as.integer(x6)
print(paste0("Count for x6=1 (count, %): (",sum(x6),", ",sum(x6)/noOfEntries*100," %)"))
print(paste0("Count for x6=0 (count, %): (",noOfEntries-sum(x6),", ",100-sum(x6)/noOfEntries*100," %)"))

cat("\n")
print("==================   Q2.e  =========================")
x7 <- marijuana$`marital status`
x7[x7=="Never been married"] = 1
x7[x7!=1] = 0
x7 <- as.integer(x7)
print(paste0("Count for x7=1 (count, %): (",sum(x7),", ",sum(x7)/noOfEntries*100," %)"))
print(paste0("Count for x7=0 (count, %): (",noOfEntries-sum(x7),", ",100-sum(x7)/noOfEntries*100," %)"))

x8 <- marijuana$`marital status`
x8[x8=="Divorced" | x8=="Separated" | x8=="Widowed"] = 1
x8[x8!=1] = 0
x8 <- as.integer(x8)
print(paste0("Count for x8=1 (count, %): (",sum(x8),", ",sum(x8)/noOfEntries*100," %)"))
print(paste0("Count for x8=0 (count, %): (",noOfEntries-sum(x8),", ",100-sum(x8)/noOfEntries*100," %)"))

cat("\n")
print("==================   Q2.f  =========================")
x9 <- marijuana$income
x9[x9=="Less than 10000" | x9=="10 to under 20000" | x9=="20 to under 30000" | x9=="30 to under 40000" | x9=="40 to under 50000"] = 1
x9[x9!=1] = 0
x9 <- as.integer(x9)
print(paste0("Count for x9=1 (count, %): (",sum(x9),", ",sum(x9)/noOfEntries*100," %)"))
print(paste0("Count for x9=0 (count, %): (",noOfEntries-sum(x9),", ",100-sum(x9)/noOfEntries*100," %)"))

x10 <- marijuana$income
x10[x10=="50 to under 75000" | x10=="75 to under 100000"] = 1
x10[x10!=1] = 0
x10 <- as.integer(x10)
print(paste0("Count for x10=1 (count, %): (",sum(x10),", ",sum(x10)/noOfEntries*100," %)"))
print(paste0("Count for x10=0 (count, %): (",noOfEntries-sum(x10),", ",100-sum(x10)/noOfEntries*100," %)"))

cat("\n")
print("==================   Q2.g  =========================")
x11 <- marijuana$educ
x11[x11=="Less than HS" | x11=="HS Incomplete" | x11=="HS" ] = 1
x11[x11!=1] = 0
x11 <- as.integer(x11)
print(paste0("Count for x11=1 (count, %): (",sum(x11),", ",sum(x11)/noOfEntries*100," %)"))
print(paste0("Count for x11=0 (count, %): (",noOfEntries-sum(x11),", ",100-sum(x11)/noOfEntries*100," %)"))

x12 <- marijuana$educ
x12[x12=="Some college" | x12=="Associate Degree"] = 1
x12[x12!=1] = 0
x12 <- as.integer(x12)
print(paste0("Count for x12=1 (count, %): (",sum(x12),", ",sum(x12)/noOfEntries*100," %)"))
print(paste0("Count for x12=0 (count, %): (",noOfEntries-sum(x12),", ",100-sum(x12)/noOfEntries*100," %)"))

cat("\n")
print("==================   Q2.h  =========================")
x13 <- marijuana$race
x13[x13=="White"] = 1
x13[x13!=1] = 0
x13 <- as.integer(x13)
print(paste0("Count for x13=1 (count, %): (",sum(x13),", ",sum(x13)/noOfEntries*100," %)"))
print(paste0("Count for x13=0 (count, %): (",noOfEntries-sum(x13),", ",100-sum(x13)/noOfEntries*100," %)"))

x14 <- marijuana$race
x14[x14=="Black"] = 1
x14[x14!=1] = 0
x14 <- as.integer(x14)
print(paste0("Count for x14=1 (count, %): (",sum(x14),", ",sum(x14)/noOfEntries*100," %)"))
print(paste0("Count for x14=0 (count, %): (",noOfEntries-sum(x14),", ",100-sum(x14)/noOfEntries*100," %)"))

cat("\n")
print("==================   Q2.i  =========================")
x15 <- marijuana$party
x15[x15=="Democrat"] = 1
x15[x15!=1] = 0
x15 <- as.integer(x15)
print(paste0("Count for x15=1 (count, %): (",sum(x15),", ",sum(x15)/noOfEntries*100," %)"))
print(paste0("Count for x15=0 (count, %): (",noOfEntries-sum(x15),", ",100-sum(x15)/noOfEntries*100," %)"))

x16 <- marijuana$party
x16[x16=="Republican"] = 1
x16[x16!=1] = 0
x16 <- as.integer(x16)
print(paste0("Count for x16=1 (count, %): (",sum(x16),", ",sum(x16)/noOfEntries*100," %)"))
print(paste0("Count for x16=0 (count, %): (",noOfEntries-sum(x16),", ",100-sum(x16)/noOfEntries*100," %)"))

cat("\n")
result = matrix(unlist(list(y,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16)),nrow=noOfEntries,ncol=16)
write.table(result, file="~/Desktop/working/q2_result.Rdata", row.names = F, col.names = F)
unlink("q2_result.Rdata")
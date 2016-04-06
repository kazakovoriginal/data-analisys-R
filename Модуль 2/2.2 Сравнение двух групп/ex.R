#ex1
dataset <- ToothGrowth

#head(ToothGrowth)
#table(dataset$supp)
dataset1 <- subset(dataset,(dataset$supp == 'OJ' & dataset$dose == 0.5) | (dataset$supp == 'VC' & dataset$dose == 2))

res <- t.test(len ~ supp, dataset1 )

print(res$statistic)
#ex2

dataLekarstva <- read.csv("lekarstva.csv")

t.test(dataLekarstva$Pressure_before, dataLekarstva$Pressure_after, paired = T)
# ex3

dataEx3 <- read.table("dataset_11504_15.txt")

bartlett.test(V1  ~ V2, dataEx3)

t.test(V1  ~ V2, dataEx3, var.equal = T)


t.test(dataEx3$V1,dataEx3$V2)
wilcox.test(Petal.Length ~ Species, dataEx3)

#ex4
dataEx3 <- read.table("dataset_11504_16.txt")

t.test(dataEx3$V1, dataEx3$V2)
mean(dataEx3$V1)
mean(dataEx3$V2)

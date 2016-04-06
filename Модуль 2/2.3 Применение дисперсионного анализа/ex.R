#ex1
data <- npk
fit <- aov(yield ~ N + P + K, data)
summary(fit)

#ex2

data2 <- iris
fit <- aov(Sepal.Width ~ Species, data2)
TukeyHSD(fit)

#ex3

data3 <- read.csv("Pillulkin.csv")
data3$patient <- as.factor(data3$patient)
fit <- aov(temperature ~ pill + Error(patient/pill), data3)
summary(fit)

#ex4

data3 <- read.csv("Pillulkin.csv")
data3$patient <- as.factor(data3$patient)

fit <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data3)
summary(fit)

#ex4

dataset <- ToothGrowth
dataset$dose <- as.factor(dataset$dose)


pd = position_dodge(0.1)
obj <- ggplot(dataset, aes(x = dose, y = len, color = supp, group = supp)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
    stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
    stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
  
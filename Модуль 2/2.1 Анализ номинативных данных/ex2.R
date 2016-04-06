#ex1
library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
dataset <- HairEyeColor
mydata1 <- mydata[mydata$Sex == 'Female',1:4]

tb <- subset(mydata, Sex == 'Female')

obj <- ggplot(data = mydata1 , aes(x =  Hair, y = Freq)) + geom_bar(stat="identity", position = "dodge", aes(fill = Eye)) + scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
#ex2
t <- table(t1)
t1 <- HairEyeColor[2,,2]

chisq.test(t1)
#ex3
head(diamonds)

t2 <- table(cut = diamonds$cut, color = diamonds$color)
chi <- chisq.test(t2)
ans <- chi$statistic
print(ans)
#ex4
diamonds$factor_price <- ifelse(mean(diamonds$price) <= diamonds$price, 1, 0)
#tail(subset(diamonds, factor_price == 1))
diamonds$factor_carat <- ifelse(mean(diamonds$carat) <= diamonds$carat, 1, 0)
#tail(subset(diamonds, diamonds$factor_carat == 0))

t1 <- table(price = diamonds$factor_price, carat = diamonds$factor_carat)

chi1 <- chisq.test(t1)

print (chi1$statistic)
#ex5

carsData <- mtcars
tableCar <- table(am = carsData$am,vs = carsData$vs)

fisherTest <- fisher.test(tableCar)
print(fisherTest$p.value)
















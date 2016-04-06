#ex1
data <- mtcars
model <- glm(am~disp+vs+mpg, data, family = "binomial")
#summary(model)
print(model$coefficients)


#ex2

library("ggplot2")

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
obj <- ggplot(data = ToothGrowth, aes(x = supp,y = len))
obj + geom_boxplot(aes(fill = dose))


#ex3

table <- read.csv("data.csv")
table$admit <- as.factor(table$admit)
table$rank <- as.factor(table$rank)

new_table <- subset(table, !is.na(table$admit))
new_table_no_data<- subset(table, is.na(table$admit))

fit <- glm(admit~rank*gpa, new_table, family = "binomial")



exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))

new_table_no_data$pred  <- predict(fit, newdata = new_table_no_data,
                                   type = "response")

new_table_no_data$pred_resp  <- (ifelse(new_table_no_data$pred >= 0.4, 1, 0))

count <- 0

for (i in 1:length(new_table_no_data$pred_resp))
{
  if (new_table_no_data$pred_resp[i] == 1) count <- count+1
}


library(ROCR)


pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)



perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)
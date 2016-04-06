#ex1
x1 <- rnorm(50) # создадим случайную выборку из 50 элементов
x2 <- rnorm(50) # создадим случайную выборку из 50 элементов
y <- rnorm(50) # создадим случайную выборку из 50 элементов
na_index <- sample(1:30, runif(1,5,15)) # случайно сгенерируем количество и позиции NA в векторе y
y[na_index] <- NA # создадим NA в векторе y
my_df <- data.frame(x_1 = x1, x_2 = x2, y = y)
my_df$y_full <- y

my_df_copy <- subset(my_df,!is.na(y))
fit <- lm(y ~ x_1 + x_2, my_df_copy)

#fit$fitted.values


new_df <- data.frame(x_1 = my_df$x_1, x_2 = my_df$x_2)
predict <- predict(fit,new_df)

for (i in 1:nrow(my_df)){
  if (is.na(my_df$y[i])){
    my_df$y_full[i] <- predict[i]
    #print(mydata$score[i]) 
  }
}

#ex2

df_numeric  <- mtcars
df <-df_numeric[,c(1,3:6)]
fit_full <- lm(wt ~ ., df) #mpg*disp*drat*disp
summary(model)$adj.r.squared


optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)


#ex3

dataset <- attitude
model <- lm(rating~critical*complaints, dataset) #mpg*disp*drat*hp
summary(model)
#ex4

df_numeric  <- mtcars
df_numeric$am <- factor(df_numeric$am, labels = c('Automatic', 'Manual'))
model <- lm(mpg ~ wt*am, df_numeric)
summary(model)

confint(model)

#ex5

library(ggplot2)
my_plot <- 

ggplot(df_numeric, aes(x = wt, y = mpg, col = am)) + 
  geom_smooth(method = 'lm')





















































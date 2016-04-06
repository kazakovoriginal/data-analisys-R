#ex1

data <- attitude

model_full <- lm(rating ~ ., data = attitude) 


model_null <- lm(rating ~ 1, data = attitude)

optimal_fit <-  step(model_full, direction = 'backward', scope = list(lower = model_null, upper = model_full))
summary(optimal_fit)

#ex2

anova(model_full, optimal_fit)


#ex3

data <- LifeCycleSavings
model <- lm(sr ~ (pop15*pop15*dpi*ddpi)^2, data)
summary(model)
?formula

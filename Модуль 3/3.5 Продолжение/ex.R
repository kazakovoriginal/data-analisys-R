# ?1
table <- read.csv("homosc.csv")
model <- lm( DV~IV, table)
res <- gvlma(model)
summary(res)


table$model_fitted <- model$fitted.values
table$model_resid <- model$residuals

ggplot(table, aes(x = model_fitted, y = model_resid)) + 
  geom_point(size = 3)

#ex1
resid.norm  <- function(fit){
  # put your code here  
  
  data <- data.frame(fit_v = fit$fitted.values,res = fit$residuals)
  test <- shapiro.test(data$res)
  
  color <- ifelse((test$p.value < 0.05),"red","green")
  
  plot <- ggplot(data, aes(x = res)) + 
    geom_histogram(fill = color)
  return(plot)
  
}

library(ggplot2)


#ex2


high.corr <- function(x){
  # put your code here  
  
}





step6 <-  read.table("step6.csv",  header=TRUE, sep=',' )

nums <- sapply(step6, is.numeric)
head(step6[, nums])
r <- corr.test(head(step6[, nums]))
a <- r$r
which.max(a) 
x <- c(1:4, 0:5)
which.min(x)
m <- max(abs(a[a!=1]))
k <- 0
for (mi in a){
  if (mi == m){
    k <- k+1
  }
}
if(k==2) print(m) else  print(m*(-1))
max(a, na.rm=TRUE)


# START_determination
library(psych)
filtered.cor <- function(x){
  nums <- sapply(x, is.numeric)
  a1 <- x[ , nums]
  r <- corr.test(a1)
  a <- r$r
  m <- max(abs(a[a!=1]))
  k <- 0
  for (mi in a){
    if (mi == m){
      k <- k+1
    }
  }
  
  if(k==2) return(m) else  return(m*(-1))
}


filtered.cor(iris)
iris$Petal.Length <- -iris$Petal.Length

# END_determination


x <- iris
x$new <- x$Sepal.Width*10
nums <- sapply(x, is.numeric)
a1 <- x[ , nums]
r <- corr.test(a1)
a <- r$r
max <- 0
col_name <- colnames(r$r)
row_name <- rownames(r$r)
for (i in 1:length(rownames(r$r)))
{
  for (j in 1:length(colnames(r$r)))
  {
    if (i!=j)
    {
      if (r$r[i,j] > max){
        max <- r$r[i,j]
        result <- c(row_name[i],col_name[j])
      }
    }
    j <- j+1
  }
  i <- i+1
}



colnames(r$r)
colnames(r$r[1])
m <- max(abs(a[a!=1]))
k <- 0
for (mi in a){
  if (mi == m){
    k <- k+1
  }
}

ifelse((k==2),m,(m*(-1)))










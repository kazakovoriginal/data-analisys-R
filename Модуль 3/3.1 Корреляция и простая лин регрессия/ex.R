#ex1

corr.calc <- function(x){
  res <- cor.test(x[,1],x[,2])
  return(c(res$estimate, res$p.value))
}

corr.calc(mtcars[, c(1,5)] )

#ex2

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

#ex3

new <-  read.table("new.csv",sep = ";")
vec1 <- new[,1]
vec2 <- new[,2]
sh1 <- shapiro.test(vec1)
sh2 <- shapiro.test(vec2)
pearson <- cor.test(vec1,vec2)
spearman <- cor(vec1,vec2,method = "spearman")
if(sh1$p.value>0.05 & sh2$p.value>0.05) print(pearson$estimate) else print(spearman)

#ex4

newTxt <-  read.table("dataset_11508_12.txt")
vec1 <- newTxt[,1]
vec2 <- newTxt[,2]
res <- lm(vec1~vec2)

print(res$coefficients)

#ex5
library(ggplot2)
data <- subset(diamonds, cut == "Ideal" &  carat == 0.46)
fit <- lm(price ~ depth, data)
print(fit$coefficients)

#ex6

regr.calc <- function(x){
  x1 <- x[,1]
  x2 <- x[,2]
  corres <- cor.test(x1,x2)
  
  if(corres$p.value<0.05){
    fit <- lm(x1~x2)
    x$fit <- fit$fitted.values
    return(x)
  } else {return("There is no sense in prediction")}
  
}

x = iris[,c(1,4)]
x1 <- x[,1]
x2 <- x[,2]
corres <- cor.test(x1,x2)

if(corres$p.value<0.05){
  fit <- lm(x1~x2)
  x$fit <- fit$fitted.values
  print(head(x))
} else {print("There is no sense in prediction")}

#ex7

plotdata <- iris
library(ggplot2)
my_plot <- 
  ggplot(iris, aes(Sepal.Width,Petal.Width,  color = Species))+
  geom_point(size = 5)+
  geom_smooth()













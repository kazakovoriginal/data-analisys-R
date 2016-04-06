df <- mtcars
m <- mean(df$qsec[df$cyl!=3 & df$mpg > 20])
print(m)
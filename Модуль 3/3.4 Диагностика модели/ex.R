#test1

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(sqrt(my_vector))

#ex1

new_vect <- scale(my_vector)

x <- mtcars[,1:6]

new_df <- data.frame(v1 = scale(x[,1]), v2 = scale(x[,2]))

model <- lm(v1 ~ v2, new_df)
model$coefficients

beta.coef <- function(x){
  
  new_df <- data.frame(v1 = scale(x[,1]), v2 = scale(x[,2]))
  
  model <- lm(v1 ~ v2, new_df)
  return(model$coefficients)
  
}

#ex2

normality.test  <- function(x){

  vect_of_name <- apply(x, 1, names)
  a <- apply(x, 2, shapiro.test)
  
  
  for (i in 1:length(vect_of_name[,1])){
    res <- shapiro.test(x[,i])
    if (i == 1) {my_vec <- res$p.value}
    else{
      my_vec <- append(my_vec, res$p.value)
    }
  }
  
  names(my_vec) <- vect_of_name[,1]
  return(my_vec)
  
}


x <- mtcars[,1:6]


vect_of_name <- apply(x, 1, names)
a <- apply(x, 2, shapiro.test)


for (i in 1:length(vect_of_name[,1])){
  res <- shapiro.test(x[,i])
  if (i == 1) {my_vec <- res$p.value}
  else{
  my_vec <- append(my_vec, res$p.value)
  }
}

names(my_vec) <- vect_of_name[,1]























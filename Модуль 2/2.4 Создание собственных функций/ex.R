#ex1

NA.position <- function(x){
  # put your code here 
  res <- 1
  i <- 1
  for (j in 1:length(x)) 
  {
    if (is.na(x[j])) 
    {
      res[i] <- j
      i <- i + 1
    }
  }
  return(res)
}

my_vector <- c(1, 2, 3, NA, NA, NA)
NA.position(my_vector)

#ex2

NA.counter <- function(x){
  k=0
  for (j in 1:length(x)) 
  {
    if (is.na(x[j])) 
    {
      k <- k + 1
      print(k)
    }
  }
  return(k)
}

a <- NA.counter(my_vector)

#ex3

filtered.sum <- function(x){
  sum <- 0
  for (j in 1:length(x)) 
  {
    if (is.na(x[j]) == 0 & x[j] > 0) 
    {
      sum <- sum + x[j]
    }
  }
  return(sum)
}

my_vector <- c(17,18,17,18,17.5, 25)
filtered.sum(my_vector)

my_vector[1] > 0 
is.na(my_vector[6]) == 0

#ex3

outliers.rm <- function(x){
  quan <- quantile(x, probs = c(0.25, 0.75))
  kv_1 <- c(quan[1]-(1.5*IQR(x)),quan[1]+1.5*IQR(x))
  kv_2 <- c(quan[2]-1.5*IQR(x),quan[2]+1.5*IQR(x))
  
      #return(c(kv_1,kv_2,IQR(x)))
  return(x[(x>= kv_1[1] & x<=kv_1[2])|(x>= kv_2[1] & x<=kv_2[2])])
}


outliers.rm(my_vector)







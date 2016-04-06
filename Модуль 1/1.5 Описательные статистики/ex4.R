m <- mean(my_vector, na.rm = T)
for (i in 1:length(my_vector)){
  if (is.na(x = my_vector[i])){
    my_vector[i]= m }}
print(my_vector)
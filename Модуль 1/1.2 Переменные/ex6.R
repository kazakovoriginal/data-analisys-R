norma <- my_vector[my_vector>mean(my_vector)-sd(my_vector) & my_vector<mean(my_vector)+sd(my_vector)]
print(norma)
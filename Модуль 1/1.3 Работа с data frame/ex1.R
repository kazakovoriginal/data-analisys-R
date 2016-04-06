df <- mtcars
df$even_gear <- (df$gear+1) %% 2
print(df$even_gear)
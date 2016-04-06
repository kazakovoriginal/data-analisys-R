df <- mtcars
plot1 <- ggplot(df, aes(x = mpg, y = disp, color = hp))+
  geom_point()

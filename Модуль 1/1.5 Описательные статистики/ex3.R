df <- airquality
new_df <- subset(df,Month >= 7 & Month <= 9)
aggregate(Ozone ~ Month, new_df, length)
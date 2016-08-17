library(data.table)

# load original
df <- fread("./input/pm/demo/2015 all.csv", stringsAsFactors=T, data.table=F)

# quarterly -------------------------------------------------------------------
# split
df_1st_quarter <- df[as.character(df$datetime) < "2015-04-01", ]
df_2nd_quarter <- df[as.character(df$datetime) >= "2015-04-01" & as.character(df$datetime) < "2015-07-01", ]
df_3rd_quarter <- df[as.character(df$datetime) >= "2015-07-01" & as.character(df$datetime) < "2015-10-01", ]
df_4th_quarter <- df[as.character(df$datetime) >= "2015-10-01", ]

# confirm range of datetime
range(as.character(df_1st_quarter$datetime))
range(as.character(df_2nd_quarter$datetime))
range(as.character(df_3rd_quarter$datetime))
range(as.character(df_4th_quarter$datetime))

# create csv
write.csv(df_1st_quarter, "./input/pm/demo/2015 1st quarter.csv", row.names = F)
write.csv(df_2nd_quarter, "./input/pm/demo/2015 2nd quarter.csv", row.names = F)
write.csv(df_3rd_quarter, "./input/pm/demo/2015 3rd quarter.csv", row.names = F)
write.csv(df_4th_quarter, "./input/pm/demo/2015 4th quarter.csv", row.names = F)

# confirm csv
df1 <- fread("./input/pm/demo/2015 1st quarter.csv", stringsAsFactors=T, data.table=F)
df2 <- fread("./input/pm/demo/2015 2nd quarter.csv", stringsAsFactors=T, data.table=F)
df3 <- fread("./input/pm/demo/2015 3rd quarter.csv", stringsAsFactors=T, data.table=F)
df4 <- fread("./input/pm/demo/2015 4th quarter.csv", stringsAsFactors=T, data.table=F)


# per month -------------------------------------------------------------------
# split
df_jan <- df[as.character(df$datetime) < "2015-02-01", ]
df_feb <- df[as.character(df$datetime) >= "2015-02-01" & as.character(df$datetime) < "2015-03-01", ]
df_mar <- df[as.character(df$datetime) >= "2015-03-01" & as.character(df$datetime) < "2015-04-01", ]
df_apr <- df[as.character(df$datetime) >= "2015-04-01" & as.character(df$datetime) < "2015-05-01", ]
df_may <- df[as.character(df$datetime) >= "2015-05-01" & as.character(df$datetime) < "2015-06-01", ]
df_jun <- df[as.character(df$datetime) >= "2015-06-01" & as.character(df$datetime) < "2015-07-01", ]
df_jul <- df[as.character(df$datetime) >= "2015-07-01" & as.character(df$datetime) < "2015-08-01", ]
df_aug <- df[as.character(df$datetime) >= "2015-08-01" & as.character(df$datetime) < "2015-09-01", ]
df_sep <- df[as.character(df$datetime) >= "2015-09-01" & as.character(df$datetime) < "2015-10-01", ]
df_oct <- df[as.character(df$datetime) >= "2015-10-01" & as.character(df$datetime) < "2015-11-01", ]
df_nov <- df[as.character(df$datetime) >= "2015-11-01" & as.character(df$datetime) < "2015-12-01", ]
df_dec <- df[as.character(df$datetime) >= "2015-12-01", ]

# confirm
sum(nrow(df_jan),nrow(df_feb),nrow(df_mar),nrow(df_apr),nrow(df_may),nrow(df_jun),
    nrow(df_jul),nrow(df_aug),nrow(df_sep),nrow(df_oct),nrow(df_nov),nrow(df_dec)) == nrow(df)

# create csv
write.csv(df_jan, "./input/pm/demo/2015-01.csv", row.names = F)
write.csv(df_feb, "./input/pm/demo/2015-02.csv", row.names = F)
write.csv(df_mar, "./input/pm/demo/2015-03.csv", row.names = F)
write.csv(df_apr, "./input/pm/demo/2015-04.csv", row.names = F)
write.csv(df_may, "./input/pm/demo/2015-05.csv", row.names = F)
write.csv(df_jun, "./input/pm/demo/2015-06.csv", row.names = F)
write.csv(df_jul, "./input/pm/demo/2015-07.csv", row.names = F)
write.csv(df_aug, "./input/pm/demo/2015-08.csv", row.names = F)
write.csv(df_sep, "./input/pm/demo/2015-09.csv", row.names = F)
write.csv(df_oct, "./input/pm/demo/2015-10.csv", row.names = F)
write.csv(df_nov, "./input/pm/demo/2015-11.csv", row.names = F)
write.csv(df_dec, "./input/pm/demo/2015-12.csv", row.names = F)


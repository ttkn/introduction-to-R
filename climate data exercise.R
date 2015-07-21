#excercises in R with climate data
hg <- read.csv(file="C:\\resolve\\hw1_data.csv", header = TRUE, sep=",")

#the complete cases function returns T/F to identiy NA values
bag <- complete.cases(hg)
hg1 <- hg[bag, ]

#we're interested in working with the first column of the data, ozone
x <- hg[bag, "Ozone"]

mean.default(x)

#now working with the rows of data where ozone is higher than 31 and temps higher than 90
m1 <- hg1$Ozone>31
m2 <- hg1$Temp>90
m <- hg1[with(hg1, Ozone>31 & Temp>90), ]

#filter the dataframe by a column value: month
x2 <- hg1$Month==6
x3 <- hg1[x2, "Temp"]
mean.default(x3)


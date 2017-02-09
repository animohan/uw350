library(dplyr)

read.auto = function(file = "Automobile price data _Raw_.csv"){
  auto.price  = read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  numcols = c("price","bore","stroke","horsepower","peak.rpm")
  auto.price[,numcols] = lapply(auto.price[,numcols], as.numeric)
  
  auto.price[complete.cases(auto.price), ]
}

auto.price = read.auto()


#FILTER THE DATASET
df = filter(auto.price, make == "audi")
df = filter(auto.price, drive.wheels = "4wd")
df


#SLICE OF DATAFRAME
df.slice = slice(auto.price, 20:30)
df.slice


#RANDOM DATA FROM SAMPLE
df.rand = sample_frac(df.slice, 0.5)
df.rand


#SELECT COLUMNS
df2 = select(df, drive.wheels, wheel.base, curb.weight, horsepower, price)
df2

#SELECT ALL COLUMNS EXCEPT ONES SPECIFIED
df3 = select(df, -symboling, -normalized.losses, -engine.size)
dim(df3)
df3

#ARRANGE ROWS IN A DATA FRAME (sorting)
df2 = arrange(df2, drive.wheels, price)
df2

#ADD NEW COL
df4 = mutate(df2, curb.weight.kg = curb.weight / 2.025, weight.horsepower = curb.weight / horsepower)
select(df4, curb.weight, curb.weight.kg, weight.horsepower)

#SUMMARY:
summarise(df4, mean.curb.weight = mean(curb.weight, na.rm = TRUE), sd.curb.weight = sd(curb.weight, na.rm = TRUE),
                                  max.curb.weight = max(curb.weight), min.curb.weight = min(curb.weight))

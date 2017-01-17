#Class Work

auto.price = read.csv("Automobile price data _Raw_.csv", header = TRUE, stringsAsFactors = FALSE)
str(auto.price)

numcols <- c('price',"bore")
numcols
auto.price[,numcols]
auto.price[complete.cases(auto.price),]
complete.cases(auto.price)
head(auto.price)
str(auto.price)

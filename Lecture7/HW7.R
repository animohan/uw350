auto = read.csv("Auto.csv", header = TRUE, stringsAsFactors = FALSE)
numcols = c("price","bore","stroke","horsepower","peak.rpm")
auto[,numcols] = lapply(auto[,numcols], as.numeric)
auto = data.frame(auto)
dim(auto)
auto = subset(auto, select = -c(symboling,normalized.losses))
dim(auto)
auto[complete.cases(auto),]

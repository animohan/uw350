auto = read.csv("Auto.csv")
numcols = c("price","bore","stroke","horsepower","peak.rpm")
auto[, numcols] =  lapply(auto[, numcols], as.numeric)
auto = auto[complete.cases(auto),]


plot.t = function(a, b, cols = c("Population A","Population B")){
  
  
}
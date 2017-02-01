library(ggplot2)
library(gridExtra)
library(repr)


dist.plot = function(n, mu = 100, sigma = 10){
  options(repr.plot.width = 6, repr.plot.height = 6)
  dists = data.frame(
    norm = rnorm(n, mu, sigma),
    poiss = rpois(n, mu)
  )
  bw =( max(max(dists$norm,dists$poiss)) - min(min(dists$norm,dists$poiss)) )/60
  
  p1 = ggplot(dists, aes(norm)) + geom_histogram(bandwidth = bw) + ggtitle("Distribution of Normal Distribution")
  p2 = ggplot(dists, aes(poiss)) + geom_histogram(bandwidth = bw) + ggtitle("Distribition of Poisson Distribution")
  grid.arrange(p1, p2, ncol =2)
  summary(dists)
}

dist.plot(100)

#SIMULATE BREAD DEMAND
sim.bread = function(n){
  bread = runif(n)
  ifelse(bread <= 0.5, "white",ifelse(bread <= 0.75,"wheat","multi"))
}

table(sim.bread(100))

#SIMULATE DEMAND

sim.demand = function(lambda, n){
  arrivals = rpois(n, lambda)
  demand.mat = matrix(0,n,3)
  i = 1
  for(a in arrivals) {
    demand.mat[i,] = t(matrix(table(sim.bread(a))))
    i = i + 1
  }
  return(data.frame(demand.mat))
}

#Plot distribution of white, wheat and multi-grain
d = sim.demand(100,10000)
colnames(d) = c("white","wheat","multi")
p1 = ggplot(d, aes(white)) + geom_histogram() + ggtitle("Distribution of white bread")
p2 = ggplot(d, aes(wheat)) + geom_histogram() + ggtitle("Distribution of wheat bread")
p3 = ggplot(d, aes(multi)) + geom_histogram() + ggtitle("Distribution of multi bread")

grid.arrange(p1,p2,p3, ncol = 3)
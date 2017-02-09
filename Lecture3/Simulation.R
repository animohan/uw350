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
  demand.mat = data.frame(demand.mat)
  colnames(demand.mat) = c("White","Wheat","Multi")
  return(demand.mat)
}

#Plot distribution of white, wheat and multi-grain
d = sim.demand(100,10000)
colnames(d) = c("white","wheat","multi")
p1 = ggplot(d, aes(white)) + geom_histogram() + ggtitle("Distribution of white bread")
p2 = ggplot(d, aes(wheat)) + geom_histogram() + ggtitle("Distribution of wheat bread")
p3 = ggplot(d, aes(multi)) + geom_histogram() + ggtitle("Distribution of multi bread")

grid.arrange(p1,p2,p3, ncol = 3)


#SIMULATE BAKED BREAD
baked.bread = function(n){
  
  baked = c(rep('white', times = floor(n/2)),rep("wheat", times = floor(n/4)), rep("multi", times = n-floor(n/2)-floor(n/4)))
  
  #table gives the summary of white, wheat and multigrain bread
  # Extract the counts and use it to create a dataframe.
  baked = data.frame(table(baked))[,2]
  names(baked) = c("Multigrain","Wheat","White") #Note how the order has changed
  # This order reversal takes place when we use table, it seems to sort by labels
  
  t(baked)
}

baked.bread(105)


#SIMULATE PROFIT 
sim.profit = function(baked, n, lambda, earned, cost){
  
  #Amount of bread of each type to be baked.
  bread = baked.bread(baked) 
  
  #Sim demand gives us the demand for each type of the bread. Demand of each type of bread
  # is calculated by # of people arriving in a timeperiod (arrival rate = poisson)
  # and the knowledge that 50% of ppl arriving want white, 25% wheat and 25% multigrain
  demand = sim.demand(lambda, n)
  
  
  #empty matrix for results
  profit = matrix(0,n,1)
  
  for(i in 1:3){
    bread.diff = bread[i]-demand[,i]    
    profit = ifelse(bread.diff >=0, #Was there enough bread of this type
                    profit + earned * demand[,i] - cost*bread[i], #if yes, compute profit
                    profit + earned * bread[i] - cost*bread[i])  #else profit is limited by available bread
    
  }
  data.frame(profit = profit, demand = demand, bread = bread)
  
}


sim.profit(100,10,100, 1.0, 0.25)


#PLOTTING PROFITS
plot.demand = function(demand){
  demand$demand = apply(demand[,2:4],1,sum) #Compute todal demand for White, Wheat and
    #multigrain bread; apply fuction applies sum() function along the rows
  p1 = ggplot(demand, aes(demand)) + geom_histogram() + ggtitle("Histogram of Demand of Sandwiches")
  p2 = ggplot(demand, aes(profit)) + geom_histogram() + ggtitle("Histrogram of profit from Sandwiches")
  print(grid.arrange(p1,p2,nrow = 2))
  summary(demand[,c(1,8)])
  
}

demand = sim.profit(100, 10000, 100, 1.00, 0.25)
plot.demand(demand)

#ABOVE WAS FOR 100 BAKED BREAD; NOW FOR VARIOUS BAKED BREAD
fun = function(x){
  demand = sim.profit(x, 10000, 100, 1.00, 0.25)
  plot.demand(demand)
}
new.breads = c(120,140,160)

lapply(new.breads, function(x) fun(x))


#PROFITABILITY vs BAKING BREAD
sim.total = function(baked, group, n = 100, lambda = 100, earned =1.0, cost = 0.25){
  bake.steps = length(baked)
  profits = rep(0, length.out = bake.steps)
  
  
}
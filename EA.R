fitness = function(x) {
  x*sin(4*x) + 1
}
plot.function(fitness, from = -4, to = 4)
set.seed(48)
x1 = runif(20, -4, 4)
y1 = fitness(x1)
newdata = data.frame(x1, y1)
#plot population
points(x1, y1, pch=16, col="blue")
#Sort the population by finess
newdataSort = newdata[order(newdata$y1),]
parents = newdataSort[c(19:20),]
#plot two parents
points(parents$x1, parents$y1, pch = 15, col = "green")
#Combine and Mutate
parentsRe = 0.5*parents$x1[1] + 0.5*parents$x1[2]
child = parentsRe + rnorm(1, mean = 0, sd = 0.5)
#Add child to the population and Sort again
intermediaPopulation = rbind(newdata, c(child, fitness(child)))
intermediaPopulationSort = intermediaPopulation[order(intermediaPopulation$y1),]
#plot child
points(child, fitness(child), pch=16, col="orange")
#Discard the individual with the lowest fitness
discard = intermediaPopulationSort[c(1),]
newPopulation = intermediaPopulationSort$x1[-1]

#plot discarded one
points(discard$x1, discard$y1, pch=16, col="red")

#A simple EA algorithm developed from scratch
evolution = function(evolutionTimes, initialPopulation, fitness){
  for (i in 1:evolutionTimes){
    
    y1 = fitness(initialPopulation)
    newdata = data.frame(initialPopulation, y1)
    newdataSort = newdata[order(newdata$y1),]
    parents = newdataSort[c(nrow(newdataSort)-1:nrow(newdataSort)),]
    parentsRe = 0.5*parents$initialPopulation[1] + 0.5*parents$initialPopulation[2]
    child = parentsRe + rnorm(1, mean = 0, sd = 0.5)
    intermediaPopulation = rbind(newdata, c(child, fitness(child)))
    intermediaPopulationSort = intermediaPopulation[order(intermediaPopulation$y1),]
    newPopulation = intermediaPopulationSort$initialPopulation[-1]
    initialPopulation = newPopulation
  }
  plot.function(fitness, from = -4, to = 4)
  points(newPopulation, fitness(newPopulation), pch=16, col="green")
}

#Test
evolution(30, x1, fitness)
#Test2 for comparison
evolution(100, x1, fitness)

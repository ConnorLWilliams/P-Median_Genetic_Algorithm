library(ggplot2)

##---- Setup----
locations <- 100 #number of total locations
p <- 10 #number of stations to be used to calculate medians
radius <- 3 #Used for Toy Datasets

pop_size <- 100
Population <- generate_population(pop_size, p, locations)
maxEpochs <- 200
print(Population)

loc_scatter <- toy_data(radius = 3, p = p, num_locations = locations)
plot(loc_scatter)

bestChrom1 <- Population[1]
bestChrom2 <- Population[2]

newPopulation <- Population

fit_over_time <- c()

##---- Rank Uniform Bit-Flip----
RankUniformBitFlip <- function(maxEpochs, Population, loc_scatter, pop_size) {
  newPopulation <- Population
  fit_over_time <- c()
  
  for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
    #Selection Matrix
    generation_rank <- sel_rank(Population, loc_scatter) #make ranking and assign ranks
    bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
    bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
    
    fit_over_time[i] <- generation_rank[100, 2]
    
    for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
      #Selection
      p1 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
      p2 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
      
      #Crossover
      u_children <- uniform(Population[p1, ], Population[p2, ])
      child1 <- as.vector(unlist(u_children[1]))
      child2 <- as.vector(unlist(u_children[2]))
      newPopulation[sel, ] <- child1
      newPopulation[pop_size/2 + sel, ] <- child2
    }
    
    #Mutation
    toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
    for(m in toMutate) {
      newPopulation[m, ] <- bit_flip(newPopulation[m, ])
    }
    
    #Elitism
    tempRankings <- sel_rank(newPopulation, loc_scatter)
    newPopulation[tempRankings[1, 1], ] <- bestChrom1
    newPopulation[tempRankings[2, 1], ] <- bestChrom2
    
    #print(length(which(bestChrom1 == 1)))
    
    Population <- newPopulation
  }
  
  return(list(Population, fit_over_time))
}

##---- Rank Two-Point Bit-Flip----
for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
  #Selection Matrix
  generation_rank <- sel_rank(Population, loc_scatter) #make ranking and assign ranks
  bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
  bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
  
  fit_over_time[i] <- generation_rank[100, 2]
  
  for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
    #Selection
    p1 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
    p2 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
    
    #Crossover
    u_children <- two_point(Population[p1, ], Population[p2, ])
    child1 <- as.vector(unlist(u_children[1]))
    child2 <- as.vector(unlist(u_children[2]))
    newPopulation[sel, ] <- child1
    newPopulation[pop_size/2 + sel, ] <- child2
  }
  
  #Mutation
  toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
  for(m in toMutate) {
    newPopulation[m, ] <- bit_flip(newPopulation[m, ])
  }
  
  #Elitism
  tempRankings <- sel_rank(newPopulation, loc_scatter)
  newPopulation[tempRankings[1, 1], ] <- bestChrom1
  newPopulation[tempRankings[2, 1], ] <- bestChrom2
  
  #print(length(which(bestChrom1 == 1)))
  
  Population <- newPopulation
}

##---- Roulette Uniform Bit-Flip----
for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
  #Selection Matrix
  generation_rank <- sel_roulette(Population, loc_scatter) #make ranking and assign ranks
  bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
  bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
  
  fit_over_time[i] <- generation_rank[100, 2]
  
  for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
    #Selection
    p1 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    p2 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    
    #Crossover
    u_children <- uniform(Population[p1, ], Population[p2, ])
    child1 <- as.vector(unlist(u_children[1]))
    child2 <- as.vector(unlist(u_children[2]))
    newPopulation[sel, ] <- child1
    newPopulation[pop_size/2 + sel, ] <- child2
  }
  
  #Mutation
  toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
  for(m in toMutate) {
    newPopulation[m, ] <- bit_flip(newPopulation[m, ])
  }
  
  #Elitism
  tempRankings <- sel_rank(newPopulation, loc_scatter)
  newPopulation[tempRankings[1, 1], ] <- bestChrom1
  newPopulation[tempRankings[2, 1], ] <- bestChrom2
  
  #print(length(which(bestChrom1 == 1)))
  
  Population <- newPopulation
}

##---- Roulette Two-Point Bit-Flip----
for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
  #Selection Matrix
  generation_rank <- sel_roulette(Population, loc_scatter) #make ranking and assign ranks
  bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
  bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
  
  fit_over_time[i] <- generation_rank[100, 2]
  
  for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
    #Selection
    p1 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    p2 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    
    #Crossover
    u_children <- two_point(Population[p1, ], Population[p2, ])
    child1 <- as.vector(unlist(u_children[1]))
    child2 <- as.vector(unlist(u_children[2]))
    newPopulation[sel, ] <- child1
    newPopulation[pop_size/2 + sel, ] <- child2
  }
  
  #Mutation
  toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
  for(m in toMutate) {
    newPopulation[m, ] <- bit_flip(newPopulation[m, ])
  }
  
  #Elitism
  tempRankings <- sel_rank(newPopulation, loc_scatter)
  newPopulation[tempRankings[1, 1], ] <- bestChrom1
  newPopulation[tempRankings[2, 1], ] <- bestChrom2
  
  #print(length(which(bestChrom1 == 1)))
  
  Population <- newPopulation
}

##---- Rank Uniform N-Bit-Flip----
for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
  #Selection Matrix
  generation_rank <- sel_rank(Population, loc_scatter) #make ranking and assign ranks
  bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
  bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
  
  fit_over_time[i] <- generation_rank[100, 2]
  
  for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
    #Selection
    p1 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
    p2 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
    
    #Crossover
    u_children <- uniform(Population[p1, ], Population[p2, ])
    child1 <- as.vector(unlist(u_children[1]))
    child2 <- as.vector(unlist(u_children[2]))
    newPopulation[sel, ] <- child1
    newPopulation[pop_size/2 + sel, ] <- child2
  }
  
  #Mutation
  toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
  for(m in toMutate) {
    newPopulation[m, ] <- n_bit_flip(newPopulation[m, ], p)
  }
  
  #Elitism
  tempRankings <- sel_rank(newPopulation, loc_scatter)
  newPopulation[tempRankings[1, 1], ] <- bestChrom1
  newPopulation[tempRankings[2, 1], ] <- bestChrom2
  
  #print(length(which(bestChrom1 == 1)))
  
  Population <- newPopulation
}

##---- Rank Two-Point N-Bit-Flip----
for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
  #Selection Matrix
  generation_rank <- sel_rank(Population, loc_scatter) #make ranking and assign ranks
  bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
  bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
  
  fit_over_time[i] <- generation_rank[100, 2]
  
  for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
    #Selection
    p1 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
    p2 <- rank_selection(generation_rank, sum(generation_rank[, 1]))
    
    #Crossover
    u_children <- two_point(Population[p1, ], Population[p2, ])
    child1 <- as.vector(unlist(u_children[1]))
    child2 <- as.vector(unlist(u_children[2]))
    newPopulation[sel, ] <- child1
    newPopulation[pop_size/2 + sel, ] <- child2
  }
  
  #Mutation
  toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
  for(m in toMutate) {
    newPopulation[m, ] <- n_bit_flip(newPopulation[m, ], p)
  }
  
  #Elitism
  tempRankings <- sel_rank(newPopulation, loc_scatter)
  newPopulation[tempRankings[1, 1], ] <- bestChrom1
  newPopulation[tempRankings[2, 1], ] <- bestChrom2
  
  #print(length(which(bestChrom1 == 1)))
  
  Population <- newPopulation
}

##---- Roulette Uniform N-Bit-Flip----
for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
  #Selection Matrix
  generation_rank <- sel_roulette(Population, loc_scatter) #make ranking and assign ranks
  bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
  bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
  
  fit_over_time[i] <- generation_rank[100, 2]
  
  for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
    #Selection
    p1 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    p2 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    
    #Crossover
    u_children <- uniform(Population[p1, ], Population[p2, ])
    child1 <- as.vector(unlist(u_children[1]))
    child2 <- as.vector(unlist(u_children[2]))
    newPopulation[sel, ] <- child1
    newPopulation[pop_size/2 + sel, ] <- child2
  }
  
  #Mutation
  toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
  for(m in toMutate) {
    newPopulation[m, ] <- n_bit_flip(newPopulation[m, ], p)
  }
  
  #Elitism
  tempRankings <- sel_rank(newPopulation, loc_scatter)
  newPopulation[tempRankings[1, 1], ] <- bestChrom1
  newPopulation[tempRankings[2, 1], ] <- bestChrom2
  
  #print(length(which(bestChrom1 == 1)))
  
  Population <- newPopulation
}

##---- Roulette Two-Point N-Bit-Flip----
for(i in 1:maxEpochs) { #Run for a maximum number of Epochs
  #Selection Matrix
  generation_rank <- sel_roulette(Population, loc_scatter) #make ranking and assign ranks
  bestChrom1 <- Population[generation_rank[100, 1], ] #elite1
  bestChrom2 <- Population[generation_rank[99, 1], ] #elite2
  
  fit_over_time[i] <- generation_rank[100, 2]
  
  for(sel in 1:pop_size/2) { #Create next generation using crossovers (100% crossover rate)
    #Selection
    p1 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    p2 <- roul_selection(generation_rank, sum(generation_rank[, 1]))
    
    #Crossover
    u_children <- two_point(Population[p1, ], Population[p2, ])
    child1 <- as.vector(unlist(u_children[1]))
    child2 <- as.vector(unlist(u_children[2]))
    newPopulation[sel, ] <- child1
    newPopulation[pop_size/2 + sel, ] <- child2
  }
  
  #Mutation
  toMutate <- sample(1:pop_size, pop_size * 0.02, replace = FALSE) #Randomly select 2% of the population to mutate
  for(m in toMutate) {
    newPopulation[m, ] <- n_bit_flip(newPopulation[m, ], p)
  }
  
  #Elitism
  tempRankings <- sel_rank(newPopulation, loc_scatter)
  newPopulation[tempRankings[1, 1], ] <- bestChrom1
  newPopulation[tempRankings[2, 1], ] <- bestChrom2
  
  #print(length(which(bestChrom1 == 1)))
  
  Population <- newPopulation
}

##---- Data Vis----
Data_Vis_Fit <- function(fit_over_time) {
  fot_df <- data.frame(
    epoch = 1:length(fit_over_time),
    fitness = fit_over_time
  )
  ggplot(fot_df, aes(epoch, fitness)) +
    geom_point() +
    geom_smooth()
}
 
Data_Vis_Chrom <- function(Chromosome){ 
  stations <- which(Chromosome == 0)
  medians <- which(Chromosome == 1)
  counter <- 0
  station_locs <- data.frame(x = double(), y = double())
  for(st in stations) {
    counter <- counter + 1
    station_locs[counter, ] <- loc_scatter[st, ]
  }
  counter <- 0
  medians_loc <- data.frame(x = double(), y = double())
  for(md in medians) {
    counter <- counter + 1
    medians_loc[counter, ] <- loc_scatter[md, ]
  }
  
  ggplot() + 
    geom_point(data=station_locs, aes(x=x, y=y), color='steelblue') + 
    geom_point(data=medians_loc, aes(x=x, y=y), color='coral2')
}

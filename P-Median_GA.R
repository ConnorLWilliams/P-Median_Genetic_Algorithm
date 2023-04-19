setwd("~/GitHub/P-Median_Genetic_Algorithm")

##---- Toy Dataset----
toy_scatter <- data.frame(
  X = c(),
  Y = c()
)

##---- Data input-----
locations <- 100 #number of total locations
p <- 10 #number of stations to be used to calculate medians

loc_scatter = data.frame(
  x = rnorm(100, mean = 50, sd=50),
  y = rnorm(100, mean = 50, sd=50)
)

##---- Initial Population----
#Generates a random Population of Chromosomes
generate_population <- function(population_size, p, locations) {
  pop_size <- population_size #population size
  
  Population <- matrix(0, pop_size, locations) #generate a matrix of [population size][locations]
  for(Chrom in 1:pop_size) {
    randStations <- sample(1:locations, p, replace = F)
    for(allele in randStations) {
      Population[Chrom, allele] <- 1
    }
  }
  
  return(Population)
}

pop_size <- 100
Population <- generate_population(pop_size, p, locations)
print(Population)

##---- Plot Connections----


##---- Distance Function----
get_dists <- function(locations, Chromosome, pop_size) { ##Distance function that should also serve as fitness
  dists <- matrix(0, pop_size)
  choice <- NULL
  for(loc in 1:dim(locations)[1]) { #For every location
    for(med in 1:pop_size) { #For every bit in Chromosome
      if(Chromosome[med] == 1) { #If the Chromosome is selected as a station
        if (is.null(choice)) { #Check if there is a computed distance
          choice <- sqrt((locations[med, 1] - locations[loc,1])^2 + (locations[med, 2] - locations[loc,2])^2) #if not find dist between location and station
        } else { #if so
          choice <- min(choice, sqrt((locations[med, 1] - locations[loc,1])^2 + (locations[med, 2] - locations[loc,2])^2)) #pick the minimum between stations seen and newest station
        }
      }
    }
    dists[loc] <- choice #Add choice Dist to Dists matrix (in case I want to return the whole matrix later)
    choice <- NULL #Reset choice
  }
  sum <- 0
  for(i in 1:dim(dists)[1]) {
    sum <- sum + dists[i] #Sum distances
  }
  return(sum)
}

## Testing Code

# test_scatter <- data.frame(
#   X = c(5, 7, 13, 15),
#   Y = c(1, 1, 1, 1))
# test_Pop <- c(0, 0, 1, 1)

# print(get_dists(loc_scatter, Population[100, ], pop_size))
# print(get_dists(test_scatter, test_Pop, 4))

##---- Selection----
rank_max <- 0
for(i in 1:pop_size) {
  rank_max <- rank_max + i
}

sel_rank <- function(Population, Locations) {
  ranks <- matrix(0, nrow(Population), 2)
  
  for(Chrom in 1:nrow(Population)) {
    fitness <- get_dists(Locations, Population[Chrom, ], nrow(Population))
    ranks[Chrom, 1] <- Chrom
    ranks[Chrom, 2] <- fitness
  }
  
  ranks <- ranks[order(ranks[,2], decreasing = TRUE), ]
  
  #Needs to be finished currently takes in the input and creates the ranking matrix, but does not define integer ranges for selection
  return(ranks)
}

sel_roulette <- function(Population, Locations) {
  ranks <- matrix(0, nrow(Population), 3)
  
  for(Chrom in 1:nrow(Population)) {
    fitness <- get_dists(Locations, Population[Chrom, ], nrow(Population))
    ranks[Chrom, 1] <- Chrom
    ranks[Chrom, 2] <- fitness
  }
  
  total_fit <- sum(ranks[, 2])
  
  ranks <- ranks[order(ranks[,2], decreasing = TRUE), ]
  
  for(Chrom in 1:nrow(ranks)) {
    ranks[Chrom, 3] <- total_fit / ranks[Chrom, 2]
  }
  
  #Needs to be finished currently takes in the input and creates the ranking matrix, but does not define integer ranges for selection
  return(ranks)
  
}

rank_selection <- function(rank_mat, r_max) { #rmax -> either rank or roulette max
  to_populate <- sample(1:r_max, 1) #take a sample within the range of the ranks 1:summation(population_size)
  rank_d <- 0
  for(i in 1:pop_size) { #step through the summation when the selected number <= the summation then it is chosen (solves range sizes)
    rank_d <- rank_d + i
    if(to_populate <= rank_d) {
      return(rank_mat[i, 1])
    }
  }
}

roul_selection <- function(rank_mat, r_max) { #rmax -> either rank or roulette max
  #to_populate <- sample(1:r_max, 1) #take a sample within the range of the ranks 1:summation(population_size)
  to_populate <- round(runif(1,1,r_max), 5)
  rank_d <- 0
  for(i in 1:pop_size) { #step through the summation when the selected number <= the summation then it is chosen (solves range sizes)
    rank_d <- rank_d + rank_mat[i, 3]
    if(to_populate <= rank_d) {
      return(rank_mat[i, 1])
    }
  }
}

# RANK TESTING CODE
generation_rank <- sel_rank(Population, loc_scatter)
print(generation_rank)
for(i in 1:100) {
  print(rank_selection(generation_rank, sum(generation_rank[, 1])))
}
rank_selection(generation_rank, sum(generation_rank[, 1]))

# ROULETTE TESTING CODE
generation_roul <- sel_roulette(Population, loc_scatter)
print(generation_roul)
print(sum(generation_roul[, 3]))
for(i in 1:100) {
  print(roul_selection(generation_roul, sum(generation_roul[, 3])))
}
roul_selection(generation_roul, sum(generation_roul[, 3]))


##---- Crossover----

##---- Mutation----

##---- Genetic Algorithm----


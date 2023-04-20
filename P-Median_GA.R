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
##Rank Selection
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

##Roulette Selection
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

##Get a parent from the Rank Selection
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

##Get a parent from the Roulette Selection
roul_selection <- function(rank_mat, r_max) {
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
rank_parent_sel <- c()
for(i in 1:200) {
  rank_parent_sel[i] <- rank_selection(generation_rank, sum(generation_rank[, 1]))
}
hist(rank_parent_sel, 100)

# ROULETTE TESTING CODE
generation_roul <- sel_roulette(Population, loc_scatter)
print(generation_roul)
print(sum(generation_roul[, 3]))
roul_parent_sel <- c()
for(i in 1:200) {
  roul_parent_sel[i] <- roul_selection(generation_roul, sum(generation_roul[, 3]))
}
hist(roul_parent_sel, 100)

##---- Crossover----
## Uniform Crossover
uniform <- function(P1, P2) {
  C1 <- c()
  C2 <- c()
  for(i in 1:length(P1)) {
    m <- sample(0:1, 1) #represents the mask bit at this position
    if(m == 0) {
      C1[i] <- P1[i]
      C2[i] <- P2[i]
    } else {
      C1[i] <- P2[i]
      C2[i] <- P1[i]
    }
  }
  
  #Needs fixup
  if(sum(C1 == 1) > p) {
    medianPos <- which(C1 == 1)
    newPos <- sample(medianPos, p, replace = FALSE) #randomly select p medians to keep
    C1 <- lapply(C1, replace, TRUE, 0)
    for(m in newPos) {
      C1[m] <- 1
    }
  }
  if(sum(C1 == 1) < p) {
    while(sum(C1 == 1) < p) {
      C1[sample(1:length(C1), 1)] <- 1 #randomly select positions and set them to 1 -- Could in theory take some number of them from P1
    }
  }
  
  if(sum(C2 == 1) > p) {
    medianPos <- which(C2 == 1)
    newPos <- sample(medianPos, p, replace = FALSE)
    C2 <- lapply(C2, replace, TRUE, 0)
    for(m in newPos) {
      C2[m] <- 1
    }
  }
  if(sum(C2 == 1) < p) {
    while(sum(C2 == 1) < p) {
      C2[sample(1:length(C2), 1)] <- 1
    }
  }
  return(list(C1, C2))

}

# Uniform Testing Code -> uses Rank Selection
u_children <- uniform(Population[rank_parent_sel[1], ], Population[rank_parent_sel[2], ])
child1 <- as.vector(unlist(u_children[1]))
child2 <- as.vector(unlist(u_children[2]))
print(sum(child1 == 1))
print(sum(child2 == 1))

##---- Mutation----

##---- Genetic Algorithm----


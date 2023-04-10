setwd("~/GitHub/P-Median_Genetic_Algorithm")

##---- Data input-----
locations <- 100 #number of total locations
p <- 10 #number of stations to be used to calculate medians

loc_scatter = data.frame(
  x = rnorm(100, mean = 50, sd=50),
  y = rnorm(100, mean = 50, sd=50)
)

##---- Initial Population----
pop_size <- 100 #population size

Population <- matrix(0, pop_size, locations) #generate a matrix of [population size][locations]
for(Chrom in 1:pop_size) {
  randStations <- sample(1:locations, p, replace = F)
  for(allele in randStations) {
    Population[Chrom, allele] <- 1
  }
}

print(Population)

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

print(get_dists(loc_scatter, Population[100, ], pop_size))
##---- Selection----

##---- Crossover----

##---- Mutation----

##---- Genetic Algorithms----

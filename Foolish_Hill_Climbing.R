library(ggplot2)

##---- Setup----
# locations <- 100 #number of total locations
# p <- 10 #number of stations to be used to calculate medians
# radius <- 3 #Used for Toy Datasets
# 
# pop_size <- 1
# Population <- generate_population(pop_size, p, locations)
# maxEpochs <- 200
# print(Population)
# 
# # loc_scatter <- toy_data(radius = 3, p = p, num_locations = locations)
# # plot(loc_scatter)
# 
# newPopulation <- Population
# 
# fit_over_time <- c()

##---- FHC Bit-Flip----
FoolishHillClimbBitFlip <- function(Population, loc_scatter, pop_size) {
  temp <- 10
  a <- 0.98
  b <- 1.02
  itt <- 1000
  f_i <- 1
  fit_over_time <- c()
  
  while(temp > 1) {
    for(i in 1:itt) {
      newS <- bit_flip(Population[1, ])
      oldDist <- new_get_dists(loc_scatter, Population[1, ], pop_size)
      newDist <- new_get_dists(loc_scatter, newS, pop_size)
      fit_over_time[f_i] <- oldDist
      if(newDist < oldDist) {
        Population[1, ] <- newS
      }
      temp <- a*temp
      itt <- b*itt
      f_i <- f_i + 1
    }
  }
  
  return(list(Population[1, ], fit_over_time))
}

##---- FHC N-Bit-Flip----
FoolishHillClimbNBitFlip <- function(Population, loc_scatter, pop_size) {
  temp <- 10
  a <- 0.98
  b <- 1.02
  itt <- 1000
  f_i <- 1
  fit_over_time <- c()
  
  while(temp > 1) {
    for(i in 1:itt) {
      newS <- n_bit_flip(Population[1, ], p)
      oldDist <- new_get_dists(loc_scatter, Population[1, ], pop_size)
      newDist <- new_get_dists(loc_scatter, newS, pop_size)
      fit_over_time[f_i] <- oldDist
      if(newDist < oldDist) {
        Population[1, ] <- newS
      }
      temp <- a*temp
      itt <- b*itt
      f_i <- f_i + 1
    }
  }
  
  return(list(Population[1, ], fit_over_time))
}




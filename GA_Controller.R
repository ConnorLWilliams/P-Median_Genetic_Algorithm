library(ggplot2)

##---- Setup----
locations <- 100 #number of total locations
p <- 10 #number of stations to be used to calculate medians
radius <- 3 #Used for Toy Datasets

pop_size <- 100
innitial_population <- generate_population(pop_size, p, locations)
maxEpochs <- 10
print(Population)

loc_scatter <- toy_data(radius = 3, p = p, num_locations = locations)
plot(loc_scatter)

##---- Rank Uniform Bit-Flip----
RaUBF <- RankUniformBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RaUBF_Pop <- as.matrix(unlist(RaUBF[[1]]))
RaUBF_Fit <- as.vector(unlist(RaUBF[[2]]))
ranks <- sel_rank(RaUBF_Pop, loc_scatter)
bestChrom <- RaUBF_Pop[ranks[pop_size, 1], ]
RaUBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RaUBF_Fit)

##---- Rank Two-Point Bit-Flip----


##---- Roulette Uniform Bit-Flip----


##---- Roulette Two-Point Bit-Flip----


##---- Rank Uniform N-Bit-Flip----


##---- Rank Two-Point N-Bit-Flip----


##---- Roulette Uniform N-Bit-Flip----


##---- Roulette Two-Point N-Bit-Flip----


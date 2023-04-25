library(ggplot2)

##---- Setup----
locations <- 50 #number of total locations
p <- 5 #number of stations to be used to calculate medians
radius <- 3 #Used for Practical Datasets

pop_size <- 100
innitial_population <- generate_population(pop_size, p, locations)
maxEpochs <- 200
print(Population)

#loc_scatter <- toy_data(radius = 3, p = p, num_locations = locations)

loc_scatter = data.frame(
  x = rnorm(locations, mean = 250, sd= 100),
  y = rnorm(locations, mean = 250, sd= 100)
)
plot(loc_scatter)

timing <- proc.time()

##---- Rank Uniform Bit-Flip----
RaUBF <- RankUniformBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RaUBF_Pop <- as.matrix(unlist(RaUBF[[1]]))
RaUBF_Fit <- as.vector(unlist(RaUBF[[2]]))
ranks <- sel_rank(RaUBF_Pop, loc_scatter)
bestChrom <- RaUBF_Pop[ranks[pop_size, 1], ]
RaUBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RaUBF_Fit)
ggsave("RankUniformBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RankUniformBitFlipChromPlot50L5PPractical.pdf")

##---- Rank Two-Point Bit-Flip----
RaTPBF <- RankTwoPointBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RaTPBF_Pop <- as.matrix(unlist(RaTPBF[[1]]))
RaTPBF_Fit <- as.vector(unlist(RaTPBF[[2]]))
ranks <- sel_rank(RaTPBF_Pop, loc_scatter)
bestChrom <- RaTPBF_Pop[ranks[pop_size, 1], ]
RaTPBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RaTPBF_Fit)
ggsave("RankTwoPointBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RankTwoPointBitFlipChromPlot50L5PPractical.pdf")

##---- Roulette Uniform Bit-Flip----
RoUBF <- RouletteUniformBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RoUBF_Pop <- as.matrix(unlist(RoUBF[[1]]))
RoUBF_Fit <- as.vector(unlist(RoUBF[[2]]))
ranks <- sel_rank(RoUBF_Pop, loc_scatter)
bestChrom <- RoUBF_Pop[ranks[pop_size, 1], ]
RoUBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RoUBF_Fit)
ggsave("RouletteUniformBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RouletteUniformBitFlipChromPlot50L5PPractical.pdf")

##---- Roulette Two-Point Bit-Flip----
RoTPBF <- RouletteTwoPointBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RoTPBF_Pop <- as.matrix(unlist(RoTPBF[[1]]))
RoTPBF_Fit <- as.vector(unlist(RoTPBF[[2]]))
ranks <- sel_rank(RoTPBF_Pop, loc_scatter)
bestChrom <- RoTPBF_Pop[ranks[pop_size, 1], ]
RoTPBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RoTPBF_Fit)
ggsave("RouletteTwoPointBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RouletteTwoPointBitFlipChromPlot50L5PPractical.pdf")

##---- Rank Uniform N-Bit-Flip----
RaUNBF <- RankUniformNBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RaUNBF_Pop <- as.matrix(unlist(RaUNBF[[1]]))
RaUNBF_Fit <- as.vector(unlist(RaUNBF[[2]]))
ranks <- sel_rank(RaUNBF_Pop, loc_scatter)
bestChrom <- RaUNBF_Pop[ranks[pop_size, 1], ]
RaUNBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RaUNBF_Fit)
ggsave("RankUniformNBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RankUniformNBitFlipChromPlot50L5PPractical.pdf")


##---- Rank Two-Point N-Bit-Flip----
RaTPNBF <- RankTwoPointNBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RaTPNBF_Pop <- as.matrix(unlist(RaTPNBF[[1]]))
RaTPNBF_Fit <- as.vector(unlist(RaTPNBF[[2]]))
ranks <- sel_rank(RaTPNBF_Pop, loc_scatter)
bestChrom <- RaTPNBF_Pop[ranks[pop_size, 1], ]
RaTPNBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RaTPNBF_Fit)
ggsave("RankTwoPointNBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RankTwoPointNBitFlipChromPlot50L5PPractical.pdf")


##---- Roulette Uniform N-Bit-Flip----
RoUNBF <- RouletteUniformNBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RoUNBF_Pop <- as.matrix(unlist(RoUNBF[[1]]))
RoUNBF_Fit <- as.vector(unlist(RoUNBF[[2]]))
ranks <- sel_rank(RoUNBF_Pop, loc_scatter)
bestChrom <- RoUNBF_Pop[ranks[pop_size, 1], ]
RoUNBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RoUNBF_Fit)
ggsave("RouletteUniformNBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RouletteUniformNBitFlipChromPlot50L5PPractical.pdf")


##---- Roulette Two-Point N-Bit-Flip----
RoTPNBF <- RouletteTwoPointNBitFlip(maxEpochs = maxEpochs, Population = innitial_population, loc_scatter = loc_scatter, pop_size = pop_size)

RoTPNBF_Pop <- as.matrix(unlist(RoTPNBF[[1]]))
RoTPNBF_Fit <- as.vector(unlist(RoTPNBF[[2]]))
ranks <- sel_rank(RoTPNBF_Pop, loc_scatter)
bestChrom <- RoTPNBF_Pop[ranks[pop_size, 1], ]
RoTPNBF_best_fit <- ranks[pop_size, 2]

Data_Vis_Fit(RoTPNBF_Fit)
ggsave("RouletteTwoPointNBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("RouletteTwoPointNBitFlipChromPlot50L5PPractical.pdf")

##---- SA/FHC SetUp----
sa_innitial_population <- generate_population(1, p, locations)

##---- Simulated Annealing Bit Flip----
SimAnnBF <- SABF(Population = sa_innitial_population, loc_scatter = loc_scatter, pop_size = 1)

bestChrom <- as.vector(unlist(SimAnnBF[[1]]))
SABF_Fit <- as.vector(unlist(SimAnnBF[[2]]))
SABF_best_fit <- new_get_dists(locations = loc_scatter, Chromosome = bestChrom, pop_size = 1)

Data_Vis_Fit(SABF_Fit)
ggsave("SimulatedAnnealingBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("SimulatedAnnealingBitFlipChromPlot50L5PPractical.pdf")

##---- Simulated Annealing N Bit Flip----
SimAnnNBF <- SANBF(Population = sa_innitial_population, loc_scatter = loc_scatter, pop_size = 1)

bestChrom <- as.matrix(unlist(SimAnnNBF[[1]]))
SANBF_Fit <- as.vector(unlist(SimAnnNBF[[2]]))
SANBF_best_fit <- new_get_dists(locations = loc_scatter, Chromosome = bestChrom, pop_size = 1)

Data_Vis_Fit(SANBF_Fit)
ggsave("SimulatedAnnealingNBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("SimulatedAnnealingNBitFlipChromPlot50L5PPractical.pdf")

##----  FHC Bit Flip----
FHCBF <- FoolishHillClimbBitFlip(Population = sa_innitial_population, loc_scatter = loc_scatter, pop_size = 1)

bestChrom <- as.vector(unlist(FHCBF[[1]]))
FHCBF_Fit <- as.vector(unlist(FHCBF[[2]]))
FHCBF_best_fit <- new_get_dists(locations = loc_scatter, Chromosome = bestChrom, pop_size = 1)

Data_Vis_Fit(FHCBF_Fit)
ggsave("FoolishHillClimbingBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("FoolishHillClimbingBitFlipChromPlot50L5PPractical.pdf")

##----  FHC N Bit Flip----
FHCNBF <- FoolishHillClimbNBitFlip(Population = sa_innitial_population, loc_scatter = loc_scatter, pop_size = 1)

bestChrom <- as.vector(unlist(FHCNBF[[1]]))
FHCNBF_Fit <- as.vector(unlist(FHCNBF[[2]]))
FHCNBF_best_fit <- new_get_dists(locations = loc_scatter, Chromosome = bestChrom, pop_size = 1)

Data_Vis_Fit(FHCNBF_Fit)
ggsave("FoolishHillClimbingNBitFlipFitnessCurve50L5PPractical.pdf")
Data_Vis_Chrom(bestChrom)
ggsave("FoolishHillClimbingNBitFlipChromPlot50L5PPractical.pdf")

##---- Data Evaluation----
Best50L5P <- data.frame(
  names = c("Rank Uniform Bit-Flip", "Rank Two-Point Bit-Flip", "Roulette Uniform Bit-Flip", "Roulette Two-Point Bit-Flip", "Rank Uniform N-Bit-Flip", "Rank Two-Point N-Bit-Flip", "Roulette Uniform N-Bit-Flip", "Roulette Two-Point N-Bit-Flip", "Simulated Annealing Bit-Flip", "Simulated Annealing N-Bit Flip", "Foolish Hill Climbing Bit-Flip", "Foolish Hill Climbing N-Bit-Flip"),
  fitnesses = c(RaUBF_best_fit, RaTPBF_best_fit, RoUBF_best_fit, RoTPBF_best_fit, RaUNBF_best_fit, RaTPNBF_best_fit, RoUNBF_best_fit, RoTPNBF_best_fit, SABF_best_fit, SANBF_best_fit, FHCBF_best_fit, FHCNBF_best_fit)
)
Location50L5P<- loc_scatter
write.csv(Best50L5P, file = "FitnessFor50L5P_PracticalData.csv")
write.csv(Location50L5P, file = "LocationsFor50L5P_PracticalData.csv")

timeused <- proc.time() - timing

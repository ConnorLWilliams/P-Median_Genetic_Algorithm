library(ggplot2)

##---- Setup----
locations <- 100 #number of total locations
p <- 10 #number of stations to be used to calculate medians
radius <- 3 #Used for Toy Datasets

pop_size <- 1
Population <- generate_population(pop_size, p, locations)
maxEpochs <- 200
print(Population)

loc_scatter <- toy_data(radius = 3, p = p, num_locations = locations)
plot(loc_scatter)

newPopulation <- Population

fit_over_time <- c()

##---- SA Bit-Flip----
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
    if((newDist < oldDist || (sample(runif(1) < exp((oldDist - newDist) / temp))))) {
      Population[1, ] <- newS
    }
    temp <- a*temp
    itt <- b*itt
    f_i <- f_i + 1
  }
}

print(new_get_dists(loc_scatter, Population[1, ], pop_size))

##---- SA N-Bit-Flip----
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
    if((newDist < oldDist || (sample(runif(1) < exp((oldDist - newDist) / temp))))) {
      Population[1, ] <- newS
    }
    temp <- a*temp
    itt <- b*itt
    f_i <- f_i + 1
  }
}

print(new_get_dists(loc_scatter, Population[1, ], pop_size))

##---- Data Vis----
fot_df <- data.frame(
  epoch = 1:length(fit_over_time),
  fitness = fit_over_time
)
ggplot(fot_df, aes(epoch, fitness)) +
  geom_point() +
  geom_smooth()

stations <- which(Population[1, ] == 0)
medians <- which(Population[1, ] == 1)
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


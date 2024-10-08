library(nycflights13,ggplot2)
head(flights)
names(flights)

#distribution of the variable distance
ggplot(flights, aes(x=distance)) + 
  geom_histogram(col='black', fill='blue', binwidth=200, na.rm=TRUE) + 
  xlab("Distance of Flights in Miles") + ylab("Frequency") + 
  ggtitle("Distribution of Flight Distances from 3-NYC Airports")

#take samples of size 5 and compute sample mean
#for each possible sample
library(mosaic)
M = do(1000)*mean(sample(flights$distance,size = 5, replace = FALSE))
dim(M)
head(M)

ggplot(M, aes(x=M$mean)) + 
  geom_histogram(col='black', fill='blue', binwidth=200, na.rm=TRUE) + 
  xlab("Sample mean distance, n = 5") + ylab("Frequency") + 
  ggtitle("Distribution of sample mean Flight Distances from 3-NYC Airports")

#take samples of size 10 and compute sample mean
#for each possible sample
M = do(1000)*mean(sample(flights$distance,size = 10, replace = FALSE))
dim(M)
head(M)

ggplot(M, aes(x=M$mean)) + 
  geom_histogram(col='black', fill='orange', binwidth=200, na.rm=TRUE) + 
  xlab("Sample mean distance, n = 10") + ylab("Frequency") + 
  ggtitle("Distribution of sample mean Flight Distances from 3-NYC Airports")



#take samples of size 30 and compute sample mean
#for each possible sample
M30 = do(1000)*mean(sample(flights$distance,size = 30, replace = FALSE))

ggplot(M30, aes(x=M30$mean)) + 
  geom_histogram(col='black', fill='green', binwidth=100, na.rm=TRUE) + 
  xlab("Sample mean distance, n = 30") + ylab("Frequency") + 
  ggtitle("Distribution of sample mean Flight Distances from 3-NYC Airports")

#take samples of size 50 and compute sample mean
#for each possible sample
M50 = do(1000)*mean(sample(flights$distance,size = 50, replace = FALSE))

ggplot(M50, aes(x=M50$mean)) + 
  geom_histogram(col='black', fill='brown', binwidth=50, na.rm=TRUE) + 
  xlab("Sample mean distance, n = 50") + ylab("Frequency") + 
  ggtitle("Distribution of sample mean Flight Distances from 3-NYC Airports")


arraymean = c(rep("M",1000),rep("M30",1000),rep("M50",1000))
Mean_values = array(data =c(unlist(M),unlist(M30),unlist(M50)))
mean_dist = data.frame(arraymean, Mean_values )
head(mean_dist)

ggplot(data = mean_dist, aes(x= Mean_values )) +
  geom_freqpoly(aes(color=arraymean),binwidth=50)


#Example 2.5, with Sample Size 50, Mean distance a flight travel is less than 1025
Mean_of_samplemean = mean(M50$mean) #Means of all
SD_of_samplemean = sd(M50$mean)/sqrt(50)

pnorm(1025,Mean_of_samplemean,SD_of_samplemean )

#practice 2.2
pnorm(12,13,(4/sqrt(40)))

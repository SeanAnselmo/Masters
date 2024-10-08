#Simulating sampling distribution of the sample proportion
set.seed(42)
# Assuming population parameter as a reference
population_size = 1000
true_proportion <- 0.34

#simulate sampling distribution of sample proportions
# by replicating 1000 possible samples of size 50
#library(mosaic)
#data = c(rep(1,340),rep(0,1000-340))
#samp_dist = do(1000)*mean(sample(data,50, replace = FALSE))

n_samples <- 1000
sample_size <- 50

#Alternative method of simulate sampling distribution of sample proportions
samp_dist <- replicate(1000, {
  binom_data <- rbinom(50, size = 1, prob = true_proportion)
  p_hat <- mean(binom_data)
  return(p_hat)
})



# visualize the distribution of p_hat
hist(samp_dist, col = "lightgreen", 
     main = "Sampling Distribution of Sample Proportions",
     xlab = "Sample Proportion", ylab = "Frequency")
abline(v = true_proportion, col = "Black", lwd = 1.5)

#Example 2.6
m_phat = 0.15
sd_phat = sqrt(0.15*(1-0.15)/300)
pnorm(c(0.1,0.18),m_phat,sd_phat) #For all values of 0.1 and 0.18 in an array

#Practice 2.3
1-pnorm(0.9, 0.85,sqrt(0.85*(1-0.85)/80))

#Practice 2.4

qnorm(0.975,0,1)

n = 1.96*


#distribution of sample standard deviation(SD)
library(nycflights13)
library(ggplot2,dyplr)

head(flights)
library(mosaic)
SD = do(1000)*sd(sample(flights$distance,size = 50, replace = TRUE))
Chi = (50-1)*SD^2/sd(flights$distance)^2

head(Chi)
ggplot(Chi, aes(x=Chi$sd)) + 
  geom_histogram(col='black', fill='blue', binwidth=1, na.rm=TRUE) + 
  xlab("Sample SD distance, n = 50") + ylab("Frequency") + 
  ggtitle("Distribution of sample SD Flight Distances from 3-NYC Airports")
ggplot(flights, aes(x = distance)) + geom_histogram()

#Example 2.6. Cont.
#Assuming population stdev. to be stdev. of the original data
sigma = sd(flights$distance)
sigma

pchisq(c(976.6917, 910.4949),999)

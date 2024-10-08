library(ggplot2)
data("iris")
head(iris)

#Example 4.3
#visualization to see how sepal length differs between species
ggplot(iris, aes(x=Species, y = Sepal.Length)) + geom_boxplot() +
  labs(title = " Boxplot of Sepal Length by Species")

setosa <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor <- iris$Sepal.Length[iris$Species == "versicolor"]

library(mosaic)
#mannual calculations
favstats(setosa)
favstats(versicolor)

m_v = 5.936
m_s = 5.006
V_v = 0.2664326
V_s = 0.124249

df = ((V_v + V_s)/50)^2/(((V_v/50)^2 + (V_s/50)^2)/49)

t = (m_v - m_s)/sqrt((V_v+V_s)/50)
  
1 - pt(t,df)  

#using t.test
t.test(versicolor,setosa, alternative = "greater", 
       conf.level=0.95, var.equal=FALSE)

#Example 4.4
#bootstrapping 
B = do(10000)*(mean(resample(versicolor)) - mean(resample(setosa)) )
#visualizing
ggplot(B, aes(x = result )) + geom_histogram(binwidth = 0.03, fill = "blue") +
  labs(title = "Bootstrapping distribution of mean differences",
       x = "mean difference", y = "frequency")

# 95% confidence intervals
quantile(B$result, c(0.025,0.975))

#Example 4.5
#permutation test
# Load necessary library
library(dplyr)
# Load the iris dataset (if not already loaded)
data(iris)

# Extract sepal length for setosa and versicolor
setosa_length <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor_length <- iris$Sepal.Length[iris$Species == "versicolor"]
length(setosa_length) # this is useful to create permuted samples later

# Observed test statistic (difference in means)
observed_diff <- mean(setosa_length) - mean(versicolor_length)

# pool both in to one set
all_lengths <- c(setosa_length, versicolor_length)

# decide nNumber of permutations
n_perm <- 1000

# create an empty vector to store permutation test statistics
perm_test_stats <- numeric(n_perm)

# Permutation test
for (i in 1:n_perm) {
  # Permute the species labels
  permuted_species <- sample(all_lengths)
  
  # Calculate the test statistic for the permuted data
  perm_setosa <- permuted_species[1:50]
  perm_versicolor <- all_lengths[51:100]
  perm_test_stats[i] <- mean(perm_setosa) - mean(perm_versicolor)
}

# Calculate the p-value
p_value <- sum(abs(perm_test_stats)>= abs(observed_diff))/1000
#alternative way
# p_value = mean(abs(perm_test_stats) >= abs(observed_diff))

# Display the results
cat("Observed Mean Difference:", observed_diff, "\n")
cat("Permutation Test p-value:", p_value, "\n")


#Example 4.6 Test for two proportions
x.june = 1540
n.june = 2000
x.sept = 553
n.sept = 2125
prop.test(c(x.june,x.sept), c(n.june,n.sept), 
          alternative = "greater",correct=FALSE)
#Practice 4.4 
D_300mg <- c(284,279,289,292,287,295,285,279,306,298)
D_600mg <- c(298,307,297,279,291,335,299,300,306,291)

#T test
t.test(D_300mg,D_600mg, alternative = "greater", 
       conf.level=0.95, var.equal=FALSE)
#Bootstrap
B = do(1000)*mean(resample(D_600mg)-resample(D_300mg))
quantile(B$mean,c(0.025,0.975))
ggplot(B,aes(x=mean))+geom_histogram(binwidth = 5)

#Permutation test
observed_difference = mean(D_600mg) - mean(D_300mg)
all <- c(D_600mg, D_300mg)
n = 1000

drug_test_stats <- numeric(n)
for (i in 1:n){
  perm_drugs <- sample(all)
  perm_600 <- perm_drugs[1:10]
  perm_300 <- perm_drugs[11:20]
  perm_stats[i] <- mean(perm_600)-mean(perm_300)
}

pvalue = mean(abs(perm_stats)>=abs(observed_difference))

#Practice 4.5
Ad_A = 25
n_Ad_A = 200
Ad_B = 30
n_Ad_B = 180

prop.test(c(Ad_A,Ad_B), c(n_Ad_A,n_Ad_B), alternative = "two.sided", correct = FALSE)

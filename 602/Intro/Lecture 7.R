#Example 4.1
# Sample data representing response times
response_times <- c(180, 210, 195, 220, 205, 198, 215, 188, 203, 197)
boxplot(response_times, 
        main = "Boxplot of Response Times",
        ylab = "Response Times (ms)",
        col = "skyblue",
        border = "black",
        horizontal = FALSE)
# Hypothesized mean for the t-test
hypothesized_mean <- 200
#part a) 
# Perform a t-test
library(mosaic)
t_test_result <- t.test(response_times, mu = hypothesized_mean, 
                        alternative ="two.sided")
# Extract the p-value
p_value_t_test <- t_test_result$p.value
# Print the results
cat("T-Test p-value:", p_value_t_test, "\n")


#part b)
observed_mean <- mean(response_times)

# Perform bootstrapping
set.seed(42)
bootstrap_means <- do(1000)*mean(resample(response_times, replace=TRUE))

#visualizing sampling distribution
ggplot(bootstrap_means) + 
  geom_histogram(aes(x=mean,bins = 30),fill = "skyblue", color = "black") +
  geom_vline(xintercept = observed_mean, color="red") + 
  labs(title = "Histogram of bootstrap means", x = "bootstrap mean response time", y = "frequency")


cat("95% confidence intervals is")
# compute 95% confidence intervals
quantile(bootstrap_means$mean, c(0.025,0.975))

#Example 4.2

rolls = c(rep(1,13),rep(0,100-13))
observed_prop = 13/100
set.seed(42)
bootstrap_props <- do(1000)*mean(resample(rolls, replace=TRUE))
ggplot(bootstrap_props) + 
  geom_histogram(aes(x=mean),binwidth = 0.01,fill = "skyblue", color = "black") +
  geom_vline(xintercept = observed_prop, color="red") + 
  geom_vline(xintercept = 1/6, color="black") +
  labs(title = "Histogram of bootstrap proportions", 
       x = "bootstrap proportion of rolling 6", y = "frequency")
# compute 95% confidence intervals
quantile(bootstrap_props$mean, c(0.025,0.975))
#p value chance of observing 13 sixes out of 100 when p = 1/6(null is true)
pvalue = pbinom(13,100,1/6)
pvalue


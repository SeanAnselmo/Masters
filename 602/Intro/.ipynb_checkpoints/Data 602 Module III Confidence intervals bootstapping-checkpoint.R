#example 3.3
Studentdebt = c(20345,34589,35999,29000,28983,34500,33345,35023,32333,29000,29456,30453, 34444,35435,33543,32346,34545)

#density plot of the sample 
densityplot(Studentdebt, xlab="Estimated Debt at Graduation", main="Distribution of Student Debt at Graduation")

n = length(Studentdebt)
t = qt(0.975, n-1)
UL = mean(Studentdebt) + t*sd(Studentdebt)/sqrt(n)
LL = mean(Studentdebt) - t*sd(Studentdebt)/sqrt(n)
cat("95% CI for mean student debt upon graduation is", "(", LL, ",", UL, ")")


#b )
library(mosaic)
M = do(1000)*mean(sample(Studentdebt, replace = TRUE))
dim(M)
head(M)

# 95% CI using the sampling distribution of sample mean

UL = quantile(M$mean, 0.975)
LL = quantile(M$mean, 0.025)
cat("95% CI for mean student debt upon graduation is", "(", LL, ",", UL, ")")

#part c
#plot bootstrapping distribution of sample means
ggplot(data =M, aes(x=M$mean)) + geom_histogram(color = "lightblue", fill = "lightblue")+ 
  xlab("Values of Bootstrap Mean") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistic: Sample Mean") + 
  geom_vline(xintercept = UL, color="red") + geom_vline(xintercept = LL, color="red")

#Example 3.4
B = do(1000)*mean(resample(c(rep(1, 382), rep(0, 1034-382)), 1034));
quantile(B$mean, 0.025);
quantile(B$mean, 0.975);

ggplot(data =B, aes(x=B$mean)) + geom_histogram(color = "lightblue", fill = "lightblue")+ 
  xlab("Values of Bootstrap proportion") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistic: Sample proportion") + 
  geom_vline(xintercept = quantile(B$mean, 0.025), color="red") + geom_vline(xintercept = quantile(B$mean, 0.975), color="red")

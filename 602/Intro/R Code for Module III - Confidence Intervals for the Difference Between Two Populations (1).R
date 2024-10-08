library(nycflights13,ggplot2)
head(flights)
library(mosaic)

#Example 3.5
library(dplyr)
new_flights = filter(flights,carrier == c("UA","B6") )
head(new_flights)
dim(new_flights)
#numerical summeries for departure delay by carrier
favstats(dep_delay ~ carrier, data = new_flights, na.rm = TRUE)

#Confidence interval for difference btween mean delay time of UA and B6
# Mannual calculation
point_est = 13.26928 - 12.00170
df = (((39.11765^2/26939)+(35.22495^2/28877))^2)/ (((1/26938)*((39.11765^2/26939)^2)) 
                                                   +((1/28876)*((35.22495^2/28877)^2)))
t = qt(0.975, 54183)
moe = t * sqrt((39.11765^2/26939)+ (35.22495^2/28877))

UL = point_est + moe
LL = point_est - moe
cat("95% confidence interval for difference in 
    population mean is (", LL, ",", UL, ")")
#alternative method (mosaic library)
t.test(~ dep_delay | carrier, data=new_flights, 
       conf.level = 0.95, var.equal=FALSE)$conf

#practice 3.1
t.test(~ dep_delay | carrier, data=new_flights, conf.level = 0.99, var.equal=FALSE)$conf

#Example 3.6
new_flights = na.omit(new_flights)
ggplot(data=new_flights, aes(x = carrier, y = distance)) + geom_violin(fill="blue") +
  geom_boxplot(width = 0.1, fill="orange") + xlab("Airline") +
  ylab("DIstance travelled") + 
  ggtitle("Boxplots of Distance Travelled") + coord_flip()

#to get rid of all NA values in our data set:
#new_flights = na.omit(new_flights)
UA = filter(new_flights, carrier == "UA")
Distance_UA = do(1000)*mean(resample(UA$distance, replace = TRUE, na.rm = TRUE))
B6 = filter(new_flights, carrier == "B6")
Distance_B6 = do(1000)*mean(resample(B6$distance, replace = TRUE, na.rm = TRUE))

#Other way to boostrap
B=c()
for (i in 1:1000){
  B[i] = abs(mean(resample(UA$distance, replace = TRUE))-
               mean(resample(B6$distance, replace = TRUE)))
}
#Display distribution of the mean difference
mean_diff = abs(Distance_B6 - Distance_UA) 
ggplot(mean_diff, aes(x=mean)) + 
  geom_histogram(col='black', fill='blue', binwidth=10, na.rm=TRUE) + 
  xlab("Sample Mean Difference, n = 50") + ylab("Frequency") + 
  ggtitle("Distribution of sample mean Difference for Distance travelled between UA and B6")

#Confidence intervals
UL = quantile(mean_diff$mean, 0.975)
LL = quantile(mean_diff$mean, 0.025)
cat("95% bootstrap confidence interval for difference in 
    population mean is (", LL, ",", UL, ")")


#practice 3.2
new_flights = na.omit(new_flights)
new_flights = filter(flights,carrier == c("UA","B6") )
t.test(~ distance | carrier, data=new_flights, 
       conf.level = 0.95, var.equal=FALSE)$conf

#part 2
new
B_dep_delay=c()
for (i in 1:1000){
  B_dep_delay[i] = abs(mean(resample(UA$dep_delay, replace = TRUE))-
               mean(resample(B6$dep_delay, replace = TRUE)))
}
#Display distribution of the mean difference
B_dep_delay = data.frame(B_dep_delay)
ggplot(B_dep_delay, aes(x=B_dep_delay)) + 
  geom_histogram(col='black', fill='blue', binwidth=0.5, na.rm=TRUE) + 
  xlab("Sample Mean Difference, n = 50") + ylab("Frequency") + 
  ggtitle("Distribution of sample mean Difference for Distance travelled between UA and B6")

#Confidence intervals
UL = quantile(mean_diff$mean, 0.975)
LL = quantile(mean_diff$mean, 0.025)
cat("95% bootstrap confidence interval for difference in 
    population mean is (", LL, ",", UL, ")")
#Example 3.7
#part I : parametric approach
phat_f = (9+1)/(56+2)
phat_m = (17+1)/(44+2)

phat_diff = phat_m - phat_f
z = qnorm(0.975)
moe = z *sqrt(phat_f*(1-phat_f)/(58) + phat_m*(1-phat_m)/(46))
 UL = phat_diff + moe
 LL = phat_diff - moe
 
cat("95% CI for difference between two population proportions is", "(", LL, ",", UL, ")")
#Alternative method
prop.test(x = c(17+1, 9+1), n = c(44+2, 56+2),correct=FALSE)$conf

#part II : Bootstrap approach
#create original data set using sample proportions
data_f = c(rep(1,9),rep(0,(56-9)))
data_m = c(rep(1,17),rep(0,(44-17)))
library(mosaic)
#re-sampling with replacement
prop_M = do(1000)*mean(resample(data_m, replace = TRUE))
prop_F = do(1000)*mean(resample(data_f, replace = TRUE))
#distribution of the difference between two proportions
prop_diff = prop_M - prop_F

#visualizing the distribution
ggplot(prop_diff,aes(x=mean)) + geom_histogram(col='black',fill = "red")+
  xlab("phat_male - phat_female") + ylab("frequency") +
  ggtitle("Bootstrap Distribution of difference between two proportions")

#bootstrap interval

quantile(prop_diff$mean,c(0.025,0.975))

#Practice 3.3
data_34 = c(rep(1,329),rep(0,(732-329)))
data_54 = c(rep(1,409),rep(0,(1241-409)))
library(mosaic)
#re-sampling with replacement
prop_34 = do(1000)*mean(resample(data_34, replace = TRUE))
prop_54 = do(1000)*mean(resample(data_54, replace = TRUE))
#distribution of the difference between two proportions
prop_diff_34_54 = prop_34 - prop_54

#visualizing the distribution
ggplot(prop_diff_34_54,aes(x=mean)) + geom_histogram(col='black',fill = "red")+
  xlab("phat_male - phat_female") + ylab("frequency") +
  ggtitle("Bootstrap Distribution of difference between two proportions")

quantile(prop_diff_34_54$mean,c(0.025,0.975))
       
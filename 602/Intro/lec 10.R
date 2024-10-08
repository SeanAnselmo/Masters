#Example 5.5
age <- c(4.4, 6.7,10.5,9.6, 12.4, 5.5, 11.1, 8.6, 14, 10.1, 7.2, 7.9)
Ave_sleep <- c(586, 565, 515, 532, 478, 560, 493, 533, 575, 490, 530, 515)
cor(age,Ave_sleep)

#Example 5.1 revisit
library(MASS)
data("Boston")
head(Boston)
na.omit(Boston)
reg <- lm(medv ~ lstat, data = Boston)
summary(reg)

#applying pt() to run one-tailed tests:
#alternative beta_1 < 0
pvalue = pt(-24.53, reg$df)

#alternatively:
pvalue = pt(summary(reg)[["coefficients"]][2, "t value"], reg$df)

#residual plots
plot(lm(medv ~ lstat, data = Boston))

#How to adjust the plot so we get randomness in residual plots
#Boston data example
#use log transform to lstat:
plot(lm(medv ~ log(lstat), data = Boston))

#another example
#https://statisticsbyjim.com/regression/heteroscedasticity-regression/
data <- read.csv("~/602/Intro/Heteroscedasticity.csv")
plot(lm(Accidents ~ Population, data = data))

#use rate of accidents instead of number of accidents
plot(lm(AccidentRate ~ Population, data = data))




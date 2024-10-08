library(ggplot2)

#Demonstration of RSS method
x =c(10,20,30,40,50,60,70,80,90)
y = c(13.5,12.3,23,18.5,33.6,32,36,38, 37.2)
#Simple Linear Regression Model
SLR <- lm(y ~ x)
# Scatter plot and linear regression line
plot(x, y)
# Segments with error terms
segments(x0 = x, x1 = x, y0 = y, y1 = predict(SLR),
         lwd = 1, col = "red") 
# Adding Regression line
abline(SLR, col = 4, lwd = 2)
# Paint the points again over the segments
points(x, y, pch = 16)


#Example 5.1
library(MASS)
library(mosaic)
data("Boston")
head(Boston)
na.omit(Boston)
#visualize the relationship between lstat and medv
ggplot(Boston, aes(x=lstat, y = medv)) + 
  geom_point(color = "red") +
  labs(title = "Scatter plot of Median Housing 
       Price and % of lower status ", 
       x = "% of lower status", y ="Median Housing Price")

y = favstats(Boston$medv)
x = favstats(Boston$lstat)
 beta_1 = sum((Boston$lstat-x$mean)*(Boston$medv - y$mean))/
   sum((Boston$lstat-x$mean)^2)
beta_2 = y$mean - beta_1*x$mean

#alternatively
reg <- lm(medv ~ lstat, data = Boston)
summary(reg)
ggplot(Boston, aes(x=lstat, y = medv)) + 
  geom_point(color = "red") + stat_smooth(method = "lm", 
                                          formula = y ~ x, geom = "smooth") +  
  labs(title = "Scatter plot of Median Housing 
       Price and % of lower status ", 
       x = "% of lower status", y ="Median Housing Price")

#prediction intervals visualization
ggplot(Boston, aes(x = lstat, y = medv)) + 
  geom_point(color = 4) + geom_smooth(method = lm)

#Example 5.4
heating <- read.csv("C:\\Users\\amali\\OneDrive - University of Calgary\\DATA 602\\Class Notes\\Data - Heating Cost.csv")
install.packages("fastDummies")
library(fastDummies)
heating <- dummy_cols(heating, remove_first_dummy = TRUE)
reg_dummy <- lm(Cost ~ Garage_Yes , data = heating)
summary(reg_dummy)


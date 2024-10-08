install.packages("purrr")
library(tidyverse,dplyr,mosaic, purrr)
bill = data.frame(billamount = c(198.58, 187.49, 174.77, 178.86, 177.03, 159.43, 178.94, 150.52, 157.48, 176.24, 162.88, 153.58, 170.91, 185.79, 182.03)) 

#part a
mean = mean(bill$billamount)
sd = sd((bill$billamount))

#part b
n = 15
t = qt(0.975, n-1)
UL = mean + t*sd/sqrt(n)
LL = mean - t*sd/sqrt(n)
cat("95% CI for mean monthly bill is", "(", LL, ",", UL, ")")

#alternative method

t.test(~billamount, data=bill)$conf 

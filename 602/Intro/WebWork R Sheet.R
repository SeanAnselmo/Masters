
#Set One Problem 7 Part 2
answer <- (choose(10,6)*choose(14,6))/choose(24,12) +
          (choose(10,7)*choose(14,5))/choose(24,12) +
  (choose(10,8)*choose(14,4))/choose(24,12) +
  (choose(10,9)*choose(14,3))/choose(24,12) +
  (choose(10,10)*choose(14,2))/choose(24,12) +
  (choose(10,11)*choose(14,1))/choose(24,12) +
  (choose(10,12)*choose(14,0))/choose(24,12) 
  
#Set One Problem 8 part 1
answer <-factorial(6)/(6^5)
answer <- 1- answer

#Set One Problem 8 part 2 roll 1,2,3,4,5 in any order
answer <-factorial(5)/(6^5)

#Set One Problem 8 part 3
answer <-factorial(1)*6/(6^5)

#Set Two Problem 1 part 1
answer <-(0.4)*(0.73)

#Set Two Problem 1 part 2
answer <-(0.4)*(0.27)

#Set Two Problem 1 part 4
answer <-(0.4)+(0.73)-2*(0.2920) #*2 the intersect to avoid doubel counting it

#Set Two Problem 4 part 1
PE <- 0.79
PBgivenE<- 0.07

PEandB = (PE*PBgivenE)*100

#Set Two Problem 6 part 1
PoneCup = 0.83
TwoConsume = PoneCup^2
OnlyOne = PoneCup*0.17*0.17

TwoNo = (1-PoneCup)^2

AtLeastTwo = 1-((1-PoneCup)^3+3*PoneCup*(1-PoneCup)^2)

n = log(0.01)/log(0.17)

#Set Two Problem 7
Res25 = 0.51
Res25to50 = 0.67
Res50 = 0.89

Age25 = 0.19
Age25to50 = 0.38
Age50 = 0.43

ResponseRate25 = Res25*Age25
ResponseRate25to50 = Res25to50*Age25to50
ResposneRate50 = Res50*Age50

OverallResponseRate = ResponseRate25+ResponseRate25to50+ResposneRate50

RespondedByOver50 = ResposneRate50/OverallResponseRate

#Set Two Problem 8
Forget = 0.19
Mail = 0.81
Deliver = 0.90
NoDeliver = 0.1

NeverDelivered = Forget+(NoDeliver)*(Mail)
RoomateFault = (NoDeliver*Mail)/NeverDelivered

#Set Two Problem 9
TwoDollarCoin = x
OneDollarCoin = 4*x

AlLPossibilities = 4+1+1
NotEnough = 4/AlLPossibilities

#Set Two Problem 10
A = 0.13
B = 0.04
C = 0.06

AFrequency = 0.25
BFrequency = 0.46
CFrequency = 1-(AFrequency+BFrequency)

ADies = A*AFrequency
BDies = B*BFrequency
CDies = C*CFrequency

NotMisplaced = 1-(ADies+BDies+CDies)
AFault = ADies/(ADies+BDies+CDies)


#Set Three Problem 1
prob <- c(0.0231, 0.1447,0.3396,0.3541,0.1385)
val <- c(0,1,2,3,4)
ev <- sum(val*prob)#Expected Value

moment2 <- sum(val^{2}*prob)
sd <- sqrt(moment2-ev^{2}) #Variance (sd)

#Set Three Problem 2
a <- 1-(0.28+0.08+0.03) #Value of the first 
prob <- c(0.61,0.28,0.08,0.03)
val <- c(-2,0,2,3)

ev <- sum(val*prob)#Expected Value

moment2 <- sum(val^{2}*prob)
sd <- sqrt(moment2-ev^{2}) #Variance (sd)

#Set Three Problem 3
PX0 = 0.7*0.8*0.9
PX1 = (0.7*0.8*0.1)+(0.7*0.2*0.7)+(0.3*0.6*0.7)
PX2 = (0.7*0.2*0.3)+(0.3*0.6*0.3)+(0.3*0.4*0.5)
PX3 = 0.3*0.4*0.5
prob <- c(PX0,PX1,PX2,PX3)
val <- c(0,1,2,3)
ev <- sum(val*prob)#Expected Value

moment2 <- sum(val^{2}*prob)
sd <- sqrt(moment2-ev^{2}) #Variance (sd)

#Set Three Problem 4
PWin = 0.34
PLose = 1-0.34
PX0 = PLose^4
PX1 = (PWin*PLose*PLose*PLose)*(choose(4,1))
PX2 = PWin*PWin*PLose*PLose*(choose(4,2))
PX3 = PWin*PWin*PWin*PLose*(choose(4,3))
PX4 = PWin^4

prob <- c(PX0,PX1,PX2,PX3,PX4)
val <- c(0,1,2,3,4)
ev <- sum(val*prob)#Expected Value

moment2 <- sum(val^{2}*prob)
sd <- sqrt(moment2-ev^{2}) #Variance (sd)

#Set Three Problem 5
PShop = 0.68
x = 16
p = 25
  
PX = choose(25,x)*(PShop)^x*(1-PShop)^(25-x)
PX9to25 = sum(dbinom(9:15, n, p))
PX13 = dbinom(9, n, p) * dbinom(3, n-1, p)
  
shoppers= 1:25
halloweenshopperprob = function(x){
  choose(25,x)*((0.68)^x)*(1-0.68)^(25-x)
}

PX13is9th = choose(12,8)*0.68^{8}*0.32^{4}*0.68

ev = sum(shoppers*halloweenshopperprob(shoppers))


ev <- sum(val*prob)#Expected Value

moment2 <- sum(shoppers^{2}*halloweenshopperprob(shoppers))
sd <- sqrt(moment2-ev^{2}) #Variance (sd)

#Set Three Problem 6

EmptySeats5 = dbinom(160,180,0.86)
Overbooking = 1-pbinom(165,180,0.86)


ev = 180*0.86 #Number of passengers * the chance they show up
sd = sqrt(ev*(1-0.86))

#Set Three Problem 7
X = 0:7
diceroll = function(x){
  choose(7,x)*(1/6)^x*(1-(1/6))^(7-x)
}

PartC = diceroll(5)+diceroll(6)+diceroll(7)
ev = sum(X*diceroll(X))

moment2 <- sum(X^{2}*diceroll(X))
sd <- sqrt(moment2-ev^{2}) #Variance (sd)

#Set Three Problem 8 Poisson distribution

a = dpois(3,5.5) #lamba is 5.5

b = sum(dpois(3:6,5.5))

c = 1-ppois(6,5.5)

d = 5.5*6
d2 = sqrt(d)

#Set Three Problem 9 Poisson distribution
lambda = 6.8
a = dpois(10,lambda) #X=10

b = 1-ppois(4,lambda) #X>=5

c = sum(dpois(7:13, lambda)) # 7<=X<=13

perHour = lambda/11
d = (sqrt(lambda))^2

#Set Three Problem 10 Poisson distribution
lambda = 19.5 #weighted average
b = dpois(15,19.5) # X= 15

c = 1-ppois(3,lambda/3) # X<=4, with lambda being a 3rd of size

range = sum(dpois(14:19, lambda))
Seventeen = dpois(17, lambda)
d = Seventeen/range


#Set Four Problem 1 Continous Dist

PartA = function(x){
  (6*x)*(1-x)
}
curve(PartA, 0,1)

a = integrate(PartA, lower = 0.25, upper = 0.49)$value # P(0.25<=X<=0.49)
b = integrate(PartA, lower = 0.82, upper = 1.0)$value # P(0.82<=X<=1.0)

ev1 = function(x){
  x*  (6*x)*(1-x)                                  #Multiple it by x to get EV function
}

c = integrate(ev1, lower = 0, upper = 1)$value #Expected Value, goes from lower limit to upper limit

moment2 = function(x){
  x^2*  (6*x)*(1-x)                                  #Multiple it by x^2 to get moment2 function
}

d = integrate(moment2, lower = 0, upper = 1)$value    #Integrate it the same
d = sqrt(d-(c^2))                                     #Square root of the variance is SD

#Set Four Problem 2 Uniform Dist

a = punif(0.5, -.05, 0.5) - punif(0.29, -0.5, 0.5)    #Computes 0.29<=X
b = punif(0.5, -.05, 0.5) - punif(0.25, -0.5, 0.5)    #Computes 0.25<=X

ev = 0.5*(-0.5+0.5)                                   #EV = lower+upper/2
sd = sqrt((0.5-(-0.5))^2/12)

lower_bound = ev - sd
upper_bound = ev + sd

d = punif(upper_bound, -0.5, 0.5)-punif(lower_bound,-0.5, 0.5)   #Computes one SD


#Set Four Problem 3 Exponential Dist

beta = 4.3
sigma = 1/beta

b = pexp(8, sigma) - pexp(2,sigma)              #P(2<= X <=8)
c = 1-pexp(7,sigma)                             #P(X>7)
d = qexp(0.5, sigma)                            #50th percentile, happens 50% of the time

e = (1-pexp(6,sigma) - pexp(7,sigma))/(1-pexp(7,sigma))        #(X>=7|X>=6)

Atleast7 = 1-pexp(7, sigma)
Atleast6 = 1-pexp(6, sigma)
e = Atleast7/Atleast6


#Set Four Problem 4 Exponential Dist Pareto Distribution
alpha = 3
bman = 4
beta = bman/alpha
sigma = alpha/bman

ev = a*b/(a-1)
sd = (b/(a-1))*sqrt(a/(a-2))

#Set Four Problem 5 Exponential Dist 
mean_time = 39
lambda = 1/mean_time

a = pexp(36, rate = lambda) - pexp(28, rate = lambda)
b = pexp(25, rate = lambda)
d = qexp(0.55, rate = lambda)


Been30 = pexp(25, lambda)             #25 more minutes to go

#Set Four Problem 6 Poisson Exp Dist 

lambda = 5.5/10
ey = 1/lambda

b = pexp(2, rate = lambda)   # X<=2
c = pexp(2.5, rate = lambda) - pexp(1.25, rate = lambda) # 1.25<= X <= 2.5

d = pexp(1, rate = lambda, lower.tail = FALSE)

#Set Four Problem 7 Normal Distribution
mu = 0.7
sigma = 0.05

a = pnorm(0.65,mu,sigma)
b = pnorm(0.82,mu,sigma) - pnorm(0.71, mu, sigma)

c = qnorm(0.31,mu,sigma)       #percentile to pass

#Set Four Problem 7 Normal Distribution
mu = 4.1
sigma = 0.68

a = pnorm(4.8, mu, sigma) - pnorm(3.8, mu, sigma)   #3.8<=X<=4.8
b = 1-pnorm(5.0,mu,sigma)        #Longer than 5 minutes

c = qnorm(0.2, mu, sigma)            #Percent that is longer than 20
d = 232*b                        #longer than 5 min * number of songs

longerThan4.1 = 1-pnorm(4.1,mu,sigma)
e = dbinom(7,16,0.5)*0.5

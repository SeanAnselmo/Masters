example2pdf = function(x)
{  #starts the body of the function
  (x/105) #equates the example2pdf function to equal x/105 for a value of x = 6, 7, ..., 15
} 

xvalues = 6:15 #assigns a vector called xvalues 6, 7, upt to and including 15
xvalues #returns the values of xvalues

px = example2pdf(xvalues) #will return the P(X = 6), P(X = 7), etc, P(X =  15) assigned to the vectore px
px #returns P(X = 6), P(X = 7), etc, P(X = 15)

px[1:4] #returns P(X = 6), P(X = 7), P(X = 8), P(X = 9)
sum(px[1:4]) #sums the probabilies in postions 1 through 4 (inclusive), P(X = 6), P(X = 7), P(X = 8), P(X = 9)

sum(px[1:7]) #comptues P(X <= 12)

#To compute P(7<=X<=9)
sum(px[2:4]) #adds P(X =7), P*X = 8), and P(X = 9)
sum(px[1:4]) - px[1] #computes P(X <= 9) - P(X <= 6)

plot(xvalues, px, xlab = "Values of the Random Variable X", ylab="P(X = x)", main="Probability Distribution of X", type="h", col='blue')

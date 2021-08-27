###############################################################################
##                   R LECTURE 2: Regression and Testing		    		         ##
##            	 Quantitative Analysis - 2019 Spring  3/11   		     	    	 ##
###############################################################################
#
#
###############################################################################
##                                Author		    		                         ##
##            	            王元翰 Spencer Wang   		     	    	           ##
###############################################################################


####################################################################
# Section 1:  Testing with heteroskedasticity                      #
####################################################################
#------------
# Coef Tests 
#------------
rm(list=ls(all=T))

# constructing data
t1 = Sys.time() #record running time

n=500
x = rnorm(n, mean=0, sd=1)
epsilon = abs(x)*rnorm(n, mean=0, sd=30) 
y = 2 + 5*x + epsilon  # Note that y (and epsilon) are no longer iid, in fact, they are heteroskedastic.

t2 = Sys.time()
usingTime = t2 - t1
print(usingTime) #show the time of running the code above

# Now in order to test the coefficients, we first has to install the package "lmtest"
library(lmtest) # call the package
lm.fit = lm(y~x)
coeftest(lm.fit) # IID assumptions

# Since we know the IID assumption no longer holds, we need the pachage "sandwich" to construct new estimators
library(sandwich)
coeftest(lm.fit, vcov = vcovHC, type = "HC0") # Ecker-White estimator 
vcovHC(lm.fit)                                # the Ecker-White covariance matrix

#---------------------------
# Testing linear hypothesis
#---------------------------

# we first construct the data
rm(list=ls(all=T))
# constructing data
n=500
x_1 = rnorm(n, mean=0, sd=10)
x_2 = rnorm(n, mean=0, sd=10)
x_3 = rnorm(n, mean=0, sd=10)
x_4 = rnorm(n, mean=0, sd=10)
epsilon = abs(x_1)*rnorm(n, mean=0, sd=30) 
y = 5 + 1*x_1 + 2*x_2 + 3*x_3 + 4*x_4 + epsilon
lm.fit = lm(y~x_1+x_2+x_3+x_4)

# First we need to install the package "car"
# Now we wish to test "b1=b2=b3=b4=1", the following code will do the trick
library(car)
library(sandwich)
linearHypothesis(lm.fit,c("x_1 = 1", "x_2 = 1", "x_3 = 1", "x_4 = 1")) # linear hypothesis with IID assumption
linearHypothesis(lm.fit,c("x_1 = 1", "x_2 = 1", "x_3 = 1", "x_4 = 1"),vcov = vcovHC, type = "HC0") #linear hypothesis with Ecker-White covariance matrix

# For the test "b1=1, b2=2, b3=3, b4=4"
linearHypothesis(lm.fit,c("x_1 = 1", "x_2 = 2", "x_3 = 3", "x_4 = 4")) #linear hypothesis with IID assumptions.
linearHypothesis(lm.fit,c("x_1 = 1", "x_2 = 2", "x_3 = 3", "x_4 = 4"),vcov = vcovHC, type = "HC0") #linear hypothesis with Ecker-White covariance matrix.

# For the test "b1+b2+b3 = 6"
linearHypothesis(lm.fit,c("x_1 + x_2 + x_3 = 6")) #linear hypothesis with IID assumptions.
linearHypothesis(lm.fit,c("x_1 + x_2 + x_3 = 6"), vcov = vcovHC, type = "HC0") #linear hypothesis with Ecker-White covariance matrix.





####################################################################
# Section 2:  Graphs                                               #
####################################################################

#-----------
# Histogram
#-----------
rm(list=ls(all=T))
y <- rnorm(5000, mean=0, sd=1)
range(y)
hist(y)

windows(width=10,height=10) #open a new windows and plot on it
hist(y)

hist(y, breaks=10) # adjusting the density of the histogram
hist(y, breaks=100) 
hist(y, breaks=1000) 

hist(y, breaks=100, xlim = c(-5,5))   # Setting the X axes' limits
hist(y, breaks=100, xlim = c(-5,5), ylim=c(-100,250))  # Setting the y axes' limits

# Naming the plot and the x,y axes.
hist(y, breaks=100, xlim = c(-4,4), main = paste("Histogram of Standard Normal, T=5000"),
     xlab = "Value", ylab = "Empirical frequency") 
# The function hist() is very flexible, for more details (like how to adjust the font type and size), type help(hist).

#-----
# plot
#-----

## Single variable plot
x = rnorm(5000, mean=0, sd=1)
plot(x, ylim = c(-5,5))
x = rnorm(5000, mean=0, sd=2)
plot(x, ylim = c(-5,5))


x = rnorm(50, mean=0, sd=1)
plot(x, type = "p")  # "p" for points (default)          
plot(x, type = "l")  # "l" for lines  
plot(x, type = "b")  # "b" for borh lines and points

plot(x, type = "l", col="red")
plot(x, type = "l", col="blue") 

## two-variable (x,y coordinate) plot
x = rnorm(5000, mean=0, sd=1)
y = rnorm(5000, mean=0, sd=1)
plot(x,y) 

y1 = 3*x + rnorm(5000, mean=0, sd=1)
plot(x,y1) # the linear trend is clear

y2 = 3*x + rnorm(5000, mean=0, sd=10)
plot(x,y2) # the linear trend is less clear


#------
# lines
#------

## line() function is for "adding lines" on original plots. Note that it can't exist on its own.

# example 1: plotting two different lines
x = rnorm(50, mean=0, sd=2)
y = rnorm(50, mean=0, sd=1)
plot(x, type = "l", col="red")
lines(y, col="blue") 

plot(x, type = "l", col="red",lwd=2) # adjusting line width (lwd)
lines(y, col="blue",lwd=2, lty=2)    # adjusting line type (lty)

# example 2: adding Gaussian kernel density estimate
x = rnorm(5000, mean=0, sd=1)
plot(density(x)) # plot the "Gaussian kernel density estimate" of x

hist(x, breaks=100, xlim = c(-5,5),freq=F)  # check help(hist) for the meaning of "freq=F"
lines(density(x),col="red",lwd=2)

# example 3: Comparing the empirical density (Gaussian kernel density estimate) with the true density.
L = seq(-10, 10, length=1000)
x1 = rnorm(500, mean=0, sd=1)
x2 = rnorm(500, mean=0, sd=1.5)
x3 = rnorm(500, mean=0, sd=2)

plot(density(x1), type = "l" ,lwd=2) # These are the empirical density of x1, x2, x3 (solid lines)
lines(density(x2), col="red",lwd=2) 
lines(density(x3), col="blue",lwd=2)  
lines(L, dnorm(L,0,1), lty=2,lwd=2)       # These are the true density of N(0,1), N(0,1.5), N(0,2) (dotted lines)
lines(L, dnorm(L,0,1.5), col="red", lty=2,lwd=2)
lines(L, dnorm(L,0,2), col="blue", lty=2,lwd=2)

# Let's increase the sample size from 500 to 50,000
x1 = rnorm(50000, mean=0, sd=1)
x2 = rnorm(50000, mean=0, sd=1.5)
x3 = rnorm(50000, mean=0, sd=2)

plot(density(x1), type = "l" ,lwd=2) # These are the empirical density of x1, x2, x3 (solid lines)
lines(density(x2), col="red",lwd=2) 
lines(density(x3), col="blue",lwd=2)  
lines(L, dnorm(L,0,1), lty=2,lwd=2)       # These are the true density of N(0,1), N(0,1.5), N(0,2) (dotted lines)
lines(L, dnorm(L,0,1.5), col="red", lty=2,lwd=2)
lines(L, dnorm(L,0,2), col="blue", lty=2,lwd=2)
#As the sample sieze grows from 500 to 50,000, the empirical distribution gets closer to its ture density function.


#----------------------------
# multiple plots in one graph
#----------------------------
# "par(mfrow=c(,))" function allows you to combine multiple plots in one graph	

par(mfrow=c(2,3)) # 6 plots combined as a graph with row=2, col=3

x = rnorm(1000, mean=0, sd=1)
y1 = 3*x + rnorm(1000, mean=0, sd=1)
plot(x,y1, ylim = c(20,-20)) 
y2 = 3*x + rnorm(1000, mean=0, sd=2)
plot(x,y2, ylim = c(20,-20)) 
y3 = 3*x + rnorm(1000, mean=0, sd=3)
plot(x,y3, ylim = c(20,-20)) 
y4 = 3*x + rnorm(1000, mean=0, sd=4)
plot(x,y4, ylim = c(20,-20)) 
y5 = 3*x + rnorm(1000, mean=0, sd=5)
plot(x,y5, ylim = c(20,-20)) 
y6 = 3*x + rnorm(1000, mean=0, sd=6)
plot(x,y6, ylim = c(20,-20))


#--------
# Legend
#--------
rm(list=ls(all=T))
x1 = rnorm(100, mean=0, sd=1)
x2 = rnorm(100, mean=0, sd=2)
x3 = rnorm(100, mean=0, sd=3)
x4 = rnorm(100, mean=0, sd=4)
x5 = rnorm(100, mean=0, sd=5)
plot(x1, type = "l", col=1,lwd=2, lty=1, ylim=c(13,-13))
lines(x2, col=2,lwd=2, lty=2) 
lines(x3, col=3,lwd=2, lty=3) 
lines(x4, col=4,lwd=2, lty=4) 
lines(x5, col=5,lwd=2, lty=5) 
# Now we have finished the plot, but the problem is we don't know which line represents which data.
# Hence we use the legend function below.
legend("topright",legend = c("x1","x2","x3","x4","x5"),col = c(1,2,3,4,5),lty=c(1,2,3,4,5), lwd = 2, cex=0.5)


#---------------------
# saving and exporting
#---------------------
## Saving the graph as an independent file such as pdf, png,...

# Code logic:
# pdf(.)/png(.)/jepg(.)/hist(.)/plot(.)/contour(.) 
# {~~your plot~~}
# dev.off() 

# example 1: png file 
png("C:\\Users\\Spencer\\Desktop\\1.png")
y = rnorm(5000, mean=0, sd=1)
hist(y)
dev.off()

# example 2: pdf file 
pdf("C:\\Users\\Spencer\\Desktop\\1.pdf")
y = rnorm(5000, mean=0, sd=1)
hist(y)
dev.off()


####################################################################
# Section 3: Functions and Loops                                   #
####################################################################

#---------
# if-else
#---------

## if() function:        if("condition A"){"what happens when A is satisfied?"}
#                        else if("condition B"){"what happens when B is satisfied?"}
#                        else if("condition C"){"what happens when C is satisfied?"}
#                        .....
#                        else{"what happens when conditions A,B,C,.... are all NOT satisfied?"}
# Note that condisions A,B,C,.. should be mutually exclusive.

## Example
x = rnorm(1)
{if (x == 0){print('zero')} 
  else if (x > 0){print('positive')} 
  else {print('negative')}
}
x

## ifelse(A,B,C) function:    There are three arguements in ifelse()
#                             the first arguement A is the "condition", second argument B is
#                             the "value" when condition A is satisfied. The third argument C is
#                             the "value" when condition A is NOT satisfied.

## Example
x <- 1:10 
ifelse((x<5|x>8) , x^2 , 888)


#-------------------------
# Creating Basic Functions
#-------------------------

## single variable function
F1 = function(a){a^2+3} 
F1(3)

## function plot
plot(F1,xlim = c(-10,10))

## multi-variable function
F2 = function(a,b,c){
  a^2+b-3*c
}
F2(1,2,3)

## function with default
F3 = function(a,b=100){
  2*a+b
}
F3(1)    # since the default for b=100
F3(1,2)  # note that although b is 100 by default, if a new number is specified, then the new number is used.

## function with if-else
F4 = function(a, b, i){
  if(i==TRUE){a+b}
  else{0}
}
F4(3,5,TRUE)
F4(3,5,FALSE)

## Creating functions that generate data
generate = function(T,sigma) 
{
  epsilon = rnorm(T,0,sigma)
  x1 = runif(T,0,100)
  x2 = runif(T,0,100)
  x3 = runif(T,0,100)
  y = 1 + x1 + 2*x2 + 4*x3 + epsilon
  result = data.frame(y,x1,x2,x3)
  return(result)
}

generate(10,3) # which contains data sets y, x1, x2, x3
# and note that "T" is the data length and "sigma" is the variance of epsilon

#----------
# for Loops
#----------

## example 1: single loop - filling out a vector from 1~10
Vec = NULL             # We set it as "NULL" to let the computer know that the variable Vec "exist" but doesn't equal anything. 
for (i in 1:10) {
  Vec = c(Vec, i)        # Computer will read from right to left for this equation. Hence for i=1,
}                        # we have c(NULL,1) = Vec, and for i=2, we have c(1,2) = Vec, and so on.  
Vec

# Or using another way, we have:
Vec = rep(0, 10)
for (i in 1:10) {
  Vec[i] = i        
}                      
Vec


## example 2: double loops - filling out a matrix from 1~100
Mat = matrix(0,nrow=10,ncol=10)
for (i in 1:10){             # observe very closely how this code is written,
  mat = NULL                 # especially the part where "mat = NULL" is written under
  for (j in 1:10){           # "i Loop" but above "j Loop". Instead of writing it at the 
    mat = c(mat,10*(i-1)+ j) # very begining.
  }
  Mat[i,] = mat
}
Mat                          # This concept of writting loop is criticle in the future!

# Or using another way, we have:
Mat = matrix(0,nrow=10,ncol=10)
for (i in 1:10){
  for (j in 1:10){
    Mat[i,j] = (10*i-10)+j
  }
}
Mat


#-------------
# while  Loops
#-------------
# while() function is of the form while(condition) {expression}, where the "expression" part
# will keep runing until the "condition" part is dissatisfied.

## example 1: We draw random samples from N(0,100), and we are curious about how large should the 
#           size of the sample be for the sample mean to be <0.001
rm(list=ls(all=T))
N=1
M=1

while (abs(M)>0.001) {
  N=N+1
  x=rnorm(N,0,100)
  M = mean(x)
}
N   # Here we can see that the sample size "T" that makes mean(x)<0.001

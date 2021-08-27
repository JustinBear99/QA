####################################################################
# Boundary and Interiror Knots for Natural Cubic Spline in R       #
####################################################################
library(splines)
library(ISLR)
attach(Wage)

agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2]) # this is a grid from 18 to 80.

#################
# natural splines

fit=lm(wage~ns(age,knots = c(30, 50, 70)),data=Wage) # Here we fit a natural spline with three "interior" knots at age = 30, 50, 70.
attr(ns(age,knots = c(30, 50, 70)),"knots") # The location for each "interior" knot.


fit2=lm(wage~ns(age,df=4),data=Wage) # Here we fit a natural spline with 4 degrees of freedom (which is a natural cubic spline with three "interior" knots at uniform quantiles)
attr(ns(age,df=4),"knots") # The location for each knot.

fit  # But note that there are 5 basis functions! Indicating that there are 5 knots.
fit2 # R's default is to add two additional "boundary knots" at the boundary of x.


fit.new=lm(wage~ns(age,knots = 50, Boundary.knots = c(30,70)),data=Wage) 
# Here we fit a natural spline with only one interior knot, and we force the two additional boundary knots NOT at the boundary of x, but rather at x=30, 70.

attr(ns(age,knots = 50, Boundary.knots = c(30,70)),"knots") # The location for each "interior" knot.

fit.new # note that there are now 3 basis functions.









######################################################################
## Plot (just for demonstration purposes, you may ignore if you wish)

library(splines)
library(ggplot2)
rm(list=ls(all=T))
set.seed(34)

N=1200
a=1.95
b=-1.95
sigma_x = 0.9
sigma =  10

X=rnorm(N,0,sigma_x )
X=X[X<a&X>b]
epsi = rnorm(length(X), 0, sigma)
Y =  function(x){ ifelse(x>0,sin(6.5*x^2)*(x+5)*(x-1.6)^2*(x+1)^3    , -10*x-10 ) }
Y = Y(X)+epsi
data = cbind(Y,X)

p = ggplot(data = data.frame(data), mapping = aes(x = X, y=Y))
fun.1 <-  function(x){  -10*x-10  }
fun.2 <-  function(x){ sin(6.5*x^2)*(x+5)*(x-1.6)^2*(x+1)^3 }

### natural cubic spline with 3 interior knots.
p + stat_function(fun = fun.1, colour = 'blue', size = 1, n=10, xlim = c(b,0),alpha = I(1.5/2)) +
  stat_function(fun = fun.2, colour = 'blue', size = 1, n=1000, xlim = c(0,a),alpha = I(1.5/2))+
  geom_point(data = data.frame(data), alpha = I(1/4)) + 
  stat_smooth(method = "lm" ,formula = y~ns(x, knots =c(-1,0,1)  )  , col = "red", se=F )+
  geom_vline(xintercept = -1, linetype="dotted", size=0.9, colour = 'red')+
  geom_vline(xintercept = 0, linetype="dotted", size=0.9, colour = 'red')+
  geom_vline(xintercept = 1, linetype="dotted", size=0.9, colour = 'red')+
  labs(title = "Natural Cubic Spline with 3 knots")+ theme(plot.title = element_text(size=24))

### natural cubic spline with 1 interior knots (3 total knots).
p + stat_function(fun = fun.1, colour = 'blue', size = 1, n=10, xlim = c(b,0),alpha = I(1.5/2)) +
  stat_function(fun = fun.2, colour = 'blue', size = 1, n=1000, xlim = c(0,a),alpha = I(1.5/2))+
  geom_point(data = data.frame(data), alpha = I(1/4)) + 
  stat_smooth(method = "lm" ,formula = y~ns(x, knots =0, Boundary.knots = c(-1,1)  )  , col = "red", se=F )+
  geom_vline(xintercept = -1, linetype="dotted", size=0.9, colour = 'red')+
  geom_vline(xintercept = 0, linetype="dotted", size=0.9, colour = 'red')+
  geom_vline(xintercept = 1, linetype="dotted", size=0.9, colour = 'red')+
  labs(title = "Natural Cubic Spline with 3 total knots")+ theme(plot.title = element_text(size=24))

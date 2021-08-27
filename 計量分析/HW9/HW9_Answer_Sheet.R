###############################################################################
##                           HW 9 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))


##############
# Question 1 #
##############
library(ISLR)
library(boot)
data(Wage)

#####
#(a)# 
#####
library(splines)
delta = 10000
opt_i = 0
for (i in 3:8){
  model = glm(wage~bs(age, df=i, degree=3) , data=Wage)
  KFCV = cv.glm(Wage, model, K=10)
  if (KFCV$delta[2] < delta){
    delta = KFCV$delta[2]
    opt_i = i
    opt_model = model
  }
}
# number of optimal interior knots = 

#####
#(b)# 
#####
x.grid = seq(from=min(Wage$age), to=max(Wage$age), by=0.01)
windows()
plot(Wage$age, Wage$wage)
lines(x.grid, predict(model, newdata=list(age=x.grid)), col="blue")

##############
# Question 2 #
##############

#####
#(a)# 
#####
delta = 10000
opt_i = 0
knots.1 = quantile(Wage$age, probs = c(0.05,0.95))
knots.2 = quantile(Wage$age, probs = c(0.1,0.9))
knots.3 = quantile(Wage$age, probs = c(0.15,0.85))
knots.4 = quantile(Wage$age, probs = c(0.2,0.8))
model.1 = glm(wage~ns(age, df=4, Boundary.knots = knots.1) , data=Wage)
KFCV.1 = cv.glm(Wage, model.1, K=10)

model.2 = glm(wage~ns(age, df=4, Boundary.knots = knots.2) , data=Wage)
KFCV.2 = cv.glm(Wage, model.2, K=10)

model.3 = glm(wage~ns(age, df=4, Boundary.knots = knots.3) , data=Wage)
KFCV.3 = cv.glm(Wage, model.3, K=10)

model.4 = glm(wage~ns(age, df=4, Boundary.knots = knots.4) , data=Wage)
KFCV.4 = cv.glm(Wage, model.4, K=10)

c(KFCV.1$delta[2], KFCV.2$delta[2], KFCV.3$delta[2], KFCV.4$delta[2])

windows()
plot(Wage$age, Wage$wage)
lines(x.grid, predict(model.1, newdata=list(age=x.grid)), col="red")
lines(x.grid, predict(model, newdata=list(age=x.grid)), col="blue")

# Optimal boundary quantiles = 5th and 95th
# Location of the boundary knots = quantile(Wage$age, probs = c(0.05,0.95))


#####
#(b)# 
#####




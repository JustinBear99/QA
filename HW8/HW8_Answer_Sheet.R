###############################################################################
##                           HW 8 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))

##############
# Question 1 #
##############
library(ISLR)
library(boot)
require(splines)
data(Wage)

#####
#(a)# 
#####
set.seed(1)
delta = 10000
opt_i = 0
for (i in 5){
  model = glm(wage~bs(age, knots=), data=Wage)
  KFCV = cv.glm(Wage, model, K=10)
if (KFCV$delta[2] < delta){
  delta = KFCV$delta[2]
  opt_i = i + 1
  opt_model = model
}
}

# Optimal degree = 10

#####
#(b)# 
#####
library(splines)
library(ggplot2)
X = Wage$age
Y = Wage$wage
data = cbind(Y,X)
coefs = opt_model$coefficients
p = ggplot(data = data.frame(data), mapping = aes(x = X, y=Y))
fun.1 = function(x){coefs[[1]] + coefs[[2]]*x + coefs[[3]]*x^2 + coefs[[4]]*x^3 + coefs[[5]]*x^4 + coefs[[6]]*x^5 
  + coefs[[7]]*x^6 + coefs[[8]]*x^7 + coefs[[9]]*x^8 + coefs[[10]]*x^9}
fun.2 = function(x){
  111.70361 + 447.06785*x - 478.31581*x^2 +125.52169*x^3 -77.91118*x^4 - 35.81289*x^5 +62.70772*x^6 + 50.54979*x^7 -11.25473*x^8 -83.69180*x^9
}
p + stat_smooth(formula = wage ~ poly(age, 9, raw = TRUE), data = Wage, col = "red", se=F) +
    geom_point(data = data.frame(data), alpha = I(1/4)) +
    labs(title = "Fitted with 10 DoF")+ theme(plot.title = element_text(size=24))

##############
# Question 2 #
##############
rm(list=ls(all=T))
library(ISLR)
library(boot)
data(Wage)

#####
#(a)# 
#####
set.seed(1)
delta = 10000
opt_i = 0
for (i in 2:16){
  Wage$tmp = cut(Wage$age,i)
  model = glm(wage~tmp, data=Wage)
  KFCV = cv.glm(Wage, model, K=10)
  if (KFCV$delta[2] < delta){
    delta = KFCV$delta[2]
    opt_i = i
    opt_model = model
  }
}
# optimal cutpoint = 16


#####
#(b)# 
#####

library(splines)
library(ggplot2)
coefs = opt_model$coefficients
X = Wage$age
Y = Wage$wage
data = cbind(Y,X)
XX = c(21.9, 25.8, 29.6, 33.5, 37.4, 41.2, 45.1, 49, 52.9, 56.8, 60.6, 64.5, 68.4, 72.2, 76.1, 80.1)
YY = c(coefs[[1]], coefs[[2]], coefs[[3]], coefs[[4]], coefs[[5]], coefs[[6]], coefs[[7]], coefs[[8]], coefs[[9]], coefs[[10]], coefs[[11]], coefs[[12]], coefs[[13]], coefs[[14]], coefs[[15]], coefs[[16]])
data2 = cbind(YY,XX)
p = ggplot(data = data.frame(data), mapping = aes(x = X, y=Y))
p + geom_step(data = data.frame(data2), mapping=aes(x=XX, y=YY), lwd=1, col="red") +
  geom_point(data = data.frame(data), alpha = I(1/4)) +
  labs(title = "Cutted with 16 points")+ theme(plot.title = element_text(size=24))

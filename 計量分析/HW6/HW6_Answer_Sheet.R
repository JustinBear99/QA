###############################################################################
##                           HW 6 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################

##############
# Question 1 #
##############
rm(list=ls(all=T))
library(ISLR)
data(Auto)

#####
#(a)# 
#####
set.seed(1)
n = nrow(Auto)
index = sample(1:n, size = n*0.50, replace = FALSE)
train.set = Auto[index,]
test.set = Auto[-index,]
model1 = lm(mpg ~ horsepower, data=train.set)
model2 = lm(mpg ~ horsepower+weight, data=train.set)
model3 = lm(mpg ~ horsepower+weight+acceleration, data=train.set)

model1.pred = predict(model1, test.set)
model2.pred = predict(model2, test.set)
model3.pred = predict(model3, test.set)

MSE1 = mean((test.set$mpg - model1.pred)^2)
MSE2 = mean((test.set$mpg - model2.pred)^2)
MSE3 = mean((test.set$mpg - model3.pred)^2)

#Best model = model2
#Test MSE = MSE2

#####
#(b)# 
##### 
library(boot)
data(Auto)
set.seed(1)
model1 = glm(mpg ~ horsepower, data=Auto)
model2 = glm(mpg ~ horsepower+weight, data=Auto)
model3 = glm(mpg ~ horsepower+weight+acceleration, data=Auto)

LOOCV1 = cv.glm(Auto, model1)
LOOCV2 = cv.glm(Auto, model2)
LOOCV3 = cv.glm(Auto, model3)

c(LOOCV1$delta[1],LOOCV2$delta[1],LOOCV3$delta[1])

#Best model = model2
#Test MSE = 18.11295

#####
#(c)# 
#####
data(Auto)
set.seed(1)
model1 = glm(mpg ~ horsepower, data=Auto)
model2 = glm(mpg ~ horsepower+weight, data=Auto)
model3 = glm(mpg ~ horsepower+weight+acceleration, data=Auto)

KFCV1 = cv.glm(Auto, model1, K=10)
KFCV2 = cv.glm(Auto, model2, K=10)
KFCV3 = cv.glm(Auto, model3, K=10)

c(KFCV1$delta[2],KFCV2$delta[2],KFCV3$delta[2])

#Best model = model2
#Test MSE = 18.16548

##############
# Question 2 #
##############

#####
#(a)# 
#####
data(Auto)

beta=NULL
for (i in 1:1000) {
  set.seed(i)
  index= sample(nrow(Auto),nrow(Auto),replace = T)
  a=lm(mpg ~ horsepower, data=Auto,subset = index)
  beta=c(beta,coeftest(a)[2,1])
}
sd(beta)

#Paired Bootstrap estimation = 0.007306607


#####
#(b)# 
#####
library(car)
data(Auto)

Bootstrap = function(data,index){
  return(coef(lm(mpg ~ horsepower,data = Auto,subset = index)))
}
set.seed(1)
residboot = Boot(lm(mpg ~ horsepower,data = Auto),f=coef, R=1000, method="residual")
sd(residboot$t[,2])

#Residual Bootstrap estimation = 0.00626424





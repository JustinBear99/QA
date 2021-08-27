###############################################################################
##                           HW 7 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters) 
sum(is.na(Hitters$Salary)) #There are 59 samples with missing information.
Hitters=na.omit(Hitters)
dim(Hitters) #The remaining 263 samples with full information.

library(leaps)
##############
# Question 1 #
##############
best.fit = regsubsets(Salary~., Hitters, nvmax=19)
summary.fit = summary(best.fit)
plot(summary.fit$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", cex.lab=1.3)
a=which.min(summary.fit$bic)
points(a,summary.fit$bic[a], col="red", cex=3, pch=20)
a # number of variable selected
coef.a = coef(best.fit,a)

##############
# Question 2 #
##############
backward.fit = regsubsets(Salary~., Hitters, nvmax=19, method = "backward")
summary.fit = summary(backward.fit)
plot(summary.fit$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", cex.lab=1.3)
b=which.min(summary(backward.fit)$bic)
points(b,summary.fit$bic[b], col="red", cex=3, pch=20)
b
coef.b = coef(backward.fit,b)

##############
# Question 3 #
##############
forward.fit = regsubsets(Salary~., Hitters, nvmax=19,method = "forward") # The codes are the same, except we now pass-in the code "method = "forward" "
summary.fit = summary(forward.fit)
plot(summary.fit$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", cex.lab=1.3)
c=which.min(summary(forward.fit)$bic)
points(c,summary.fit$bic[c], col="red", cex=3, pch=20)
c
coef.c = coef(forward.fit,c)


##############
# Question 4 #
##############
library(boot)
# For best subset selection and forward stepwise, selected variables are AtBat, Hits, Walks, CRBI, DivisionW and PutOuts.
lm.fit1 = glm(Salary~AtBat+Hits+Walks+CRBI+Division+PutOuts, data = Hitters)
KFCV1 = cv.glm(Hitters, lm.fit1, K=10)

# For backward stepwise, selected variables are AtBat, Hits, Walks, CRuns, CRBI, CWalks, DivisionW and PutOuts.
lm.fit2 = glm(Salary~AtBat+Hits+Walks+CRuns+CRBI+CWalks+Division+PutOuts, data = Hitters)
KFCV2 = cv.glm(Hitters, lm.fit2, K=10)

c(KFCV1$delta[1], KFCV2$delta[2])
# which model is better: backward stepwise


##############
# Question 5 #
##############
X = model.matrix(Salary~.,Hitters)[,-1]
Y= Hitters$Salary
library(glmnet)

grid = seq(0,100,length = 1000)
ridge.fit = glmnet(X,Y,alpha = 0, lambda = grid)

#####
#(a)# 
#####
plot(ridge.fit$lambda ,coef(ridge.fit)[1,],type="l",ylim =c(-5,9), xlim = c(0,100))
for (i in 2:19){
  lines(ridge.fit$lambda,coef(ridge.fit)[i,])}

set.seed(45)
ridge.kfold = cv.glmnet(X,Y,alpha=0, nfolds=263, lambda = grid)
best.s = ridge.kfold$lambda.min 
best.s 

# Best lambda = 5.405405

#####
#(b)# 
#####
predict(ridge.fit, s=best.s,type="coefficients")

# Coefficient for "Hits" = 5.26311705


##############
# Question 6 #
##############
grid = seq(0,100,length = 1000)
lasso.fit = glmnet(X,Y,alpha = 1, lambda = grid)

#####
#(a)# 
#####
set.seed(45)
lasso.kfold = cv.glmnet(X,Y,alpha=1, nfolds=263, lambda = grid)
best.s = lasso.kfold$lambda.min 
best.s

#Best lambda = 2.602603


#####
#(b)# 
#####
predict(lasso.fit, s=best.s,type="coefficients")

# How many variables are forced to zero = 5

###############################################################################
##                           HW 11 Answer Sheet         			    		       ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))

##############
# Question 1 #
##############
library(MASS)
library(tree)
library(randomForest)
attach(Boston)
High=ifelse(medv<22,0,1)
boston = Boston[1:13]
#####
# a #
#####
set.seed(1)
bag.boston=randomForest(High~., data=boston, mtry=(ncol(Boston)-1), ntree=500,importance=TRUE)
importance(bag.boston)

# the variable that decrease the Gini index the most (hence most important) according to this model is: lstat

#####
# b #
#####
set.seed(1)
rf.boston=randomForest(High~.,data=boston,mtry=3, ntree=500, importance=TRUE)
importance(rf.boston)
#bag.boston$mse
#rf.boston$mse

plot(bag.boston$mse, type = "l", xlab = "Number of Trees", ylab = "OOB error", cex.lab = 1.2, col="blue") # OOB error for bagging
a =length(rf.boston$mse)
lines(seq(from = 1,to = a, length =a), rf.boston$mse, col = "red") # OOB error for random forest
legend("topright",legend = c("Bagging", "Random Forest"),col = c("blue", "red"), lty = c(1, 1))

#####
# c #
#####
library(gbm)
library(MASS)

set.seed(1)
boost.boston1 = gbm(medv~., data=boston, distribution="gaussian", n.trees=1000, interaction.depth=1, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston1$cv.error)
# The optimal tree for d=1 is : 896

boost.boston2 = gbm(medv~., data=boston, distribution="gaussian", n.trees=1000, interaction.depth=2, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston2$cv.error)
# The optimal tree for d=2 is : 814

boost.boston3 = gbm(medv~., data=boston, distribution="gaussian", n.trees=1000, interaction.depth=3, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston3$cv.error)
# The optimal tree for d=3 is : 375

boost.boston4 = gbm(medv~., data=boston, distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston4$cv.error)
# The optimal tree for d=4 is : 454


#####
# d #
#####
boost.boston1$cv.error[896]
boost.boston2$cv.error[814]
boost.boston3$cv.error[375]
boost.boston4$cv.error[454]

# The smallest 10-fold cv error is the model with d = 2



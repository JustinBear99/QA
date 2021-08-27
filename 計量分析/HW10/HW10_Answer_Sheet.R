###############################################################################
##                           HW 10 Answer Sheet         			    		       ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))


##############
# Question 1 #
##############
pm = seq(from=0, to=1, by=0.001)
cls = 0
for(i in pm){
  cls_err_rate = 1-max(i, 1-i)
  cls = c(cls, cls_err_rate)
}
cls = cls[c(2:end(cls))]
gini = 2*pm*(1-pm)
cross_entropy = -pm*log(pm)-(1-pm)*log(1-pm)
windows()
plot(pm, cls, col='green', xlim=0:1, ylim=0:1)
lines(pm, gini, col='red')
lines(pm, cross_entropy, col='blue')

##############
# Question 2 #
##############
rm(list=ls(all=T))

library(MASS)
library(tree)
attach(Boston)
High=ifelse(medv<22,"No","Yes")
#####
# a #
#####
set.seed(1)
tree.boston = tree(medv~.-medv, data=Boston)
summary(tree.boston)

set.seed(1)
cv.boston=cv.tree(tree.boston, K=10) 
cv.boston
plot(cv.boston$size,cv.boston$dev,type='b') 

#The optimal number of nodes is 9

#####
# b #
#####
set.seed(1)
Boston = data.frame(Boston, High)
tree.boston2 = tree(High~.-medv, data=Boston, split = 'gini', mincut=5)
summary(tree.boston2)

plot(tree.boston2,  type = "proportional") # If type = "uniform", the branches are of uniform length. Otherwise they are proportional to the decrease in impurity.
text(tree.boston2,pretty=0)
tree.boston2

set.seed(1)
cv.boston2=cv.tree(tree.boston2, FUN=prune.misclass ,K = 10)

names(cv.boston2)
plot(cv.boston2)

cv.boston2

#The optimal number of nodes is 16



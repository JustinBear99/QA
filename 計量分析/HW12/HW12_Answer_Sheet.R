###############################################################################
##                           HW 12 Answer Sheet         			    		       ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))
library(ISLR)
data = Carseats[c(1:6,8,9)]

##############
# Question 1 #
##############

#####
# a #
#####
set.seed(1234)
index = sample(1:nrow(data), round(0.8*nrow(data)))
train = data[index,]
test = data[-index,]

lm.fit = lm(Sales ~ CompPrice+Income+Advertising+Population+Price+Age+Education, data = train)
pr.lm = predict(lm.fit, test)
MSE.lm = sum(((pr.lm - test$Sales)^2) / nrow(test))

# MSE = 4.4198



#####
# b #
#####
library(neuralnet)
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

set.seed(1234)
n <- names(train_)
f <- as.formula(paste("Sales ~", paste(n[!n %in% "Sales"], collapse = " + ")))
nn <- neuralnet(f, data = train_, hidden = c(5,3,2), linear.output = T)
pr.nn = compute(nn, as.data.frame(test_))
pr.nn_ <- pr.nn$net.result * (max(data$Sales) - min(data$Sales)) + min(data$Sales)
MSE.nn <- sum((pr.nn_ - test$Sales)^2) / nrow(test)

#MSE = 5.63462


#####
# c #
#####



# Linear regression performs better



#####
# d #
#####

windows()
par(mfrow=c(1,2))
plot(test$Sales, pr.lm)
plot(test$Sales, pr.nn_)


###############################################################################
##                           HW 1 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################

##############
# Question 1 #
##############
#(a)# 
x <- c(1:150)

#(b)# 
x[x > 135 | x <= 5]

#(c)#
x[x > 70 & x<90]

#(d)# 
x[x %% 4 == 0 & x %% 5 == 0]


##############
# Question 2 #
##############
#(a)# 
x = rep(1,100)

#(b)# 
x = rep(c(1,8),c(100,50))

#(c)#
x = rep(c(1,4,7), 100)


##############
# Question 3 #
##############
#(a)# 
X = rnorm(150000)

#(b)# 
X_mean = mean(X)
X_median = median(X)
X_max = max(X)
X_min = min(X)

#(c)#
Y = sample(X, 5000)
Y_mean = mean(Y)
Y_variance = var(Y)

#(d)# 
Z = sample(X, 5000, replace = T)
Z_mean = mean(Z)
Z_variance = var(Z)

#(e)# 
percentile_45 = quantile(X, 0.45)
z = qnorm(p = 0.45)

#(f)# 
pr_x = length(X[X > -0.55 & X <= 1.25])/length(X)
pr_a = pnorm(1.25, mean=0, sd=1) - pnorm(-0.55, mean=0, sd=1)

##############
# Question 4 #
##############
#(a)# 
X = matrix(c(7,4,9,0,5,2,6,2,9,3,3,7,0,0,5),5,3)
Y = matrix(c(6,2,4,2,1),5,1)
beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y

#(b)# 
x_star = matrix(c(0,4,3),1,3)
y_hat = beta_hat %*% x_star

###############################################################################
##                           Solution to HW 1          			    		         ##
##            	 Quantitative Analysis - 2020 Spring  3/9   		     	    	 ##
###############################################################################

##############
# Question 1 #
##############
#(a)# 
x=c(1:150)

#(b)# 
x[(x>135) | (x<=5)]

#(c)#
x[(x>70) & (x<90)]

#(d)# 
x[x%%4==0&x%%5==0]


##############
# Question 2 #
##############
#(a)# 
rep(1,100)

#(b)# 
rep( c(1,8), c(100,50) )

#(c)#
rep( c(1,4,7), 100 )


##############
# Question 3 #
##############
#(a)# 
X=rnorm(150000)

#(b)# 
mean(X)
median(X)
max(X)
min(X)
var(X)

#(c)#
Y = sample(X, 5000)
mean(Y)
var(Y)

#(d)# 
Z = sample(X, 5000, replace = T) 
mean(Z)
var(Z)

#(e)# 
quantile(X,0.45) # 45 percentile in X
qnorm(p=0.45)    # z

#(f)# 
sum((X>-0.55) & (X<= 1.25))/length(X)
pnorm(q=1.25)-pnorm(q=-0.55)


##############
# Question 4 #
##############
#(a)# 
X = cbind(c(7,4,9,0,5), c(2,6,2,9,3), c(3,7,0,0,5))
Y = c(6,2,4,2,1)
b_hat = solve(t(X)%*%X)%*%(t(X)%*%Y)

#(b)# 
x=c(0,4,3)
y_hat = t(x)%*%b_hat






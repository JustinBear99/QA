###############################################################################
##                           Solution to HW 2          			    		         ##
##            	 Quantitative Analysis - 2020 Spring  3/16   		     	    	 ##
###############################################################################

##############
# Question 1 #
##############
rm(list=ls(all=T))
data(mtcars)  # load the data set "mtcars" from R
mtcars        # use help(mtcars) to get help from the definition of this dataset in R
attach(mtcars)

#(a)# 
mtcars["Duster 360",] 

#(b)#
mtcars$qsec

#(c)#
mtcars[mtcars$cyl==6,]
subset(mtcars,cyl==6) 

#(d)# 
subset(mtcars,mpg>15 & vs==1 & hp<150 & hp>50)

#(e)#
R_ur = summary(lm(drat~wt+hp+qsec+vs))$r.squared #R-square for unconstrained model
R_r  = summary(lm(drat~hp+vs))$r.squared #R-square for constrained model
F_R.square = ( (R_ur - R_r)/2 ) / ( (1-R_ur) / (nrow(mtcars)-4-1) ) # note that n=nrow(mtcars), k=4
F_R.square


#(f)#
e_ur = summary(lm(drat~wt+hp+qsec+vs))$residuals
e_r = summary(lm(drat~hp+vs))$residuals

SSR_ur = sum(e_ur^2)
SSR_r = sum(e_r^2)

F_SSR = ( (SSR_r-SSR_ur)/2 )/ ( SSR_ur / (nrow(mtcars)-4-1) )
F_SSR #which is identical to (e)


#(g)# 
library(car)
library(sandwich)

result = linearHypothesis(lm(drat~wt+hp+qsec+vs),c("wt=0", "qsec=0")) 
names(result)
result$F #which is identical to (e)

###############################################################################
##                           HW 2 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################

##############
# Question 1 #
##############
#(a)# 
data(mtcars)
mtcars[7,1:11]

#(b)# 
mtcars[1:32,7]

#(c)#
for (i in 1:nrow(mtcars)){
  if (mtcars[i,2]==6) print(mtcars[i,1:8])
}

#(d)# 
for (i in 1:nrow(mtcars)){
  if (mtcars[i,'mpg']>15 && mtcars[i,'vs']==1 && mtcars[i,'hp']<150 && mtcars[i,'hp']>50) print(row.names(mtcars)[i])
}

#(e)#
model_constrained = lm(drat~hp+vs-1, data=mtcars)
model_unconstrained = lm(drat~wt+hp+qsec+vs, data=mtcars)
R2_ur = summary(model_unconstrained)$r.squared
R2_r = summary(model_constrained)$r.squared
if (summary(model_constrained)$fstatistic[1] > summary(model_unconstrained)$fstatistic[1]){
  print('The null hypothesis is rejected')
} else {
  print('The null hypothesis is not rejected')
}

#(f)#
SST=var(mtcars$drat) *(nrow(mtcars)-1)
SSE_unconstrained = sum(model_unconstrained$residuals^2)
SSE_constrained = sum(model_constrained$residuals^2)
SSR_unconstrained = SST - SSE_unconstrained
SSR_constrained = SST - SSE_constrained

#(g)# 
linearHypothesis(model_unconstrained,c("wt = 0", "qsec = 0"))

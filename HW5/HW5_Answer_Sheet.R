###############################################################################
##                           HW 5 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################

##############
# Question 1 #
##############
#(a)# 
library(AER)
data(HMDA)
HMDA$deny = as.numeric(HMDA$deny) - 1
denylogit = glm(deny~hirat, data=HMDA, family=binomial(link = "logit"))
coeftest(denylogit, vcov = vcovHC(denylogit, type="HC0"))

#(b)# 
predict(denylogit, newdata=data.frame('hirat'=c(0.2)), type='response')

#(c)#
predict(denylogit, newdata=data.frame('hirat'=c(0.8)), type='response')

#(d)# 
HMDA$afam = ifelse(HMDA$afam=='yes',1 ,0)
denylogit2 = glm(deny~hirat+afam, data=HMDA, family=binomial(link = "logit"))

#(e)#
predict(denylogit2, newdata=data.frame('hirat'=c(0.2), 'afam'=1), type='response')
predict(denylogit2, newdata=data.frame('hirat'=c(0.2), 'afam'=0), type='response')

#(f)#
predict(denylogit2, newdata=data.frame('hirat'=c(0.8), 'afam'=1), type='response')
predict(denylogit2, newdata=data.frame('hirat'=c(0.8), 'afam'=0), type='response')


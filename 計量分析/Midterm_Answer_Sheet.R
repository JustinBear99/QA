###############################################################################
##                        Midterm Answer Sheet         			    		         ##
##            	             NTU ID:________  	    	              	    	 ##
###############################################################################

##############
# Question 1 #
##############
data(mtcars)

#####
#(a)# 
#####
lm.a = lm(mpg~wt+hp+drat+gear, data=mtcars)

#hat_b1= summary(lm.a)$coefficients[2,1]


#####
#(b)# 
#####
library("lmtest")

result_paired=NULL
for (i in 1:1000) {
  set.seed(i)
  index= sample(32,32,replace = T)
  a=lm(mpg~wt+hp+drat+gear, data=mtcars, subset=index)
  result_paired=c(result,coeftest(a)[2,2])
}
sd(result_paired)

result_resid=NULL
a=lm(mpg~wt+hp+drat+gear, data=mtcars)

for (i in 1:1000) {
  set.seed(i)
  index= sample(32,32,replace = T)
  Y=a$fitted.values + a$residuals[index]
  result_resid=c(result_resid,coeftest(lm(Y~wt+hp+drat+gear, data=mtcars))[2,2])
}
sd(result_resid)

#Paired Bootstrap estimation = 0.1818064
#Residual Bootstrap estimation = 0.1180525


#####
#(c)# 
#####
library(sandwich)
model.c = lm(mpg~wt+hp+drat+gear, data=mtcars)
result_c = linearHypothesis(model.c,c("wt+drat=-3")) 
names(result_c)
result_c$F

#p_value= result$F 

#####
#(d)# 
#####
result_d = linearHypothesis(model.c, c("wt=0","drat=0","gear=0"))
names(result_d)
result_d$F

#p_value= result_d$F




##############
# Question 2 #
##############
library(ISLR)
data(Wage)

#####
#(a)# 
#####
x = Wage$age
count =x[x>=50 & x<100]
length(count)

#How many white man = 851

#####
#(b)# 
#####
Divorced = NULL
i=1
y = Wage$maritl
for (data in y){
  if (data == '4. Divorced'){
    Divorced[i] = 1
  }
  else{
    Divorced[i] = 0
  }
  i = i+1
}
Divorced_rate = sum(Divorced)/length(Divorced)

#Divorced rate(%) = 6.8%


#####
#(c)# 
#####
divprobit = glm(Divorced~logwage+age, data=Wage, family=binomial(link = "probit"))
coeftest(divprobit, vcov = vcovHC(divprobit, type="HC0"))

#coefficeint for wage = -0.4364901 


#####
#(d)# 
#####
library(boot)
set.seed(1)
divlogit = glm(Divorced~logwage+age, data=Wage, family=binomial(link = "logit"))
KFCV = cv.glm(Wage, divlogit ,K=10)

#___________ model is better, with testing MSE = 


##############
# Question 3 #
##############

sampleSize = c(5,500)
DGPs = c('U(-1,1)')
Moments = c('y^3')
rep = 2000

windows(width = 10, height = 10)
par(mfrow = c(1,2))

for(size in sampleSize){
  for(DGP in DGPs){
    for(moment in Moments){
      recordMn = c()
      for(r in 1:rep){
        if(DGP == 'U(-1,1)'){
          randomSample = rnorm(size,0,1)
        }

        if(moment == 'y^3'){
          momentSample = randomSample^3
        }

        varN = var(momentSample)
        Mn = sum(momentSample)/sqrt(size*varN)
        recordMn[r] = Mn
      }
      hist(recordMn, main=paste('The histogram, ', 'samplesize : ',toString(size), ', DGP = ', DGP, ', Moment function is ', moment), xlim=c(-2.5,2.5))
#      abline(v=1.6, col='red')
#      abline(v=2.3, col='blue')
      lines(density(recordMn), col='blue', lty=2) #Gaussian KDE
      L = seq(min(recordMn), max(recordMn), length=size) #PDF of standard normal distribution
      lines(lines(L, dnorm(L,0,1), col='blue', lty=2))
      
    }
  }
}














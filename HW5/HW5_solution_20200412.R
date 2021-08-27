###############################################################################
##                           HW 5 Solution            			    		         ##
###############################################################################

##############
# Question 1 #
##############
#(a)# 
library(AER)
data(HMDA) 
HMDA$deny = as.numeric(HMDA$deny) - 1

denylogit <- glm(deny ~ hirat,family = binomial(link = "logit"), data = HMDA)
coeftest(denylogit, vcov. = vcovHC, type = "HC0")

#(b)# 
predict(denylogit,newdata = data.frame("hirat" =  0.2),type = "response")

#(c)#
predict(denylogit,newdata = data.frame("hirat" =  0.8),type = "response")

#(d)# 
denylogit2 <- glm(deny ~ hirat + afam,family = binomial(link = "logit"), data = HMDA)

#(e)# 
predictions <- predict(denylogit2,newdata = data.frame("afam" = c("no", "yes"),"hirat" = c(0.2, 0.2)),type = "response")
diff(predictions)

#(f)# 
predictions2 <- predict(denylogit2,newdata = data.frame("afam" = c("no", "yes"),"hirat" = c(0.8, 0.8)),type = "response")
diff(predictions2)

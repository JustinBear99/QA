###############################################################################
##                   R LECTURE 2: Regression and Testing		    		         ##
##            	 Quantitative Analysis - 2019 Spring  3/4   		     	    	 ##
###############################################################################
#
#
###############################################################################
##                                Author		    		                         ##
##            	            王元翰 Spencer Wang   		     	    	           ##
###############################################################################





####################################################################
# Section 1: List and Data Frames                                  #
####################################################################

#------
# List
#------

#Lists are a special type of vector that can contain elements of different classes. 
#Lists are a very important data type in R and you should get to know them well.
n <-c(2,3,5)
s <-c("aa","bb","cc","dd","ee")
b <-c(TRUE,FALSE,TRUE,FALSE,FALSE)
x <-list(n,s,b,3) # list() is the code to create list
x

x = list(1, "a", TRUE, 1 + 4i)
x
class(x) 

y = c(1, "a", TRUE, 1 + 4i)
y
class(y)  # Observe that elements in y are now "characters".

x[[1]]+1
y[1]+1 # since y[1] = "1", and "1" are character to R, NOT numbers.


#----------------
# Indexing a list
#----------------
v=list(bob=c(2,3,5),john=c("aa","bb"))
v
names(v) # call out the names in the list
v$bob # "$" will extract the value under "bob"

##Example
x = rnorm(100)
y = rnorm(100)
LM = summary(lm(y~x)) # This is the summary of the simple linear regression x on y.
# In this summary we can see the coefficients, p-value, R-squared,...
LM

names(LM) # Suppose we want to extract the value of R-squared in this summary, then the code is
LM$residuals # we can extract the residuals form the model. 


#-----------
# Data frams
#-----------

#Data frame is similiar as the matrix but more useful. Bascially, it's just like a sheet.
N  = 10
u  = rnorm(N) 
x1 = rnorm(N) 
x2 = rnorm(N) 
y  = 1 + x1 + x2 + u 

mydataframe = data.frame(y,x1,x2) # data.frame() is the code to create data frame 
mydataframe 

names(mydataframe) # Note that the data in data frame are "named"
mydataframe["x1"]  # which is equivalent as "mydataframe$x1"
mydataframe$x1

mymatrix <- cbind(y, x1, x2)
mymatrix
names(mymatrix) 
mymatrix["x1"] # Note that here the data are not named, hence we can not extract the data 
mymatrix$x1

## Example
id  <- c(1, 2, 3, 4)
age <- c(15, 25, 26, 38)
sex <- c("M", "F", "M", "M")
pay <- c(20, 3, 67, 98)
X.dataframe <- data.frame(id, age, sex, pay)
X.dataframe 
X.dataframe [3, 2]
X.dataframe$age     # refer to the content in age
X.dataframe[2]      # refer to the second column
edit(X.dataframe)   # click the cell twice, then edit like excel

## Example: R in-built data set 
data(mtcars)  # load the data set "mtcars" from R
mtcars        # use help(mtcars) to get help from the definition of this dataset in R

mtcars["Mazda RX4", "cyl"] # select a specific value, identical to "mtcars[1,2]"
mtcars["Mazda RX4",]       # select a row
mtcars$cyl                 # select a column

nrow(mtcars) # number of rows
ncol(mtcars) 



#------------------
# Naming Data frams
#------------------

x = data.frame(company_A = 1:4, familyfirm = c(T, T, F, F))
rownames(x) = c("1998", "1999", "2000", "2001")
x

row.names(x)     # row names
names(x)         # column names


#---------------------------------
# Logical indexing for Data frams
#---------------------------------

## Question: how do we select the column "drat" in mtcar with the condition that "am=0"?
mtcars$am           # the column "am" in mtcar
mtcars$am==0        # indicate the location where am=0 as "True"
L1 = mtcars$am==0
mtcars[L1,]         # here is the full data set with "am=0"      
mtcars[L1,]$drat    # job done!

# Here's a short cut, using the function "subset()"
SC = subset(mtcars,am==0)
SC$drat

# Here's the data set with both "am=0" and "gear=4"
subset(mtcars,am==0 & gear==4)


#------------------------------
# Importing and Exporting Data
#------------------------------

# Import cvs files. "C:\\Users\\User\\Desktop" is the directory while "crime.csv" is the files name.
read.csv("C:\\Users\\User\\Desktop\\crime.csv") 

# Export data set "Crime" as cvs files. "C:\\Users\\User\\Desktop" is the directory while "crime.csv" is the files name.
write.table(Crime, file = "C:\\Users\\User\\Desktop\\crime.csv", sep = ",")


####################################################################
# Section 2: Regrssion and Testing                                 #
####################################################################

#----------------
# Regression: OLS
#----------------
rm(list=ls(all=T))

## Simple linear regression with intercept
n=5000
x = rnorm(n, mean=0, sd=10)
epsilon = rnorm(n, mean=0, sd=1)
y = 2 + 5*x + epsilon 

# regress by hand
X=cbind(1,x)
beta_hat = solve(t(X)%*%X)%*%t(X)%*%y
beta_hat

## Run linear regression: lm()
lm(y~x)   # regress y on x (with intercept)
lm(y~x-1) # regress y on x without intercept

## Call the data stored automatically by R
lm(y~x) 
summary(lm(y~x))
coef(lm(y~x))
coef(summary(lm(y~x)))

names(summary(lm(y~x))) # see the data that is stored in "summary(lm(y~x))                                                                  "


#------------
# Coef Tests 
#------------
rm(list=ls(all=T))

# constructing data
n=500
x = rnorm(n, mean=0, sd=1)
epsilon = rnorm(n, mean=0, sd=5)
y = 2 + 5*x + epsilon  


# Now in order to test the coefficients, we first has to install the package "lmtest"
library(lmtest) # call the package
lm.fit = lm(y~x)
coeftest(lm.fit) # IID assumptions



#---------------------------
# Testing linear hypothesis
#---------------------------

# we first construct the data
rm(list=ls(all=T))
# constructing data
n=500
x_1 = rnorm(n, mean=0, sd=1)
x_2 = rnorm(n, mean=0, sd=1)
x_3 = rnorm(n, mean=0, sd=1)
x_4 = rnorm(n, mean=0, sd=1)
epsilon = rnorm(n, mean=0, sd=5)
y = 5 + 1*x_1 + 2*x_2 + 3*x_3 + 4*x_4 + epsilon
lm.fit = lm(y~x_1+x_2+x_3+x_4)

# First we need to install the package "car"
# Now we wish to test "b1=b2=b3=b4=1", the following code will do the trick
library(car)
library(sandwich)
linearHypothesis(lm.fit,c("x_1 = 1", "x_2 = 1", "x_3 = 1", "x_4 = 1")) # linear hypothesis with IID assumption

# For the test "b1=1, b2=2, b3=3, b4=4"
linearHypothesis(lm.fit,c("x_1 = 1", "x_2 = 2", "x_3 = 3", "x_4 = 4")) 

# For the test "b1+b2+b3 = 6"
linearHypothesis(lm.fit,c("x_1 + x_2 + x_3 = 6")) 


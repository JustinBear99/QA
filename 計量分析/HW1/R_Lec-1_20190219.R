###############################################################################
##                   R LECTURE 1: INTORDUCTION TO R   			    		         ##
##            	 Quantitative Analysis - 2019 Spring  2/25  		     	    	 ##
###############################################################################
#
#
###############################################################################
##                                Author		    		                         ##
##            	            ??‹å?ƒç¿° Spencer Wang   		     	    	           ##
###############################################################################



####################################################################
# Section 1: Background, Data Type, and Basic Operations           #
####################################################################

#--------------
# Preliminary
#--------------
# Run codes
# Select codes, press "control + Enter"
1-3
#1-3
# the compiler would skip the lines after seeing "#"

# clear R console
# control + L

# clear workspace
x=5
rm(list=ls(all=T))	# where T is the abbreviation for "TRUE"


#-----------------
# help and search
#-----------------
# For instance, if you don't recognize the function "lm"
# You can either Google it, or use the following codes
?lm # Get help for an object. You can also type: help(lm)
??lm # Search the help pages for anything that has the word "lm". You can also type: help.search("lm")

#-----------------
# Install Packages
#-----------------
# Install
install.packages("package_name") # or in Rstudio, press "Tools", then "Install Packages"
# Update
update.packages("package_name")
# Use
library("package_name")

#----------------------------------
# numbers, operations, and infinity
#----------------------------------
x = 12345		  #assign 12345 to x
x				      #display x
x = 2     		#assign 2 to x again
x					    #x is replaced, so be aware!

y = 7
x # print the value of x
y # print the value of y
x + y
z = x + y
z
w = x*y
w
v = x/y
v

x^y # x to the power of y

x=7 
x%%2  # the remainder of 7/2
x%%3  # the remainder of 7/3
x%%4
x%%7


z = 2/0
z     #"Inf" means the value is positive infinity
xx = 0/0 
xx    #NaN stands for "Not a Number" and is applied to "Undefined values"
A = Inf/Inf
A
B = Inf/5
B
C = -5/Inf
C

x=7
x
x==5 #Note that the meaning of "==" is very different from "="
x==7 


#-------------
# c() function
#-------------
x1 = c(0.1, 0.3, 0.5) #c() function combines the elements
x2 = c(TRUE, FALSE, TRUE)
x3 = c(T, F, F)
x4 = c("I", "am", "so", "good")
x5 = c(2+3i, 3-4i)
x1
x2
x3
x4
x5

x=1:5
y=c(1,2,3,4,5)
x
y   #note that 1:5 represent the number from 1 to 5, hence is identical to c(1,2,3,4,5)


#------------------
# Logical operators
#------------------
# <  		less than
# <=		no greater than
# >			greater than
# >=		no less than
# ==		exactly equal to
# !			not
# !=		not qual to
# |			or	
# &			and	
x=c(1,2,3,4,5,6,7,8,9,1)
x > 8
x < 5
x >= 8 | x < 5
x >= 8 & x < 5
x != 1


#---------------
# Data selection 
#---------------
x=c(10,20,30,40,50,60,70,80,90,10)
x
x[1] #x's first element
x[2] #x's second element
x[1:4] 
x[-3] #x subtracted of third element 
x[-(1:3)]

x[c(T,T,T,T,T,T,T,T,F,F)]

x[(x>80) | (x<=50)] #select the element in x that satisfied the discription in "[]"
x[(x!=10)]

## which() function
# This function indicates which of the element in a sepcific array that satisfy the condition.
which(x<50|x>80)



#---------------------
# character and string 
#---------------------
msg = "Hello Sofia"
msg
msg2 = "Hello World"
msg2
test = c(msg,msg2) 
test

#-------------------
# checking data type 
#-------------------
a = 3                 
b = c(1,2,3)
c = "Hello World"
d = c("pp4", "kai", "c", "wwwWW")
e = 5+3i 
f = TRUE
class(a);class(b);class(c);class(d); class(e);class(f)  # class() funciton displays the data types
length(a);length(b);length(c);length(d);length(e);length(f) # length() function displays the length of the variavles
nchar(c);nchar(d) # string length

# list objects
ls()
# list objects in detail
ls.str()


#-------------------
# changing data type 
#-------------------
x <- 1:9
class(x)               # ask what class of x is?
y = as.numeric(x)      # explcitly ask x become numeric
class(y)
z = as.logical(x)      # explicitly ask x becomes logical variables 
class(z)
as.complex(x)          # explicitly ask x become complex numbers
as.character(x)






####################################################################
# Section 2: Basic Mathematical and Statistical Functions          #
####################################################################

#-----------------------
# Mathematical functions
#-----------------------
x = c(1:10,0,-1)
sqrt(x)
exp(x)
log(x) #log=ln
abs(x)
log10(x)
log2(x)
cos(x)  

z <-  seq(from=-2, to=2, by=0.7) 
z
floor(z)  # report the largest integers not greater than the corresponding elements of z
round(z)  # rounds the values in z
ceiling(z) # report the smallest integers not less than the corresponding elements of z
trunc(z)   # truncating the decimal part 


#---------
# ordering
#---------
x <-c(1, 2, 4, 8, 4, 6, 1, 4, 7, 9,45, 34, 67)
sort(x)       # order x in a increasing manner
rev(sort(x))  # reverse the sorting order
sort(x, decreasing=TRUE)

unique(x)        # delete the duplicates 
duplicated(x)    # identify the duplicates from the 2nd replicates on
diff(x, lag=1)  # takt the first difference, x(t)-x(t-1), t= 2, 3, 4, ..., T
diff(x, lag=4)  # take the 4th difference, x(t) - x(t-4), t= 5, 6, 7, ..., T


#-----------------------
# descriptive statistics
#-----------------------
x <- c(0.5,2.5,3,4,5,6,7,8,9,10,20,30,40,50)
max(x)
which.max(x)
which.min(x)
min(x)
length(x)
sd(x)
var(x)
mean(x)
range(x)
quantile(x, c(0.1, 0.2, 0.5, 0.7, 0.9))
median(x)

y=x-1
cor(x,y) # correlation of x and y
cov(x,y) # covariance

#---------------------
# common distribution
#---------------------
runif(10, min=0, max=1) # draw 10 samples from uniform distribution with min=0 and max=1
rnorm(10) # draw 10 samples from standard Normal distribution
rnorm(10, mean=10, sd=15) # draw 10 samples from Normal distribution with mean=10, SD=15
rt(5, df=2) # draw 5 samples from t distribution with df=2

qnorm(p=0.95)
qnorm(p=0.975) # quantile of standard normal distribution with p=0.975
qnorm(p=0.025) 
qnorm(p=0.975, mean=10, sd=15)

pnorm(q=1.959964) # probability of drawing sample <=1.959964 from the standard normal distribution
pnorm(q=-1.959964)

dnorm(0) # the probability density of standard normal when evaluated at x=0
dnorm(1)
dunif(0.5,min=0, max=1)
dunif(1.5,min=0, max=1)

##other comman distributions can be found using "help()", these includes:
# rexp, rgamma, rpois, rweibull, rcauchy, rbeta, rchisq, 
# rbinom, rmultinom, rgeom, rhyper, rlogis, rlnorm, rnbinom


#-------------------------------
# Random sample generator
#-------------------------------

x = c(1,2,3,4,5,6,7,8,9)       # draw 10 uniform distributed  form 4 to 7
x
sample(x, 3) # randomly select 3 element from x without replacement
sample(x, 9)
sample(x, 9, replace = T) # randomly select 9 element from x WITH replacement

class = c("Kai","HuiChing", "Sofia", "Ashly", "Martin", "John", "Joan", "Susi", "Nini")
class
sample(class, 3)





####################################################################
# Section 3: Vector Operations                                     #
####################################################################

#-------------
# vector names
#-------------
x = 1:3
names(x) = c("one", "two", "three")
x


#------------------
# sequence creation
#------------------
y = 1:17
y
A = seq(from=-2, to=2, by=0.5)    # sequences form -2 to 2 and increase by 0.5
A
a = seq(from= 5, to=-3, by= -0.5)
a
R = rep(1,15) # repeat 1 for 15 times
R

#------------------------
# Time sequence creation
#------------------------
seq(as.POSIXlt("2018-04-03"), by="month", length= 6)     # generate a time sequence of month
seq(as.POSIXlt("2018-04-03"), by="month", length= 12)
seq(as.POSIXlt("2018-04-03"), by="week", length= 24)

ts(1:100,start= c(1998,1),frequency = 12) #generate a monthly time sequence starting from 1998 Jan
ts(1:100,start= c(1998,2),frequency = 12) #generate a monthly time sequence starting from 1998 Feb
ts(1:365,start= c(1998,1),frequency = 365) #generate a daily time sequence starting from 1998 1/1
ts(1:365,start= c(1998,2),frequency = 365) #generate a daily time sequence starting from 1998 1/2
ts(1:100,start= 1998,frequency = 1) #generate a yearly time sequence starting from 1998


#-------------------------
# Zero and Repeated vector 
#-------------------------
vector("numeric", length = 10)  ## un-specified the contents in the vector
h = 10
vector("complex", length = h)

rep(1:4, c(2, 3, 4, 5)) # repet show "1" two time; "2" three time; "3" four times...
rep(1:2, c(10, 5))
rep(1:3, 8)


#--------------------------------------------
# Missing Values in a vector and its deletion
#--------------------------------------------
x = c(1, 2, NA, 10, 1) #NA stands for "Not Available" and is applied to "Missing Values"
is.na(x)
is.nan(x)
x = c(0/0, Inf/Inf, -Inf ,NaN, NA, 3i, Inf) #NaN stands for "Not a Number" and is applied to "Undefined values"
is.na(x)
is.nan(x) # Note that undefined values are viewed as missing, but not vice versa.


# Delete missing values in a vector
x = c(1, 2, NA, 4, NA, 5,6,7,8)
bad = is.na(x)
x_new = x[!bad]
x_new

sum(x)
sum(x_new) # observe that data with "NA" in it can NOT be computed by R

# Or one can utilize the function "na.omit"
na.omit(x)


#------------------
# vector operations
#------------------
x = as.vector(c(5,6,7,8)) # Note that this(the default) is a column vector
y = as.vector(c(2,3,4,5))
x
y
x + y
x * y
x / y # Note that +,-,*,/ are all element wise

t(x) # Transpose of "x", hence "t(x)" is a row vector

t(x)%*%x # "%*%" is the matrix multiple, hence it's a scalar
x%*%t(x) # This is a 4 by 4 matrix


x > 5
x >= 5
y == 8




####################################################################
# Section 4: Matrix Operations                                     #
####################################################################

#----------------
# Matrix creation
#----------------
m = matrix(0,nrow=3, ncol=4)
m
dim(m) # dimention of m (3 by 4)
t(m) # transpose of m
dim(t(m))

m1 = matrix(1:12,nrow=3, ncol=4) # fill in numbers into a matrix (note that the default is by column)
m1
m2 = matrix(1:12, 3, 4, byrow=TRUE) # fill in numbers into a matrix "by row" 
m2

I = diag(1:5) # creating a diagonal matrix
I

M = matrix(1:25,5,5)
M
diag(M) # diagonal of M 
ncol(M) # number of column for M
nrow(M) # number of row for M


#---------------------
# row and column names
#---------------------
m = matrix(1:12, nrow = 3, ncol = 4)
m
rownames(m) = c("1993q1", "1993q2", "1993q3")
colnames(m) = c("weight", "hight", "BMI", "tsmx")

m
rownames(m)
colnames(m)


#----------------
# Combine vectors
#----------------
x <- 2:4
y <- 7:9
A=cbind(x, y)
B=rbind(x,y)
A
B

is.matrix(x)
is.matrix(A)

is.vector(x) 
dim(x) # note that a vector is NOT a matrix in R hence doesn't have dimension

Mx = as.matrix(x)
dim(Mx)


#---------------------
# statistics of matrix
#---------------------
M = matrix(c(1,1,1,1,1,1,8,4,7,1,2,2,5,4,6,4,7,5,2,500), nrow=5) 
table(M) # count for each element
sum(M) 
mean(M) 
median(M) 
rowSums(M) 
rowMeans(M) 
colSums(M) 
colMeans (M) 


#------------------
# matrix operation
#------------------
x <- matrix(1:4,2, 2)   
y <- matrix(rep(10, 4), 2, 2)
x;y
x+3
x*3
x/3            # observe that  +-*/ are all element-wise

x+y
x-y

x*y            # element-wise times
x/y            # element-wise divide
x %*% y        # matrix multiple
x %x% y        # kronecker product
det(x)         # matrix determinant

solve(x)       # matrix invertion
solve(x) %*% x     # identity matrix


#---------------
# Data selection 
#---------------
M = matrix(1:25,5,5)
M

M[2,3] # (2,3) element of M
M[2,]  # second row of M
M[,3]  # third column of M
M[1:2,]  # row 1 and 2  of M
M[1:2,1:2]  # row 1, 2 and column 1,2 of M
M[-1,] # M subtracted of row 1
M[-(3:5),-(3:5)]


#--------------------
# Matrix Decompostion 
#--------------------
x=matrix(c(4,5,3,6),2,2)
x
chol(x)           # Cholesky decomposition
qr(x)             # QR decomposition
svd(x)            # Singular value decomposition of x
eigen(x)          # eigenvalues and eigenvectors 



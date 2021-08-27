###############################################################################
##                           HW 3 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################



sampleSize = c(10,500)
DGPs = c('N(0,1)', 't(4)', 't(1)')
Moments = c('y', 'y^3', 'sin(y)', 'cos(y)')
rep = 1000

windows(width = 10, height = 10)
par(mfrow = c(6,4))

for(size in sampleSize){
  for(DGP in DGPs){
    for(moment in Moments){
      recordMn = c()
      for(r in 1:rep){
        if(DGP == 'N(0,1)'){
          randomSample = rnorm(size,0,1)
        }
        else if(DGP == 't(4)'){
          randomSample = rt(size, 4)
        }
        else if(DGP == 't(1)'){
          randomSample = rt(size, 1)
        }
        
        if(moment == 'y'){
          momentSample = randomSample
        }
        else if(moment == 'y^3'){
          momentSample = randomSample^3
        }
        else if(moment == 'sin(y)'){
          momentSample = sin(randomSample)
        }
        else if(moment == 'cos(y)'){
          momentSample = cos(randomSample)
        }
        varN = var(momentSample)
        Mn = sum(momentSample)/(size*varN)
        recordMn[r] = Mn
      }
      hist(recordMn, breaks = 10, main=paste('The histogram of Mn, ', 'samplesize is ',toString(size), ', DGP = ', DGP, ', Moment function is ', moment), xlim=c(-2.5,2.5))
      abline(v=1.6, col='red') #Z95的位置
      abline(v=2.3, col='blue') #Z99的位置
      lines(density(recordMn), col='red', lty=2) #Gaussian KDE
      L = seq(min(recordMn), max(recordMn), length=size) #PDF of standard normal distribution
      lines(lines(L, dnorm(L,0,1), col='blue', lty=2))
      
    }
  }
}
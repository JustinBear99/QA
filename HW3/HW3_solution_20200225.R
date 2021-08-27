###############################################################################
##                           Solution to HW 3          			    		         ##
##            	 Quantitative Analysis - 2019 Spring  3/30   		     	    	 ##
###############################################################################

rm(list=ls(all=T))
jpeg("MatrixPlot.jpeg",width=900,height=900)
sample_size<-c(10,500)
#windows(width=10,height=10)
par(mfrow=c(6,4))
rep<-1000
sample<-c(0)
name_DGP<-c("N(0,1)","t(4)","t(1)")
name_mf<- c("y","y^3","sin(y)","cos(y)")
library('scales')

t1 = Sys.time() #record running time
for(a in 1:2)
{
  for(b in 1:3) #number of DGP
  {
    for(c in 1:4) #number of mf
    {
      result<-function(i,j,l) #i for sample size, 
      {
        DGP<-cbind(matrix((rnorm(sample_size[i]*rep,0,1)),sample_size[i],rep),
                   matrix((rt(sample_size[i]*rep,df=4)),sample_size[i],rep),
                   matrix((rt(sample_size[i]*rep,df=1)),sample_size[i],rep))
        
        mf<-cbind(DGP[1:sample_size[i],(rep*(j-1)+1):(rep*j)],
                  (DGP^3)[1:sample_size[i],(rep*(j-1)+1):(rep*j)],
                  (sin(DGP))[1:sample_size[i],(rep*(j-1)+1):(rep*j)],
                  (cos(DGP))[1:sample_size[i],(rep*(j-1)+1):(rep*j)])
        
        for(k in (rep*(l-1)+1):(rep*l))
        {
          sigma_hat2<-sum((mf[,k]-sum(mf[,k])/sample_size[i])^2)/sample_size[i]
          M<-sum(mf[,k])/sqrt(sigma_hat2*sample_size[i])
          sample<-c(M,sample)
        }
        L99<-sum((sample)>qnorm(0.99))/length(sample)
        L95<-sum((sample)>qnorm(0.95))/length(sample)
        hist(sample,breaks=100,
             xlab= paste("pr(>2.3)=",percent(round(L99,3)),"/ pr(>1.6)=",percent(round(L95,3)))
             ,main= paste("N=",sample_size[i]," ",name_DGP[j],
                          " ",name_mf[l]),freq=F, cex.lab = 1.2, cex.main = 1.4)
        lines(density(sample),lwd=2, col="blue")
        lines(seq(-4, 4, length=100), dnorm(seq(-4, 4, length=100)), lwd=2, col="red")
      }
      
      example<-result(a,b,c)
    }
  }
}
dev.off()
t2 = Sys.time()
usingTime = t2 - t1
print(usingTime)
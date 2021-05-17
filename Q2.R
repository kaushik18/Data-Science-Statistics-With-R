

##############Whole function#############


Calculatecoverage=function(n,lambda){


#########Taking a common sample to calculate both the CIs

truemean=1/lambda
x<-rexp(n,lambda)
lambda.est=1/mean(x)

#######using large-sample Z interval
####Estimating mean of the population
mean.est=mean(x)
SD.est=mean(x)
Z.CI=mean.est+c(-1,1)*qnorm(1-(1-.95)/2)*SD.est/sqrt(n)
if(Z.CI[1]<=truemean && Z.CI[2]>=truemean){lieswithin.ZCI=1} else {lieswithin.ZCI=0}


#####Percentile bootstrap method
nboot=1000
#########Function to done bootstrap sample
mean.star.boot=function(n,lambda.est){
  xstar=rexp(n,lambda.est)
  mean.star=mean(xstar)
  return(mean.star)
}

####Get boot distribution using the above function
mean.boot.dist=replicate(nboot,mean.star.boot(n,lambda.est))
#percentile bootstrap 
boot.CI=sort(mean.boot.dist)[c(25, 975)]
#Return 1 if it contains the true lambda , else return 0
if(boot.CI[1]<=truemean && boot.CI[2]>=truemean){lieswithin.bootCI=1} else {lieswithin.bootCI=0}

CIcoverage=c(lieswithin.ZCI,lieswithin.bootCI)
return(CIcoverage)

}


nvalues=c(5,10,30,100)
lambdavalues=c(0.01,0.1,1,10)
for (n in nvalues) {
  for (lambda in lambdavalues) {
    
  cat("n is ",n)
  cat("\n")
    
  cat("lambda is ",lambda)
  cat('\n')
   
  

#n=5
#lambda=1
coverage.proportion=replicate(5000, Calculatecoverage(n,lambda))

#Each row -one for Z coverage and one for bootstrap coverage.
cat("The coverage means are(Z and bootstrap respectively)")
meancoverage=rowMeans(coverage.proportion)
cat("\n",meancoverage)
cat("\n")
cat("\n")

zprop=coverage.proportion[c(TRUE,FALSE)]
bootprop=coverage.proportion[c(FALSE,TRUE)]
#mean(zprop)
#mean(bootprop)

#truemean=1/lambda
#x<-rexp(n,lambda)
#lambda.est=1/mean(x)

}
}

###############################DEBUGGING##############
########using large-sample Z interval
####Estimating mean of the population
#mean.est=mean(x)
#SD.est=mean(x)
#Z.CI=mean.est+c(-1,1)*qnorm(1-(1-.95)/2)*SD.est/sqrt(n)
#if(Z.CI[1]<=truemean && Z.CI[2]>=truemean){lieswithin.ZCI=1} else {lieswithin.ZCI=0}


Generating the simulated data
sim.data<-function(n,k,S,alpha,beta,Sigma,sigma0,shape,rate,seed,distribution,file,threshold.censoring=FALSE,last.obs=FALSE,old.data=list(),max.time=10){
	data<-old.data
	set.seed(seed)
	if(is.null(distribution)) stop("distribution argument must be specified, e.g. `exponential` or `weibull`")
	sc<-function(times=NULL){
			exp(-rate * times^shape)
	}	
	hr<-function(times=NULL){
		mu<-function(t){if(length(beta)==2) cbind(1,t)%*%t(t(beta)) + cbind(1,t)%*%t(t(b[i,]))
			else cbind(1,t,t^2)%*%t(t(beta)) + cbind(1,t,t^2)%*%t(t(b[i,])) }
		mean.mu<-ifelse(alpha==0,beta[1],beta[1]-1/alpha*log(10*alpha*beta[2]/(exp(alpha*beta[2]*10)-1)))
		exp(alpha*(mu(times)-mean.mu))
	}

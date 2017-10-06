getLambda<-function(x,y,family="gaussian",lambdaType="lambda.min",alpha=0,nIter=100,seed=123){
	#default values: ridge for gaussian variable, 100 different CV, seed initialized at 123
	stockLambdas<-stockMSE<-stockSelVar<-list()
	stockLambdaMin<-lambdaTypeMSE<-rep(0,nIter)
	set.seed(seed)
	for(i in 1:nIter){
		fit<-cv.glmnet(x=x,y=y,family=family,alpha=alpha)
		stockLambdas<-c(stockLambdas,list(fit$lambda))
		stockMSE<-c(stockMSE,list(fit$cvm))
		stockLambdaMin[i]<-fit[[lambdaType]]
		lambdaTypeMSE[i]<-fit$cvm[fit$lambda==fit[[lambdaType]]]
		fitFixLambda<-glmnet(x=x,y=y,family=family,alpha=alpha,lambda=fit[[lambdaType]])
		stockSelVar<-c(stockSelVar,list(which(fitFixLambda$beta!=0)))
	}
	return(list(lambdas=stockLambdas,MSEs=stockMSE,lambdaType=stockLambdaMin,lambdaTypeMSE=lambdaTypeMSE,selectedVar=stockSelVar))
}

completeTable<-function(tab,labels){
	if(length(names(tab))<length(labels) & sum(names(tab)%in%labels)!=0){
		dif<-labels[!labels%in%names(tab)]
		tab<-c(tab,rep(0,length(dif)))
		names(tab)[names(tab)==""]<-dif
	}
	return(tab[order(as.numeric(names(tab)))])
}

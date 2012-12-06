covProb.sim <- function(iters=1000, n=1000, dist0=list(mu=rep(0,10), Sigma=diag(10), N=100000), dist1=list(mu=rep(0.5,10), Sigma=diag(10), N=100000), confidence=0.95, V=10, parallel=FALSE){

  starttime <- Sys.time()
  require(MASS)  #to use mvrnorm
  if (parallel==TRUE){
    require(parallel)
    print("Parallel option (forking on available cores) will be used.")
  }
  
  # Argument checks
  if (length(dist0)!=3 | length(dist1)!=3 | is.list(dist1)==FALSE | is.list(dist0)==FALSE){
    stop("dist0 and dist1 must each be a list containing a mu vector a Sigma covariance matrix and a population size N.")
  }
  if (length(dist0$mu)!=length(dist1$mu)){
    stop("The mean vectors for dist1 and dist2 must be the same size.")
  }
  if (is.matrix(dist0$Sigma) * is.matrix(dist1$Sigma)==FALSE){
    stop("Sigma in dist0 and dist1 must be a (covariance) matrix.")
  }
  if (confidence <=0 | confidence >= 1){
    print("confidence must be a value between 0 and 1.")
  }
  # dist0$N + dist1$N is population size
  # n is sample size
  # Create sample sizes from dist0 and dist1 that share the same distribution as the "population" data.
  n0 <- round(n*(dist0$N/(dist0$N + dist1$N)))
  n1 <- n-n0    
  .check_dist <- function(dist, n){
    mu <- dist$mu
    Sigma <- dist$Sigma
    N <- dist$N
    if(length(mu)!=dim(Sigma)[1] | length(mu)!=dim(Sigma)[2] | is.numeric(mu)==FALSE | is.matrix(Sigma)==FALSE) {
      stop("Dimensions of the mu vector and Sigma matrix must match.")
    }
    if (N < n){
      stop("The population size, N, must be greater than the sample size, n.")
    }
  }
  .check_dist(dist0, n0)
  .check_dist(dist1, n1)

  .create_data <- function(dist, label, n=NULL){
    if (is.null(n)==TRUE){
      return(data.frame(mvrnorm(n=dist$N, mu=dist$mu, Sigma=dist$Sigma), label=label)) #Population data
    } else {
      return(data.frame(mvrnorm(n=n, mu=dist$mu, Sigma=dist$Sigma), label=label))  #Sample data
    }
  }

  # Create the population data  
  print("Generating population data")
  pop0 <- .create_data(dist=dist0, label=0)
  pop1 <- .create_data(dist=dist1, label=1)
  pop <- rbind(pop0, pop1)

  # Create cross-validation folds (stratify by outcome)
  .cvFolds <- function(Y, V){  
    Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
    Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
    folds <- vector("list", length=V)
    for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}		
    return(folds)
  }

  # Fit a logistic regression model to single training fold
  # Use the fit to generate predicted values on validation fold
  .doFit <- function(v, folds, samp, pop){
    fit <- glm(label~., data=samp[-folds[[v]],], family=binomial)
    fold.pred <- predict(fit, newdata=samp[folds[[v]],], type="response")
    pop.pred <- predict(fit, newdata=pop, type="response")    
    true.fold.auc <- cvAUC(predictions=pop.pred, labels=pop$label)$fold.AUC  #AUC (not CV)
    return(list(fold.pred=fold.pred, true.fold.auc=true.fold.auc))
  }

  # Returns 1/0: CI did or did not contain truth
  .doSim <- function(seed, n0, n1, dist0, dist1, pop, V, confidence){   
    set.seed(seed)
    samp0 <- .create_data(dist=dist0, label=0, n=n0)
    samp1 <- .create_data(dist=dist1, label=1, n=n1)
    samp <- rbind(samp0, samp1)
    if (sum(samp$label) < V | sum(!samp$label) < V) {
      stop("The number of observations with (Y=1) or (Y=0) must be greater than the number of folds.")
    }
    folds <- .cvFolds(Y=samp$label, V=V)
    rlist <- sapply(seq(V), .doFit, folds=folds, samp=samp, pop=pop, simplify=FALSE)
    true.cvauc <- mean(unlist(sapply(seq(V), function(i, rlist){return(rlist[[i]][2])}, rlist=rlist)))
    predictions <- unlist(sapply(seq(V), function(i, rlist){return(rlist[[i]][1])}, rlist=rlist))
    predictions[as.numeric(unlist(folds))] <- predictions
    perf <- ci.cvAUC(predictions=predictions, labels=samp$label, folds=folds, confidence=confidence)
    covered.truth <- ifelse(true.cvauc>=perf$ci[1] & true.cvauc<=perf$ci[2], 1, 0)
    return(covered.truth)	
  }
  if (parallel==TRUE){
    print(sprintf("Starting forked cluster on %s cores.  Beginning simulation of %s iterations...", detectCores(), iters))
    cl <- makeCluster(detectCores(), type="FORK")
    cov.prob <- sum(parSapply(cl=cl, X=seq(iters), FUN=.doSim, n0=n0, n1=n1, dist0=dist0, dist1=dist1, pop=pop, confidence=confidence, V=V))/iters
    stopCluster(cl)
  } else {
    cov.prob <- sum(sapply(seq(iters), .doSim, n0=n0, n1=n1, dist0=dist0, dist1=dist1, pop=pop, confidence=confidence, V=V))/iters
  }
  runtime <- Sys.time() - starttime
  return(list(k=length(dist0$mu), cov.prob=cov.prob, runtime=runtime))
}

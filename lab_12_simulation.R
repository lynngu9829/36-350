generate_data = function(n, p){
  data = rep(dnorm(x,mean=0,sd=1), times = n*p ) 
  covariates = matrix(data, nrow=n, ncol=p)
  responses = as.vector( rep(dnorm(x,mean=0,sd=1), times = n))
  return(list(covariates, responses))
}

model_select = function(covariates, responses, cutoff){
  lm1 = lm(responses ~ covariates)
  p.values = summary(lm1)$coefficients[,4]
  lm2 = lm(responses ~ covariates[, p.values < cutoff ])
  new.p.values = summary(lm2)$coefficients[,4]
  return(new.p.values)
}

run_simulation = function(n_trials, n, p, cutoff){
  for (i in 1:n_trials){
    data = generate_data(n,p)
    p.val = model_select(data[[1]], data[[2]])
    hist(p.val)
  }
}
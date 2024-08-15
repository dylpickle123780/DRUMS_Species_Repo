MCMC.jags.wrapper = function(true_b1, x, true_phi = 0.015, max_dist = 15, 
                             theta = 1, B = 10, S = 3, burn = 1000, iters = 10000,
                             n.chains = 2,thin = 1,true_b0){
  
  #converting to lambda from b1
  true_lambda <- exp(true_b0+true_b1*x)

  true_lambda = matrix(true_lambda,nrow = B) 

  #make all responses matrices instead of just vectors for different sites
  
  #bin area
  A      <- ((theta*(seq(2,max_dist,length=B))^2)/2) - 
    ((theta*(seq(0,max_dist,length=B+1))^2)/2)[-(B+1)]
  
  A = matrix(rep(A,S),nrow = B)
  

  #distance from camera
  d      <- seq(0.1,max_dist,length=B)
  d2     <- d^2
  d2 = matrix(rep(d2,S),nrow = B)

  #detection probability
  true_p <- exp(-true_phi*d2)   
  
  # Simulate dataset
  true_N <- matrix(rpois(B*S,true_lambda*A),nrow = B)
  Y      <- matrix(rbinom(B*S,true_N,true_p),nrow = B)

  require(rjags)
  
  data   <- list(Y=Y,A=A,B=B,d2=d2,S=S,x=x)
  params <- c("N","phi","b1","b0")
  init   <- list(N=Y+1,b1=0,b0 = -8,phi=1/median(d)^2)

  model_string <- textConnection("model{
  for(s in 1:S){
   for(b in 1:B){
     Y[b,s]  ~ dbinom(p[b,s],N[b,s])
     lambda[b,s] <- exp(b0 + b1*x[b,s])
     N[b,s]  ~ dpois(A[b,s]*lambda[b,s])
     p[b,s] <- exp(-phi*d2[b,s])
    }
   }
   phi ~ dgamma(0.1,0.1)
   b1 ~ dnorm(0,1/10000)
   b0 ~ dnorm(0,1/10000)
 }")
  
  model   <- jags.model(model_string,data = data,inits=init,
                        n.chains=n.chains,quiet=TRUE)

  update(model, burn, progress.bar="none")
  samples <- coda.samples(model, variable.names=params, 
                          thin=thin, n.iter=iters, progress.bar="none")
  samples <- rbind(samples[[1]],samples[[2]])

  N       <- samples[,1:B*S]
  b0 <- samples[,B*S+1]
  b1  <- samples[,B*S+2]
  phi     <- samples[,B*S+3]

  return(b1)
}

MCMC.jags.wrapper.given = function(x, true_phi = 0.015, max_dist = 15, 
                             theta = 1, B = 10, S = 3, burn = 3000, iters = 10000,
                             n.chains = 2,thin = 1, true_N, Y, b0_init = 0, b1_init = 0,
                             m = 50,bin_size = 2){
  
  #make all responses matrices instead of just vectors for different sites
  
  #bin area
  A      <- ((theta*(seq(bin_size,max_dist,length=B))^2)/2) - 
    ((theta*(seq(0,max_dist,length=B+1))^2)/2)[-(B+1)]

  A = matrix(rep(A,S),nrow = B)
  
  #distance from camera
  d      <- seq(bin_size/2, max_dist-bin_size/2, length=B)
  d2     <- d^2
  d2 = matrix(rep(d2,S),nrow = B)

  #detection probability
  true_p <- exp(-true_phi*d2)   

  
  # Simulate dataset

  
  
  require(rjags)
  
  data   <- list(Y=Y, A=A, B=B, d2=d2, S=S, x=x, m = m)
  params <- c("N", "phi", "b1", "b0")

  init   <- list(N=Y+1, b1= b1_init, b0 = b0_init, phi=0.015)
  
  
  model_string <- textConnection("model{
  for(s in 1:S){
   for(b in 1:B){
     Y[b,s]  ~ dbinom(p[b,s],N[b,s])
     lambda[b,s] <- m*exp(b0 + b1*x[b,s])
     N[b,s]  ~ dpois(A[b,s]*lambda[b,s])
     p[b,s] <- exp(-phi*d2[b,s])
    }
   }
   phi ~ dgamma(0.1,0.1)
   b1 ~ dnorm(0,1/10000)
   b0 ~ dnorm(0,1/10000)
 }")
  
  model   <- jags.model(model_string,data = data,inits=init,
                        n.chains=n.chains,quiet=TRUE)
  
  update(model, burn, progress.bar="none")
  
  samples <- coda.samples(model, variable.names=params, 
                          thin=thin, n.iter=iters, progress.bar="none")
  
  samples <- rbind(samples[[1]],samples[[2]])

  N <- samples[,1:(B*S)]

  b0 <- samples[,B*S+1]
  b1  <- samples[,B*S+2]
  phi     <- samples[,B*S+3]
  print(mean(rowSums(N)))
  return(b1)
}

MCMC.jags.wrapper.lambda = function(x, true_phi = 0.015, max_dist = 15, 
                                   theta = 1, B = 10, S = 3, burn = 1000, iters = 10000,
                                   n.chains = 2,thin = 1, true_N, Y){
  
  #make all responses matrices instead of just vectors for different sites
  
  #bin area
  A <- ((theta*(seq(2,max_dist,length=B-1))^2)/2) - 
    ((theta*(seq(0,max_dist,length=B+1))^2)/2)[-B]
  
  A = matrix(rep(A,S),nrow = B)
  print(A)
  
  
  #distance from camera
  d      <- seq(0.1,max_dist,length=B)
  d2     <- d^2
  d2 = matrix(rep(d2,S),nrow = B)
  print(d2)
  #detection probability
  true_p <- exp(-true_phi*d2)   
  print(true_p)
  
  # Simulate dataset
  set.seed(123780)
  
  
  require(rjags)
  
  data   <- list(Y=Y,A=A,B=B,d2=d2,S=S,x=x)
  params <- c("N","phi","lambda")
  
  init   <- list(N=Y+1,lambda = sum(Y)/sum(A), phi=1/median(d)^2)
  
  print(init)
  
  model_string <- textConnection("model{
  for(b in 1:B){
   for(s in 1:S){
     Y[b,s]  ~ dbinom(p[b,s],N[b,s])
     N[b,s]  ~ dpois(A[b,s]*lambda)
     p[b,s] <- exp(-phi*d2[b,s])
    }
  }
   lambda ~ dgamma(0.1,0.1)
   phi ~ dgamma(0.1,0.1)
 }")
  
  model   <- jags.model(model_string,data = data,inits=init,
                        n.chains=n.chains,quiet=TRUE)
  
  update(model, burn, progress.bar="none")
  samples <- coda.samples(model, variable.names=params, 
                          thin=thin, n.iter=iters, progress.bar="none")
  samples <- rbind(samples[[1]],samples[[2]])
  
  N       <- samples[,1:B*S]
  lambda  <- samples[,B*S+1]
  phi     <- samples[,B*S+2]
  
  return(mean(lambda))
}
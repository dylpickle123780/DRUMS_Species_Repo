# Activation functions
act  <- function(x){pmax(x,0)}
actp <- function(x){x>0}

#####################################################################
# Random initial values for the weights:
#  p is the number of inputs
#  L1 and L2 are the number of nodes in the two layer
#  init_mn and init_sd are initial values for the mean and sd
#  sigma determines the random jitter in the initial values
#####################################################################

init_hetnorm <- function(p,L1,L2,init_mn=0,init_sd=1,sigma=0.01){
  w       <- list()
  w[[1]]  <- sigma*rnorm(L1)
  w[[2]]  <- sigma*matrix(rnorm(p*L1),p,L1)
  w[[3]]  <- sigma*rnorm(L2)
  w[[4]]  <- sigma*matrix(rnorm(L1*L2),L1,L2)
  w[[5]]  <- init_mn + sigma*rnorm(1)
  w[[6]]  <- sigma*rnorm(L2)
  w[[7]]  <- sigma*rnorm(L1)
  w[[8]]  <- sigma*matrix(rnorm(p*L1),p,L1)
  w[[9]]  <- sigma*rnorm(L2)
  w[[10]] <- sigma*matrix(rnorm(L1*L2),L1,L2)
  w[[11]] <- log(init_sd) + sigma*rnorm(1)
  w[[12]] <- sigma*rnorm(L2)
  return(w)}

# Prediction function given weights w and features x
predict_hetnorm <- function(w,x){
  m <- act(sweep(x%*%w[[2]],2,w[[1]],"+"))
  m <- act(sweep(m%*%w[[4]],2,w[[3]],"+"))
  m <- m%*%w[[6]] + w[[5]]
  s <- act(sweep(x%*%w[[8]],2,w[[7]],"+"))
  s <- act(sweep(s%*%w[[10]],2,w[[9]],"+"))
  s <- s%*%w[[12]] + w[[11]] 
  return(list(mu=m,sigma=exp(s)))}

# Loss function
loss_hetnorm <- function(w,x,y){
  l <- predict_hetnorm(w,x)
  l <- -sum(dnorm(y,l$mu,l$sigma,log=TRUE))
  return(l)}

# Gradient of the loss function
grad_hetnorm <- function(w,x,y){
  M1 <- sweep(x%*%w[[2]],2,w[[1]],"+")
  m1 <- act(M1)
  M2 <- sweep(m1%*%w[[4]],2,w[[3]],"+")
  m2 <- act(M2)
  m3 <- m2%*%w[[6]] + w[[5]]
  S1 <- sweep(x%*%w[[8]],2,w[[7]],"+")
  s1 <- act(S1)
  S2 <- sweep(s1%*%w[[10]],2,w[[9]],"+")
  s2 <- act(S2)
  s3 <- s2%*%w[[12]] + w[[11]] 
  n  <- length(y)
  
  g  <- w
  
  dldm3  <- -exp(-2*s3)*(y-m3)
  dlds3  <- 1+dldm3*(y-m3)
  
  part2  <- sweep(actp(M2),1,dldm3,"*")
  part1  <- (t(actp(M1))%*%part2)*w[[4]]
  g[[1]] <- as.vector(part1%*%w[[6]])
  g2     <- NULL
  for(j in 1:ncol(x)){ # remove the loop?
    PP <- sweep(actp(M2),1,dldm3*x[,j],"*")
    PP <- (t(actp(M1))%*%PP)*w[[4]]
    g2 <- rbind(g2,as.vector(PP%*%w[[6]]))
  }
  g[[2]] <- g2
  g[[3]] <- colSums(part2)*w[[6]]
  g[[4]] <- sweep(t(m1)%*%part2,2,w[[6]],"*")
  g[[5]] <- colSums(dldm3)
  g[[6]] <- as.vector(t(m2)%*%dldm3)
  
  part2   <- sweep(actp(S2),1,dlds3,"*")
  part1   <- (t(actp(S1))%*%part2)*w[[10]]
  g[[7]]  <- as.vector(part1%*%w[[12]])
  g8      <- NULL
  for(j in 1:ncol(x)){ # remove the loop?
    PP <- sweep(actp(S2),1,dlds3*x[,j],"*")
    PP <- (t(actp(S1))%*%PP)*w[[10]]
    g8 <- rbind(g8,as.vector(PP%*%w[[12]]))
  }
  g[[8]]  <- g8
  g[[9]]  <- colSums(part2)*w[[12]]
  g[[10]] <- sweep(t(s1)%*%part2,2,w[[12]],"*")
  g[[11]] <- colSums(dlds3)
  g[[12]] <- as.vector(t(s2)%*%dlds3)
  return(g)}

# Functions to scale the inputs:

stdq <- function(x,q,low=-1,high=1){
  m         <- length(q)
  x[x<q[1]] <- q[1]
  x[x>q[m]] <- q[m]
  U         <- 1+0*x
  for(j in 2:(m-1)){U[x>q[j]]<-j} 
  U         <- U+(x-q[U])/(q[U+1]-q[U])
  return(low + (high-low)*U/m)}

quantile_scale <- function(x,q){
  for(k in 1:ncol(x)){
    x[,k] <- stdq(x[,k],q[,k])
  }
  return(x)}

########################################################################
# This is a general implementation of ADAM
#   w are initial values of the weights
#   x are inputs
#   y are outputs
#   loss is the loss function
#   grad is its gradient
#   everything else is tuning parameters with (hopefully) obvious names and decent defaults
########################################################################

adam <- function(w, x, y, loss, grad,
                 lr = 0.001, batchsize = 100, epochs = 50, propval = 0.2,
                 beta1=0.9, beta2=0.999, epsilon=0.1^8,
                 verbose=2,early_stop=5){
  
  # https://arxiv.org/pdf/1412.6980.pdf
  
  val   <- seq(0,1,length=length(y)) > 1-propval
  x_val <- x[val,]
  y_val <- y[val]
  x     <- x[!val,]
  y     <- y[!val]
  n     <- length(y)
  nb    <- floor(n/batchsize)
  bs    <- rep(1:nb,length=n)[1:n]
  
  m <- v <- w
  for(k in 1:length(w)){
    m[[k]] <- 0*m[[k]]
    v[[k]] <- 0*v[[k]]
  }  
  
  train_loss <- val_loss <- rep(NA,epochs)
  
  t <- 1
  stop = FALSE
  for(iter in 1:epochs){if(!stop){
    batch <- sample(bs)
    w0  <- w; m0  <- m; v0  <- v
    for(b in 1:nb){
      sub <- which(batch==b)
      N   <- n/length(sub)
      g   <- grad(w,x[sub,],y[sub])
      for(k in 1:length(w)){
        W         <- w        
        G         <- N*g[[k]]
        m[[k]]    <- beta1*m[[k]] + (1-beta1)*G
        v[[k]]    <- beta2*v[[k]] + (1-beta2)*(G^2)
        mhat      <- m[[k]]/(1-(beta1^t))
        vhat      <- v[[k]]/(1-(beta2^t))
        w[[k]]    <- w[[k]] - lr*mhat/(sqrt(vhat)+epsilon) 
        if(sum(is.na(w[[k]]))>0){w<-W;lr<-lr/2}
      } 
      t <- t+1
    }
    if(is.na(loss(w,x,y))){w<-w0;m<-m0;v<-v0}
    if(is.na(loss(w,x_val,y_val))){w<-w0;m<-m0;v<-v0}
    
    train_loss[iter] <- loss(w,x,y)/length(y)
    val_loss[iter]   <- loss(w,x_val,y_val)/length(y_val)
    if(val_loss[iter]==min(val_loss[1:iter])){bestw<-w}   
    
    if(verbose == 1){
      print(c(b,train_loss[iter],val_loss[iter]))
    }
    if(verbose == 2){
      plot(train_loss,xlab="Epoch",type="l",ylab="Loss",col=1,xlim=c(1,iter),
           ylim=range(c(val_loss,train_loss),na.rm=TRUE))
      lines(val_loss,col=2)
      legend("topright",c("Training","Validation"),lty=1,col=1:2,bty="n")     
    }
    
    if(iter>early_stop){
      j    <- iter-early_stop
      stop <- min(val_loss[which(1:iter > j)]-val_loss[j])>0        
    }
  }}   
  tuning <- list(lr = lr, batchsize = batchsize, epochs = epochs, propval = propval,
                 beta1=beta1, beta2=beta2, epsilon=epsilon, early_stop=early_stop)
  output <- list(w=bestw,tuning=tuning,train_loss=train_loss,val_loss=val_loss)
  return(output)}
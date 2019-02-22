# notities tijdens de les
# relatie tussen correlatie en covariantie
# r12 = sigma12 /sqrt(sigma1^2,sigma2^2)



  par(mfrow=c(2,2))
  n <- 100; 
  mu <- c(2,3); 
  for(s12 in c(0,0.3,0.6,0.9)*sqrt(5*2)){
  sigma <- matrix(c(5,s12,s12,2),nrow=2);
  X <- mvrnorm(n,mu=mu,Sigma =sigma);
  plot(X,main = paste("r12 =",s12/sqrt(5*2)))}
   
  set.seed(2019)
  r12 <- 0.6;
  s12 <- r12*sqrt(5*2);
  sigma <- matrix(c(5,s12,s12,2),nrow=2);
  X <- mvrnorm(n,mu=mu,Sigma =sigma);
  muX <- colMeans(X);
  # exercise 2 ----------------------------------------
  Z <- scale(X,center=mu,scale=TRUE)
  
  # exercise 3 ----------------------------------------
  set.seed(2019)
  A <- matrix(c(3,5,2,1),ncol=2)
  d <- rnorm(2)
  Y <- t(t(X%*%A)+d)
  colMeans(Y)
  colMeans(X)%*%A+d
  cov(Y)
  t(A)%*%cov(x)%*%A
  
  # exercise 4 -----------------------------------------
  
  Mal_X = sqrt(mahalanobis(X,colMeans(X),cov(X)))
  Mal_Y = sqrt(mahalanobis(Y,colMeans(Y),cov(Y)))
  Mal_X-Mal_Y #==O?
  
  # exercise 5 -----------------------------------------
  set.seed(2019)
  r12 <- 0.6;
  s12 <- r12*sqrt(5*2);
  sigma <- matrix(c(5,s12,s12,2),nrow=2);
  X <- mvrnorm(n,mu=mu,Sigma =sigma);
  X_stand <- scale(X)
  e <- eigen(cov(X))
  V <- e$vectors
  S = V%*%diag(e$values^(-1/2))%*%t(V)
  X_sphere <- X%*%S
  par(mfrow=c(1,3))
  par(pty="s")
  plot(X)
  lines(ellipse(cov(X),centre = colMeans(X),level = 0.95))
  lines(ellipse(sigma,centre = mu,level = 0.95),col="blue")
  plot(X_stand)
  lines(ellipse(cov(X_stand),centre = colMeans(X_stand),level = 0.95))
  plot(X_sphere)
  lines(ellipse(cov(X_sphere),centre = colMeans(X_sphere),level = 0.95))

        

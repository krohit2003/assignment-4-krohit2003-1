### Paste all your codes for model building
### and cross-validation here
data=read.csv("assign4_train.csv")
y <- data$y

X <-  as.matrix(data[ ,-1])

n <- dim(X)[1]
p <- dim(X)[2]

lam.vec <- c(seq(545,550,by=1))
CV.error <- numeric(length = length(lam.vec))

for(l in 1:length(lam.vec))
{
  track.cv <- 0
  lam <- lam.vec[l]
  for(i in 1:n)
  {
    # Making training data
    X.train <- X[-i,] # removing ith X
    y.train <- y[-i] #removing ith y
    # fitting model for training data
    beta.train <- solve(t(X.train) %*% X.train + lam*diag(1,p)) %*% t(X.train) %*% y.train
    # test error
    track.cv <- track.cv + (y[i] - X[i,] %*% beta.train)^2
  }
  CV.error[l] <- track.cv/n
}
chosen.lam <- lam.vec[which.min(CV.error)]
beta.final <- solve(t(X) %*% X + chosen.lam*diag(p)) %*% t(X) %*% y

save(beta.final,file='fit_params.Rdata')

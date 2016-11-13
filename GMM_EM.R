mu<- c(0, 50)
sigma<- c(5, 10)
Data<- c(rnorm(3000, mu[1], sigma[1]), rnorm(1000, mu[2], sigma[2]))

mu_update<- c(10, 100)
sigma_update<- c(10, 5)
p<- c(0.7, 0.3)

mu_trace<- mu_update
sigma_trace<- sigma_update
p_trace<- p

likh<- sum(log(p[1]*dnorm(Data, mu_update[1], sigma_update[1])+ p[2]*dnorm(Data, mu_update[2], sigma_update[2])))

for(i in 1:100){
  w1<- sapply(Data, function(x) p[1]*dnorm(x, mu_update[1], sigma_update[1]))/sapply(Data, function(x) sum(p[1]*dnorm(x, mu_update[1], sigma_update[1])+p[2]*dnorm(x, mu_update[2], sigma_update[2]))) 
  w2<- sapply(Data, function(x) p[2]*dnorm(x, mu_update[2], sigma_update[2]))/sapply(Data, function(x) sum(p[1]*dnorm(x, mu_update[1], sigma_update[1])+p[2]*dnorm(x, mu_update[2], sigma_update[2])))
  
  p<- c(mean(w1), mean(w2))
  mu_update[1]<- sum(w1*Data)/sum(w1)
  mu_update[2]<- sum(w2*Data)/sum(w2)
  sigma_update[1]<- sqrt(sum(w1*(Data-mu_update[1])^2)/sum(w1))
  sigma_update[2]<- sqrt(sum(w2*(Data-mu_update[2])^2)/sum(w2))
  
  mu_trace<- rbind(mu_trace, mu_update)
  sigma_trace<- rbind(sigma_trace, sigma_update)
  p_trace<- rbind(p_trace, p)
  
  likh<- c(likh, sum(log(p[1]*dnorm(Data, mu_update[1], sigma_update[1])+ p[2]*dnorm(Data, mu_update[2], sigma_update[2]))))
}

plot(likh, type = "l", xlab = "Number of iterations", ylab = "", main = "Likelihood")

plot(mu_trace[,1], type = "p", ylim = c(min(mu_trace),max(mu_trace)), xlab = "Number of iterations", ylab = "", main = "Parameters: mu", col = 2)
points(mu_trace[,2], col = 4)

plot(sigma_trace[,1], type = "p", ylim = c(min(sigma_trace),max(sigma_trace)), xlab = "Number of iterations", ylab = "", main = "Parameters: Sigma", col = 2)
points(sigma_trace[,2], col = 4)

plot(p_trace[,1], type = "p", ylim = c(min(p_trace),max(p_trace)), xlab = "Number of iterations", ylab = "", main = "Parameters: P", col = 2)
points(p_trace[,2], col = 4)

hist(Data, nclass = 50, freq=FALSE, ylim = c(0,0.065))
rug(Data)
curve(p[1]*dnorm(x, mean=mu_update[1], sd=sigma_update[1]), add=TRUE, col="red", lty=1, lwd = 2, xaxt="n")
curve(p[2]*dnorm(x, mean=mu_update[2], sd=sigma_update[2]), add=TRUE, col="blue", lty=1, lwd = 2,  xaxt="n")
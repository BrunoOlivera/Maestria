pi= .5 # prior
n =1000 #size of the sample
population <- sample(1:0, n, rep = TRUE, prob = c(pi, 1- pi))
table(population)
n1 <- table(population)["1"]; n2 <- table(population)["0"]
# parameters
mu.1 <- 2.5; sigma.1 <- 1
mu.2 <- 7; sigma.2 <- 2
x1 <- rnorm(n1, mu.1, sigma.1)
x2 <- rnorm(n2, mu.2, sigma.2)
x12 <- c(x1, x2)
mean(x1); mean(x2); mean(x12)
sd(x1); sd(x2); sd(x12)



hist(x1, freq = F); curve(dnorm(x, mean = mu.1, sd = sigma.1), add = T, lwd = 2, col = 'red')
hist(x2, freq = F); curve(dnorm(x, mean = mu.2, sd = sigma.2), add = T, lwd = 2, col = 'blue')
hist(x12, freq = F, ylim=c(0,55))
rug(x1, col = 'red')
rug(x2, col = 'blue')
curve(dnorm(x, mean = mu.1, sd = sigma.1), lty = 3, add = T, col = 'red')
curve(dnorm(x, mean = mu.2, sd = sigma.2), lty = 2, add = T, col = 'blue')




#Density:
g.mixture <- function(x, pi, mu, sigma) {
  g <- pi * dnorm(x, mu[1], sigma[1]) + (1 - pi) * dnorm(x, mu[2], sigma[2])
  return(g)
}
curve(g.mixture(x, pi = n1/n, c(mu.1, mu.2), c(sigma.1, sigma.2)), lwd = 2, add = T)



x.test <- sample(c(0, 1, 3.5, 8, 10, 12))
clasificacion <- ifelse(alpha * dnorm(x.test, mu.1, sigma.1) > (1 - alpha) * dnorm(x.test, mu.2, sigma.2), 'Group 1', 'Group 2')
cbind(x.test, poblacion = clasificacion)



mu.1 <- 2.5; sigma.1 <- 1
mu.2 <- 7; sigma.2 <- 2
x1 <- rnorm(n1, mu.1, sigma.1)
x2 <- rnorm(n2, mu.2, sigma.2)
# plot(,ylim=c(0,60))
curve(dnorm(x, mean = mu.1, sd = sigma.1),xlim=c(-10,10), lty = 1, col = 'red', ylab='densities',main='marginales')
curve(dnorm(x, mean = mu.2, sd = sigma.2), lty = 1,add=T, col = 'blue')
boundary=function(x){ dnorm(x,mu.1,sigma.1)/dnorm(x,mu.2,sigma.2)-1 }
curve(boundary(x), lty = 1,add=F, col = 'black', xlim = c(-10,10))


#Equal prior
boundary=function(x) { dnorm(x,mu.1,sigma.1)/dnorm(x,mu.2,sigma.2)-1 }
library(rootSolve) #required by the function uniroot.all
raices <- uniroot.all(boundary,c(-100,100)); raices


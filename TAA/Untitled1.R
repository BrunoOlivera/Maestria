n=100
x=sort(10*runif(n))
y=2*x+3+rnorm(n,sd=4)
modelo=lm(y~x)
plot(x,y,pch=19,main="mi simulacion")
abline(3,2,lwd=2)
abline(coef(modelo),lwd=2,col="red")
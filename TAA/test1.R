datos = read.table("miejemplo.txt", sep = ",", header = T)
datos2 = read.csv("misdatos.csv", sep = ",", header = T)
write.table(datos2, "misotrosdatos.txt")

a = seq(0,10,.5)
# generate samples from a probability distribiution
set.seed(2019)
x1 = rnorm(10, mean=0, sd=1)
x2 = rnorm(100, mean=-2, sd=4)
x3 = runif(50, min=0, max=1)
x4 = runif(30, min=-2, max=4)

library(MASS)
mean1 = c(-2,2)
cov1 = matrix(c(1,.5,.5,1),nrow=2)
data = mvrnorm(15,mean1,cov1)


x = seq(0,10,.5)
y = seq(5,15,.5)
plot(x,y)
lines(x,y)
abline(a=5,b=1,col="red")
barplot(x)


plot(density(x))
curve(dnorm(x,mean=5,sd=.08) ,col="blue",add=T)

data("iris")
attach(iris)
Sepal.Length
mean(Sepal.Length)
var(Sepal.Length)
summary(iris)

data("cars")
cars
attach(cars)
summary(cars)
model = lm(dist - speed)
summary(model)
plot(cars, )
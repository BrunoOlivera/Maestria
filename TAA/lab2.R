# lab2

#ej. 2-1
x1=c('a','a','b','a','a','b','b','b')
x2=c('b','a','a','a','a','b','b','b')
y=c(1,1,1,1,-1,-1,-1,-1)

train = t(rbind(x1,x2,y=as.numeric(y))); train


x1=c('a','a','b','b')
x2=c('a','b','a','b')
pred=c(0,0,0,0)
real=c(-1,1,-1,1)

test=t(rbind(x1,x2,pred,real)); test

prior = function(y){ sum(train[,3] == y)/dim(train)[1] }
g = function(x1,x2,y) { sum(colSums(t(train) == c(x1, x2, y)) == 3) / sum(train[,3] == y) }
classifyValue = function(v) { ifelse(prior(1)*g(v[1],v[2],1) > prior(-1)*g(v[1],v[2],-1), 1, -1) }
test[,3] = apply(test,1,classifyValue); test
error = sum(test[,3] != test[,4]) / dim(test)[1]; error

#ej. 2-3

#a)

install.packages("mvtnorm")
library(mvtnorm)

xx_1=rmvnorm(100,mean=c(3,1),sigma=matrix(c(1,0,0,1),2,2))
xx_1=cbind(xx_1,1)
xx_0=rmvnorm(100,mean=c(1,3),sigma=matrix(c(1,0,0,1),2,2))
xx_0=cbind(xx_0,0)
xx=rbind(xx_1,xx_0); xx

#b)

xrange=c(min(floor(min(xx_1[,1])),floor(min(xx_0[,1]))),max(ceiling(max(xx_1[,1])),ceiling(max(xx_0[,1]))))
yrange=c(min(floor(min(xx_1[,2])),floor(min(xx_0[,2]))),max(ceiling(max(xx_1[,2])),ceiling(max(xx_0[,2]))))

plot(xx_1,xlim=xrange,ylim=yrange,xlab="x1",ylab="x2")
points(xx_1,pch=19,col="red")
points(xx_0,pch=19,col="blue")

#c)

# mu.1=c(3,1); mu.2=c(1,3)
# sigma.1=sigma.2=matrix(c(1,0,0,1),2,2)

# d=function(x,mu,sigma){exp(-0.5*t(x-mu)%*%sigma%*%(x-mu))/2*pi}
# d2=function(x,mu,sigma){exp(-0.5*sum((x-mu)^2))/2*pi}
# 
# classifier=function(x){ dmvnorm(x,mean=mu.1,sigma=sigma.1) > dmvnorm(x,mean=mu.2,sigma=sigma.2) }
# classifierWOT=function(x){ d(x,mu.1,sigma.1) > d(x,mu.2,sigma.2) }
# classifierWOT2=function(x){ d2(x,mu.1,sigma.1) > d2(x,mu.2,sigma.2) }

# g=function(x,mu,sigma){exp(-0.5*sum((x-mu)^2))/2*pi}
# classifier=function(x){ g(x,mu.1,sigma.1) > g(x,mu.2,sigma.2) }

# classifier=function(x){x[2]<x[1]}

#d)
ind=sample(c(TRUE, FALSE), 200, replace=TRUE, prob=c(0.8, 0.2))
xtrain=xx[ind,1:2]; xtrain
ytrain=xx[ind,3]; ytrain
xtest=xx[!ind,1:2]; xtest
ytest=xx[!ind,3]; ytest

# error=sum(classifier(xtrain)!=ytrain)/dim(xtrain)[1]; error
# errorWOT=sum(apply(xtrain,1,classifierWOT)!=ytrain)/dim(xtrain)[1]; errorWOT
# errorWOT2=sum(apply(xtrain,1,classifierWOT2)!=ytrain)/dim(xtrain)[1]; errorWOT2

error_train=sum((xtrain[,2]<xtrain[,1])!=ytrain)/dim(xtrain)[1]; error_train

#e)
model=lm(ytrain~xtrain)
A=model$coef[1]
B=model$coef[-1]



#f)

# (0.5-model$coef[1]-x[1]*model$coef[2])/model$coef[3]=x[2]

a=(0.5-model$coef[1])/model$coef[3]
b=-model$coef[2]/model$coef[3]

abline(a=a, b=b,lwd=2,col="green")
abline(c(0,1),lwd=2,col="black")

#g)
A=model$coef[1]
B=model$coef[-1]
data_pred=as.vector(1*(A+xtest%*%B > 0.5)); data_pred
# data_pred=as.vector(1*(A+xtest%*%B > 0.5))
# lm_error=mean(ytest!=data_pred); lm_error
lm_error=mean(ytest!=data_pred); lm_error
error_test=sum((xtest[,2]<xtest[,1])!=ytest)/dim(xtest)[1]; error_test

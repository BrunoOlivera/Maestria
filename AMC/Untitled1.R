x=1:100
X=sample(x,150,replace=T)
Y=2+3*X+rnorm(150,0,50)
modelo=lm(Y~X)
modelo

summary(modelo)

modelo$res
rstudent(modelo)

plot(modelo$fitted,rstudent(modelo),
     xlab="predicciones",
     ylab="residuos estandarizados")

abline(h=2,col="red")
abline(h=-2,col="red")

par(mfrow=c(2,2))
plot(modelo)


res=resid(modelo)
par(mfrow=c(1,2))
plot(res,main=paste("Plot de los residuos"))
hist(modelo$res,breaks=10,col="red",proba=T)
xfit=seq(min(res),max(res),length=31)
yfit=dnorm(xfit,mean=mean(res),sd=sd(res))
lines(xfit,yfit,col="blue",lwd=2)

shapiro.test(res)



n <- 50
x <- sort(10 * runif(n))
y <- 2 + 3 * x + rnorm(n, sd = 5)
fit <- lm(y ~ x)
plot(x, y, pch = 19) # datos
abline(2, 3, lwd = 2) # verdadera
abline(coef(fit), lwd = 2, col = 'red') # estimaci\’on
legend("topleft", c("verdadera", "estimacion"),
       lty = 1, lwd = 2, col = c(1, 2))
new=data.frame(x=seq(0, 10, .5))
pred=predict(fit, interval="confidence")
pred2=predict(fit,newdata=new,interval="prediction")
lines(x, pred[, 2], col = "blue", lwd = 2)
lines(x, pred[, 3], col = "blue", lwd = 2)
lines(new[,1], pred2[, 2], col = "green", lwd = 2)
lines(new[,1], pred2[, 3], col = "green", lwd = 2)
title("Ejemplo Simulado")
predict(fit,newdata=data.frame(x=c(5,6)),interval="confidence")
predict(fit,newdata=data.frame(x=c(5,6)),interval="prediction")





vif_calc<-function(Xmat){
  VIF<-numeric()
  for(i in 1:ncol(Xmat)){
    Xmat_Y<-Xmat[,i]
    dataMAT<-cbind(Xmat_Y, Xmat[,-i])
    R2<-summary(lm(Xmat_Y~.,data=dataMAT, na.action="na.exclude"))$r.squared
    VIF[i]<-1/(1-R2)
  }
  names(VIF)<-colnames(Xmat)
  print(VIF)
}
vif_calc(iris[,1:4])
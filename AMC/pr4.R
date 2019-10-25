# a)
n=100

y=0
x1=rnorm(n/2,1,1)
x2=rnorm(n/2,3,1)
X1=cbind(x1,x2,y)

y=1
x1=rnorm(n/2,2,1)
x2=rnorm(n/2,2,1)
X2=cbind(x1,x2,y)
X=rbind(X1,X2)

# b)
plot(X[,1],X[,2], col=c("red","blue")[X[,3]+1], pch=20)

# c)

X_1=X1[1:50,-3]
X_2=X2[1:50,-3]

S_1=var(X_1)
S_2=var(X_2)

S=1/98*(49*S_1+49*S_2)
S_inv=solve(S)

d=colMeans(X_1)-colMeans(X_2)
w=S_inv%*%as.matrix(d)

modelo=lda(X[,3]~X[,-3])
plot(modelo)

# vemos que tienen la misma dirección
modelo$scaling/w

# d)
n=100

y=0
x1_t=rnorm(n/2,1,1)
x2_t=rnorm(n/2,3,1)
X1_t=cbind(x1_t,x2_t,y)

y=1
x1_t=rnorm(n/2,2,1)
x2_t=rnorm(n/2,2,1)
X2_t=cbind(x1_t,x2_t,y)
test=rbind(X1_t,X2_t)

preds.lda=predict(modelo, data.frame(test))
preds.lda$class

# % de bien clasificados
sum(preds.lda$class==test[,3])/n

# e)
modelo.cv=lda(test[,3]~test[,-3],CV=TRUE)
loopred=table(test[,3],modelo.cv$class)
sum(loopred[1,1]+loopred[2,2])/n

# f)
# n=100
# 
# y=0
# x1=rnorm(n/2,1,1)
# x2=rnorm(n/2,3,1)
# X1=cbind(x1,x2,y)
# 
# y=1
# x1=rnorm(n/2,2,1)
# x2=rnorm(n/2,2,1)
# X2=cbind(x1,x2,y)
# X=rbind(X1,X2)

modelo.qda=qda(X[,3]~X[,-3])
preds.qda=predict(modelo.qda, data.frame(test))
preds.qda$class

# % de bien clasificados
sum(preds.qda$class==test[,3])/n


modelo.cv.qda=lda(test[,3]~test[,-3],CV=TRUE)
loopred.qda=table(test[,3],modelo.cv.qda$class)
sum(loopred.qda[1,1]+loopred.qda[2,2])/n


# g)
modelo.logit=glm(X[,3]~X[,-3])
preds.glm=predict(modelo.logit, data.frame(test))
# % de bien clasificados
sum(as.numeric(preds.glm>.5)==test[,3])/n
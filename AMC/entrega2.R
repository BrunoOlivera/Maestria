set.seed(2019)
# 1)
# a)
n=100; a=-2; b=2; c=3
x1=runif(n,-4,5); x2=runif(n,-4,5)
y=exp(a*x1+b*x2+c + rnorm(n))
y=y/(1+y) ; y=rbinom(n,1,y)

# b)
points(x1,  col='red', pch=20)
points(x2,  col='blue', pch=20)

plot(x1+x2,y)

plot(x1, y, col='red', pch=20)
plot(x2, y, col='blue', pch=20)

# c)
glm.res=glm(y~x1+x2,family=binomial)
summary(glm.res)
# sacar
# plot(glm.res)

# d)
yhat=predict(glm.res,data.frame(x1=x1,x2=x2),type='response')
pred = as.numeric(yhat > 0.5); pred
table(pred,y)

# e)
n=100; a=-2; b=2; c=3
x1=runif(n,-4,5); x2=runif(n,-4,5)
y=exp(a*x1+b*x2+c + rnorm(n))
y=y/(1+y) ; y=rbinom(n,1,y)

yhat=predict(glm.res,data.frame(x1=x1,x2=x2),type='response')
pred = as.numeric(yhat > 0.5); pred
conf_matrix=table(pred,y); conf_matrix

VP=conf_matrix[2,2]
FN=conf_matrix[1,2]
VN=conf_matrix[1,1]
FP=conf_matrix[2,1]

sensibilidad = VP/(VP+FN)
especificidad = VN/(VN+FP)

seq(0,1,0.01);

plot(roc(y ~ pred))

# f)
n=100; a=-2; b=2; c=3
x1=runif(n,-4,5);
y=exp(a*x1+c + rnorm(n))
y=y/(1+y) ; y=rbinom(n,1,y)


glm.res=glm(y~x1,family=binomial)

yhat=predict(glm.res,data.frame(x1=x1),type='response')
pred = as.numeric(yhat > 0.5); pred
table(pred,y)
plot(roc(y ~ pred), add=TRUE, col='red')



# Ejercicio 2
set.seed(2019)
dlearn=read.csv('breast-cancer-wisconsin.data',sep=',',header=FALSE)
dlearn[dlearn == '?'] = as.numeric(NA)
dlearn <- na.omit(dlearn)
dlearn[,] <- sapply(dlearn[,], as.numeric)
dlearn[1] <- NULL
colnames(dlearn) <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','Class')
dlearn[,10] <- as.numeric(dlearn[,10]==4)

smp_size <- floor(0.8 * nrow(dlearn))
train_ind <- sample(seq_len(nrow(dlearn)), size = smp_size)

X_train <- dlearn[train_ind, ]
X_test <- dlearn[-train_ind, ]
y_train <- X_train[,10]
y_test <- X_test[,10]


# a)
glm.modelo=glm(Class~.,family=binomial, data=X_train, maxit=100)
summary(glm.modelo)

AIC(glm.modelo)

# b)
chi2=glm.modelo$null.deviance - glm.modelo$deviance
ddl=glm.modelo$df.null-glm.modelo$df.residual
pvalor=pchisq(chi2,ddl,lower.tail=F)
pvalor

anova(glm.modelo)

# c)
res <- NULL
for(var in row.names(summary(glm.modelo)$coefficients)) {
  if(var != '(Intercept)'){
    if(summary(glm.modelo)$coefficients[var,4] < .05){
      res <- rbind(res,var)
    }
  }
}
formula <- as.formula(paste("Class~", paste(res, collapse="+")))
formula
reduced.model <- glm(formula,family=binomial, data=X_train, maxit=100)
summary(reduced.model)

# d)
forward.model <- stepAIC(glm.modelo, direction = "forward", trace = FALSE)
summary(forward.model)

# e)
stepwise.model <- stepAIC(glm.modelo, direction = "both", trace = FALSE)
summary(stepwise.model)

# f)
AIC(forward.model)-AIC(stepwise.model)
AIC(reduced.model)-AIC(stepwise.model)

# g)
forward.model.preds <- as.numeric(predict(forward.model, X_test[,-10], type='response') > .5)
table(forward.model.preds,y_test)
mean(forward.model.preds==y_test)

stepwise.model.preds <- as.numeric(predict(stepwise.model, X_test[,-10], type='response') > .5)
table(stepwise.model.preds,y_test)
mean(stepwise.model.preds==y_test)

reduced.model.preds <- as.numeric(predict(reduced.model, X_test[,-10], type='response') > .5)
table(reduced.model.preds,y_test)
mean(reduced.model.preds==y_test)


# f)
# Curvas ROC


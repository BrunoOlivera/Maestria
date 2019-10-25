# Ejercicio 2
set.seed(123)

dlearn=read.csv('breast-cancer-wisconsin.data',sep=',',header = FALSE)
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
empty.model <- glm(Class~1,family = binomial, data=X_train)
forward.model <- stepAIC(empty.model, scope=formula(glm.modelo), direction="forward", trace = FALSE)
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
yhat_modelo=predict(glm.modelo,X_test[,-10],type='response')
yhat_reducido=predict(reduced.model,X_test[,-10],type='response')
yhat_forw=predict(forward.model,X_test[,-10],type='response')
yhat_both=predict(stepwise.model,X_test[,-10],type='response')

rocplot =function (pred , truth , C,...){
  predob = prediction (pred , truth)
  perf = performance (predob , "tpr", "fpr")
  return(perf)}

AUC_ROC =function (pred , truth , ...){
  predob = prediction (pred , truth)
  Area = performance (predob , "auc")
  return(Area@y.values)}

plot(rocplot(yhat_modelo,y_test),col="#00AFBB",main="Curva ROC")
par(new=TRUE)
plot(rocplot(yhat_reducido,y_test),col="#6BB82E",main="Curva ROC")
par(new=TRUE)
plot(rocplot(yhat_forw,y_test),col="#B30417",main="Curva ROC")
par(new=TRUE)
plot(rocplot(yhat_both,y_test),col="#325b82",main="Curva ROC")
par(new=TRUE)
lines(c(seq(0,1,0.01)), c(seq(0,1,0.01)), col = "#FC4E07", type="l", lty=2)

AUC_ROC_mod = AUC_ROC(yhat_modelo,y_test)
AUC_ROC_res = AUC_ROC(yhat_reducido,y_test)
AUC_ROC_fwd = AUC_ROC(yhat_forw,y_test)
AUC_ROC_bot = AUC_ROC(yhat_both,y_test)

legend(0.5, 0.4, legend=c(paste("M.Completo - AUC = ", AUC_ROC_mod) , paste("M.Reducido - AUC = ", AUC_ROC_res),
                          paste("M.Forward - AUC = ", AUC_ROC_fwd),paste("M.Both - AUC = ", AUC_ROC_bot)),
       col=c("#00AFBB", "#6BB82E", "#B30417", "#325b82"), lty=c(1,1), cex=0.9,
       title="Comparación ROC Modelos", text.font=4, bg='lightblue')

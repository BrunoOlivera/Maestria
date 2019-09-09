# ej. 6

# a)
library(rpart)
library(partykit)
library(kernlab)

data(spam)

## 2/3 de la muestra
smp_size <- floor(2/3 * nrow(spam))

set.seed(2019)
train_ind <- sample(seq_len(nrow(spam)), size = smp_size)

train <- spam[train_ind, ]
test <- spam[-train_ind, ]

T=rpart(type~.,data=train)
plot(T,margin=0.04,main="T")
text(T)
rparty.tree = as.party(T)    
plot(rparty.tree)

DS=rpart(type~.,data=train,maxdepth=1,main="DS")
plot(DS,margin=0.1)
text(DS)

T$frame

# análisis propio o una descripción

# b)

#   1)
# 14 0.00329489     18   0.18946 0.22570 0.013012
# 15 0.00247117     24   0.16969 0.22570 0.013012
#With crossvalidation
CP_cross_validation = T$cptable[which.min(T$cptable[,"xerror"]),"CP"]

T1=rpart(type~.,data=train,cp=CP_cross_validation)
plot(T1,margin=0.1,main="T1")
text(T1)

#   2)
# umbral=0.22570+0.013012=0.238712

# 12 0.00453048     12   0.21499 0.23558 0.013265

#with 1-SErule
xerror=T$cptable[,4]
xstd <- T$cptable[, 5]

t.opt <- min(seq(along = xerror)[xerror <= min(xerror) + xstd[which.min(xerror)]])
CP_1_SE=T$cptable[t.opt,1]
CP_1_SE

T2=rpart(type~.,data=train,cp=CP_1_SE)
plot(T2,margin=0.1,main="T2")
text(T2)

#   3)

Tmax=rpart(type~.,data=train,cp=0)
plot(Tmax,margin=0.1,main="Tmax")
text(Tmax)
# 
# xerror=Tmax$cptable[,4]
# xstd <- Tmax$cptable[, 5]
# Tmax$cptable
# t.opt <- min(seq(along = xerror)[xerror <= min(xerror) + xstd[which.min(xerror)]])
# CP_1_SE=Tmax$cptable[t.opt,1]
# CP_1_SE


# c)
library(ipred)
library(randomForest)

bag=bagging(type~.,data=train,cp=0.01,coob=TRUE)
bag_pred=predict(bag,type="class",test)
bag_error=sum(bag_pred!=test[,58])/dim(test)[1]
bag_error

rf=randomForest(type~.,data=train,cp=0.01,coob=TRUE)
rf_pred=predict(rf,type="class",test)
rf_error=sum(rf_pred!=test[,58])/dim(test)[1]
rf_error

T_pred=predict(T,type="class",test)
T_error=sum(T_pred!=test[,58])/dim(test)[1]
T_error

# d)
rf_trace=randomForest(type~.,data=train,cp=0.01,coob=TRUE,do.trace=TRUE)
# prop.table(table(train$type)) %*% c(.0275, .0791)
# graficar si se puede
plot(100*rf_trace$err.rate[,1],type="l",ylab="OOB(%)",xlab="ntree",lwd=2,col="blue")


# e)
dim(importance(rf)[order(importance(rf),decreasing=TRUE),])
barplot(importance(rf)[order(importance(rf),decreasing=TRUE),],las=2)
a=importance(rf)
a[order(a[,1],decreasing=TRUE)]

# f)
rf_stump=randomForest(type~.,data=train,cp=0.01,maxdep=1,coob=TRUE)
importance(rf_stump)

# g)
importance=NULL
oob=NULL
for(k in 1:25){
  rf=randomForest(type~.,data=train,coob=TRUE,mtry=k)
  importance=cbind(importance,rf$importance)
  oob=cbind(oob,rf$err.rate[500,1])
}
# 52,53 High - 26 46 Medium - 38 47 Low
# 52,53 High - 26 46 Medium - 38 47 Low
matplot(t(importance), type = "l")

plot(1:25,oob,type="l")


# h)
library(e1071)

error.table = NULL

for(k in 1:50){
  set.seed(2019+k)
  # separamos datos en train/test
  smp_size = floor(2/3 * nrow(spam))
  train_ind = sample(seq_len(nrow(spam)), size = smp_size)
  train =  spam[train_ind,]
  test  = spam[-train_ind,]
  
  # entrenamos modelos
  model.cart =        rpart(type~.,data=train)
  model.bag  =      bagging(type~.,data=train)
  model.rf   = randomForest(type~.,data=train)
  model.svm  =          svm(type~.,data=train)
  
  # obtenemos predicciones
  pred.cart = predict(model.cart, type="class", test)
  pred.bag  = predict(model.bag,  type="class", test)
  pred.rf   = predict(model.rf,   type="class", test)
  pred.svm  = predict(model.svm,  type="class", test)
  
  # computamos los errores
  error.cart = sum(pred.cart != test[,58]) / dim(test)[1]
  error.bag  = sum(pred.bag  != test[,58]) / dim(test)[1]
  error.rf   = sum(pred.rf   != test[,58]) / dim(test)[1]
  error.svm  = sum(pred.svm  != test[,58]) / dim(test)[1]
  
  # los agregamos a la tabla
  error.table = rbind(error.table,c(error.cart,error.bag,error.rf,error.svm))
  
}

error.table

# calculamos la media de los errores
error.means=colMeans(error.table); error.means

# nos quedamos con el mejor y entrenamos sobre todo el data set
index = which.min(error.means)

ifelse(index==1,
       model.best<-rpart(type~.,data=spam),
       ifelse(index==2,
              model.best<-bagging(type~.,data=spam),
              ifelse(index==3,
                     model.best<-randomForest(type~.,data=spam),
                     model.best<-svm(type~.,data=spam))))

model.best=ifelse(index==1,
       rpart(type~.,data=spam),
       ifelse(index==2,
              bagging(type~.,data=spam),
              ifelse(index==3,
                     randomForest(type~.,data=spam),
                     svm(type~.,data=spam))))
model.best


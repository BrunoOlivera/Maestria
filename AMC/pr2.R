# 1)
n <- 1000

x1 <- sort(runif(n))
x2 <- sort(runif(n))
x3 <- sort(runif(n))
y <- 3 + 2*x1 -2*x2 + x3 + rnorm(n, sd = 1)

# a)
data <- cbind(x1,x2,x3,y)

# b)
x0 <- rep(1, n)
Xdata <- cbind(x1,x2,x3)
X <- cbind(x0,Xdata)
B <- solve((t(X)%*%X))%*%t(X)%*%y
model <- lm(y ~ Xdata)
# βb0 − tn−2(α/2)s.e(βb0), βb0 + tn−2(α/2)s.e(βb0)
# 
# # c)
# # tau 1
# x2_t1 <- x1 + rnorm(n, mean=0, sd=0)
# Xdata_t1 <- cbind(x1,x2_t1,x3)
# X_t1 <- cbind(x0,Xdata_t1)
# B_t1 <- solve((t(X_t1)%*%X_t1))%*%t(X_t1)%*%y; B_t1
# model_t1 <- lm(y ~ Xdata_t1)
# var_model_t1 = (sum((model_t1$residuals)**2)/(n-4))*solve((t(X_t1)%*%X_t1))
# var_model_t1
# 
# # tau 2
# x2_t2 <- x1 + rnorm(n, 0, .01)
# Xdata_t2 <- cbind(x1,x2_t2,x3)
# X_t2 <- cbind(x0,Xdata_t2)
# B_t2 <- solve((t(X_t2)%*%X_t2))%*%t(X_t2)%*%y; B_t2
# model_t2 <- lm(y ~ Xdata_t2)
# var_model_t2 = (sum((model_t2$residuals)**2)/(n-4))*solve((t(X_t2)%*%X_t2))
# var_model_t2
# 
# # tau 3
# x2_t3 <- x1 + rnorm(n, 0, .1)
# Xdata_t3 <- cbind(x1,x2_t3,x3)
# X_t3 <- cbind(x0,Xdata_t3)
# B_t3 <- solve((t(X_t3)%*%X_t3))%*%t(X_t3)%*%y; B_t3
# model_t3 <- lm(y ~ Xdata_t3)
# var_model_t3 = (sum((model_t3$residuals)**2)/(n-4))*solve((t(X_t3)%*%X_t3))
# var_model_t3
# 
# # tau 4
# x2_t4 <- x1 + rnorm(n, 0, 1)
# Xdata_t4 <- cbind(x1,x2_t4,x3)
# X_t4 <- cbind(x0,Xdata_t4)
# B_t4 <- solve((t(X_t4)%*%X_t4))%*%t(X_t4)%*%y; B_t4
# model_t4 <- lm(y ~ Xdata_t4)
# summary_t4 <- summary(model_t4); summary_t4
# 
# # calculamos la variación de los Betas mediante el cuadrado
# # del Std. Error devuelto por el modelo
# var_B0_t4_1 = (summary_t4$coefficients[,2]**2)[1]
# var_B1_t4_1 = (summary_t4$coefficients[,2]**2)[2]
# var_B2_t4_1 = (summary_t4$coefficients[,2]**2)[3]
# var_B3_t4_1 = (summary_t4$coefficients[,2]**2)[4]
# 
# # aproximamos la variación del modelo y calculamos la matriz σ2(X'X)−1
# var_model_t4 = (sum((model_t4$residuals)**2)/(n-4))*solve((t(X_t4)%*%X_t4))
# 
# # calculamos la variación de los beta como la diagonal de la matriz anterior 
# var_B0_t4_2 = var_model_t4[1,1]
# var_B1_t4_2 = var_model_t4[2,2]
# var_B2_t4_2 = var_model_t4[3,3]
# var_B3_t4_2 = var_model_t4[4,4]
# 
# 
# 
# 
# # tau 5
# x2_t5 <- x1 + rnorm(n, 0, 0)
# Xdata_t5 <- cbind(x1,x2_t5,x3)
# X_t5 <- cbind(x0,Xdata_t5)
# B_t5 <- solve((t(X_t5)%*%X_t5))%*%t(X_t5)%*%y; B_t5
# model_t5 <- lm(y ~ Xdata_t5)
# summary_t5 <- summary(model_t5); summary_t5
# 
# # calculamos la variación de los Betas mediante el cuadrado
# # del Std. Error devuelto por el modelo
# var_B0_t5_1 = (summary_t5$coefficients[,2]**2)[1];var_B0_t5_1
# var_B1_t5_1 = (summary_t5$coefficients[,2]**2)[2];var_B1_t5_1
# var_B2_t5_1 = (summary_t5$coefficients[,2]**2)[3];var_B2_t5_1
# var_B3_t5_1 = (summary_t5$coefficients[,2]**2)[4];var_B3_t5_1
# 
# # aproximamos la variación del modelo y calculamos la matriz σ2(X'X)−1
# var_model_t5 = (sum((model_t5$residuals)**2)/(n-4))*solve((t(X_t5)%*%X_t5))
# 
# # calculamos la variación de los beta como la diagonal de la matriz anterior 
# var_B0_t5_2 = var_model_t5[1,1];var_B0_t5_2
# var_B1_t5_2 = var_model_t5[2,2];var_B1_t5_2
# var_B2_t5_2 = var_model_t5[3,3];var_B2_t5_2
# var_B3_t5_2 = var_model_t5[4,4];var_B3_t5_2


# función para clacular el vif
vif_calc<-function(Xmat){
  VIF<-numeric()
  for(i in 1:ncol(Xmat)){
    Xmat_Y<-Xmat[,i]
    dataMAT<-cbind(Xmat_Y, Xmat[,-i])
    R2<-summary(lm(Xmat_Y~.,data=dataMAT, na.action="na.exclude"))$r.squared
    VIF[i]<-1/(1-R2)
  }
  names(VIF)<-colnames(Xmat)
  return(VIF)
}


iterations = 5
variables = 8

results <- matrix(ncol=variables, nrow=iterations)
VIFs <- NULL
B_estimates <- NULL

tau <- c(0, .01, .1, 1, 10)
# for(val in tau){
for(i in 1:iterations){
  x2_new <- x1 + rnorm(n, mean=0, sd=tau[i])
  Xdata_new <- cbind(x1,x2_new,x3)
  X_new <- cbind(x0,Xdata_new)
  if(tau[i] != 0) {
    B_estimates <- solve((t(X_new)%*%X_new))%*%t(X_new)%*%y
  }
  model_new <- lm(y ~ Xdata_new)
  summary_model <- summary(model_new)
  
  # para el caso de tau = 0 tenemos que sacar los estimadores del
  # modelo porque la matriz (X'X) no es invertible
  if(tau[i] == 0) {
    B_estimates[1] <- summary_model$coefficients[,1][1]
    B_estimates[2] <- summary_model$coefficients[,1][2]
    B_estimates[3] <- NA
    B_estimates[4] <- summary_model$coefficients[,1][3]
  }
  
  # calculamos la variación de los Betas mediante el cuadrado
  # del Std. Error devuelto por el modelo
  var_B0_1 = (summary_model$coefficients[,2]**2)[1]
  var_B1_1 = (summary_model$coefficients[,2]**2)[2]
  if(tau[i] != 0){
    var_B2_1 = (summary_model$coefficients[,2]**2)[3]
    var_B3_1 = (summary_model$coefficients[,2]**2)[4]
  }else{
    var_B2_1 = NA
    var_B3_1 = (summary_model$coefficients[,2]**2)[3]
  }
  
  if(tau[i] != 0) {
    # aproximamos la variación del modelo y calculamos la matriz σ2(X'X)−1
    var_model_new = (sum((model_new$residuals)**2)/(n-4))*solve((t(X_new)%*%X_new))
    
    # calculamos la variación de los beta como la diagonal de la matriz anterior 
    var_B0_2 = var_model_new[1,1]
    var_B1_2 = var_model_new[2,2]
    var_B2_2 = var_model_new[3,3]
    var_B3_2 = var_model_new[4,4]
    
    # controlamos que sean iguales ambas formas de calcular las varianzas
    assertthat::are_equal(var_B0_1,var_B0_2)
    assertthat::are_equal(var_B2_1,var_B1_2)
    assertthat::are_equal(var_B3_1,var_B2_2)
    assertthat::are_equal(var_B3_1,var_B3_2)
  }
  
  newRow.data <- c(B_estimates[1],
                   B_estimates[2],
                   B_estimates[3],
                   B_estimates[4],
                   var_B0_1,
                   var_B1_1,
                   var_B2_1,
                   var_B3_1)
  
  results[i,] = newRow.data
  
  # calculamos el VIF y lo guardamos para la parte d)
  VIFs <- rbind(VIFs,vif_calc(data.frame(Xdata_new)))
}

row.names(results) <- c('tau-0','tau-.01','tau-.1','tau-1','tau-10')
colnames(results) <- c('Beta_0','Beta_1','Beta_2','Beta_3','Var_Beta_0','Var_Beta_1','Var_Beta_2','Var_Beta_3')

# resultados
results
VIFs

# d)


# # calculamos el VIF para cada caso
# a <- vif_calc(data.frame(Xdata_t1))
# vif_calc(data.frame(Xdata_t2))
# vif_calc(data.frame(Xdata_t3))
# vif_calc(data.frame(Xdata_t4))
# vif_calc(data.frame(Xdata_t5))

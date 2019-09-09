#rpart <--- este
#tree
library(rpart)
attach(airquality)

arbol_1=rpart(Ozone~.,data = airquality)
arbol_2=rpart(Ozone~Wind+Solar.R,data = airquality)

plot(arbol_1)
text(arbol_1)


# busco mejor variable, mejor umbral, 
# tal que los hijos sean más puros(conjuntamente) 
# que el padre(con respecto a la etiqueta)


# devueve el arbol con alfa=0.01
#-------------------------------------------------------#
#------------------------ Ej. 3 ------------------------#
#-------------------------------------------------------#
# a)

# creo matriz 4x5 con 1 en todas las entradas
matrix(1,4,5)

# b)

# creo matriz 4x5 con 1 a 20 en las entradas
matrix(1:20,4,5)

# c)

# creo vector v con 20 numeros cualesquiera
v = floor(10*runif(20)); v
# creo matriz 5x4 por filas con los valores de v
matrix(v,5,4,byrow=T)
# creo matriz 2x10 por columnas con los valores de v
matrix(v,2,10)

# d)

# creo matriz A 2x3
A = matrix(1:6,2,3); A
# creo matriz B 3x4 conformable con A
B = matrix(1:12,3,4); B
# hago el producto matricial entre A y B
A%*%B
# hago el producto de A y B elemento a elemento. debería
# fallar porque las matrices tienen distintas dimensiones
A*B
# hago el producto de A con sí misma elemento a elemento
A*A
# hago el producto matricial de A con sí misma.
# debería fallar porque A no es cuadrada
A%*%A

# e)

# creo matriz C 2x2 con los números 2,3,4,7
C = matrix(c(2,3,4,7),2); C
# creo matriz D con las entradas de C multiplicadas por 4
D = 4*C; D
# concateno C y D por filas
rbind(C,D)
# concateno C y D por columnas
cbind(C,D)


#-------------------------------------------------------#
#------------------------ Ej. 5 ------------------------#
#-------------------------------------------------------#

# creo vector v
v = c(4,5,9,2,1,4); v

# sustituyo entradas impares por 1's
v[v%%2!=0] = 1; v


#-------------------------------------------------------#
#------------------------ Ej. 8 ------------------------#
#-------------------------------------------------------#

# creo archivo ej8.txt con una tabla 6x5 con 4 columnas numéricas y una
# una categórica con texto. le doy nombre a las columnas.
tabla = data.frame(matrix(floor(runif(24,1,10)),6,4))
colnames(tabla) = c("Feature_1","Feature_2","Feature_3","Feature_4")
tabla$Label = c("class_1","class_2","class_2","class_3","class_1","class_3")
write.table(tabla,"ej8.txt",col.names=T)

# a)

# importo ej8.txt en un data frame
data = read.table("ej8.txt",header=T); data
# verifico el nombre de las filas
rownames(data)
# verifico el nombre de las columnas
colnames(data)
# verifico las dimensiones
dim(data)

# b)

# doy un nombre a las filas
rownames(data) = c("Sample_1","Sample_2","Sample_3","Sample_4","Sample_5","Sample_6"); data

# c)

# agrego columna binaria que indica si el valor de la columna 2 es mayor que 6
data=cbind(data,"(Feature_2 > 6)"=as.numeric(data[,2]>6)); data

# d)

# exporto el data frame a un nuevo archivo txt sin escribir
# los nombres de las filas ni de las columnas
write.table(data,"ej8-d).txt",row.names=F,col.names=F)
# vuelvo a importar el data frame
data2 = read.table("ej8-d).txt"); data2

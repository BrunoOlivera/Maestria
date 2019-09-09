attach(iris)
x=iris[,1:4]
colMeans(x)
apply(x,2,var);apply(x, 2, sd)
S=cov(x)
R=cor(x)
eigen(S)

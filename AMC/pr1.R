t1e3 <- read_csv("~/Desktop/AMC/t1e3.csv") # cambiar ruta

# a
x_a_d <- unlist(t1e3['x(a-d)'], use.names = FALSE)
y_a   <- unlist(t1e3['y(a)'], use.names = FALSE)

b1_a <- cov(x_a_d, y_a) / var(x_a_d)
b0_a <- mean(y_a) - b1_a*mean(x_a_d)

plot(y_a ~ x_a_d)
abline(b0_a, b1_a, lwd = 3, col = 'green')

# b
y_b <- unlist(t1e3['y(b)'], use.names = FALSE)

b1_b <- cov(x_a_d, y_b) / var(x_a_d)
b0_b <- mean(y_b) - b1_b*mean(x_a_d)

plot(y_b ~ x_a_d)
abline(b0_b, b1_b, lwd = 3, col = 'orange')

# c
y_c <- unlist(t1e3['y(c)'], use.names = FALSE)

b1_c <- cov(x_a_d, y_c) / var(x_a_d)
b0_c <- mean(y_c) - b1_c*mean(x_a_d)

plot(y_c ~ x_a_d)
abline(b0_c, b1_c, lwd = 3, col = 'red')

# d
y_d <- unlist(t1e3['y(d)'], use.names = FALSE)

b1_d <- cov(x_a_d, y_d) / var(x_a_d)
b0_d <- mean(y_d) - b1_d*mean(x_a_d)

plot(y_d ~ x_a_d)
abline(b0_d, b1_d, lwd = 3, col = 'blue')

# e
x_e <- unlist(t1e3['x(e)'], use.names = FALSE)
y_e <- unlist(t1e3['y(e)'], use.names = FALSE)

b1_e <- cov(x_e, y_e) / var(x_e)
b0_e <- mean(y_e) - b1_e*mean(x_e)

plot(y_e ~ x_e)
abline(b0_e, b1_e, lwd = 3, col = 'black')

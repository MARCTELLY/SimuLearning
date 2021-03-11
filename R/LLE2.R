library(plot3D)
set.seed(200)

p <- runif(1000)*(2*pi-0.55)
t <- runif(1000)*pi

indices <- ((t < (pi - (pi / 8))) & (t > ((pi / 8))))
colors <- p[indices]

x <- sin(t[indices]) * cos(p[indices])
y <- sin(t[indices]) * sin(p[indices])
z <- cos(t[indices])

scatter3D(x, y, z, phi=10, pch = 20,colvar=colors)

matrixData <- cbind(x,y,z)
x0 <- seq(0, 1, length.out = 512)
X <- polym(x0, degree = 3)
data <- matrixData %*% t(X)

library(lle)
calc_k(data, 3) # k = 7
data.lle <-  lle(data, m = 3, k = 20)
plot(data.lle$Y, col=colors)

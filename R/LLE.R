n <- 1000  # number of points
dh <- data.frame(id <- 1:n, phi = NA, x = NA, y = NA, z = NA)
dh$phi <- seq(0, 3*pi, length.out = n)
dh$phi[seq(1, n, 2)] <- dh$phi[seq(1, n, 2)] - pi


dh$x <- jitter(sin(dh$phi), 100)+rnorm(100, 0, 0.08)
dh$y <- jitter(cos(dh$phi), 100)+rnorm(100, 0, 0.08)
dh$z <- seq(0,2, length.out = n)

library(plot3D)
scatter3D(dh$x, dh$y, dh$z, phi = 50, theta = 10)

library(lle)
calc_k(data, 3) # k = 7
data.lle <-  lle(data, m = 3, k = 20)
plot(data.lle$Y, col=colors)

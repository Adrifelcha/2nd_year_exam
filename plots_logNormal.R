library(gamlss.dist)

layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))

mu <- 3
sigma <- 0.8
n <- 5000
x <- rnorm(n,mu,sigma)

x <- seq(-10,20,0.01)
plot(x,dnorm(x,mu,sigma),type="l", xlim=c(0,6), 
     ann=F, axes=F, col="forestgreen", lwd=4)
axis(1,seq(0,20,0.5))
text(3, 3, col="darkgreen", cex=1.2,
     expression(paste("Y ~ Exponential(",tau,")")))

plot(x,dLOGNO(x,mu,sigma),type="l", xlim=c(0,6), 
     ann=F, axes=F, col="forestgreen", lwd=4)
axis(1,seq(0,20,0.5))
text(3, 3, col="darkgreen", cex=1.2,
     expression(paste("Y ~ Exponential(",tau,")")))

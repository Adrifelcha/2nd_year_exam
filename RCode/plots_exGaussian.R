library(gamlss.dist)

layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))

tau <- 4
mu <- 3
sigma <- 0.8

x <- seq(-10,20,0.01)
plot(x,dexp(x,tau),type="l", xlim=c(0,6), 
     ann=F, axes=F, col="forestgreen", lwd=4)
axis(1,seq(0,20,0.5))
text(3, 3, col="darkgreen", cex=1.2,
     expression(paste("Y ~ Exponential(",tau,")")))
plot(x,dnorm(x,mu,sigma),type="l", xlim = c(0,6), 
     ann=F, axes=F, col="purple2",lwd=4)
axis(1,seq(0,20,0.5))
text(1, 0.4, col="purple4", cex=1.2,
     expression(paste("X ~ Normal(",mu,",",sigma,")")))
plot(x,dexGAUS(x,mu,sigma,tau),type="l", xlim = c(0,20), 
     ann=F, axes=F, col="orange2", lwd=4)
axis(1,seq(0,20,0.5))
text(12, 0.12, col="darkorange3", cex=1.2,
     expression(paste("Y+X ~ exGaussian(",mu,",",sigma,",",tau,")")))

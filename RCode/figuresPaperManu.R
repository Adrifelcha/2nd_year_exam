set.seed(123)
library(grid)
library(shape)

trials = 100
mu1 = 2.5
mu2 = 2.75 
boundary= 3

#randomWalk = cddm.randomWalk(trials,mu1,mu2,boundary)
state  <- randomWalk$state
finalT <- randomWalk$RT
trials <- length(finalT)
choices <- cddm.getFinalState(state)
polar <- rectToPolar(choices[1,1],choices[1,2])
boundary <- round(polar[,"dLength"],2)

circle <- polarToRect(all.Angles,boundary)
polar.coordinates = rectToPolar(mu1,mu2)
draw.angle = polarToRect(seq(0,as.numeric(polar.coordinates[1]),0.01), 1)

drift.line.up <- polarToRect(polar.coordinates[1],boundary+0.2)
drift.line.down <- polarToRect(polar.coordinates[1],-(boundary+0.5))

par(pty="s")
par(mar = c(0, 0, 0, 0)) # Set the margin on all sides to 2
pm <- boundary+0.2 #Plot margin
plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE,
     xlim=c(-pm,pm),ylim=c(-pm,pm))
z = seq(1,sum(!is.na(state[,,1])),length.out=300)
abline(a=0, b=mu2/mu1, lwd=2, col="navy")
points(state[z,,1], type = "l", col=rgb(0.2,0.7,0.6,0.5), lwd=2)
points(circle[,1],circle[,2], type="l")
points(draw.angle[,1],draw.angle[,2], type="l", col="navy", lwd=3)
text(1.4,0.35,expression(theta), cex=1.4, col="navy", f=2)
text(1.45,0.14,"Drift angle", cex=0.9, col="navy", f=2)
text(1.15,1.6,expression(delta), cex=1.4, col="navy", f=2)
text(1.15,1.8,"Drift length", cex=0.9, col="navy", f=2)
lines(c(0,tail(draw.angle,1)[1]),c(tail(draw.angle,1)[2],tail(draw.angle,1)[2]),
      lty=2, col="dodgerblue4")
lines(c(tail(draw.angle,1)[1],tail(draw.angle,1)[1]),c(0,tail(draw.angle,1)[2]),
      lty=2, col="dodgerblue4")
text(0.35,0.85,expression(paste(mu,1)), cex=1.1, col="dodgerblue4", f=2)
text(0.35,1.18,"Step size", cex=0.9, col="dodgerblue4", f=2)
text(0.35,1.06,"on X", cex=0.9, col="dodgerblue4", f=2)
text(0.55,0.15,expression(paste(mu,2)), cex=1.1, col="dodgerblue4", f=2)
text(0.55,-0.1,"Step size", cex=0.9, col="dodgerblue4", f=2)
text(0.55,-0.22,"on Y", cex=0.9, col="dodgerblue4", f=2)
text(choices[1,1]+0.12,choices[1,2]+0.34,"Response", 
     cex=0.9, col=rgb(0.2,0.5,0.5,1), f=2)
text(choices[1,1]+0.12,choices[1,2]+0.23,"observed", 
     cex=0.9, col=rgb(0.2,0.5,0.5,1), f=2)
abline(h = 0, lty=2, col="gray50")
abline(v = 0, lty=2, col="gray50")
points(choices[1,1],choices[1,2], type = "p", pch =16, cex=1.2,
         col=rgb(0.2,0.5,0.5,1))
text(-1.7,-1,"Drift vector", cex=0.9, f=2, col="navy")
text(-1.85,-1.2,expression(paste("{ ",theta," , ",delta," }")), cex=1, f=2, col="navy")
text(-1.745,-1.39,expression(paste("{ ",mu,"1 , ",mu,"2 }")), cex=1, f=2, col="dodgerblue4")
text(3.35,0.15,"0", cex=0.8, f=2, col="black")
text(3.25,-0.15,"2", cex=0.8, f=2, col="black")
text(3.35,-0.15,expression(pi), cex=1, f=1, col="black")
signal.boundary = polarToRect(3,boundary)
arrow.color = "skyblue4"
Arrows(-0.05, 0.05, -2.5, 1.5, code = 2, arr.length = 0.2, arr.width = 0.2, 
       arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
       col = arrow.color, lcol = arrow.color, lty = 2, 
       arr.col = arrow.color, lwd = 1, arr.lwd = 2)
Arrows(-2.5, 1.5, -0.05, 0.05, code = 2, arr.length = 0.2, arr.width = 0.2, 
       arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
       col = arrow.color, lcol = arrow.color, lty = 2, 
       arr.col = arrow.color, lwd = 1, arr.lwd = 2)
text(-1.5,1.1,expression(eta), cex=1.4, col=arrow.color, f=2)
text(-1.5,1.3,"Boundary", cex=0.9, col=arrow.color, f=2)



############################################################
#########
############################################################
par(pty="s")
layout.matrix <- matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2)
layout(mat = layout.matrix,
       heights = c(2, 2), # Heights of the two rows
       widths = c(3, 2)) # Widths of the two columns
#par(mfrow=c(1,2))
par(mar = c(0, 0, 2, 2)) # Set the margin on all sides to 2
pm <- boundary+0.1 #Plot margin
plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE,
     xlim=c(-pm,pm),ylim=c(-pm,pm))
z = seq(1,sum(!is.na(state[,,1])),length.out=300)
for(i in 1:trials){
    points(state[z,,i], type = "l", col=rgb(0.2,0.7,0.6,0.08), lwd=2)
    }
points(circle[,1],circle[,2], type="l")
draw.angle = polarToRect(seq(0,as.numeric(polar.coordinates[1]),0.01), 0.5)
points(draw.angle[,1],draw.angle[,2], type="l", col="navy", lwd=2)
text(0.6,0.25,expression(theta), cex=1.4, col="navy", f=2)
text(1.15,1.6,expression(delta), cex=1.4, col="navy", f=2)
abline(h = 0, lty=2, col="gray50")
abline(v = 0, lty=2, col="gray50")
for(i in 1:trials){
    points(choices[i,1],choices[i,2], type = "p", pch =16, cex=1.2,
           col=rgb(0.2,0.5,0.5,0.25))
    }
text(3.2,0.15,"0", cex=0.8, f=2, col="black")
text(3.1,-0.15,"2", cex=0.8, f=2, col="black")
text(3.2,-0.15,expression(pi), cex=1, f=1, col="black")
signal.boundary = polarToRect(3,boundary)
arrow.color = "skyblue4"
Arrows(-0.05, 0.05, -2.5, 1.5, code = 2, arr.length = 0.2, arr.width = 0.2, 
       arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
       col = arrow.color, lcol = arrow.color, lty = 2, 
       arr.col = arrow.color, lwd = 1, arr.lwd = 2)
Arrows(-2.5, 1.5, -0.05, 0.05, code = 2, arr.length = 0.2, arr.width = 0.2, 
       arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
       col = arrow.color, lcol = arrow.color, lty = 2, 
       arr.col = arrow.color, lwd = 1, arr.lwd = 2)
text(-2.7,1.6,expression(eta), cex=1.4, col=arrow.color, f=2)
lines(c(drift.line.down[1],drift.line.up[2]), 
      c(drift.line.down[1],drift.line.up[2]), lwd=2, col="navy")

hist(rnorm(100,1,0.1))
hist(rnorm(100,1,0.1))

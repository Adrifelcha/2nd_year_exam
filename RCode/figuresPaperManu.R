set.seed(123)
library(grid)
library(shape)

trials = 500
boundary= 3
mu1 = 2.5
mu2 = 2.75 
polar.coordinates = rectToPolar(mu1,mu2)
dangle <- polar.coordinates[1]
dlength <- polar.coordinates[2]
draw.angle = polarToRect(seq(0,as.numeric(polar.coordinates[1]),0.01), 1)

if(!exists("randomWalk")){
   randomWalk = cddm.randomWalk(trials,mu1,mu2,boundary)
}

state  <- randomWalk$state
finalT <- randomWalk$RT
trials <- length(finalT)
choices <- cddm.getFinalState(state)
polar <- rectToPolar(choices[1,1],choices[1,2])
boundary <- round(polar[,"dLength"],2)

all.Angles <- seq(0,2*pi,0.001)
circle <- polarToRect(all.Angles,boundary)

X <- seq(-(boundary-0.75),boundary-0.75,0.01)
Y <-  (mu2/mu1)*X
tmp <- tail(draw.angle,1)[1]
X2 <- seq(0,as.numeric(tmp),0.01)
Y2 <- (mu2/mu1)*X2


#############################################################
# Figure 1: Showing parameters in CDDM
#############################################################
cex.text <- 1.25
cex.greek <- 1.7
f = 1
# Set up margins
########################################
par(pty="s")             # Square canvas 
par(mfrow=c(1,1),        # A single plot   
    mar = c(0, 0, 0, 0)) # outer margins
pm <- boundary+0.2  # xlim and ylim
########################################
# Draw base circle
########################################
plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE,
     xlim=c(-pm,pm),ylim=c(-pm,pm))      # Plotting space
points(circle[,1],circle[,2], type="l")  # Draw circle
abline(h = 0, lty=2, col="gray60")  # X axis
abline(v = 0, lty=2, col="gray60")  # Y axis
text(3.2,0.15,"0", cex=cex.text, f=1, col="black")                # Pi markers
text(3.25,-0.15,expression(2*pi), cex=cex.text+0.1, f=1, col="black")
text(-3.15,0.15,expression(pi), cex=cex.text+0.1, f=1, col="black")
text(-0.25,3.15,expression(pi/2), cex=cex.text+0.1, f=1, col="black")
text(-0.25,-3.15,expression(3*pi/2), cex=cex.text+0.1, f=1, col="black")
########################################
# Draw random walk
########################################
z = seq(1,sum(!is.na(state[,,1])), # Isolate state points from random walk
        length.out=55)            # make random walk more defined
points(state[z,,1], type = "l", col=rgb(0.2,0.7,0.6,0.5), lwd=2) # Draw r.w.
########################################
# Mark response observed
########################################
points(choices[1,1],choices[1,2], type = "p", pch =16, cex=1.2,
       col=rgb(0.2,0.5,0.5,1))
text(choices[1,1]+0.4,choices[1,2]+0.475,"Response", 
     cex=cex.text, col=rgb(0.2,0.5,0.5,1), f=f)
text(choices[1,1]+0.4,choices[1,2]+0.245,"observed", 
     cex=cex.text, col=rgb(0.2,0.5,0.5,1), f=f)
########################################
# Boundary radius
########################################
arrow.color = "skyblue4"
Arrows(-0.05, 0.05, -2.65, 1.3, code = 2, arr.length = 0.2, arr.width = 0.2, 
       arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
       col = arrow.color, lcol = arrow.color, lty = 2, 
       arr.col = arrow.color, lwd = 1, arr.lwd = 2)
Arrows(-2.65, 1.3, -0.05, 0.05, code = 2, arr.length = 0.2, arr.width = 0.2, 
       arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
       col = arrow.color, lcol = arrow.color, lty = 2, 
       arr.col = arrow.color, lwd = 1, arr.lwd = 2)
text(-2.1,0.8,expression(eta), cex=cex.greek, col=arrow.color, f=f)
text(-2.1,0.5,"Boundary", cex=cex.text, col=arrow.color, f=f)
text(-2.1,0.27,"radius", cex=cex.text, col=arrow.color, f=f)
########################################
# Drift vector
########################################
# Drift vector
lines(X,Y, lwd=2, col="navy", lty=2)
# text(-1.7,-0.65,"Drift vector", cex=1.2, f=2, col="navy")
# text(-1.78,-1.2,expression(paste("{ ",theta,"   ,  ",delta,"  }")), cex=1.3, f=2, col="navy")
# text(-1.78,-0.95,expression(paste("( ",mu,"1 , ",mu,"2 )")), cex=1.3, f=2, col="dodgerblue4")
# signal.boundary = polarToRect(3,boundary)
# Mu1
color <- "dodgerblue1"
lines(c(0,tail(draw.angle,1)[1]),c(tail(draw.angle,1)[2],tail(draw.angle,1)[2]),
      lty=4, col=color, lwd=2)
text(-0.21,0.8,expression(paste(mu)), cex=cex.greek, col=color, f=f)
text(-0.08,0.7,"1", cex=cex.greek-0.8, col=color, f=f)
text(-0.73,1.15,"Step size", cex=cex.text, col=color, f=f)
text(-0.65,0.92,"on X", cex=cex.text, col=color, f=f)
# Mu 2
lines(c(tail(draw.angle,1)[1],tail(draw.angle,1)[1]),c(0,tail(draw.angle,1)[2]),
      lty=4, col=color, lwd=2)
text(0.7,-0.15,expression(paste(mu)), cex=cex.greek, col=color, f=f)
text(0.85,-0.22,"2", cex=cex.greek-0.8, col=color, f=f)
text(0.7,-0.45,"Step size", cex=cex.text, col=color, f=f)
text(0.7,-0.68,"on Y", cex=cex.text, col=color, f=f)
# Drift length
color <- "navy"
lines(X2,Y2, lwd=3, col=color)
text(0.45,0.75,expression(delta), cex=cex.greek, col=color, f=f)
text(0.45,1.25,"Drift", cex=cex.text, col=color, f=f)
text(0.45,1.02,"length", cex=cex.text, col=color, f=f)
# Drift angle
points(draw.angle[,1],draw.angle[,2], type="l", col="navy", lwd=3)
text(1.1,0.4,expression(theta), cex=cex.greek, col="navy", f=f)
text(1.51,0.49,"Drift", cex=cex.text, col="navy", f=f)
text(1.54,0.26,"angle", cex=cex.text, col="navy", f=f)


#############################################################
# Figure 2: Variability over several trials
#############################################################
tmp <- tail(draw.angle,1)[1]
X2 <- seq(0,0.5,0.01)
Y2 <- (mu2/mu1)*X2
cex.text <- 1.25
cex.greek <- 2.7
f = 1
# Set up margins
########################################
par(pty="s")               # Square canvas 
par(mar = c(0, 0, 0, 0.5)) # Outer margins
layout.matrix <- matrix(c(1, 0, 2, 
                          1, 0, 0, 
                          1, 0, 3), 
                        nrow = 3, ncol = 3, byrow = TRUE)
layout(mat = layout.matrix,
       heights = c(2.5, -0.75, 4), # Heights of the two rows
       widths = c(5.5, 0.1, 2.5)) # Widths of the two columns
pm <- boundary #Plot margin
########################################
# Left panel: The circle
########################################
plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE,
     xlim=c(-pm,pm),ylim=c(-pm,pm))
points(circle[,1],circle[,2], type="l")
show.trials <- 150
for(i in 1:show.trials){
    z = seq(1,sum(!is.na(state[,,i])),length.out=250)
    points(state[z,,i], type = "l", col=rgb(0.2,0.7,0.6,0.09), lwd=2)
    }
draw.angle = polarToRect(seq(0,as.numeric(polar.coordinates[1]),0.01), 0.75)
points(draw.angle[,1],draw.angle[,2], type="l", col="navy", lwd=2)
text(0.95,0.4,expression(theta), cex=cex.greek, col="navy", f=2)
text(0.18,0.6,expression(delta), cex=cex.greek, col="navy", f=2)
abline(h = 0, lty=2, col="gray50")
abline(v = 0, lty=2, col="gray50")
for(i in 1:show.trials){
  points(choices[i,1],choices[i,2], type = "p", pch =16, cex=2,
         col=rgb(0.2,0.5,0.5,0.2))
}
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
text(-2.8,1.7,expression(eta), cex=cex.greek, col=arrow.color, f=2)
lines(X,Y, lwd=2, col="navy", lty=3)
lines(X2,Y2, lwd=2, col="navy")
########################################
# Top right: Responses
########################################
polar <- rectToPolar(choices[,1],choices[,2])
hist(polar[,1], xlim=c(0,2*pi), ann=F, axes=F, col=rgb(0.2,0.5,0.5,0.8))
axis(1,c(0,pi,2*pi),c(0,expression(pi),expression(paste(2,pi))), 
     cex.axis=cex.text+0.5)
mtext("Responses", 1, f=1, line=2.9, cex = cex.text)
########################################
# Bottom right: Response Times
########################################
hist(finalT, breaks = 15, ann=F, axes=F, xlim=c(0.25,2.5), col="#5458DA")
axis(1,seq(0.5,2.5,0.5),seq(0.5,2.5,0.5), cex.axis=cex.text+0.5)
mtext("Response times", 1, f=1, line=2.9, cex = cex.text)
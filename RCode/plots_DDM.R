# Variable dictionary: ##################################################
# mu1 and mu2 - Individual drift rates for the motion on the x and y axes
# boundary - Boundary (radius)
# ndt - Non decision time
# drift.Coeff - Within-trial variability on the sampling process
# dt - Step size ("delta-t")
# state - rectangular coordinates recorded during the random walk
#########################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulate the full random walk across many trials (for each trial, 
# keeps the full chain of coordinates visited and response times)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
trials <- 200
mu1 <- 0.7
boundary <- 2
ndt <- 0.2

randomWalk <- function(trials, mu1, boundary, ndt=0.1, drift.Coeff=1, dt=0.00015){
  boundary <- boundary/2
  sqDT <- sqrt(dt)
  s.init <- c(0,0) 
  iter <- round(15/dt)  # Maximum number of iterations on the random walk 
  state <- array(NA, dim = c(iter, trials))   # States are saved in a 3dimensional array
  finalT <- rep(NA,trials) # Empty vector to store RT (a.k.a. total number of iterations)
  additional_steps_needed <- rep(0,trials)
  
  # Arrays to be used in simulation
  random_deviations <- rnorm(trials*iter,0,1)*(drift.Coeff*sqDT)   # Deviations from step sizes 
  motion <- array(random_deviations,dim = c(iter,trials))          # Store deviations in array
  steps <- motion+(mu1*dt)
  
  # Set initial state for every trial
  state[1,] <- s.init # Set initial point for every random-walk on each trial
  
  for(a in 1:trials){   
    ### Random walk per trial
    for(t in 2:iter){
      d1 <- steps[t,a]
      state[t,a] <- state[t-1,a]+d1
      pass <- abs(state[t,a])
      # Stop random-walk if boundary is passed
      if(pass >= boundary){
        finalT[a] <- t+(ndt/dt)   #Total no. of iterations required on each trial
        break
      }
    }
    
    # Test whether the random-walk reached the boundary, and re-sample if not.
    not.finished <- is.na(finalT[a])
    if(not.finished){ additional_steps_needed[a] <- 1 }
    
    whileLoopNo <- 1
    while(not.finished){
      last_state <- state[t,a]   # Store last state
      state[,a] <- NA            # Reset random-walk
      state[1,a] <- last_state   # Start at last state
      
      # Get a new list of random step sizes
      more_random_deviations <- rnorm(iter,0,1)*(drift.Coeff*sqDT)
      more_motion <- array(more_random_deviations,dim = c(iter,1))
      more_steps_d1 <- more_motion[,1]+(mu1*dt)
      
      for(t in 2:iter){
        d1 <- more_steps_d1[t]
        state[t,a] <- state[t-1,a]+d1
        pass <- abs(state[t,a])
        
        if(pass >= boundary){
          added_iterations <- iter*whileLoopNo
          finalT[a] <- (t+added_iterations)+(ndt/dt)   #Total no. of iterations required on each trial
          break
        }
      }
      
      not.finished <- is.na(finalT[a])  # Re-evaluate
      whileLoopNo <- whileLoopNo + 1    # Register while loop iteration
    }
    
    state[t,a] <- round(state[t,a],0)
  }
  
  finalT <- finalT*dt
  output <- list(state,finalT,additional_steps_needed)
  names(output) <- c("state","RT","repeated.Walk")
  return(output)
}

X <- randomWalk(trials, mu1, boundary, ndt)

par(mfrow=c(2,2))
par(mar = c(2.5, 3.5, 2, 2.5))

keep <- seq(1,nrow(X$state),3)
par(mfrow=c(1,1), mar = c(2.5, 3.5, 2, 2.5))
plot(X$state[!is.na(X$state[,1]),1], type="l", ylim=c(-1,1), ann=F, axes=F)
lines(c(0,90000),c(-1,-1))
lines(c(0,90000),c(1,1))
lines(c(0,0),c(-1,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Take full random walk coordinates and extract final response
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cddm.getFinalState <- function(randomWalk.states){
  randomWalk <- randomWalk.states
  K <- nrow(randomWalk)
  I <- dim(randomWalk)[3]
  
  coord <- matrix(NA, ncol=2,nrow=I)
  for(i in 1:I){
    for(k in 1:K){
      if(!is.na(randomWalk[k,1,i])){
        a <- k
      }else{
        break
      }
    }
    coord[i,] <- randomWalk[a,,i]
  }
  return(coord)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulate data from the 4 parameters used to implement the cddm jags 
# module (with default values for the drift.Coefficient and dt)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Cartesian implementation
cddm.simData.cart <- function(trials, mu1, mu2, boundary, ndt=0.1, drift.Coeff=1, dt=0.0015){
  randomWalk <-  cddm.randomWalk(trials=trials,mu1=mu1,mu2=mu2,boundary=boundary,
                                 ndt=ndt,drift.Coeff=drift.Coeff,dt=dt)
  RT <- randomWalk$RT
  add.Iterations <- randomWalk$repeated.Walk
  randomWalk <- randomWalk$state
  coord <- cddm.getFinalState(randomWalk)
  degrees <- cddm.coordToDegrees(coord)
  radians <- cddm.degToRad(degrees)
  radians <- round(radians,4)
  
  data <- as.data.frame(cbind(radians,RT))
  colnames(data) <- c("Choice","RT")
  
  output <- list(data,add.Iterations)
  names(output) <- c("data","repeated.Walk")
  
  return(output)
}

### Polar coordinate implementation
cddm.simData <- function(trials, drift.Angle, drift.Length, boundary, ndt=0.1, drift.Coeff=1, dt=0.0015){
  Mu <- cddm.polarToRect(drift.Angle,drift.Length)
  mu1 <- Mu$mu1
  mu2 <-Mu$mu2
  output <- cddm.simData.cart(trials = trials,
                              mu1 = mu1, mu2 = mu2,
                              boundary = boundary,
                              ndt = ndt,
                              drift.Coeff = drift.Coeff,
                              dt = dt)
  return(output)
}




############################################################################
#####  Plotting functions
#####  Note: The margins of the plotting space may need to be adjusted to 
#####        fully appreciate the symmetry of the circle drawn on screen.
############################################################################
all.Angles <- seq(0,2*pi,0.001)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot the random walk (and RT distribution) from cddm.randomWalk()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cddm.plotRW <- function(randomWalk){
  state  <- randomWalk$state
  finalT <- randomWalk$RT
  trials <- length(finalT)
  choices <- cddm.getFinalState(state)
  boundary <- cddm.getVectorLength(choices[1,1],choices[1,2])
  boundary <- round(boundary,2)
  
  circle <- cddm.polarToRect(all.Angles,boundary)
  
  par(mfrow = c(1,2))  # Open space for 2 plots
  pm <- boundary+0.5 #Plot margin
  plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE,
       xlim=c(-pm,pm),ylim=c(-pm,pm))
  for(b in 1:trials){
    points(state[,,b], type = "l", col=rgb(1,0,0.5,0.1))
  }
  points(circle[,1],circle[,2], type="l")
  abline(h = 0, lty=2, col="gray50")
  abline(v = 0, lty=2, col="gray50")
  legend("topright",paste("No. trials =", trials), 
         pch=16, col="white",bty = "n", cex=0.8)
  for(b in 1:trials){
    points(choices[b,1],choices[b,2], type = "p", pch =16, cex=0.9,
           col=rgb(0.75,0.25,0.5,0.2))
  }
  
  maxRT <- max(finalT)+5
  x.axis <- round(c(0,seq(0,maxRT,length.out=10)),2)
  hist(finalT, col = "darkorchid4", breaks = 50, ann=FALSE, axes=FALSE)
  mtext("Response Times", 1, line=2, f=2)
  mtext("Frequency", 2, line = 2.5, cex=0.8)
  axis(2, seq(0,trials,5), seq(0,trials,5), las=2)
  axis(1, x.axis,x.axis)
  
  par(mfrow = c(1,1)) #As a precaution, go back to single plot spaces
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot  observed choices and RT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cddm.plotData <- function(randomWalk.bivariateData){
  choice <- randomWalk.bivariateData$Choice
  RT <- randomWalk.bivariateData$RT
  trials <- length(RT)
  
  direction <- cddm.radToDeg(choice) # Transform radian choices into degrees
  boundary <- 9 # Arbitrary radius, used to define magnitude
  circle <- cddm.polarToRect(all.Angles,boundary)
  magnitude <- rep(boundary,length(choice)) 
  coord.on.circumference <- cddm.polarToRect(choice,magnitude) #get Rectangular coordinates
  
  par(mfrow = c(1,2))  # Open space for 2 plots
  
  plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE)
  for(b in 1:trials){
    points(coord.on.circumference[b,1],coord.on.circumference[b,2], 
           type = "p", pch =16, cex=0.9,
           col=rgb(0.75,0.25,0.5,0.2))
  }
  points(circle[,1],circle[,2], type="l")
  abline(h = 0, lty=2, col="gray50")
  abline(v = 0, lty=2, col="gray50")
  legend("topright",paste("No. trials =", trials), 
         pch=16, col="white",bty = "n", cex=0.8)
  
  maxRT <- max(RT)+5
  x.axis <- round(c(0,seq(0,maxRT,length.out=10)),2)
  hist(RT, col = "darkorchid4", breaks = 50, ann=FALSE, axes=FALSE)
  mtext("Response Times", 1, line=2, f=2)
  mtext("Frequency", 2, line = 2.5, cex=0.8)
  axis(2, seq(0,trials,5), seq(0,trials,5), las=2)
  axis(1, x.axis,x.axis)
  
  par(mfrow = c(1,1)) #As a precaution, go back to single plot spaces
}

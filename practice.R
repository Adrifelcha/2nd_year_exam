nC <- 3
nT <- 300
nP <- 25
N <- nC*nP

a <- runif(N,1,2)
hist(a)
t <- rnorm(N,0.3,0.05)
hist(t)
v1 <- abs(rnorm(nP,1.2,0.3))
hist(v1)
v2 <- rnorm(nP,1.8,0.3)
v3 <- rnorm(nP,2.5,0.3)
v <- c(v1,v2,v3)

data <- c()
for(i in 1:N){
    this.part <- generate_dataset(a[i],v[i],t[i],nT)
    data <- rbind(data,this.part)
}

pID <- rep(1:(nP*nC), each=nT)
cID <- rep(1:nC, each=nT*nP)
data <- data.frame("sub" = pID, "cond" = cID, "rt" = data[,1], "accuracy" = data[,2])
write.csv(data, file = "./toyData.csv")


tapply(data$accuracy, data$cond, mean)
range(data$rt)
hist(data$rt)

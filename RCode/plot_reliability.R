
r.dd <- function(r.xx,r.yy,r.xy){
  a <- 1/2*(r.xx+r.yy)-r.xy
  b <- 1 - r.xy
  out <- a/b
  return(out)
}

r.dd <- function(r.xx,r.xy){
  a <- r.xx-r.xy
  b <- 1 - r.xy
  out <- a/b
  return(out)
}

r.space <- seq(0,1,0.01)

r.xx <- 0.9
r.yy <- r.xx
r.xy <- r.space


val1 <- 0.9
val2 <- 0.8
val3 <- 0.7

plot(r.space,r.dd(val1,r.space),type="l")

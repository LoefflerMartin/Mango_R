RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}
# mu is the drift
#set.seed(15)
P1<-RW(150,66,0,10)
P2<-RW(150,66,0,10)
plot(P1, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(0,100),
     typ='l', col="red")
par(new=T)  #to draw in the same plot
plot(P2, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(0,100),
     typ='l', col="blue")

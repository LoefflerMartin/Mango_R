RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}
# mu is the drift
set.seed(50)
P1<-RW(150,70,0,3)
set.seed(12)
P2<-RW(150,30,0,3)
plot(P1, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(0,100),
     typ='l', col="red")
par(new=T)  #to draw in the same plot
plot(P2, main="Random Walk without Drift", 
     xlab="t",ylab="Price", ylim=c(0,100),
     typ='l', col="blue")






ntrials=200
stepsize = 3
offset1=70 #maybe replace with random starting point?
offset2=30 #maybe replace with random starting point?
upper_limit=80
lower_limit=20

set.seed(10)
pos1 <- (cumsum(rnorm(ntrials))*stepsize )+offset1
pos1[pos1>upper_limit] = upper_limit;
pos1[pos1<lower_limit] = lower_limit;
set.seed(15)
pos2 <- (cumsum(rnorm(ntrials))*stepsize )+offset2
pos2[pos2>upper_limit] = upper_limit;
pos2[pos2<lower_limit] = lower_limit;
plot(seq_along(pos1), pos1,
     xlab="Step Number", ylab="Current Position", main = "random walks with gaussian distribution and upper/lower limit",
     type = "l", col="green",
     ylim = c(0,100), lwd = 4
)
lines(seq_along(pos2), pos2,
      type = "l", col="blue", lwd = 4
)
abline(h=lower_limit, col="red", lwd=2, lty=2)
abline(h=upper_limit, col="red", lwd=2, lty=2)

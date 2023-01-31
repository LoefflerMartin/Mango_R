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






# graph for illustration purpose (DO NOT USE LIKE THIS FOR ANYTHING SERIOUS!)
set.seed(12)
P1<-RW(500,50,0,3)
set.seed(12)
P2<-RW(500,0,0,40)
plot(P1, main="Random Walk without Drift", 
     xlab="t",ylab="Probability", ylim=c(-100,100), lwd=3,
     typ='l', col="red")
par(new=T)  #to draw in the same plot
plot(P2, main="Random Walk without Drift", 
     xlab="t",ylab="Probability", ylim=c(-100,100), lwd = 3,
     typ='l', col="blue")
dat <- data.frame(P1, P2, X = sort(rep(1:100, 5)))


library("ggplot2") 
cexaxistitle = 15
cexaxisticks = 12
cexpointsize = 4
l1 <- ggplot(dat, aes(X, P1)) +                                     
  geom_line(color = "dark green", size = 1) +
  geom_smooth(method = "gam", alpha = .2, col = "black", fill = "dark green", size = 0.5, level = .9999) +
  geom_line(aes(X, P2), color = "red", size = 1) +
  geom_smooth(aes(X, P2), method = "gam", alpha = .2, col = "black", fill = "red", size = 0.5, level = .999999999999999) +
  theme_classic() +
  labs(x ="Time", y = "Probability") +
  theme(
    axis.title.y = element_text(size = cexaxistitle),
    axis.title.x = element_text(size = cexaxistitle),
    axis.text = element_text(size = cexaxisticks)
  )
ggsave(
  filename = paste(dir_out, "random_walk_l1.png", sep = "/"),
  plot = l1,
  width = 6,
  height = 4
)




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

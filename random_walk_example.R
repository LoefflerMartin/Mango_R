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

# random walk with limits -------------------------------------------------


library(tidyverse)
library(patchwork)
library(ggpmisc)
rand_walk <- function(n=100, mu=0, var=3, lower=-Inf, upper=Inf, init=runif(1, lower, upper)) {
  message('+++ random walk +++')
  message('n: ', n)
  message('mu: ', mu)
  message('variance: ', var)
  message('lower limit: ', lower)
  message('upper limit: ', upper)
  message('init: ', init)
  message('walking...\n')
  cur_seed <- .Random.seed
  rw <- 
    Reduce(
      function(prev_val, cur_trial){
        cur_step <- rnorm(n=1, mean=mu, sd=sqrt(var))
        
        # if next step ends outside the limits take the same step size into the opposite direction
        # if (prev_val+cur_step>upper||prev_val+cur_step<lower)
        #   cur_step <- cur_step*-1
        
        # if next step ends outside the limits draw new step from Gaussian distribution that stays within limits
        while (prev_val+cur_step>upper||prev_val+cur_step<lower)
          cur_step <- rnorm(n=1, mean=mu, sd=sqrt(var))
        return(prev_val+cur_step)
      }, 
      x=2:n, 
      init = init,
      accumulate = T
    )
  attr(rw, 'seed') <- cur_seed
  return(rw)
}

set.seed(1060)
ntrial <- 100
drift = 0
variance <- 30
upper_limit <- 80
lower_limit <- 20

# set.seed(42)
rw1 <- rand_walk(ntrial, drift, variance, lower_limit, upper_limit)
rw2 <- rand_walk(ntrial, drift, variance, lower_limit, upper_limit)
rw.dat <- 
  rbind(
    data.frame(trial=1:length(rw1), P=rw1, walk='walk1'),
    data.frame(trial=1:length(rw2), P=rw2, walk='walk2')
  )

rw.dat %>%
  ggplot(aes(x=trial, y=P)) +
  xlab('Trial') + ylab('Probability') +
  geom_line(aes(color=walk)) + 
  geom_hline(yintercept = upper_limit, linetype = 2) +
  geom_hline(yintercept = lower_limit, linetype = 2) +
  coord_cartesian(ylim=c(0, 100)) + 
  theme_classic() + theme(legend.position = 'bottom') 


# now let's create "a bunch of" (say 60) random walks that at least fullfill the followinf criteria
# * at least 1 crossing of walks
# * correlation of walks < 0

rw.list <- list()
ntrial <- 100
drift = 0
variance <- 30
upper_limit <- 80
lower_limit <- 20
current_seed <- 42
set.seed(current_seed)
while (length(rw.list)<60) {
  message('seed: ', current_seed)
  set.seed(current_seed)
  rw1 <- rand_walk(ntrial, drift, variance, lower_limit, upper_limit)
  rw2 <- rand_walk(ntrial, drift, variance, lower_limit, upper_limit)
  rw.dat <- 
    data.frame(trial = 1:ntrial, prob1 = rw1, prob2 = rw2) %>%
     mutate(
      step1 = prob1-lag(prob1), # actual step taken in a given trial
      step2 = prob2-lag(prob2),
      prob_diff = prob1-prob2, # difference between probabilities of the 2 walks
      prob_diff_sign=prob_diff/abs(prob_diff), # sign of that difference
      prob_diff_sign=case_when(is.nan(prob_diff_sign)~lag(prob_diff_sign), T~prob_diff_sign),
      crossed = prob_diff_sign!=lag(prob_diff_sign), # did the walks cross, i.e. did the sign change?
      crossed2 = prob_diff_sign!=lag(prob_diff_sign) & abs(lead(prob_diff))>abs(prob_diff), # did the sign change and does this change increase in the next trial?
      crossed3 = prob_diff_sign!=lag(prob_diff_sign) & prob_diff_sign == lead(prob_diff_sign, n=3), # did the sign change and stay changed for at least 3 trials?
      cor = cor.test(prob1, prob2)$estimate, # correlation between walks
      seed = current_seed # store current seed
      )
  n_crossings <- length(which(rw.dat$crossed))
  cor <- rw.dat$cor[1]
  current_seed <- current_seed + 1 
  if (n_crossings>0 && cor<0) {
    message('n_crossings: ', n_crossings)
    message('cor: ', cor)
    message('')
    rw.dat$random_walk_no <- length(rw.list)+1
    rw.list[[length(rw.list)+1]] <- rw.dat
  }
}

rw.plots <- 
  rw.list %>%
  map(
    function(rw.d){
      rw.no <-rw.d$random_walk_no[1]
      rw.cor <- rw.d$cor[1]
      rw.crossings <- length(which(rw.d$crossed))
      rw.mad <- mean(abs(rw.d$prob_diff))
      rw.d %>%
        pivot_longer(c(prob1, prob2), names_to = 'walk', values_to = 'P') %>%
        ggplot(aes(x=trial, y=P)) +
        xlab('Trial') + ylab('Probability') + 
        ggtitle(paste0('random walk #', rw.no), subtitle = paste0('correlation: ', round(rw.cor, 2), ' crossings: ', rw.crossings, ', mean abs diff: ', round(rw.mad, 2))) +
        geom_line(aes(color=walk)) + 
        geom_hline(yintercept = upper_limit, linetype = 2) +
        geom_hline(yintercept = lower_limit, linetype = 2) +
        geom_vline(aes(xintercept=trial), data=rw.d%>%filter(crossed), linetype=2, color='darkgrey') +
        coord_cartesian(ylim=c(0, 100)) + 
        theme_classic() + theme(legend.position = 'bottom')
      }
    )

rw.plots[1:12] %>%wrap_plots(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom')
rw.plots[13:24] %>%wrap_plots(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom')
rw.plots[25:36] %>%wrap_plots(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom')
rw.plots[37:48] %>%wrap_plots(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom')
rw.plots[49:60] %>%wrap_plots(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom')

okayish <- c(2, 3, 5, 14, 16, 23, 24, 32, 51, 54, 60)
rw.plots[okayish] %>%wrap_plots(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom')
chosen <- c(3, 16, 32)

# save to file that can be fed into Presentation script for 2-armed bandit task
out_tbl <- 
  rw.list[chosen] %>%
  bind_rows() %>%
  select(random_walk_no, trial, prob1, prob2) %>% 
  mutate(across(everything(), ~round(.x, 0)))
write.csv2(out_tbl, file.path(getwd(), 'random_walks_3_n100.csv'), row.names = F)

rw.pl <- rw.plots[chosen] %>%wrap_plots(ncol = 1, guides = 'collect') & theme(legend.position = 'bottom')
out_tbl.sum <- 
  rw.list[chosen] %>%
  bind_rows() %>%
  # mutate(across(is.numeric(), ~round(.x, 0))) %>%
  group_by(random_walk_no) %>%
  filter(!is.na(step1)) %>%
  summarise(
    variance1=var(step1),
    sd1=sd(step1),
    variance2=var(step2),
    sd2=sd(step2),
    cor = first(cor),
    mean_abs_diff = mean(abs(prob_diff)),
    n_crossings = length(which(crossed)),
    n_crossings2 = length(which(crossed2)),
    n_crossings3 = length(which(crossed3))
  ) 
rw.sum <- 
  ggplot() + 
  theme_void() +
  annotate(
    geom='table',
    x = 1, y = 1,
    label=list(out_tbl.sum%>%select(1:8)%>%mutate(across(where(is.numeric), ~round(.x, 2))))
  )

(out_tbl.sum.plot <- (rw.pl) / rw.sum)
ggsave(file.path(getwd(), 'random_walks_3_n100.png'), out_tbl.sum.plot)

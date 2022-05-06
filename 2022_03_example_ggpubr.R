library(tidyverse)
library(ggpubr)


n_subjects = 10
n_trials = 100

DAT = data.frame(ID=numeric(), trial=numeric(), choice=numeric(), reward = numeric(), reaction_time=numeric())
for (ID in 1:n_subjects){
  set.seed(ID)
  dat = data.frame(
    ID = ID,
    trial = seq(1,n_trials),
    choice = sample(1:2,n_trials,replace=T),
    reward = sample(0:1,n_trials,replace=T),
    reaction_time = rnorm(n = n_trials, mean = 100 + (ID*10), sd = 10))
  DAT = rbind(DAT, dat)
}


DAT$ID = as.factor(DAT$ID)
ggdensity(DAT, x = "reaction_time",
          #add = "mean", rug = TRUE,
          color = "ID", fill = "ID")


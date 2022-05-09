# based on: https://z3tt.github.io/Rainclouds/
packs = c("ggplot2", "ggforce", "ggdist", "gghalves", "ggbeeswarm")
if (length(setdiff(packs, rownames(installed.packages()))) > 0) {               # check if packages are installed
  install.packages(setdiff(packs, rownames(installed.packages())))              # if packages are not installed install them
}
lapply(packs, require, character.only = TRUE) 
rm(packs)

# set a general theme for all plots
theme_set(theme_light(base_size = 16))

# most basic version to depict the data
ggplot(iris, aes(Species, Sepal.Width)) + 
  geom_boxplot(width = .2)

# add distribution: violin plots
g1 <- 
  ggplot(iris, aes(Species, Sepal.Width)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA)                                  # note that we remove outliers here for the next steps
g1

# add raw data: geom_point/geom_jitter/geom_beeswarm
g1 + geom_point(position = position_jitter(seed = 1))                           # kind of chaotic still and hard to read because of the wide distribution
g1 + geom_point(alpha = .4, position = position_jitter(seed = 1, width = .1), size = 3) # order graph by playing with width and size. alpha for transparency
g1 + ggbeeswarm::geom_beeswarm(alpha = .4, cex = 1.2, size = 3)                 # another stacked way with a different package


# raincloud plots: combination of boxplot, raw data and distribution
# version 1
ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) +           # distribution plus interval (based on min-max)
  ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.05, binwidth = .1) # add raw data

# version 2
ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .3, justification = 1.1, binwidth = .1)

# version 3
ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .7, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  geom_jitter(width = .05, alpha = .3)                                          # another way to add raw data

# version 4
ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .4, size = 2) # another way to add raw data




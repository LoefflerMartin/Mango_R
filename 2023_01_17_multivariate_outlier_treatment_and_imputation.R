# collection of options to visualize outliers, identify and impute them.
# ML 2023-01-17
# see also: https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/
# and here: https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html

rm(list = ls()) #clear workspace

#install.packages("dplyr")
library(dplyr)
data <- starwars
data <- data %>% dplyr::select(!c(films, vehicles, starships))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                       as.factor)


########### a quick way to see
# 1) how many missings the dataset has per variable (left)
# 2) which variables are missing in the same subjects (right)
# 3) how many subjects have complete data (right)
#install.packages("VIM")
library(VIM)
aggr_plot <-
  aggr(
    data,
    col = c('navyblue', 'red'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(data),
    cex.axis = 1,
    cex.lab = 2,
    cex.numbers = 1.3,
    gap = 1,
    ylab = c("Histogram of missing data", "Pattern")
  )


# do something similar with finalfit
#install.packages("finalfit")
library(finalfit)
explanatory = c("mass", "birth_year", "gender", "hair_color")
dependent = "height"
data %>% missing_pattern(dependent, explanatory)


########### a quick way to visualize if variables are missing at random
marginplot(data[names(data) %in% c( "birth_year", "mass")]) #red is the distribution of data where the other variable is missing

# now do something similar for multiple variables
data %>% missing_pairs(dependent, explanatory, position = "fill")

# now test if missings in a set of variables (-> explanatory) depends on missings in one other variable (->dependent)
explanatory = c("mass", "height", "birth_year", "hair_color")
dependent = "gender" # here refers only to the variable being tested for missingness against the explanatory variables
data %>% missing_compare(dependent, explanatory)

# if you want to use the Little's MCAR Test on the whole dataset (of interest)
#install.packages("misty")
library(misty)
data %>% dplyr::select(c(dependent, explanatory)) %>% na.test()




########### a quick way to define outliers based on a given model

# THE MODEL that cooks distance shall be based on
mod <- lm(height ~ mass+birth_year, data = data)
cooksd <- cooks.distance(mod)

# Plot the Cook's Distance using the traditional 4/n criterion (attention, there are different opinions on the cutoff for Cook's distance)
sample_size <- nrow(data)
dev.off()
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

# alternative wrapper for plotting
#install.packages("olsrr")
library(olsrr)
ols_plot_cooksd_bar(mod)

# REMOVE Outliers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) # influential row numbers
data_screen <- data[-influential, ]




########### a quick way to do multiple imputation using mice package
# setting the seed is important to have reproducible results.
# m sets the number of imputed datasets that are being generated.
# Ideally, analyses should be carried out on multiple datasets and then pooled.
#install.packages("mice")
library(mice)
imp_mult <- mice(data_screen,m=5,maxit=50,meth='pmm',seed=500)
imp_mult50 <- mice(data_screen,m=50,maxit=50,meth='pmm',seed=500)

# tell me what was done with mice.
summary(imp_mult)

# give me a specific dataset and save it to a new dataframe
imp_1 <- complete(imp_mult,1)

# plot distribution of observed vs. imputed data
densityplot(imp_mult)

# check if the relationship between different variables differs between observed and imputed data (here for "Ozone")
mice::xyplot(imp_mult,height ~ mass+birth_year+gender,pch=18,cex=1) # blue=observed, red=imputed



########### Now do an analysis and use pooled information
# first how we would do the analysis with one dataframe
modelFit1 <- with(imp_1,lm(height ~ mass+birth_year)) # --> is the same as: modelFit1 <- lm(height ~ mass+birth_year, data = imp_1)
summary(modelFit1)

# now do the same analysis on all dataframes at the same time and pool the results
modelFit_mult <- with(imp_mult,lm(height ~ mass+birth_year))
summary(pool(modelFit_mult))

# just for playing around: do the same with more dataframes to see if results stay robust.
modelFit_mult50 <- with(imp_mult50,lm(height ~ mass+birth_year))
summary(pool(modelFit_mult50))

# and compare it to the results without imputation
modelFit_raw <- with(data_screen,lm(height ~ mass+birth_year)) # --> is the same as: modelFit1 <- lm(height ~ mass+birth_year, data = imp_1)
summary(modelFit_raw)



# check some indices in the pooled analysis:
# fmi: fraction of missing information
# lambda: proportion of total variance that is attributable to the missing data. 
pool(modelFit_mult50)



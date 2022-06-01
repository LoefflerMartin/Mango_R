library(conflicted)

#####################################################################################################################
# measure runtimes, see https://www.r-bloggers.com/2017/05/5-ways-to-measure-running-time-of-r-code/
# base 1
start_time <- Sys.time()
Sys.sleep(0.5)
end_time <- Sys.time()
end_time - start_time

# base 2
# verstrichen/elapsed: time taken; system and User represent CPU runtimes of user and system on the process(es)
system.time({
  Sys.sleep(0.55)
})



# tictoc
library(tictoc)
tic("sleeping")
Sys.sleep(0.5)
toc()

tic("total")
tic("data generation")
X <- matrix(rnorm(5000*100), 5000, 100)
b <- sample(1:100, 100)
y <- runif(1) + X %*% b + rnorm(5000)
toc()
tic("model fitting")
model <- lm(y ~ X)
toc()
toc()


# rbenchmark
library(rbenchmark)
# elapsed: time taken; system and User represent CPU runtimes of user and system on the process(es)
# relative is time relative to fastest process (useful when comparing different code versions)

benchmark("lm" = {
  X <- matrix(rnorm(1000), 100, 10)
  y <- X %*% sample(1:10, 10) + rnorm(100)
  b <- lm(y ~ X + 0)$coef
},
"pseudoinverse" = {
  X <- matrix(rnorm(1000), 100, 10)
  y <- X %*% sample(1:10, 10) + rnorm(100)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
},
"linear system" = {
  X <- matrix(rnorm(1000), 100, 10)
  y <- X %*% sample(1:10, 10) + rnorm(100)
  b <- solve(t(X) %*% X, t(X) %*% y)
},
replications = 1000,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))





#####################################################################################################################
# lubridate to handle time variables
library(lubridate)
my.date <- as.Date("2022-06-08")
inclusion.date <- as.Date("2022-01-27")
class(inclusion.date)

month(inclusion.date)
year(inclusion.date)
wday(inclusion.date, label = T)

int <- interval(inclusion.date, my.date)
int

time_length(int, "day")
time_length(int, "month")
time_length(int, "year")


# Time zones
my.time <- ymd_hms("2022-06-08 10:30:00")
force_tzs(my.time, tzones = "Europe/Amsterdam", tzone_out = "America/Los_Angeles")

# character to date if not in standard format
dates <- c("01/27/92", "01/27/92", "03/14/92", "03/28/92", "03/01/92")
times <- c("22:03:20", "21:29:56", "03:03:30", "10:21:03", "10:56:26")
x <- paste(dates, times)
x

time.vec <- strptime(x, "%m/%d/%y %H:%M:%S")
time.vec
int <- interval(time.vec[1], time.vec[2])
time_length(int, "minutes")
time_length(int, "seconds")





# collection of options to change the scales in your plot with the "scales" packages from the tidyverse
# ML 2022-06-22

library(tidyverse)
#library(scales)

# 1) dealing with numeric axes
# create some random plot we can play with
p <- ggplot(mpg, aes((displ-5)*10000, cty)) + geom_point()
p

# now play around with axes 
p + scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ','))
p + scale_x_continuous(
  labels = scales::comma_format(big.mark = '´',
                                decimal.mark = '.',
                                accuracy = 0.01))

p + scale_y_continuous(breaks = breaks_width(width = 1))
p + scale_x_continuous(breaks = breaks_pretty())
p + scale_x_continuous(breaks = breaks_extended())
p + scale_y_log10(n.breaks = 10)
p + scale_y_log10(n.breaks = 10, labels = scales::number_format(accuracy = 0.01, decimal.mark = ','))


# 2) dealing with date format axes
# partly taken from: https://ggplot2.tidyverse.org/reference/scale_date.html
# create random data and plot
last_month <- Sys.Date() - 0:29
df <- data.frame(date = last_month, price = runif(30))
p <- ggplot(df, aes(date, price)) + geom_line()
p

# now play around with date axis
p + scale_x_date(date_breaks = "1 week", date_labels = "%W") # Kalenderwoche
p + scale_x_date(date_breaks = "week", date_minor_breaks = "1 day") # change background lines
p + scale_x_date(date_breaks = "2 weeks", labels = date_format("%m/%d")) # moth and day


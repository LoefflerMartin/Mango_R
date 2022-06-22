# collection of options to change the scales in your plot with the "scales" packages from the tidyverse
# ML 2022-06-22

library(tidyverse)
#library(scales)
p <- ggplot(mpg, aes((displ-5)*10000, cty)) + geom_point()
p
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

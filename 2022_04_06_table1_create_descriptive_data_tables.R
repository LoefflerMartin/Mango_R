#install.packages("table1")
#library(tidyverse)
#library(textreadr)
#library(rvest)

#####################
### creating descriptive data table with table 1
# example script
#####################
library(table1)

# base version
tbl1 = table1::table1( ~ Sepal.Length + Sepal.Width + Petal.Length |
                         Species, data = iris)
tbl1


# individualize:
my.render.cont <- function(x) {
  with(
    stats.default(x),
    sprintf(
      "M±SD: %0.2f±%0.2f; MED(IQR): %0.2f (%0.2f-%0.2f) ",
      MEAN,
      SD,
      MEDIAN,
      Q1,
      Q3
    )
  )
}
tbl1 = table1::table1(
  ~ Sepal.Length + Sepal.Width + Petal.Length |
    Species,
  data = iris,
  render.continuous = my.render.cont
)
tbl1
#read_html(tbl1)
#write.table(x = t[[1]], file = "table.txt", sep = "\t", row.names = F, col.names = T, quote = T)

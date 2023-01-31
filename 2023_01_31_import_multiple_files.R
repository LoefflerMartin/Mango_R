# use purrr::map_dfr to import multiple files at the same time and merge into one dataframe.
# ML 2023-01-31
# based on: 


rm(list = ls())

library(tidyverse)
library(purrr)

dir_data<- "D:/Dokumente_Martin/Programmierung/R/github_repositories/PRIMA_Study2_Testdata"





# Version 1: "by foot"
input_files <- list.files(path = dir_data, pattern = "*.log", full.names = T) #find all files that we want (also works recursive with pattern if not in the same folder)

d1 <- read_delim(input_files[1]) #create dataframe based on the first sample
d1 <- d1[0,] #empty that dataframe

for(i in input_files){ # 
  data <- read_delim(i)
  d1 <- rbind(d1,data)
}



# Version 2: the same as above but easier
d2 <- fs::dir_ls(path = dir_data, glob = "*.log") %>% 
  purrr::map_dfr(readr::read_delim, .id = "source") # the "source" argument is optional


# Version 3: if you want to skip a specific file
d3 <- fs::dir_ls(path = dir_data, glob = "*.log") %>% 
  stringr::str_subset(., "participant_PRIMA2_057|participant_PRIMA2_050", negate = TRUE) %>% # just use the pattern you want to exclude (also multiple with "|" seperator)
  purrr::map_dfr(readr::read_delim, .id = "source") # the "source" argument is optional


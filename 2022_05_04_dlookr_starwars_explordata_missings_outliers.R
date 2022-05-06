library(tidyverse)
starwars <- dplyr::starwars
#str(starwars)
starwars <- starwars[!names(starwars) %in% c("films", "vehicles", "starships")]

# give me a general overview on the dataset
dlookr::overview(starwars)
dlookr::diagnose_numeric(starwars)

# give me an overview on missing data
dlookr::diagnose(starwars)
starwars$birth_year <- dlookr::imputate_na(starwars, birth_year, method = "median")

# give me an overview on outliers
dlookr::diagnose_outlier(starwars)
dlookr::plot_outlier(starwars)
starwars$height <- dlookr::imputate_outlier(starwars, height)
#dlookr::find_outliers(starwars, rate = T)

# wrap up the whole thing and export
dlookr::diagnose_report(starwars, output_format = "html")






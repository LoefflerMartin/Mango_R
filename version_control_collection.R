install.packages("renv")
install.packages("usethis")
library(usethis)
use_git_config(user.name = "LoefflerMartin", user.email = "martin_loeffler@gmx.net")

usethis::create_github_token()

gitcreds::gitcreds_set()


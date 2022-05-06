install.packages("renv")
install.packages("usethis")
library(usethis)
use_git_config(user.name = "LoefflerMartin", user.email = "martin_loeffler@gmx.net")

usethis::create_github_token()
"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"
gitcreds::gitcreds_set()


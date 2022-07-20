# How to share an R-Project including its libraries (with the versions used)
# see https://www.rstudio.com/blog/renv-project-environments-for-r/

install.packages("renv")
renv::init() # initialize a project with package management (not necessary if you already have a project)
renv::activate() # activate renv for your current project
renv::snapshot() # create a lockfile for current porject
renv::restore() # use existing lockfile to restore packages and dependencies
renv::history() # find prior versions of the lockfile


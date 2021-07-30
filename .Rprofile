## Set CRAN Mirror:
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org/"
  options(repos = r)
})

## Create hidden environment
.env <- new.env()

## Define new q() function
.env$q <- function(save = "no", ...) {

  quit(save = save, ...)

}

## Attach hidden environment
attach(.env, warn.conflicts = FALSE)

local({
  n <- parallel::detectCores()                                # Detect number of CPU cores
  options(Ncpus = n)                                          # Parallel package installation in install.packages()
  options(mc.cores =  n)                                      # Parallel apply-type functions via 'parallel' package
})

options(digits = 15)                                          # Number of digits to print. Default is 7, max is 15
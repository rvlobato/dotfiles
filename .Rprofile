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
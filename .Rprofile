options(repos = c(
  MPN = "https://mpn.metworx.com/snapshots/stable/2021-02-01", 
  # a value must be set to CRAN or R will complain, so we'll point both to MPN
  # renv/packrat do some funkiness with the manifest where they look for older sys dep versions
  # that need to fallback, so need regular cran with its complete archive to find with
  # publishing sometimes
  CRAN = "https://cran.rstudio.com"
  # set some rbabylon opinionated defaults, these won't impact even if you don't use rbabylon
)
)

# Run renv/activate.R after setting repos to bootstrap from MPN (if needed)
source("renv/activate.R")

if(interactive()){
  message("repos set to: \n\t", paste0(unique(getOption('repos')), collapse = "\n\t"))
  message("library paths set to: \n\t", paste0(.libPaths(), collapse = "\n\t"))
  
}
